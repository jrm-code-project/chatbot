;;; -*- Lisp -*-
;;; agentic-loop-monitor.lisp - autonomous background loop watchdogs and global monitor threads

(in-package "CHATBOT")

(defvar *agentic-loop-monitor-active* nil
  "Flag indicating whether the background monitor thread is currently active.")
(defvar *agentic-loop-monitor-thread* nil
  "The active background thread supervising the monitor loop.")
(defvar *agentic-loop-monitor-lock* (sb-thread:make-mutex :name "agentic-loop-monitor-lock"))

(defparameter *agentic-loop-monitor-log-level* :warn
  "Configured minimum log-level threshold used by the monitor thread context.")

(defun make-agentic-loop-monitor-runtime-context (&optional template-context)
  "Returns the runtime context used by the background monitor thread."
  (runtime-context-with-logging-settings template-context
                                         :log-level *agentic-loop-monitor-log-level*))

(defun monitor-agentic-loops-once (&optional context)
  "Scans all registered loops and pushes stuck, pending, or zombie loops into valid states."
  (let ((now (get-high-precision-timestamp)))
    (dolist (loop (list-agentic-loops context))
      (let ((status (agentic-loop-status loop))
            (alive (agentic-loop-thread-alive-p loop)))
      (cond
        ;; 1. Stuck in :pending (registered but worker thread never spawned/started)
        ((eq status :pending)
         (when (agentic-loop-watchdog-backoff-elapsed-p loop now)
           (log-message :info (format nil "Monitor: Spawning pending loop ~A" (agentic-loop-id loop)))
           (setf (agentic-loop-supervisor-restart-not-before loop) nil)
           (spawn-agentic-loop-thread loop)))

        ;; 2. Timed-out running steps are killed and restarted under watchdog policy.
        ((agentic-loop-watchdog-timeout-expired-p loop now)
         (log-message :warn (format nil "Monitor: Detected timed-out loop ~A" (agentic-loop-id loop)))
         (ensure-agentic-loop-watchdog-restart
          loop
          (format nil "Watchdog timeout after ~,2F seconds."
                  (- now (agentic-loop-active-step-started-at loop)))
          :terminate-thread-p t))

        ;; 3. Zombie state: status expects a worker thread, but the worker is gone.
        ((and (agentic-loop-live-status-p status) (not alive))
         (log-message :warn (format nil "Monitor: Detected zombie loop ~A (status: ~A, thread dead)"
                                    (agentic-loop-id loop) status))
         (ensure-agentic-loop-watchdog-restart loop
                                               "Worker thread terminated unexpectedly."))

        ;; 4. Retry failed loops under watchdog policy instead of leaving them dead immediately.
        ((eq status :failed)
         (when (agentic-loop-watchdog-restart-allowed-p loop)
           (ensure-agentic-loop-watchdog-restart loop
                                                 (or (agentic-loop-last-error loop)
                                                     "Loop failed unexpectedly."))))

        ;; 5. Invalid/unrecognized states that are not completed or aborted should be pushed to failed.
        ((not (member status '(:pending :running :awaiting-approval :completed :failed :limit-reached :interrupted)))
         (log-message :error (format nil "Monitor: Detected loop ~A in invalid state: ~A. Aborting."
                                     (agentic-loop-id loop) status))
         (mark-agentic-loop-supervisor-failed loop
                                              (format nil "Unrecognized loop status: ~A" status))))))))

(defparameter *reaper-interval-seconds* 600
  "Interval in seconds between thread and memory reaper sweeps (default 10 minutes).")

(defvar *last-reaper-execution-time* 0
  "Timestamp of the last thread and memory reaper sweep.")

(defun reap-orphaned-threads-and-sockets ()
  "Garbage-collects terminal loops from the registry and terminates orphaned background threads."
  (let ((all-threads (sb-thread:list-all-threads)))
    (sb-thread:with-mutex (*agentic-loop-registry-lock*)
      (let ((ids-to-remove nil))
        (maphash (lambda (id loop)
                   (let ((status (agentic-loop-status loop))
                         (finished (agentic-loop-finished-at loop)))
                     (when (and (member status '(:completed :failed :limit-reached :interrupted))
                                finished
                                (> (- (get-high-precision-timestamp) finished) 300))
                       (push id ids-to-remove))))
                 *agentic-loop-registry*)
        (dolist (id ids-to-remove)
          (let ((loop (gethash id *agentic-loop-registry*)))
            (log-message :info
                         (format nil "Reaper: Pruning completed loop ~A from registry." id))
            (when loop
              (remove-runtime-worker-entry
               (format nil "loop:~A" id)
               (agentic-loop-runtime-context loop)))
            (remhash id *agentic-loop-registry*)))))
    (dolist (thread all-threads)
      (let ((name (sb-thread:thread-name thread)))
        (when (and name
                   (alexandria:starts-with-subseq "Agentic-Loop-Worker-" name))
          (let* ((id-str (subseq name (length "Agentic-Loop-Worker-")))
                 (id (parse-integer id-str :junk-allowed t))
                 (loop (and id
                            (sb-thread:with-mutex (*agentic-loop-registry-lock*)
                              (gethash id *agentic-loop-registry*)))))
            (when (and id
                       (or (null loop)
                           (agentic-loop-terminal-status-p (agentic-loop-status loop))))
              (log-message :warn
                           (format nil "Reaper: Terminating orphaned worker thread: ~A" name))
              (handler-case
                  (sb-thread:terminate-thread thread)
                (error ()
                  nil)))))))))

(defun run-agentic-loop-monitor ()
  "The execution loop for the background monitor."
  (loop
    while *agentic-loop-monitor-active*
    do (handler-case
           (progn
             (monitor-agentic-loops-once)
             ;; Run reaper sweep if interval has elapsed
             (let ((now (get-high-precision-timestamp)))
               (when (>= (- now *last-reaper-execution-time*) *reaper-interval-seconds*)
                 (setf *last-reaper-execution-time* now)
                 (reap-orphaned-threads-and-sockets)))
             (sleep 5))
         (error (condition)
           (log-message :error (format nil "Agentic loop monitor error: ~A" condition))
           (sleep 5)))))

(defun start-agentic-loop-monitor ()
  "Starts the background monitor thread."
  (sb-thread:with-mutex (*agentic-loop-monitor-lock*)
    (unless *agentic-loop-monitor-active*
      (let ((monitor-context
              (make-agentic-loop-monitor-runtime-context
               (or (resolve-runtime-context nil)
                   *default-runtime-context*))))
        (setf *agentic-loop-monitor-active* t)
        (setf *agentic-loop-monitor-thread*
              (sb-thread:make-thread
               (lambda ()
                 (call-with-runtime-context monitor-context
                                            #'run-agentic-loop-monitor
                                            :default-conversation-compatibility-p nil
                                            :legacy-function-seam-compatibility-p nil))
               :name "Agentic-Loop-Monitor"))
        (log-message :info "Agentic loop monitor started."))))
  *agentic-loop-monitor-thread*)

(defun stop-agentic-loop-monitor ()
  "Stops the background monitor thread."
  (sb-thread:with-mutex (*agentic-loop-monitor-lock*)
    (when *agentic-loop-monitor-active*
      (setf *agentic-loop-monitor-active* nil)
      (let ((thread *agentic-loop-monitor-thread*))
        (when (and thread (sb-thread:thread-alive-p thread))
          (sb-thread:join-thread thread :timeout 5)
          (when (sb-thread:thread-alive-p thread)
            (sb-thread:terminate-thread thread))))
      (setf *agentic-loop-monitor-thread* nil)
      (log-message :info "Agentic loop monitor stopped.")))
  t)
