;;; -*- Lisp -*-
;;; rate-limiter.lisp - thread-safe sliding-window rate limiter for API backends

(in-package "CHATBOT")

(defclass rate-limiter ()
  ((max-rpm :initarg :max-rpm :type integer :accessor max-rpm-of :initform 60)
   (max-tpm :initarg :max-tpm :type integer :accessor max-tpm-of :initform 100000)
   (request-history :type list :accessor request-history-of :initform '())
   (token-history :type list :accessor token-history-of :initform '())
   (lock :reader lock-of :initform (sb-thread:make-mutex :name "rate-limiter-lock"))))

(defun prune-rate-limiter-history (limiter now)
  "Remove timestamps and token entries older than 60 seconds from the rate limiter's history."
  (let* ((cutoff (- now 60))
         (new-request-history (remove-if (lambda (time) (<= time cutoff)) (request-history-of limiter)))
         (new-token-history (remove-if (lambda (entry) (<= (car entry) cutoff)) (token-history-of limiter))))
    (setf (request-history-of limiter) new-request-history
          (token-history-of limiter) new-token-history)))

(defun acquire-rate-limit-slot (limiter estimated-tokens)
  "Acquire a rate limit slot, pruning history and checking RPM and TPM limits.
Thread-safe using SB-THREAD:WITH-MUTEX."
  (when limiter
    (sb-thread:with-mutex ((lock-of limiter))
      (loop
        (let* ((now (get-universal-time))
               (_ (prune-rate-limiter-history limiter now))
               (current-rpm (length (request-history-of limiter)))
               (current-tpm (reduce #'+ (token-history-of limiter) :key #'cdr :initial-value 0)))
          (declare (ignore _))
          (cond
            ((and (max-rpm-of limiter)
                  (>= current-rpm (max-rpm-of limiter)))
             (let ((wait-seconds (max 1 (- (+ (car (last (request-history-of limiter))) 60) now))))
               (log-message :info (format nil "[RATE-LIMITER] RPM limit reached (~A/~A). Throttling for ~A seconds..."
                                          current-rpm (max-rpm-of limiter) wait-seconds))
               (sleep wait-seconds)))
            
            ((and (max-tpm-of limiter)
                  (> (+ current-tpm estimated-tokens) (max-tpm-of limiter)))
             (let ((wait-seconds (max 1 (- (+ (car (last (token-history-of limiter))) 60) now))))
               (log-message :info (format nil "[RATE-LIMITER] TPM limit reached (~A + ~A > ~A). Throttling for ~A seconds..."
                                          current-tpm estimated-tokens (max-tpm-of limiter) wait-seconds))
               (sleep wait-seconds)))
            
            (t
             (push now (request-history-of limiter))
             (push (cons now estimated-tokens) (token-history-of limiter))
             (return t)))))))))

(defun make-gemini-rate-limiter (&key (max-rpm 60) (max-tpm 100000))
  "Constructor for a rate-limiter with specified RPM and TPM limits."
  (make-instance 'rate-limiter :max-rpm max-rpm :max-tpm max-tpm))
