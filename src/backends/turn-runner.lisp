;;; -*- Lisp -*-
;;; turn-runner.lisp - shared provider turn recursion orchestration

(in-package "CHATBOT")

(defun make-provider-turn-final-outcome (text &rest metadata)
  "Returns a normalized final-text turn outcome."
  (append (list :kind :final
                :text text
                :usage (getf metadata :usage)
                :thought-text (getf metadata :thought-text))
          metadata))

(defun make-provider-turn-tool-outcome (tool-calls &rest metadata)
  "Returns a normalized tool-calling turn outcome."
  (append (list :kind :tools
                :tool-calls tool-calls)
          metadata))

(defun make-provider-turn-retry-outcome (&rest metadata)
  "Returns a normalized retry outcome."
  (append (list :kind :retry)
          metadata))

(defun provider-turn-outcome-kind (outcome)
  "Returns OUTCOME's normalized kind."
  (getf outcome :kind))

(defun provider-turn-outcome-tool-calls (outcome)
  "Returns OUTCOME's normalized tool call list."
  (getf outcome :tool-calls))

(defun provider-turn-outcome-text (outcome)
  "Returns OUTCOME's final text."
  (getf outcome :text))

(defun provider-turn-outcome-usage (outcome)
  "Returns OUTCOME's usage metadata."
  (getf outcome :usage))

(defun provider-turn-outcome-thought-text (outcome)
  "Returns OUTCOME's thought text, when any."
  (getf outcome :thought-text))

(defun run-provider-turn-loop (backend initial-state submit-turn
                               &key continue-with-tools retry-turn finalize-turn error-handler
                                 (initial-recursion-depth 0))
  "Runs the shared provider turn loop for BACKEND starting from INITIAL-STATE.

SUBMIT-TURN must return one of the normalized provider-turn outcomes. CONTINUE-WITH-TOOLS,
RETRY-TURN, and FINALIZE-TURN receive the current STATE plus the normalized outcome and
either recurse through the supplied STEP function or return the final turn result."
  (labels ((run-step (state recursion-depth)
             (ensure-chatbot-tool-recursion-depth backend recursion-depth)
             (handler-case
                 (let* ((start-time (get-internal-real-time))
                        (outcome (funcall submit-turn state recursion-depth))
                        (end-time (get-internal-real-time))
                        (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
                   (when *last-interaction-model-call-duration*
                     (incf *last-interaction-model-call-duration* duration))
                   (case (provider-turn-outcome-kind outcome)
                     (:retry
                      (unless retry-turn
                        (error "Provider ~A returned a retry outcome without a retry handler." backend))
                      (funcall retry-turn state outcome recursion-depth #'run-step))
                     (:tools
                      (unless continue-with-tools
                        (error "Provider ~A returned tool calls without a continuation handler." backend))
                      (let ((next-depth (next-chatbot-tool-recursion-depth backend recursion-depth)))
                        (funcall continue-with-tools state outcome next-depth #'run-step)))
                     (:final
                      (unless finalize-turn
                        (error "Provider ~A returned a final outcome without a finalizer." backend))
                      (funcall finalize-turn state outcome))
                     (t
                      (error "Unknown provider turn outcome kind: ~S" (provider-turn-outcome-kind outcome)))))
               (agentic-loop-interrupted (condition)
                 (error condition))
               (chatbot-tool-recursion-limit-error (condition)
                 (error condition))
               (error (condition)
                 (if error-handler
                     (funcall error-handler state condition recursion-depth)
                     (error condition))))))
    (run-step initial-state initial-recursion-depth)))

(defun provider-tool-call-results (bot tool-calls context-builder result-builder &key error-builder)
  "Executes normalized TOOL-CALLS for BOT and returns provider-specific result payloads."
  (map-chatbot-json-tool-call-results bot
                                      tool-calls
                                      context-builder
                                      result-builder
                                      :error-builder error-builder))

(defun continue-stateless-provider-tool-recursion (bot history-messages tool-calls
                                                       context-builder result-builder
                                                       recursion-message-builder continuation
                                                       &key error-builder)
 "Executes TOOL-CALLS, threads the resulting recursion messages into history,
and then invokes CONTINUATION with the updated history and recursion messages."
 (let* ((tool-results (provider-tool-call-results bot
                                                  tool-calls
                                                  context-builder
                                                  result-builder
                                                  :error-builder error-builder))
         (recursion-messages (funcall recursion-message-builder tool-calls tool-results)))
   (continue-stateless-tool-recursion history-messages
                                      recursion-messages
                                      continuation)))
