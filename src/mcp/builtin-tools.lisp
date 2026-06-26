;;; -*- Lisp -*-
;;; builtin-tools.lisp - built-in chatbot tool catalog

(in-package "CHATBOT")

(defun builtin-read-file-lines-tool ()
  "Returns the built-in readFileLines tool definition."
  '((:name . "readFileLines")
    (:description . "Reads an inclusive line range from a file.")
    (:input-schema . ((:type . "object")
                      (:properties . (("filename" . ((:type . "string")
                                                     (:description . "Path to the file, relative to the persona directory or absolute within it.")))
                                      ("beginningLine" . ((:type . "integer")
                                                          (:description . "Inclusive starting line number (1-based).")))
                                      ("endingLine" . ((:type . "integer")
                                                       (:description . "Inclusive ending line number (1-based).")))))
                      (:required . ("filename" "beginningLine" "endingLine"))))))

(defun builtin-directory-tool ()
  "Returns the built-in directory tool definition."
  '((:name . "directory")
    (:description . "Lists files in a directory that match a filename pattern.")
    (:input-schema . ((:type . "object")
                      (:properties . (("pathname" . ((:type . "string")
                                                     (:description . "Path to the directory, relative to the persona directory or absolute within it.")))
                                      ("pattern" . ((:type . "string")
                                                    (:description . "Filename pattern to match within the directory, for example *.txt.")))))
                      (:required . ("pathname" "pattern"))))))

(defun builtin-write-file-tool ()
  "Returns the built-in writeFile tool definition."
  '((:name . "writeFile")
    (:description . "Creates or overwrites a file from provided lines and newline settings.")
    (:input-schema . ((:type . "object")
                      (:properties . (("pathname" . ((:type . "string")
                                                     (:description . "Path to the file, relative to the persona directory or absolute within it.")))
                                      ("useLfOnly" . ((:type . "boolean")
                                                      (:description . "When true, use LF line endings; otherwise use CRLF line endings.")))
                                      ("endWithEol" . ((:type . "boolean")
                                                       (:description . "When true, end the file with a trailing line ending when lines are present.")))
                                      ("lines" . ((:type . "array")
                                                  (:items . ((:type . "string")))
                                                  (:description . "Array of file lines to write in order.")))))
                      (:required . ("pathname" "useLfOnly" "endWithEol" "lines"))))))

(defun builtin-delete-file-tool ()
  "Returns the built-in deleteFile tool definition."
  '((:name . "deleteFile")
    (:description . "Deletes a file.")
    (:input-schema . ((:type . "object")
                      (:properties . (("pathname" . ((:type . "string")
                                                     (:description . "Path to the file, relative to the persona directory or absolute within it.")))))
                      (:required . ("pathname"))))))

(defun builtin-eval-tool ()
  "Returns the built-in eval tool definition."
  '((:name . "eval")
    (:description . "Reads and evaluates one Lisp s-expression after explicit user approval.")
    (:input-schema . ((:type . "object")
                      (:properties . (("expression" . ((:type . "string")
                                                       (:description . "A single Lisp s-expression to read and evaluate.")))))
                      (:required . ("expression"))))))

(defun builtin-web-search-tool ()
  "Returns the built-in webSearch tool definition."
  '((:name . "webSearch")
    (:description . "Searches the web for grounding information.")
    (:input-schema . ((:type . "object")
                      (:properties . (("query" . ((:type . "string")
                                                  (:description . "The general web search query.")))))
                      (:required . ("query"))))))

(defun builtin-hyperspec-search-tool ()
  "Returns the built-in hyperspecSearch tool definition."
  '((:name . "hyperspecSearch")
    (:description . "Searches the Common Lisp HyperSpec for grounding information.")
    (:input-schema . ((:type . "object")
                      (:properties . (("query" . ((:type . "string")
                                                  (:description . "The Common Lisp / HyperSpec search query.")))))
                      (:required . ("query"))))))

(defun builtin-read-system-instructions-tool ()
  "Returns the built-in readSystemInstructions tool definition."
  '((:name . "readSystemInstructions")
    (:description . "Reads the current persona system instructions as an ordered paragraph vector.")
    (:input-schema . ((:type . "object")
                      (:properties . nil)))))

(defun builtin-insert-system-instruction-paragraph-tool ()
  "Returns the built-in insertSystemInstructionParagraph tool definition."
  '((:name . "insertSystemInstructionParagraph")
    (:description . "Inserts one paragraph into the persona system instructions and saves the backing file.")
    (:input-schema . ((:type . "object")
                      (:properties . (("index" . ((:type . "integer")
                                                  (:description . "Zero-based insertion index. Use the current paragraph count to append.")))
                                      ("paragraph" . ((:type . "string")
                                                      (:description . "The paragraph text to insert.")))))
                      (:required . ("index" "paragraph"))))))

(defun builtin-update-system-instruction-paragraph-tool ()
  "Returns the built-in updateSystemInstructionParagraph tool definition."
  '((:name . "updateSystemInstructionParagraph")
    (:description . "Replaces one paragraph in the persona system instructions and saves the backing file.")
    (:input-schema . ((:type . "object")
                      (:properties . (("index" . ((:type . "integer")
                                                  (:description . "Zero-based paragraph index to replace.")))
                                      ("paragraph" . ((:type . "string")
                                                      (:description . "The new paragraph text.")))))
                      (:required . ("index" "paragraph"))))))

(defun builtin-delete-system-instruction-paragraph-tool ()
  "Returns the built-in deleteSystemInstructionParagraph tool definition."
  '((:name . "deleteSystemInstructionParagraph")
    (:description . "Deletes one paragraph from the persona system instructions and saves the backing file.")
    (:input-schema . ((:type . "object")
                      (:properties . (("index" . ((:type . "integer")
                                                  (:description . "Zero-based paragraph index to delete.")))))
                      (:required . ("index"))))))

(defun builtin-replace-system-instructions-tool ()
  "Returns the built-in replaceSystemInstructions tool definition."
  '((:name . "replaceSystemInstructions")
    (:description . "Replaces the entire persona system-instruction paragraph vector and saves the backing file.")
    (:input-schema . ((:type . "object")
                      (:properties . (("paragraphs" . ((:type . "array")
                                                       (:items . ((:type . "string")))
                                                       (:description . "Array of paragraphs to store in order.")))))
                      (:required . ("paragraphs"))))))

(defun builtin-read-sampling-parameters-tool ()
  "Returns the built-in readSamplingParameters tool definition."
  '((:name . "readSamplingParameters")
    (:description . "Reads the current runtime temperature and top-p sampling defaults.")
    (:input-schema . ((:type . "object")
                      (:properties . nil)))))

(defun builtin-set-sampling-parameters-tool ()
  "Returns the built-in setSamplingParameters tool definition."
  '((:name . "setSamplingParameters")
    (:description . "Updates the runtime temperature and/or top-p sampling defaults for this conversation.")
    (:input-schema . ((:type . "object")
                      (:properties . (("temperature" . ((:type . "number")
                                                        (:description . "Sampling temperature between 0.0 and 2.0 inclusive.")))
                                      ("topP" . ((:type . "number")
                                                 (:description . "Nucleus sampling top-p greater than 0.0 and at most 1.0.")))))))))

(defun builtin-reset-sampling-parameters-tool ()
  "Returns the built-in resetSamplingParameters tool definition."
  '((:name . "resetSamplingParameters")
    (:description . "Clears the runtime temperature and top-p defaults so provider defaults apply again.")
    (:input-schema . ((:type . "object")
                      (:properties . nil)))))

(defun default-get-all-builtin-tools (bot)
  "Returns all built-in tools enabled for BOT as (source . tool) pairs."
  (let ((tools nil))
    (push (cons :built-in (builtin-reset-sampling-parameters-tool)) tools)
    (push (cons :built-in (builtin-set-sampling-parameters-tool)) tools)
    (push (cons :built-in (builtin-read-sampling-parameters-tool)) tools)
    (when (chatbot-web-tools-p bot)
      (push (cons :built-in (builtin-hyperspec-search-tool)) tools)
      (push (cons :built-in (builtin-web-search-tool)) tools))
    (when (chatbot-enable-eval-p bot)
      (push (cons :built-in (builtin-eval-tool)) tools))
    (when (chatbot-filesystem-tools-p bot)
      (push (cons :built-in (builtin-delete-file-tool)) tools)
      (push (cons :built-in (builtin-write-file-tool)) tools)
      (push (cons :built-in (builtin-directory-tool)) tools)
      (push (cons :built-in (builtin-read-file-lines-tool)) tools))
    (when (chatbot-system-instruction-path bot)
      (push (cons :built-in (builtin-replace-system-instructions-tool)) tools)
      (push (cons :built-in (builtin-delete-system-instruction-paragraph-tool)) tools)
      (push (cons :built-in (builtin-update-system-instruction-paragraph-tool)) tools)
      (push (cons :built-in (builtin-insert-system-instruction-paragraph-tool)) tools)
      (push (cons :built-in (builtin-read-system-instructions-tool)) tools))
    (nreverse tools)))

(defun default-find-builtin-tool (bot tool-name)
  "Finds a built-in tool enabled for BOT by TOOL-NAME."
  (dolist (entry (default-get-all-builtin-tools bot))
    (let ((tool (cdr entry)))
      (when (string= (mcp-val :name tool) tool-name)
        (return-from default-find-builtin-tool (values (car entry) tool)))))
  (values nil nil))
