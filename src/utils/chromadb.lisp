;;; -*- Lisp -*-
;;; chromadb.lisp - Common Lisp bindings for ChromaDB REST API

(in-package "CHATBOT")

(defvar *chroma-host* "localhost"
  "The hostname of the ChromaDB server.")

(defvar *chroma-port* 8411
  "The port of the ChromaDB server.")

(defvar *chroma-scheme* "http"
  "The protocol scheme used to connect to ChromaDB (http or https).")

(defvar *chroma-api-version* "/api/v1"
  "The base API path version used for ChromaDB requests.")

(defvar *chroma-token* nil
  "An optional static authentication token for ChromaDB authorization.")

(defun chroma-url (path)
  "Constructs a complete ChromaDB API URL for the given PATH."
  (format nil "~A://~A:~D~A~A"
          *chroma-scheme*
          *chroma-host*
          *chroma-port*
          *chroma-api-version*
          path))

(defun chroma-headers ()
  "Constructs standard HTTP headers for ChromaDB requests, including auth token if present."
  (let ((headers (list (cons "Content-Type" "application/json"))))
    (when *chroma-token*
      (push (cons "Authorization" (format nil "Bearer ~A" *chroma-token*)) headers))
    headers))

(defun chroma-alive-p ()
  "Checks if ChromaDB is running on localhost:8411 (or configured host/port)
by making a lightweight GET request to the /heartbeat endpoint with a short 1-second timeout.
Does not perform back-off retries to allow for immediate detection when the host is down."
  (handler-case
      (let ((url (chroma-url "/heartbeat"))
            (headers (chroma-headers))
            (get-fn (current-http-get-function)))
        (funcall get-fn url :headers headers :connect-timeout 1 :read-timeout 1)
        t)
    (error () nil)))

(defun chroma-json-payload (alist)
  "Encodes an association list (alist) into a JSON payload compatible with cl-json."
  (cl-json:encode-json-to-string (json-encodable-value alist)))

(defun chroma-request (method path &key content)
  "Sends an HTTP request to ChromaDB and parses the JSON response.
If the host is not running, bypasses retry backoffs and returns (values nil :host-unavailable).
On successful request but empty response, returns t.
On any other error, returns (values nil :error)."
  (if (not (chroma-alive-p))
      (values nil :host-unavailable)
      (handler-case
          (let ((url (chroma-url path))
                (headers (chroma-headers)))
            (let ((response-body
                    (ecase method
                      (:get (get-web-request url :headers headers))
                      (:post (post-web-request url headers content))
                      (:delete (delete-web-request url :headers headers)))))
              (cond
                ((and (stringp response-body) (string/= "" response-body))
                 (cl-json:decode-json-from-string response-body))
                ((and (stringp response-body) (string= "" response-body))
                 t)
                (t response-body))))
        (error (c)
          (log-message :warn "ChromaDB request failed"
                       :context `(("url" . ,(chroma-url path))
                                  ("error" . ,(princ-to-string c))))
          (values nil :error)))))

;;; --- API Endpoints ---

(defun chroma-heartbeat ()
  "Returns the heartbeat response from ChromaDB (nanosecond timestamp), or nil if unavailable."
  (chroma-request :get "/heartbeat"))

(defun chroma-version ()
  "Returns the version of ChromaDB as a string, or nil if unavailable."
  (chroma-request :get "/version"))

(defun chroma-list-collections ()
  "Returns a list of all collections, or nil if unavailable.
Each collection is decoded as an association list (alist)."
  (chroma-request :get "/collections"))

(defun chroma-get-collection (collection-name)
  "Retrieves details for a specific collection by name.
Returns the collection details alist, or nil if not found or unavailable."
  (chroma-request :get (format nil "/collections/~A" collection-name)))

(defun chroma-create-collection (name &key metadata (get-or-create nil))
  "Creates a new collection with the given NAME.
Optionally accepts METADATA (alist or hash-table).
Returns the created collection details alist, or nil if unavailable."
  (let ((payload `((:name . ,name)
                   (:get--or--create . ,(if get-or-create t :false)))))
    (when metadata
      (push (cons :metadata (json-encodable-value metadata)) payload))
    (chroma-request :post "/collections" :content (chroma-json-payload payload))))

(defun chroma-delete-collection (collection-name)
  "Deletes the entire collection identified by COLLECTION-NAME.
Returns t on success, or nil if unavailable."
  (multiple-value-bind (response status)
      (chroma-request :delete (format nil "/collections/~A" collection-name))
    (declare (ignore response))
    (if (eq status :host-unavailable)
        (values nil :host-unavailable)
        t)))

(defun chroma-add (collection-id ids &key embeddings metadatas documents)
  "Adds records to the collection identified by COLLECTION-ID (UUID).
IDS must be a list of strings.
EMBEDDINGS, METADATAS, and DOCUMENTS are optional lists of corresponding values.
Returns the response details, or nil if unavailable."
  (let ((payload `((:ids . ,(coerce ids 'vector)))))
    (when embeddings
      (push (cons :embeddings (coerce (mapcar (lambda (e) (coerce e 'vector)) embeddings) 'vector)) payload))
    (when metadatas
      (push (cons :metadatas (coerce (mapcar #'json-encodable-value metadatas) 'vector)) payload))
    (when documents
      (push (cons :documents (coerce documents 'vector)) payload))
    (chroma-request :post (format nil "/collections/~A/add" collection-id)
                    :content (chroma-json-payload payload))))

(defun chroma-get (collection-id &key ids where where-document limit offset include)
  "Retrieves records from the collection identified by COLLECTION-ID (UUID) by ID or filter.
IDS is an optional list of strings.
WHERE is an optional metadata filter alist.
WHERE-DOCUMENT is an optional document content filter alist.
LIMIT and OFFSET are optional integers.
INCLUDE is an optional list of strings (e.g., '(\"embeddings\" \"documents\" \"metadatas\")).
Returns the matching records, or nil if unavailable."
  (let ((payload nil))
    (when ids
      (push (cons :ids (coerce ids 'vector)) payload))
    (when where
      (push (cons :where (json-encodable-value where)) payload))
    (when where-document
      (push (cons :where--document (json-encodable-value where-document)) payload))
    (when limit
      (push (cons :limit limit) payload))
    (when offset
      (push (cons :offset offset) payload))
    (when include
      (push (cons :include (coerce include 'vector)) payload))
    (chroma-request :post (format nil "/collections/~A/get" collection-id)
                    :content (chroma-json-payload payload))))

(defun chroma-query (collection-id query-embeddings &key (n-results 10) where where-document include)
  "Queries the collection identified by COLLECTION-ID (UUID) for nearest neighbors of QUERY-EMBEDDINGS.
QUERY-EMBEDDINGS must be a list of embedding vectors.
N-RESULTS is the number of results to return (default 10).
WHERE is an optional metadata filter alist.
WHERE-DOCUMENT is an optional document content filter alist.
INCLUDE is an optional list of strings.
Returns the query results, or nil if unavailable."
  (let ((payload `((:query--embeddings . ,(coerce (mapcar (lambda (v) (coerce v 'vector)) query-embeddings) 'vector))
                   (:n--results . ,n-results))))
    (when where
      (push (cons :where (json-encodable-value where)) payload))
    (when where-document
      (push (cons :where--document (json-encodable-value where-document)) payload))
    (when include
      (push (cons :include (coerce include 'vector)) payload))
    (chroma-request :post (format nil "/collections/~A/query" collection-id)
                    :content (chroma-json-payload payload))))

(defun chroma-delete (collection-id &key ids where where-document)
  "Deletes records from the collection identified by COLLECTION-ID (UUID).
IDS is an optional list of strings.
WHERE is an optional metadata filter alist.
WHERE-DOCUMENT is an optional document content filter alist.
Returns the list of deleted IDs, or nil if unavailable."
  (let ((payload nil))
    (when ids
      (push (cons :ids (coerce ids 'vector)) payload))
    (when where
      (push (cons :where (json-encodable-value where)) payload))
    (when where-document
      (push (cons :where--document (json-encodable-value where-document)) payload))
    (chroma-request :post (format nil "/collections/~A/delete" collection-id)
                    :content (chroma-json-payload payload))))
