;;; -*- Lisp -*-
;;; tests-chromadb.lisp - FiveAM test suite for ChromaDB bindings

(in-package "CHATBOT")

(fiveam:in-suite chatbot-suite)

(fiveam:def-test test-chroma-alive-p-success ()
  "Verifies that chroma-alive-p returns T when the server responds successfully."
  (let* ((mock-get-called-p nil)
         (context (make-test-backend-runtime-context nil)))
    ;; Override the http-get-function to simulate a running ChromaDB server
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (setf mock-get-called-p t)
            (fiveam:is (not (null (search "/heartbeat" url))))
            "{\"nanosecond heartbeat\": 1718218128310}"))
    (call-with-runtime-context context
      (lambda ()
        (fiveam:is (eq t (chroma-alive-p)))
        (fiveam:is (not (null mock-get-called-p)))))))

(fiveam:def-test test-chroma-alive-p-failure ()
  "Verifies that chroma-alive-p returns NIL when the server is down (signals connection error)."
  (let* ((mock-get-called-p nil)
         (context (make-test-backend-runtime-context nil)))
    ;; Override the http-get-function to simulate connection refused error
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore url args))
            (setf mock-get-called-p t)
            (error "Connection refused")))
    (call-with-runtime-context context
      (lambda ()
        (fiveam:is (null (chroma-alive-p)))
        (fiveam:is (not (null mock-get-called-p)))))))

(fiveam:def-test test-chroma-host-unavailable-bypasses-calls ()
  "Verifies that ChromaDB API functions instantly return (values nil :host-unavailable) if the server is offline."
  (let* ((mock-get-called-p nil)
         (mock-post-called-p nil)
         (context (make-test-backend-runtime-context nil)))
    ;; Simulate a down server: heartbeat check fails immediately
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore url args))
            (setf mock-get-called-p t)
            (error "Connection refused")))
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (declare (ignore url args))
            (setf mock-post-called-p t)
            (error "Should not be called!")))
    (call-with-runtime-context context
      (lambda ()
        ;; Test heartbeat
        (fiveam:is (null (chroma-heartbeat)))
        ;; Test list collections
        (multiple-value-bind (val status) (chroma-list-collections)
          (fiveam:is (null val))
          (fiveam:is (eq :host-unavailable status)))
        ;; Test create collection
        (multiple-value-bind (val status) (chroma-create-collection "test-coll")
          (fiveam:is (null val))
          (fiveam:is (eq :host-unavailable status)))
        ;; Test add records
        (multiple-value-bind (val status) (chroma-add "coll-id-123" '("id1"))
          (fiveam:is (null val))
          (fiveam:is (eq :host-unavailable status)))
        ;; Test post calls were never made
        (fiveam:is (not mock-post-called-p))
        (fiveam:is (not (null mock-get-called-p)))))))

(fiveam:def-test test-chroma-collection-operations-success ()
  "Verifies successful collection listing, details retrieval, and creation."
  (let* ((mock-get-called-p nil)
         (mock-post-called-p nil)
         (context (make-test-backend-runtime-context nil)))
    ;; Override HTTP GET to mock heartbeat and retrieval calls
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (setf mock-get-called-p t)
            (cond
              ((search "/heartbeat" url)
               "{\"nanosecond heartbeat\": 1718218128310}")
              ((search "/collections/my-coll" url)
               "{\"name\": \"my-coll\", \"id\": \"coll-uuid-abc\", \"metadata\": null}")
              ((search "/collections" url)
               "[{\"name\": \"my-coll\", \"id\": \"coll-uuid-abc\", \"metadata\": null}]")
              (t (error "Unexpected URL in GET: ~A" url)))))
    ;; Override HTTP POST to mock creation
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (setf mock-post-called-p t)
            (fiveam:is (not (null (search "/collections" url))))
            (let* ((content (getf args :content))
                   (parsed (cl-json:decode-json-from-string content)))
              (fiveam:is (string= "new-coll" (cdr (assoc :name parsed))))
              (fiveam:is (eq t (normalize-test-json-value (cdr (assoc :get--or--create parsed))))))
            "{\"name\": \"new-coll\", \"id\": \"coll-uuid-xyz\", \"metadata\": {\"foo\": \"bar\"}}"))
    (call-with-runtime-context context
      (lambda ()
        ;; Test List Collections
        (let ((collections (chroma-list-collections)))
          (fiveam:is (= 1 (length collections)))
          (fiveam:is (string= "my-coll" (cdr (assoc :name (first collections)))))
          (fiveam:is (string= "coll-uuid-abc" (cdr (assoc :id (first collections))))))
        ;; Test Get Collection
        (let ((coll (chroma-get-collection "my-coll")))
          (fiveam:is (not (null coll)))
          (fiveam:is (string= "my-coll" (cdr (assoc :name coll))))
          (fiveam:is (string= "coll-uuid-abc" (cdr (assoc :id coll)))))
        ;; Test Create Collection
        (let ((new-coll (chroma-create-collection "new-coll" :get-or-create t)))
          (fiveam:is (not (null new-coll)))
          (fiveam:is (string= "new-coll" (cdr (assoc :name new-coll))))
          (fiveam:is (string= "coll-uuid-xyz" (cdr (assoc :id new-coll)))))
        (fiveam:is (not (null mock-get-called-p)))
        (fiveam:is (not (null mock-post-called-p)))))))

(fiveam:def-test test-chroma-record-operations ()
  "Verifies adding, getting, querying, and deleting records in a collection."
  (let* ((mock-get-called-p nil)
         (mock-post-called-p nil)
         (post-url-visited nil)
         (post-payloads nil)
         (context (make-test-backend-runtime-context nil)))
    ;; Heartbeat mock
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore url args))
            (setf mock-get-called-p t)
            "{\"nanosecond heartbeat\": 1718218128310}"))
    ;; POST mock for CRUD operations
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (setf mock-post-called-p t)
            (let ((content (getf args :content)))
              (push url post-url-visited)
              (push (cl-json:decode-json-from-string content) post-payloads)
              (cond
                ((search "/add" url)
                 "{\"status\": \"success\"}")
                ((search "/get" url)
                 "{\"ids\": [\"id1\"], \"documents\": [\"hello world\"]}")
                ((search "/query" url)
                 "{\"ids\": [[\"id1\"]], \"distances\": [[0.123]], \"documents\": [[\"hello world\"]]}")
                ((search "/delete" url)
                 "[\"id1\"]")
                (t (error "Unexpected URL in POST: ~A" url))))))
    (call-with-runtime-context context
      (lambda ()
        ;; 1. Test ADD
        (let ((add-resp (chroma-add "uuid-1" '("id1") :embeddings '((0.1 0.2)) :documents '("hello world") :metadatas '(((:foo . "bar"))))))
          (fiveam:is (not (null add-resp)))
          (fiveam:is (string= "success" (cdr (assoc :status add-resp)))))
        ;; 2. Test GET
        (let ((get-resp (chroma-get "uuid-1" :ids '("id1") :limit 5 :offset 0 :include '("documents"))))
          (fiveam:is (not (null get-resp)))
          (fiveam:is (equal '("id1") (coerce (cdr (assoc :ids get-resp)) 'list)))
          (fiveam:is (equal '("hello world") (coerce (cdr (assoc :documents get-resp)) 'list))))
        ;; 3. Test QUERY
        (let ((query-resp (chroma-query "uuid-1" '((0.1 0.2)) :n-results 5 :where '((:foo . "bar")) :include '("documents" "distances"))))
          (fiveam:is (not (null query-resp)))
          (fiveam:is (equal '(("id1")) (map 'list (lambda (item) (coerce item 'list)) (cdr (assoc :ids query-resp)))))
          (fiveam:is (equal '(("hello world")) (map 'list (lambda (item) (coerce item 'list)) (cdr (assoc :documents query-resp))))))
        ;; 4. Test DELETE records
        (let ((del-resp (chroma-delete "uuid-1" :ids '("id1"))))
          (fiveam:is (equal '("id1") del-resp)))

        ;; Assert visited endpoints and structure correctness
        (fiveam:is (= 4 (length post-url-visited)))
        ;; Note: nreverse because we pushed onto stack
        (let ((urls (nreverse post-url-visited))
              (payloads (nreverse post-payloads)))
          ;; Check order and URL suffixes
          (fiveam:is (not (null (search "/collections/uuid-1/add" (nth 0 urls)))))
          (fiveam:is (not (null (search "/collections/uuid-1/get" (nth 1 urls)))))
          (fiveam:is (not (null (search "/collections/uuid-1/query" (nth 2 urls)))))
          (fiveam:is (not (null (search "/collections/uuid-1/delete" (nth 3 urls)))))
          
          ;; Validate payloads
          ;; Payload 0: add
          (let ((p0 (nth 0 payloads)))
            (fiveam:is (equal '("id1") (coerce (cdr (assoc :ids p0)) 'list)))
            (fiveam:is (equal '("hello world") (coerce (cdr (assoc :documents p0)) 'list)))
            ;; Embedded list of float vector
            (fiveam:is (equal '((0.1 0.2)) (map 'list (lambda (item) (coerce item 'list)) (cdr (assoc :embeddings p0))))))
          ;; Payload 1: get
          (let ((p1 (nth 1 payloads)))
            (fiveam:is (equal '("id1") (coerce (cdr (assoc :ids p1)) 'list)))
            (fiveam:is (= 5 (cdr (assoc :limit p1))))
            (fiveam:is (= 0 (cdr (assoc :offset p1))))
            (fiveam:is (equal '("documents") (coerce (cdr (assoc :include p1)) 'list))))
          ;; Payload 2: query
          (let ((p2 (nth 2 payloads)))
            (fiveam:is (equal '((0.1 0.2)) (map 'list (lambda (item) (coerce item 'list)) (cdr (assoc :query--embeddings p2)))))
            (fiveam:is (= 5 (cdr (assoc :n--results p2))))
            (fiveam:is (equal '("documents" "distances") (coerce (cdr (assoc :include p2)) 'list))))
          ;; Payload 3: delete
          (let ((p3 (nth 3 payloads)))
            (fiveam:is (equal '("id1") (coerce (cdr (assoc :ids p3)) 'list)))))))))

(fiveam:def-test test-chroma-diary-prompt-injection-success ()
  "Verifies that relevant diary entries are queried and injected into the user prompt."
  (let* ((mock-get-called-p nil)
         (mock-post-called-p nil)
         (mock-embed-called-p nil)
         (chatbot (make-instance 'chatbot :persona-name "V"))
         (context (make-test-backend-runtime-context nil)))
    ;; 1. Mock GET for heartbeat and get-collection
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (setf mock-get-called-p t)
            (cond
              ((search "/heartbeat" url)
               "{\"nanosecond heartbeat\": 1718218128310}")
              ((search "/collections/V_Diary" url)
               "{\"name\": \"V_Diary\", \"id\": \"v-diary-uuid-123\", \"metadata\": null}")
              (t (error "Unexpected GET URL: ~A" url)))))
    ;; 2. Mock POST for embedding generation and collection query
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (setf mock-post-called-p t)
            (cond
              ((search "embedContent" url)
               (setf mock-embed-called-p t)
               "{\"embedding\": {\"values\": [0.1, 0.2, 0.3]}}")
              ((search "/query" url)
               "{\"ids\": [[\"diary-01\"]], \"documents\": [[\"This is V's secret entry text.\"]], \"metadatas\": [[{\"entry_number\": 1, \"date\": \"2026-07-19\", \"tone\": \"cynical\", \"topic\": \"K-machine\"}]]}")
              (t (error "Unexpected POST URL: ~A" url)))))
    (call-with-runtime-context context
      (lambda ()
        (let ((decorated (decorate-live-user-input chatbot "Help me with the K-machine!")))
          (fiveam:is (not (null mock-get-called-p)))
          (fiveam:is (not (null mock-post-called-p)))
          (fiveam:is (not (null mock-embed-called-p)))
          ;; Assert that the prompt contains our query and the transient injected context
          (fiveam:is (not (null (search "Help me with the K-machine!" decorated))))
          (fiveam:is (not (null (search "[Relevant Historical Diary Entries (Transient Context)]" decorated))))
          (fiveam:is (not (null (search "Entry Number: 1" decorated))))
          (fiveam:is (not (null (search "Date: 2026-07-19" decorated))))
          (fiveam:is (not (null (search "Tone: cynical" decorated))))
          (fiveam:is (not (null (search "Topic: K-machine" decorated))))
          (fiveam:is (not (null (search "This is V's secret entry text." decorated)))))))))
