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

(fiveam:def-test test-chroma-diary-prompt-injection-relevance-filtering ()
  "Verifies that diary entries exceeding *chroma-diary-relevance-threshold* are filtered out."
  (let* ((chatbot (make-instance 'chatbot :persona-name "V"))
         (context (make-test-backend-runtime-context nil)))
    ;; Mock GET for heartbeat and get-collection
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore url args))
            "{\"nanosecond heartbeat\": 1718218128310}"))
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (cond
              ((search "embedContent" url)
               "{\"embedding\": {\"values\": [0.1, 0.2, 0.3]}}")
              ((search "/query" url)
               ;; Return two records: one highly relevant (distance 0.2), one irrelevant (distance 0.9)
               "{\"ids\": [[\"diary-01\", \"diary-02\"]],
                 \"distances\": [[0.2, 0.9]],
                 \"documents\": [[\"Highly relevant doc.\", \"Irrelevant doc.\"]],
                 \"metadatas\": [[{\"entry_number\": 1, \"topic\": \"Good Match\"}, {\"entry_number\": 2, \"topic\": \"Bad Match\"}]]}")
              (t (error "Unexpected POST URL: ~A" url)))))
    (call-with-runtime-context context
      (lambda ()
        (let ((*chroma-diary-relevance-threshold* 0.5))
          (let ((decorated (decorate-live-user-input chatbot "Help me with the K-machine!")))
            ;; highly relevant document (distance 0.2 <= 0.5) must be included
            (fiveam:is (not (null (search "Highly relevant doc." decorated))))
            ;; irrelevant document (distance 0.9 > 0.5) must be excluded
            (fiveam:is (null (search "Irrelevant doc." decorated)))))))))

(fiveam:def-test test-stronger-model ()
  "Verifies that stronger-model correctly moves up the Gemini model strength hierarchy."
  (fiveam:is (string= "gemini-2.5-flash" (stronger-model "gemini-1.5-flash")))
  (fiveam:is (string= "gemini-3.5-flash" (stronger-model "gemini-2.5-flash")))
  (fiveam:is (string= "gemini-3-flash-preview" (stronger-model "gemini-3.5-flash")))
  (fiveam:is (string= "gemini-flash-latest" (stronger-model "gemini-3-flash-preview")))
  (fiveam:is (string= "gemini-pro-latest" (stronger-model "gemini-flash-latest")))
  (fiveam:is (string= "gemini-pro-latest" (stronger-model "gemini-1.5-pro")))
  (fiveam:is (string= "gemini-pro-latest" (stronger-model "gemini-2.5-pro")))
  ;; Strongest model remains as-is
  (fiveam:is (string= "gemini-pro-latest" (stronger-model "gemini-pro-latest")))
  ;; Unrecognized models return as-is
  (fiveam:is (string= "unrecognized-model" (stronger-model "unrecognized-model")))
  ;; Prefix models/ is correctly preserved
  (fiveam:is (string= "models/gemini-2.5-flash" (stronger-model "models/gemini-1.5-flash")))
  (fiveam:is (string= "models/gemini-pro-latest" (stronger-model "models/gemini-pro-latest")))
  ;; Model name is case-insensitive during matching but returned in standard format
  (fiveam:is (string= "gemini-2.5-flash" (stronger-model "GEMINI-1.5-FLASH")))
  ;; Dynamic heuristic tests for unlisted preview/future models
  (fiveam:is (string= "gemini-4-pro-preview" (stronger-model "gemini-4-flash-preview")))
  (fiveam:is (string= "models/gemini-4-pro-preview" (stronger-model "models/gemini-4-flash-preview")))
  (fiveam:is (string= "gemini-pro-latest" (stronger-model "gemini-4-pro-preview")))
  (fiveam:is (string= "models/gemini-pro-latest" (stronger-model "models/gemini-4-pro-preview"))))

(fiveam:def-test test-google-chat-retries-empty-response-on-stronger-model ()
  "Verifies that when a Google backend response is empty, it is retried on a stronger model."
  (let* ((bot (make-instance 'chatbot :backend :google :model "gemini-1.5-flash"))
         (context (make-test-backend-runtime-context nil))
         (urls-called nil))
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (push url urls-called)
            (cond
              ;; First call is gemini-1.5-flash -> return empty text
              ((search "gemini-1.5-flash" url)
               (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"\"}]}}]}" 200))
              ;; Second call should be gemini-2.5-flash (stronger model!) -> return valid response
              ((search "gemini-2.5-flash" url)
               (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"I am a stronger model response!\"}]}}]}" 200))
              (t (error "Unexpected model URL: ~A" url)))))
    (call-with-runtime-context context
      (lambda ()
        (multiple-value-bind (response status)
            (chat-google bot "Hello" (new-chat :backend :google :model "gemini-1.5-flash") nil :return-turn-result-p t)
          (declare (ignore status))
          ;; Assert that both the weak and stronger model URLs were called in order
          (fiveam:is (= 2 (length urls-called)))
          (fiveam:is (not (null (search "gemini-1.5-flash" (second urls-called)))))
          (fiveam:is (not (null (search "gemini-2.5-flash" (first urls-called)))))
          ;; Assert that the final result returned is from the stronger model
          (fiveam:is (string= "I am a stronger model response!" (chat-turn-result-text response))))))))

(fiveam:def-test test-chroma-knowledge-graph-sync ()
  "Verifies that successful execution of add_observations and create_entities syncs to the ChromaDB <persona>_Memory collection."
  (let* ((bot (make-instance 'chatbot :persona-name "V"))
         (context (make-test-backend-runtime-context nil))
         (urls-called nil)
         (added-docs nil))
    ;; Mock HTTP GET for ChromaDB heartbeat/get-collection
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (cond
              ((search "/heartbeat" url)
               "{\"nanosecond heartbeat\": 1718218128310}")
              ((search "/collections/V_Memory" url)
               "{\"name\": \"V_Memory\", \"id\": \"v-memory-uuid-456\", \"metadata\": null}")
              (t (error "Unexpected GET URL: ~A" url)))))
    ;; Mock HTTP POST for embedding generator, complete-sentence generator, and ChromaDB add
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (push url urls-called)
            (let ((content (getf args :content)))
              (cond
                ;; 1. Embedding generator
                ((search "embedContent" url)
                 "{\"embedding\": {\"values\": [0.5, 0.5, 0.5]}}")
                ;; 2. Complete-sentence generator (stateless generateContent)
                ((search "generateContent" url)
                 "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"V likes Common Lisp.\"}]}}]}" )
                ;; 3. ChromaDB record add
                ((search "/collections/" url)
                 (let ((parsed (cl-json:decode-json-from-string content)))
                   (push (coerce (cdr (assoc :documents parsed)) 'list) added-docs))
                 "{\"status\": \"success\"}")
                (t (error "Unexpected POST URL: ~A" url))))))
    (call-with-runtime-context context
      (lambda ()
        (let ((*execute-mcp-tool-function*
                (lambda (server tool-name arguments)
                  (declare (ignore server tool-name arguments))
                  "ok")))
          ;; Test case 1: execute add_observations
          (let ((args '((:observations . #(((:entity--name . "V") (:contents . #("Likes Common Lisp"))))))))
            (execute-chatbot-tool bot :mcp "add_observations" args)
            (fiveam:is (= 1 (length added-docs)))
            (fiveam:is (string= "V likes Common Lisp." (first (first added-docs)))))
          ;; Test case 2: execute create_entities with vectors
          (setf added-docs nil)
          (let ((args '((:entities . #(((:name . "V") (:entity--type . "Persona") (:observations . #("Likes Common Lisp"))))))))
            (execute-chatbot-tool bot :mcp "create_entities" args)
            (fiveam:is (= 1 (length added-docs)))
            (fiveam:is (string= "V likes Common Lisp." (first (first added-docs)))))
          ;; Test case 3: execute add_observations with parsed lists (representing JSON arrays decoded as lists)
          (setf added-docs nil)
          (let ((args '((:observations . (((:entity-name . "The Boss")
                                           (:contents . "The Boss takes Pramipexole ER, 1.75mg (updated from 1.5mg) as part of his medication cocktail."))
                                          ((:entity-name . "Pramipexole ER")
                                           (:contents . "1.75 mg (updated from 1.5mg)")))))))
            (execute-chatbot-tool bot :mcp "add_observations" args)
            ;; Both observations should be synced, so we expect 2 added documents
            (fiveam:is (= 2 (length added-docs)))
            (fiveam:is (string= "V likes Common Lisp." (first (first added-docs))))
            (fiveam:is (string= "V likes Common Lisp." (first (second added-docs)))))
          ;; Test case 4: execute create_entities with parsed lists
          (setf added-docs nil)
          (let ((args '((:entities . (((:name . "V") (:entity--type . "Persona") (:observations . ("Likes Common Lisp"))))))))
            (execute-chatbot-tool bot :mcp "create_entities" args)
            (fiveam:is (= 1 (length added-docs)))
            (fiveam:is (string= "V likes Common Lisp." (first (first added-docs))))))))))

(fiveam:def-test test-chroma-memory-prompt-injection-success ()
  "Verifies that relevant memory observations are queried, filtered by threshold, and injected into the user prompt."
  (let* ((mock-get-called-p nil)
         (mock-post-called-p nil)
         (mock-embed-called-p nil)
         (chatbot (make-instance 'chatbot :persona-name "V"))
         (context (make-test-backend-runtime-context nil)))
    ;; Mock GET for heartbeat and V_Memory collection
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (setf mock-get-called-p t)
            (cond
              ((search "/heartbeat" url)
               "{\"nanosecond heartbeat\": 1718218128310}")
              ((search "/collections/V_Memory" url)
               "{\"name\": \"V_Memory\", \"id\": \"v-memory-uuid-789\", \"metadata\": null}")
              ((search "/collections/V_Diary" url)
               nil) ; return nil to only focus on memories injection in this test
              (t (error "Unexpected GET URL: ~A" url)))))
    ;; Mock POST for embedding generation and collection query
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (setf mock-post-called-p t)
            (cond
              ((search "embedContent" url)
               (setf mock-embed-called-p t)
               "{\"embedding\": {\"values\": [0.1, 0.2, 0.3]}}")
              ((search "/query" url)
               "{\"ids\": [[\"mem-1\", \"mem-2\"]],
                 \"distances\": [[0.1, 0.9]],
                 \"documents\": [[\"V is a formidable ghost.\", \"Irrelevant observation fact.\"]],
                 \"metadatas\": [[{\"entity\": \"V\", \"entity_type\": \"Persona\"}, {\"entity\": \"Other\", \"entity_type\": \"Thing\"}]]}")
              (t (error "Unexpected POST URL: ~A" url)))))
    (call-with-runtime-context context
      (lambda ()
        (let ((*chroma-memory-relevance-threshold* 0.5))
          (let ((decorated (decorate-live-user-input chatbot "Tell me about V!")))
            (fiveam:is (not (null mock-get-called-p)))
            (fiveam:is (not (null mock-post-called-p)))
            (fiveam:is (not (null mock-embed-called-p)))
            ;; Assert that the prompt contains our query and the transient injected memories block
            (fiveam:is (not (null (search "Tell me about V!" decorated))))
            (fiveam:is (not (null (search "[Relevant Historical Memories (Transient Context)]" decorated))))
            (fiveam:is (not (null (search "Entity: V" decorated))))
            (fiveam:is (not (null (search "Entity Type: Persona" decorated))))
            (fiveam:is (not (null (search "Relevance Distance: 0.1" decorated))))
            (fiveam:is (not (null (search "Memory: V is a formidable ghost." decorated))))
            ;; Assert that the irrelevant memory (distance 0.9 > 0.5) is filtered out
            (fiveam:is (null (search "Irrelevant observation fact." decorated)))))))))

(fiveam:def-test test-chroma-automatic-collection-creation-on-query ()
  "Verifies that when get-relevant-diary-entries-text and get-relevant-memories-text find their collections missing, they automatically create them."
  (let* ((mock-get-called-p nil)
         (created-collections nil)
         (queried-collections nil)
         (chatbot (make-instance 'chatbot :persona-name "V"))
         (context (make-test-backend-runtime-context nil)))
    ;; Mock GET: Return NIL for the collections to simulate they don't exist yet
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (setf mock-get-called-p t)
            (cond
              ((search "/heartbeat" url)
               "{\"nanosecond heartbeat\": 1718218128310}")
              ((search "/collections/V_Diary" url)
               nil)
              ((search "/collections/V_Memory" url)
               nil)
              (t (error "Unexpected GET URL: ~A" url)))))
    ;; Mock POST: Handle creation and queries
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (cond
              ((search "embedContent" url)
               "{\"embedding\": {\"values\": [0.1, 0.2, 0.3]}}")
              ((search "/query" url)
               (push url queried-collections)
               "{\"ids\": [[]], \"distances\": [[]], \"documents\": [[]], \"metadatas\": [[]]}")
              ((search "/collections" url)
               (let* ((content (getf args :content))
                      (parsed (cl-json:decode-json-from-string content))
                      (name (cdr (assoc :name parsed))))
                 (push name created-collections)
                 (format nil "{\"name\": \"~A\", \"id\": \"created-uuid-~A\", \"metadata\": null}" name name)))
              (t (error "Unexpected POST URL: ~A" url)))))
    (call-with-runtime-context context
      (lambda ()
        (let ((decorated (decorate-live-user-input chatbot "Tell me about V!")))
          (declare (ignore decorated))
          (fiveam:is (not (null mock-get-called-p)))
          ;; Check that both collections were created
          (fiveam:is (not (null (member "V_Diary" created-collections :test #'string=))))
          (fiveam:is (not (null (member "V_Memory" created-collections :test #'string=))))
          ;; Check that both collections were subsequently queried
          (fiveam:is (= 2 (length queried-collections)))
          (fiveam:is (not (null (search "created-uuid-V_Diary" (second queried-collections)))))
          (fiveam:is (not (null (search "created-uuid-V_Memory" (first queried-collections))))))))))

(fiveam:def-test test-save-and-query-persona-skills ()
  "Verifies saving a persona skill to disk and ChromaDB, querying it, and verifying that it is not retrieved automatically."
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-skills/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (v-persona-dir (merge-pathnames "V/" personas-dir))
         (skills-dir (merge-pathnames "Skills/" v-persona-dir))
         (mock-get-called-p nil)
         (mock-post-called-p nil)
         (added-records nil)
         (queried-collections nil)
         (context (make-test-backend-runtime-context nil)))
    (ensure-directories-exist skills-dir)
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (setf mock-get-called-p t)
            (cond
              ((search "/heartbeat" url)
               "{\"nanosecond heartbeat\": 1718218128310}")
              ((search "/collections/V_Skills" url)
               "{\"name\": \"V_Skills\", \"id\": \"skills-uuid-123\", \"metadata\": null}")
              (t (error "Unexpected GET URL: ~A" url)))))
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (setf mock-post-called-p t)
            (cond
              ((search "embedContent" url)
               "{\"embedding\": {\"values\": [0.1, 0.2, 0.3]}}")
              ((search "/add" url)
               (let* ((content (getf args :content))
                      (parsed (cl-json:decode-json-from-string content)))
                 (setf added-records parsed)
                 "{\"status\": \"success\"}"))
              ((search "/query" url)
               (push url queried-collections)
               "{\"ids\": [[\"skill-1\"]],
                 \"distances\": [[0.15]],
                 \"documents\": [[\"Always format lisp nicely.\"]],
                 \"metadatas\": [[{\"skill--name\": \"FormatLisp\", \"description\": \"Formatting style\"}]]}")
              (t (error "Unexpected POST URL: ~A" url)))))
    (let ((*user-homedir-pathname-function* (lambda () mock-home)))
      (call-with-runtime-context context
        (lambda ()
          ;; 1. Test SAVE-PERSONA-SKILL
          (multiple-value-bind (response status)
              (save-persona-skill "V" "FormatLisp" "Always format lisp nicely." :description "Formatting style")
            (declare (ignore response status))
            ;; Check that file was written to disk
            (let ((file-path (merge-pathnames "FormatLisp.txt" skills-dir)))
              (fiveam:is (not (null (probe-file file-path))))
              (fiveam:is (string= "Always format lisp nicely." (uiop:read-file-string file-path))))
            ;; Check that records were added to ChromaDB
            (fiveam:is (not (null added-records)))
            (fiveam:is (string= "skill-FormatLisp" (car (json-array-elements (cdr (assoc :ids added-records))))))
            (fiveam:is (string= "Always format lisp nicely." (car (json-array-elements (cdr (assoc :documents added-records)))))))

          ;; 2. Test GET-RELEVANT-SKILLS
          (let ((skills (get-relevant-skills "V" "How to format lisp" :n-results 1 :threshold 0.5)))
            (fiveam:is (= 1 (length skills)))
            (let ((skill (first skills)))
              (fiveam:is (string= "Always format lisp nicely." (getf skill :document)))
              (fiveam:is (equal 0.15 (getf skill :distance)))
              (let ((meta (getf skill :metadata)))
                (fiveam:is (string= "FormatLisp" (cdr (assoc :skill--name meta))))
                (fiveam:is (string= "Formatting style" (cdr (assoc :description meta)))))))

          ;; 3. Test ensure-persona-skills-collection
          (let ((coll (ensure-persona-skills-collection "V")))
            (fiveam:is (not (null coll)))
            (fiveam:is (string= "V_Skills" (cdr (assoc :name coll))))
            (fiveam:is (string= "skills-uuid-123" (cdr (assoc :id coll)))))

          ;; 4. Verify decorate-live-user-input does NOT automatically query or retrieve skills
          (setf queried-collections nil)
          (let* ((chatbot (make-instance 'chatbot :persona-name "V"))
                 ;; Mock get function to return NIL for diary/memory collections to prevent them from calling query
                 (custom-get (lambda (url &rest args)
                               (declare (ignore args))
                               (cond
                                 ((search "/heartbeat" url)
                                  "{\"nanosecond heartbeat\": 1718218128310}")
                                 ((search "/collections/V_Diary" url) nil)
                                 ((search "/collections/V_Memory" url) nil)
                                 ((search "/collections/V_Skills" url) nil)
                                 (t (error "Unexpected GET URL: ~A" url)))))
                 ;; Override http-get-function on the context
                 (custom-post (lambda (url &rest args)
                                (cond
                                  ((search "embedContent" url)
                                   "{\"embedding\": {\"values\": [0.1, 0.2, 0.3]}}")
                                  ((search "/query" url)
                                   (push url queried-collections)
                                   "{\"ids\": [[]], \"distances\": [[]], \"documents\": [[]], \"metadatas\": [[]]}")
                                  ((search "/collections" url)
                                   "{\"name\": \"any\", \"id\": \"any-uuid\", \"metadata\": null}")
                                  (t (error "Unexpected POST URL: ~A" url))))))
            (setf (runtime-context-http-get-function context) custom-get)
            (setf (runtime-context-http-post-function context) custom-post)
            (decorate-live-user-input chatbot "Prompt text")
            ;; Because Diary/Memory collections returned NIL, they would create and query.
            ;; But V_Skills should NOT be queried during decorate-live-user-input!
            ;; If it was queried, the queried-collections would contain "V_Skills" UUID. It should not.
            (fiveam:is (null (find "skills-uuid-123" queried-collections :test (lambda (uuid url) (search uuid url)))))))))
    (when (uiop:directory-exists-p mock-home)
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:def-test test-add-skill ()
  "Verifies that add-skill correctly extracts description from SKILL.md and adds the skill with proper metadata."
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-add-skill/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (v-persona-dir (merge-pathnames "V/" personas-dir))
         (skills-dir (merge-pathnames "Skills/" v-persona-dir))
         (test-skill-dir (merge-pathnames "test-skill/" skills-dir))
         (test-skill-resources-dir (merge-pathnames "resources/" test-skill-dir))
         (skill-md-path (merge-pathnames "SKILL.md" test-skill-dir))
         (resource-1-path (merge-pathnames "one.md" test-skill-resources-dir))
         (resource-2-path (merge-pathnames "two.md" (merge-pathnames "sub/" test-skill-resources-dir)))
         (added-records nil)
         (context (make-test-backend-runtime-context nil)))
    (ensure-directories-exist test-skill-resources-dir)
    (ensure-directories-exist (merge-pathnames "sub/" test-skill-resources-dir))
    (with-open-file (s skill-md-path :direction :output :if-exists :supersede)
      (write-line "---" s)
      (write-line "This is a great skill." s)
      (write-line "It helps with Lisp." s)
      (write-line "---" s)
      (write-line "Ignored details down here." s))
    (with-open-file (s resource-1-path :direction :output :if-exists :supersede)
      (write-line "Resource 1" s))
    (with-open-file (s resource-2-path :direction :output :if-exists :supersede)
      (write-line "Resource 2" s))
    
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (cond
              ((search "/heartbeat" url)
               "{\"nanosecond heartbeat\": 1718218128310}")
              ((search "/collections/V_Skills" url)
               "{\"name\": \"V_Skills\", \"id\": \"skills-uuid-add-123\", \"metadata\": null}")
              (t (error "Unexpected GET URL: ~A" url)))))
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (cond
              ((search "embedContent" url)
               "{\"embedding\": {\"values\": [0.4, 0.5, 0.6]}}")
              ((search "/add" url)
               (let* ((content (getf args :content))
                      (parsed (cl-json:decode-json-from-string content)))
                 (setf added-records parsed)
                 "{\"status\": \"success\"}"))
              (t (error "Unexpected POST URL: ~A" url)))))
    
    (let ((*user-homedir-pathname-function* (lambda () mock-home)))
      (call-with-runtime-context context
        (lambda ()
          (multiple-value-bind (response status) (add-skill test-skill-dir :persona-name "V")
            (declare (ignore response status))
            (fiveam:is (not (null added-records)))
            (let ((id (car (json-array-elements (cdr (assoc :ids added-records)))))
                  (doc (car (json-array-elements (cdr (assoc :documents added-records)))))
                  (meta (car (json-array-elements (cdr (assoc :metadatas added-records))))))
              (fiveam:is (string= "skill-test-skill" id))
              (fiveam:is (string= (format nil "This is a great skill.~%It helps with Lisp.") doc))
              (fiveam:is (string= "test-skill" (cdr (assoc :skill--name meta))))
              (fiveam:is (string= (namestring skill-md-path) (cdr (assoc :skill--md--path meta))))
              (let ((resource-paths-str (cdr (assoc :resource--paths meta))))
                ;; resource-paths-str is a JSON string of paths
                (let ((decoded (cl-json:decode-json-from-string resource-paths-str)))
                  (fiveam:is (= 2 (length decoded)))
                  (fiveam:is (not (null (find (namestring resource-1-path) decoded :test #'string=))))
                  (fiveam:is (not (null (find (namestring resource-2-path) decoded :test #'string=)))))))))))
    (when (uiop:directory-exists-p mock-home)
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:def-test test-add-skills ()
  "Verifies that add-skills correctly finds subdirectories and calls add-skill on them."
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-add-skills/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (v-persona-dir (merge-pathnames "V/" personas-dir))
         (skills-dir (merge-pathnames "Skills/" v-persona-dir))
         (test-skill-dir-1 (merge-pathnames "test-skill-1/" skills-dir))
         (test-skill-dir-2 (merge-pathnames "test-skill-2/" skills-dir))
         (skill-md-path-1 (merge-pathnames "SKILL.md" test-skill-dir-1))
         (skill-md-path-2 (merge-pathnames "SKILL.md" test-skill-dir-2))
         (added-records nil)
         (context (make-test-backend-runtime-context nil)))
    (ensure-directories-exist test-skill-dir-1)
    (ensure-directories-exist test-skill-dir-2)
    (with-open-file (s skill-md-path-1 :direction :output :if-exists :supersede)
      (write-line "---" s)
      (write-line "Skill 1" s)
      (write-line "---" s))
    (with-open-file (s skill-md-path-2 :direction :output :if-exists :supersede)
      (write-line "---" s)
      (write-line "Skill 2" s)
      (write-line "---" s))
    
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (cond
              ((search "/heartbeat" url)
               "{\"nanosecond heartbeat\": 1718218128310}")
              ((search "/collections/V_Skills" url)
               "{\"name\": \"V_Skills\", \"id\": \"skills-uuid-add-123\", \"metadata\": null}")
              (t (error "Unexpected GET URL: ~A" url)))))
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (cond
              ((search "embedContent" url)
               "{\"embedding\": {\"values\": [0.4, 0.5, 0.6]}}")
              ((search "/add" url)
               (let* ((content (getf args :content))
                      (parsed (cl-json:decode-json-from-string content)))
                 (push parsed added-records)
                 "{\"status\": \"success\"}"))
              (t (error "Unexpected POST URL: ~A" url)))))
    
    (let ((*user-homedir-pathname-function* (lambda () mock-home)))
      (call-with-runtime-context context
        (lambda ()
          (add-skills skills-dir :persona-name "V")
          (fiveam:is (= 2 (length added-records)))
          (let* ((all-ids (mapcar (lambda (rec) (car (json-array-elements (cdr (assoc :ids rec))))) added-records))
                 (all-docs (mapcar (lambda (rec) (car (json-array-elements (cdr (assoc :documents rec))))) added-records)))
            (fiveam:is (not (null (find "skill-test-skill-1" all-ids :test #'string=))))
            (fiveam:is (not (null (find "skill-test-skill-2" all-ids :test #'string=))))
            (fiveam:is (not (null (find "Skill 1" all-docs :test #'string=))))
            (fiveam:is (not (null (find "Skill 2" all-docs :test #'string=))))))))
    (when (uiop:directory-exists-p mock-home)
      (uiop:delete-directory-tree mock-home :validate t))))
