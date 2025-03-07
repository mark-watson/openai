(in-package #:openai)

;; define the environment variable "OPENAI_KEY" with the value of your OpenAI API key

(defvar *model-host* "https://api.openai.com/v1/chat/completions")
;; use gpt-4o for very good results, or gpt-4o-mini to save abt 20x on costs, with similar results:
(defvar *model* "gpt-4o-mini")

;; Hash table to store available functions for tool calling
(defvar *available-functions* (make-hash-table :test 'equal))

(defstruct openai-function
  name
  description
  parameters)

(defun register-function (name description parameters)
  (setf (gethash name *available-functions*)
        (make-openai-function
         :name name
         :description description
         :parameters parameters)))

; #S(openai-function
;    :name get_weather
;    :description Get current weather for a location
;    :parameters ((type . object)
;                 (properties
;                  (location (type . string)
;                            (description . The city name)))
;                 (required location)))

(defun lisp-to-json-string (data)
  (with-output-to-string (s)
    (json:encode-json data s)))

(defun substitute-subseq (string old new &key (test #'eql))
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
        string)))

(defun escape-json (str)
  (with-output-to-string (out)
    (loop for ch across str do
         (if (char= ch #\")
             (write-string "\\\"" out)
             (write-char ch out)))))

(defvar *xx* nil)

(defun handle-function-call (function-call)
  (setf *xx* function-call)
  ;; *xx* looks like: ((:name . "get_weather") (:arguments . "{\"location\":\"New York\"}"))
  (format t "~% ** handle-function-call (DUMMY) fucntion-call: ~A~%" function-call)
  (let* ((name (cdr (assoc :name function-call)))
         (args-string (cdr (assoc :arguments function-call)))
         (args (and args-string (cl-json:decode-json-from-string args-string)))
         (func (gethash name *available-functions*)))
    (format t "~% handle-function-call name: ~A" name)
    (format t "~% handle-function-call args-string: ~A" args-string)
    (format t "~% handle-function-call args: ~A" args)
    (format t "~% handle-function-call func: ~A" func)
    (if func
        (format nil "Function ~a called with args: ~a" name args)
        (error "Unknown function: ~a" name))))

(defun openai-helper (curl-command)
  (terpri)
  (princ curl-command)
  (terpri)
  (let ((response (uiop:run-program curl-command
                                    :output :string
                                    :error-output :string)))
    (terpri)
    (princ response)
    (terpri)
    (with-input-from-string (s response)
      (let* ((json-as-list (json:decode-json s))
             (choices (cdr (assoc :choices json-as-list)))
             (first-choice (car choices))
             (message (cdr (assoc :message first-choice)))
             (function-call (cdr (assoc :function--call message)))
             (content (cdr (assoc :content message))))
	(format t "~% json-as-list: ~A~%" json-as-list)
	(format t "~% choices: ~A~%" choices)
	(format t "~% first-choice: ~A~%" first-choice)
	(format t "~% message: ~A~%" message)
	(format t "~% function-call: ~A~%" function-call)
	(format t "~% content: ~A~%" content)
        (if function-call
            (handle-function-call function-call)
            (or content "No response content"))))))


(defun completions (starter-text max-tokens &optional functions)
  "Send a completion request to OpenAI API with given text, token limit, and optional functions for tool calling.
Example:
  (completions \"Use function calling for: What's the weather like in New York?\" 1000 '(\"get_weather\" \"calculate\"))"
  (unless (numberp max-tokens)
    (error "max-tokens must be a number, got: ~a" max-tokens))
  (let* ((function-defs (when functions
                          (mapcar (lambda (f)
                                    (let ((func (gethash f *available-functions*)))
                                      (list (cons :name (openai-function-name func))
                                            (cons :description (openai-function-description func))
                                            (cons :parameters (openai-function-parameters func)))))
                                  functions)))
         (message (list (cons :role "user")
                        (cons :content starter-text)))
         (base-data `((model . ,*model*)
                      (messages . ,(list message))
                      (max_tokens . ,max-tokens)))
         (data (if function-defs
                   (append base-data (list (cons :functions function-defs)))
                   base-data))
         (request-body (cl-json:encode-json-to-string data))
         (fixed-json-data (substitute-subseq request-body ":null" ":false" :test #'string=))
         (escaped-json (escape-json fixed-json-data))
         (curl-command
          (format nil "curl ~A -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~A\" -d \"~A\""
                  *model-host*
                  (uiop:getenv "OPENAI_KEY")
                  escaped-json)))
    (openai-helper curl-command)))

(defun summarize (some-text max-tokens)
  (let ((curl-command
         (concatenate 'string
                      "curl " *model-host*
                      " -H \"Content-Type: application/json\""
                      " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" "
                      " -d '{\"messages\": [{\"role\": \"user\", \"content\": \"Summarize: " some-text 
                      "\"}], \"model\": \"gpt-4\", \"max_tokens\": " (write-to-string max-tokens) "}'")))
    (openai-helper curl-command)))

(defun answer-question (question-text max-tokens)
  (completions question-text max-tokens))

(defun embeddings (text)
  "Get embeddings using text-embedding-3-small model (1536 dimensions)"
  (let* ((curl-command
          (concatenate 'string
                       "curl https://api.openai.com/v1/embeddings "
                       " -H \"Content-Type: application/json\""
                       " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" "
                       " -d '{\"input\": \"" text 
                       "\", \"model\": \"text-embedding-3-small\"}'"))
         (response (uiop:run-program curl-command :output :string)))
    (with-input-from-string (s response)
      (let ((json-as-list (json:decode-json s)))
        (cdr (nth 2 (cadr (cadr json-as-list))))))))

(defun dot-product-recursive (a b)
  "Calculate dot product recursively"
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product-recursive (rest a) (rest b)))))

(defun dot-product (list1 list2)
  "Calculate dot product iteratively"
  (let ((sum 0))
    (loop for x in list1
          for y in list2
          do (setf sum (+ sum (* x y))))
    sum))

;;; Sample registrations for functions used in tool calling

(register-function
 "get_weather"
 "Get current weather for a location"
 (list (cons :type "object")
       (cons :properties (list (cons :location (list (cons :type "string")
                                                     (cons :description "The city name")))))
       (cons :required '("location"))))

(register-function
 "calculate"
 "Perform a mathematical calculation"
 (list (cons :type "object")
       (cons :properties (list (cons :expression (list (cons :type "string")
                                                         (cons :description "Math expression like 2 + 2")))))
       (cons :required '("expression"))))

;(openai::completions "Use function calling for: What's the weather like in New York?" 1000 '("get_weather" "calculate"))
;(terpri) (terpri) (terpri) (terpri) (terpri) 
;(completions "The President went to Congress" 20)

#|
;; Example calls:

(print (completions "The President went to Congress" 20))
(print (summarize "Jupiter is the fifth planet from the Sun..." 30))
(print (answer-question "Where were the 1992 Olympics held?" 60))
(print (answer-question "Where is the Valley of Kings?" 60))
(print (answer-question "Mary is 30 years old and Bob is 25. Who is older?" 60))
(print (completions "Use function calling for: What's the weather like in New York?" 100 '("get_weather" "calculate")))
|#

