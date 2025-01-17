(in-package #:openai)

;; define the environment variable "OPENAI_KEY" with the value of your OpenAI API key

(defvar *model-host* "https://api.openai.com/v1/chat/completions")
;; use gpt-4o for very good results, or gpt-4o-mini to save abt 20x on costs, with similar results:
(defvar *model* "gpt-4o-mini")

(defun openai-helper (curl-command)
  ;;(princ curl-command)
  (let ((response
          (uiop:run-program
           curl-command
           :output :string)))
    ;;(pprint response)
    (with-input-from-string
        (s response)
      (let* ((json-as-list (json:decode-json s)))
        ;; extract text (this might change if OpenAI changes JSON return format):
        (cdr (assoc :content (cdr (assoc :message (cadr (assoc :choices json-as-list))))))))))


(defun completions (starter-text max-tokens)
  "Send a completion request to OpenAI API with given text and token limit"
  (let* ((input-text (write-to-string starter-text))
         (request-body
          (cl-json:encode-json-to-string
           `((:messages . (((:role . "user") (:content . ,input-text))))
             (:model . ,*model*) 
             (:max_tokens . ,max-tokens))))
         (curl-command
           (format nil 
                  "curl ~A ~
                   -H \"Content-Type: application/json\" ~
                   -H \"Authorization: Bearer ~A\" ~
                   -d '~A'"
                   *model-host*
                   (uiop:getenv "OPENAI_KEY")
                   request-body)))
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

;; (print (dot-product '(1 2 3) '(4 5 6)))
;; (print (openai::embeddings "John bought a new car"))

#|

(print (openai:completions "The President went to Congress" 20))

(print (openai:summarize "Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter.[19] When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows,[20] and is on average the third-brightest natural object in the night sky after the Moon and Venus." 30))

(print (openai:answer-question "Where were the 1992 Olympics held?" 60))
(print (openai:answer-question "Where is the Valley of Kings?" 60))
(print (openai:answer-question "Mary is 30 years old and Bob is 25. Who is older?" 60))

(print  (openai:completions "A train leaves Boston heading west at 60 mph. Two hours later, a car leaves from the same place heading in the same direction at 80 mph. At what time does the car catch up to the train?" 1000))

|#
