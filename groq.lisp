(ql:quickload '(:drakma :cl-json :uiop))

(defun send-groq-request (content)
  (let* ((url "https://api.groq.com/openai/v1/chat/completions")
         (api-key (uiop:getenv "GROQ_API_KEY"))
         (headers `(("Authorization" . ,(concatenate 'string "Bearer " api-key))
                    ("Content-Type" . "application/json")))
         (data `(("model" . "llama3-70b-8192")
		 ("messages" . ((("role" . "system")
				 ("content" . "content"))
                                (("role" . "user")
				 ("content" . ,content))))))
         (json-data (cl-json:encode-json-to-string data)))
    (cl-json:decode-json-from-string
     (flexi-streams:octets-to-string
      (drakma:http-request url
                           :method :post
                           :content-type "application/json"
                           :additional-headers headers
                           :content json-data)))))

(defun extract-content (resp)
  (cdr (nth 2 (cadr (cadr (assoc :choices resp))))))

(defun testg ()
  (let ((resp (send-groq-request "How do I get better at programming?")))
    (print resp)
    (terpri)
    (print (extract-content resp))))
