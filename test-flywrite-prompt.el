;;; test-flywrite-prompt.el --- Prompt regression tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression tests that send text samples to a real LLM API and verify
;; the system prompt catches (or does not flag) specific writing flaws.
;;
;; Requires FLYWRITE_API_KEY_ANTHROPIC env var.
;; Results are cached in test-flywrite-prompt-cache.json to avoid
;; redundant API calls.
;;
;; Run with:
;;   emacs -Q --batch -l flywrite-mode.el -l test-flywrite-prompt.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'flywrite-mode)
(require 'json)
(require 'url)
(require 'url-http)

;;;; ---- Test inputs ----

(defconst flywrite-prompt-test--inputs
  '((:text "The quick brown fox jumped over the lazy dog."
     :description "clean"
     :expected 0)
    (:text "The morning light filtered through the curtains and cast long shadows across the floor."
     :description "clean"
     :expected 0)
    (:text "She picked up her coffee, took a quiet sip, and turned to the first page of the newspaper."
     :description "clean"
     :expected 0)
    (:text "The results don't support the hypothesis, and it's really not a big deal."
     :description "contractions and informal language in academic writing"
     :expected 2)
    (:text "Him and his friend went to the store to buy some grocerys."
     :description "pronoun case error and misspelling"
     :expected 2)
    (:text "Their going to the park later today, irregardless of the rain."
     :description "wrong homophone and nonstandard word"
     :expected 2)
    (:text "Each of the students need to submit there homework by Friday."
     :description "subject-verb disagreement and wrong homophone"
     :expected 2)
    (:text "She could of finished the report on time if she would have started earlier."
     :description "could of and would have"
     :expected 2)
    (:text "Between you and I, this project is more bigger than we expected."
     :description "pronoun case and double comparative"
     :expected 2)
    (:text "The weather was very extremely hot outside yesterday."
     :description "redundant intensifiers"
     :expected 1))
  "Test inputs: each entry is a plist with :text, :description, :expected.")

;;;; ---- Cache ----

(defvar flywrite-prompt-test--cache-file
  (expand-file-name "test-flywrite-prompt-cache.json"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the prompt test cache file.")

(defvar flywrite-prompt-test--cache nil
  "In-memory cache: list of alists read from the cache file.")

(defun flywrite-prompt-test--load-cache ()
  "Load cache from `flywrite-prompt-test--cache-file'."
  (setq flywrite-prompt-test--cache
        (if (file-readable-p flywrite-prompt-test--cache-file)
            (condition-case nil
                (let ((json-array-type 'list)
                      (json-object-type 'alist)
                      (json-key-type 'string))
                  (json-read-file flywrite-prompt-test--cache-file))
              (error nil))
          nil)))

(defun flywrite-prompt-test--save-cache ()
  "Write cache to `flywrite-prompt-test--cache-file'."
  (with-temp-file flywrite-prompt-test--cache-file
    (insert (json-encode flywrite-prompt-test--cache))
    (json-pretty-print (point-min) (point-max))))

(defun flywrite-prompt-test--prompt-hash ()
  "Return MD5 hash of the current system prompt string."
  (md5 (flywrite--get-system-prompt)))

(defun flywrite-prompt-test--cache-lookup (text model prompt-hash)
  "Find a cache entry matching TEXT, MODEL, and PROMPT-HASH."
  (cl-find-if
   (lambda (entry)
     (and (equal (alist-get "text" entry nil nil #'equal) text)
          (equal (alist-get "model" entry nil nil #'equal) model)
          (equal (alist-get "prompt_hash" entry nil nil #'equal) prompt-hash)))
   flywrite-prompt-test--cache))

(defun flywrite-prompt-test--cache-store (text model prompt-hash response)
  "Store a cache entry for TEXT, MODEL, PROMPT-HASH, and RESPONSE."
  (let ((entry `(("text" . ,text)
                 ("model" . ,model)
                 ("prompt_hash" . ,prompt-hash)
                 ("response" . ,response)
                 ("timestamp" . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))))
    (push entry flywrite-prompt-test--cache)
    (flywrite-prompt-test--save-cache)))

;;;; ---- Synchronous API call ----

(defun flywrite-prompt-test--call-api (text)
  "Send TEXT to the LLM API synchronously and return the response string.
Uses flywrite configuration for URL, model, API key, and system prompt."
  (let* ((api-key (or (getenv "FLYWRITE_API_KEY_ANTHROPIC")
                      (error "No API key.  Set FLYWRITE_API_KEY_ANTHROPIC")))
         (request (flywrite--build-request text api-key))
         (url-request-method "POST")
         (url-request-extra-headers (cdr request))
         (url-request-data (encode-coding-string (car request) 'utf-8))
         (response-buf (url-retrieve-synchronously flywrite-api-url t nil 30)))
    (unless response-buf
      (error "API call returned no response buffer"))
    (unwind-protect
        (with-current-buffer response-buf
          (goto-char (point-min))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (error "Malformed HTTP response"))
          (let* ((json-data (json-read))
                 (text-content
                  (if (flywrite--anthropic-api-p)
                      (let* ((content (alist-get 'content json-data))
                             (block (and (arrayp content)
                                         (> (length content) 0)
                                         (aref content 0))))
                        (and block (alist-get 'text block)))
                    (let* ((choices (alist-get 'choices json-data))
                           (choice (and (arrayp choices)
                                        (> (length choices) 0)
                                        (aref choices 0)))
                           (message (and choice (alist-get 'message choice))))
                      (and message (alist-get 'content message))))))
            (unless text-content
              (error "No text in API response: %S" json-data))
            text-content))
      (kill-buffer response-buf))))

(defun flywrite-prompt-test--parse-suggestions (response-text)
  "Parse RESPONSE-TEXT as JSON and return the suggestions array as a list."
  (let* ((clean (replace-regexp-in-string
                 "\\`[ \t\n]*```\\(?:json\\)?[ \t]*\n?" ""
                 (replace-regexp-in-string
                  "\n?```[ \t\n]*\\'" "" response-text)))
         (json-array-type 'list)
         (parsed (json-read-from-string clean)))
    (alist-get 'suggestions parsed)))

;;;; ---- Test configuration ----

(defun flywrite-prompt-test--configure ()
  "Set up flywrite API configuration for prompt tests.
Uses Anthropic as the default provider."
  (unless flywrite-api-url
    (setq flywrite-api-url "https://api.anthropic.com/v1/messages"))
  (setq flywrite-api-temperature 0))

;;;; ---- Core test runner ----

(defun flywrite-prompt-test--run-one (input)
  "Run a single prompt test for INPUT plist.
Returns the number of suggestions from the API (using cache when available)."
  (flywrite-prompt-test--configure)
  (let* ((text (plist-get input :text))
         (model (flywrite--effective-model))
         (prompt-hash (flywrite-prompt-test--prompt-hash))
         (cached (flywrite-prompt-test--cache-lookup text model prompt-hash))
         (response-text
          (if cached
              (progn
                (message "  [cached] %s" (plist-get input :description))
                (alist-get "response" cached nil nil #'equal))
            (message "  [api] %s" (plist-get input :description))
            (let ((resp (flywrite-prompt-test--call-api text)))
              (flywrite-prompt-test--cache-store text model prompt-hash resp)
              resp)))
         (suggestions (flywrite-prompt-test--parse-suggestions response-text)))
    (length suggestions)))

;;;; ---- ERT tests ----

(defun flywrite-prompt-test--run-all ()
  "Run all prompt regression tests.  Return list of (input count pass) triples."
  (flywrite-prompt-test--load-cache)
  (let ((results nil))
    (dolist (input flywrite-prompt-test--inputs)
      (let* ((expected (plist-get input :expected))
             (count (flywrite-prompt-test--run-one input))
             (pass (= count expected)))
        (push (list input count pass) results)))
    (nreverse results)))

(ert-deftest flywrite-prompt-test-regression ()
  "Verify system prompt correctly catches or ignores writing flaws.
Each sample is sent to the LLM and must return the exact expected
number of suggestions."
  (let ((results (flywrite-prompt-test--run-all))
        (failures nil))
    (dolist (result results)
      (let* ((input (nth 0 result))
             (count (nth 1 result))
             (pass (nth 2 result))
             (text (plist-get input :text))
             (desc (plist-get input :description))
             (expected (plist-get input :expected)))
        (unless pass
          (push (format "FAIL: %s\n  text: %s\n  expected: %d, got: %d"
                        desc text expected count)
                failures))))
    (when failures
      (ert-fail (mapconcat #'identity (nreverse failures) "\n\n")))))

(provide 'test-flywrite-prompt)

;;; test-flywrite-prompt.el ends here
