;;; text-transform.el --- Org text transformations -*- lexical-binding: t; -*-

(require 'org-element)
(require 'aio)
(require 'json)
(require 'subr-x)

(defconst text-transform--tokenizer-script
  (expand-file-name "code/tokenizer.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the Python tokenizer, expected next to this file.")

;; fn to_ast(document: org-document) -> ast
;;  extracts the syntax tree from a given ~.org~ DOCUMENT.
(defun to-ast (&optional buffer)
  "Parse BUFFER (or current buffer) into an Org syntax tree."
  (with-current-buffer (or buffer (current-buffer))
    (org-element-parse-buffer)))

;; fn sections(tree: ast) -> list[ast?]
;;  extracts all SECTIONS from a given TREE.
(defun sections (tree)
  "Return a list of all `section' elements in TREE."
  (org-element-map tree 'section #'identity))

;; fn to_text(tree: ast) -> string
(defun to-text (tree)
  "Interpret TREE as canonical Org syntax and return it as a string."
  (org-element-interpret-data tree))

;; fn async tokenize(string) -> Promise[list[token]]
;;  runs tokenizer.py in a subprocess on a given STRING.
(defun async-tokenize (string)
  "Asynchronously tokenize STRING using the Python tokenizer subprocess.

Return an `aio' promise that resolves to a list of tokens, or that
propagates an error if the subprocess fails or emits invalid JSON."
  (let ((promise    (aio-promise))
        (output     "")
        (stderr-buf (generate-new-buffer " *tokenizer-stderr*")))
    (let ((proc (make-process
                 :name "tokenizer"
                 :buffer nil
                 :stderr stderr-buf
                 :command (list "python3" text-transform--tokenizer-script)
                 :connection-type 'pipe
                 :coding 'utf-8
                 :noquery t
                 :filter (lambda (_proc chunk)
                           (setq output (concat output chunk)))
                 :sentinel
                 (lambda (proc _event)
                   (when (memq (process-status proc) '(exit signal))
                     (let* ((status (process-exit-status proc))
                            (err    (with-current-buffer stderr-buf
                                      (string-trim (buffer-string))))
                            (raw    (string-trim output))
                            (msg    nil)
                            (tokens nil))
                       (kill-buffer stderr-buf)
                       (cond
                        ((not (zerop status))
                         (setq msg (format "tokenizer exited %d: %s" status err)))
                        ((string-empty-p raw)
                         (setq msg (format "tokenizer produced no output (stderr: %s)" err)))
                        (t
                         (condition-case e
                             (setq tokens (json-parse-string raw :array-type 'list))
                           (error (setq msg (format "invalid tokenizer JSON: %s"
                                                    (error-message-string e)))))))
                       ;; `aio-resolve' takes a *function*; signalling inside it
                       ;; rejects the promise.  Signalling directly in a sentinel
                       ;; would be swallowed and the promise would hang forever.
                       (aio-resolve promise
                                    (lambda ()
                                      (if msg
                                          (signal 'error (list msg))
                                        tokens)))))))))
      (process-send-string proc string)
      (process-send-eof proc))
    promise))

(aio-defun transform-current-buffer ()
  "Tokenize all sections of the current buffer and report the counts."
  (let* ((tree     (to-ast))
         (secs     (sections tree))
         (text     (mapconcat #'to-text secs "\n"))
         (tokens   (aio-await (async-tokenize text))))
    (message "Parsed %d sections → %d tokens" (length secs) (length tokens))
    tokens))

(provide 'text-transform)
;;; text-transform.el ends here
