;;; tangle.el --- Tangle de todos os org/*.org  -*- lexical-binding: t; -*-
;; Uso: emacs -Q --batch -l scripts/tangle.el

(require 'org)
(require 'ob-tangle)

(let* ((raiz (file-name-directory
              (directory-file-name (file-name-directory load-file-name))))
       (org-confirm-babel-evaluate nil))
  (dolist (arquivo (directory-files (expand-file-name "org" raiz) t "\\`[^.#].*\\.org\\'"))
    (message "Tangle: %s" (file-name-nondirectory arquivo))
    (org-babel-tangle-file arquivo)))
