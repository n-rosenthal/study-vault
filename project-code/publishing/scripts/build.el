;;; build.el --- Ponto de entrada do build (local e CI)  -*- lexical-binding: t; -*-
;; Uso: emacs -Q --batch -l scripts/build.el
;; Variáveis de ambiente: NR_VAULT, NR_SAIDA, NR_SITE_URL (veja org/01-nucleo.org).

(require 'package)
(package-initialize)   ; para achar o htmlize instalado, se houver

(let ((raiz (file-name-directory
             (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" raiz)))

(require 'nr)

(condition-case err
    (nr/publicar t)
  (error
   (message "ERRO NO BUILD: %S" err)
   (kill-emacs 1)))
