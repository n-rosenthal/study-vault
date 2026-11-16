;;; ci-deps.el --- Dependências do Emacs no CI  -*- lexical-binding: t; -*-
;; htmlize é opcional: sem ele o código sai sem destaque de sintaxe.

(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless (package-installed-p 'htmlize)
  (with-timeout (120 (message "AVISO: tempo esgotado instalando htmlize; código sem destaque"))
    (condition-case err
        (progn (package-refresh-contents)
               (package-install 'htmlize))
      (error (message "AVISO: htmlize não instalado (%S); código sem destaque" err)))))
