;;; nr.el --- Pipeline de publicação org → PDF e web -*- lexical-binding: t; -*-

;; Gerado por tangle de org/04-operacao.org. Edite o .org, não este arquivo.

;; Uso no init.el:
;;   (add-to-list 'load-path "~/org/roam/project-code/internal/publishing/lisp")
;;   (require 'nr)

;;; Code:

(require 'nr-core)
(require 'nr-index)
(require 'nr-export)
(require 'nr-diario)
(require 'nr-sinteses)
(require 'nr-relacionadas)
(require 'nr-grafo)
(require 'nr-og)
(require 'nr-template)
(require 'nr-site)

(provide 'nr)
;;; nr.el ends here
