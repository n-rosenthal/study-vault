;;; nr-grafo.el --- Grafo de links em JSON -*- lexical-binding: t; -*-

;; Gerado por tangle de org/03-site.org. Edite o .org, não este arquivo.

;;; Code:

(require 'nr-core)
(require 'nr-index)
(require 'json)

(defun nr/grafo-id (nota)
  "Identificador do nó de NOTA no JSON: caminho relativo sem extensão."
  (file-name-sans-extension (nr/nota-rel nota)))

(defun nr/grafo-dados ()
  "Alist (nodes . [...]) (links . [...]) das notas publicáveis."
  (let ((notas (nr/notas t)) nos arestas vistas)
    (dolist (n notas)
      (push `((id . ,(nr/grafo-id n))
              (titulo . ,(nr/nota-titulo n))
              (url . ,(nr/url n))
              (tipo . ,(nr/nota-tipo n))
              (status . ,(or (nr/nota-status n) ""))
              (tags . ,(vconcat (nr/tags-visiveis n))))
            nos))
    (dolist (n notas)
      (dolist (id (delete-dups (copy-sequence (nr/nota-links-id n))))
        (when-let ((alvo (nr/nota-por-id id)))
          (when (and (nr/nota-publicavel alvo) (not (eq alvo n)))
            (let* ((a (nr/grafo-id n))
                   (b (nr/grafo-id alvo))
                   (chave (if (string< a b) (cons a b) (cons b a))))   ; sem direção
              (unless (member chave vistas)
                (push chave vistas)
                (push `((source . ,a) (target . ,b)) arestas)))))))
    `((nodes . ,(vconcat (nreverse nos)))
      (links . ,(vconcat (nreverse arestas))))))

(defun nr/gerar-grafo (&optional destino)
  "Escreve grafo.json em DESTINO (padrão: raiz do site gerado)."
  (interactive)
  (let ((destino (or destino (expand-file-name "grafo.json" nr/saida)))
        (dados (nr/grafo-dados))
        (json-encoding-pretty-print nil))
    (make-directory (file-name-directory destino) t)
    (with-temp-file destino
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert (json-encode dados)))
    (message "Grafo: %d nós, %d arestas → %s"
             (length (alist-get 'nodes dados)) (length (alist-get 'links dados)) destino)
    destino))

(provide 'nr-grafo)
;;; nr-grafo.el ends here
