;;; nr-sinteses.el --- Bloco dinâmico notas-por-tag -*- lexical-binding: t; -*-

;; Gerado por tangle de org/03-site.org. Edite o .org, não este arquivo.

;;; Code:

(require 'nr-core)
(require 'nr-index)
(require 'org)

(defun nr/--ordenar-notas (notas ordem)
  (if (eq ordem 'titulo)
      (sort notas (lambda (a b) (string< (downcase (nr/nota-titulo a))
                                         (downcase (nr/nota-titulo b)))))
    (sort notas (lambda (a b)
                  (or (string> (nr/nota-data a) (nr/nota-data b))
                      (and (string= (nr/nota-data a) (nr/nota-data b))
                           (string< (nr/nota-rel a) (nr/nota-rel b))))))))

(defun nr/--sem-colchetes (texto)
  (replace-regexp-in-string "[][]" "" (or texto "")))

(defun nr/--linha-nota (nota resumo)
  "Item de lista Org para NOTA (com link por id, ou por arquivo se não houver id)."
  (let* ((titulo (nr/--sem-colchetes (nr/nota-titulo nota)))
         (link (if (nr/nota-id nota)
                   (format "[[id:%s][%s]]" (nr/nota-id nota) titulo)
                 (format "[[file:%s][%s]]"
                         (file-relative-name (nr/nota-arquivo nota)
                                             (file-name-directory (or (buffer-file-name)
                                                                      default-directory)))
                         titulo)))
         (desc (and resumo (nr/nota-desc nota))))
    (concat "- " link " — " (nr/nota-data nota)
            (when (nr/nota-status nota) (format " · %s" (nr/nota-status nota)))
            (when desc (format "\n  %s" desc)))))

(defun org-dblock-write:notas-por-tag (params)
  "Lista notas por tag. Parâmetros: veja org/03-site.org."
  (let* ((tag (or (plist-get params :tag) (user-error ":tag é obrigatório")))
         (tipo (or (plist-get params :tipo) "estudo"))
         (ordem (or (plist-get params :ordem) 'recentes))
         (limite (plist-get params :limite))
         (resumo (plist-get params :resumo))
         (so-publicaveis (if (plist-member params :somente-publicaveis)
                             (plist-get params :somente-publicaveis)
                           t))
         (este (and (buffer-file-name) (expand-file-name (buffer-file-name))))
         (notas (seq-filter
                 (lambda (n)
                   (and (member tag (nr/nota-tags n))
                        (equal (nr/nota-tipo n) tipo)
                        (not (equal (nr/nota-arquivo n) este))
                        (or (not so-publicaveis) (nr/nota-publicavel n))))
                 (nr/notas))))
    (setq notas (nr/--ordenar-notas notas ordem))
    (when limite (setq notas (seq-take notas limite)))
    (insert (if notas
                (mapconcat (lambda (n) (nr/--linha-nota n resumo)) notas "\n")
              "/(nenhuma nota encontrada)/"))))

(defun nr/--tem-dblock-p (arquivo)
  (with-temp-buffer
    (insert-file-contents arquivo)
    (let ((case-fold-search t))
      (re-search-forward "^[ \t]*#\\+BEGIN:[ \t]+notas-\\(?:por-tag\\|relacionadas\\)" nil t))))

(defun nr/atualizar-dblocks ()
  "Atualiza os blocos notas-por-tag de todo o vault (fora de diretórios proibidos).
Devolve quantos arquivos mudaram."
  (interactive)
  (nr/indice-construir)
  (let ((n 0))
    (dolist (nota (nr/notas))
      (let ((arq (nr/nota-arquivo nota)))
        (when (and (not (nr/diretorio-proibido-p arq)) (nr/--tem-dblock-p arq))
          (let ((buf (find-file-noselect arq)))
            (with-current-buffer buf
              ;; org-update-all-dblocks sempre marca o buffer como modificado:
              ;; compare o conteúdo para só gravar (e contar) o que mudou de fato.
              (let ((antes (buffer-string)))
                (org-update-all-dblocks)
                (when (not (equal antes (buffer-string)))
                  (save-buffer)
                  (setq n (1+ n))))
              (set-buffer-modified-p nil))
            (kill-buffer buf)))))
    (message "Blocos atualizados em %d arquivo(s)" n)
    n))

(provide 'nr-sinteses)
;;; nr-sinteses.el ends here
