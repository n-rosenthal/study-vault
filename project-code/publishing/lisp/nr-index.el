;;; nr-index.el --- Índice do vault: notas, ids, links, backlinks -*- lexical-binding: t; -*-

;; Gerado por tangle de org/01-nucleo.org. Edite o .org, não este arquivo.

;;; Code:

(require 'nr-core)
(require 'url-util)

(cl-defstruct (nr/nota (:constructor nr/nota--novo))
  arquivo rel titulo id ids tipo tags data status desc publicavel
  palavras links-id links-arq)

(defvar nr/indice nil "Hash arquivo → `nr/nota', ou nil se ainda não construído.")
(defvar nr/indice--por-id nil "Hash id → `nr/nota'.")
(defvar nr/indice--backlinks nil "Hash arquivo → lista de `nr/nota' que linkam para ele.")

;;;; Leitura de uma nota

(defun nr/--remover-blocos ()
  "Apaga do buffer atual blocos src/example/export/comment/verse."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (re-search-forward
            "^[ \t]*#\\+begin_\\(src\\|example\\|export\\|comment\\|verse\\)\\b" nil t)
      (let ((ini (line-beginning-position))
            (tipo (match-string 1)))
        (if (re-search-forward (format "^[ \t]*#\\+end_%s\\b.*$" tipo) nil t)
            (delete-region ini (min (1+ (point)) (point-max)))
          (delete-region ini (point-max)))))))

(defun nr/--palavra-chave (chave limite)
  "Valor de #+CHAVE: antes de LIMITE no buffer atual, ou nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             (format "^#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote chave)) limite t)
        (let ((v (string-trim (match-string 1))))
          (unless (string-empty-p v) v))))))

(defun nr/--tags (valor)
  (split-string (or valor "") "[: \t]+" t))

(defun nr/--coletar (regexp grupo)
  "Lista (na ordem do arquivo) das capturas GRUPO de REGEXP no buffer."
  (goto-char (point-min))
  (let (res)
    (while (re-search-forward regexp nil t)
      (push (match-string grupo) res))
    (nreverse res)))

(defun nr/--sem-ancora (caminho)
  (car (split-string caminho "::")))

(defun nr/nota-ler (arquivo)
  "Lê ARQUIVO e devolve uma `nr/nota'."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents arquivo))
    (nr/--remover-blocos)
    (goto-char (point-min))
    (let* ((limite (save-excursion
                     (if (re-search-forward "^\\*+ " nil t)
                         (match-beginning 0)
                       (point-max))))
           (kw (lambda (k) (nr/--palavra-chave k limite)))
           (tags (nr/--tags (funcall kw "FILETAGS")))
           (rel (file-relative-name arquivo nr/vault))
           ids id-arquivo)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:ID:[ \t]+\\([^ \t\n]+\\)" nil t)
        (when (and (null id-arquivo) (< (match-beginning 0) limite))
          (setq id-arquivo (match-string 1)))
        (push (match-string 1) ids))
      (nr/nota--novo
       :arquivo arquivo
       :rel rel
       :titulo (or (funcall kw "TITLE") (file-name-base arquivo))
       :id id-arquivo
       :ids (nreverse ids)
       :tipo (nr/normalizar-tipo (funcall kw "NOTE_TYPE"))
       :tags tags
       :data (or (nr/data-curta (funcall kw "DATE"))
                 (format-time-string
                  "%Y-%m-%d" (file-attribute-modification-time (file-attributes arquivo))))
       :status (nr/normalizar-status (funcall kw "STATUS"))
       :desc (funcall kw "DESCRIPTION")
       :publicavel (and (member nr/tag-publicar tags)
                        (not (member nr/tag-privada tags))
                        (not (nr/diretorio-proibido-p arquivo))
                        t)
       :palavras (count-words (point-min) (point-max))
       :links-id (nr/--coletar "\\[\\[id:\\([^]:\n]+\\)" 1)
       :links-arq (mapcar #'nr/--sem-ancora
                          (append (nr/--coletar "\\[\\[file:\\([^]\n]+\\)\\]" 1)
                                  (nr/--coletar "\\[\\[\\(\\.\\.?/[^]\n]+\\)\\]" 1)))))))

;;;; Índice

(defun nr/indice-construir ()
  "Lê todo o vault e (re)constrói o índice. Devolve o hash de notas."
  (interactive)
  (let ((notas (make-hash-table :test 'equal))
        (por-id (make-hash-table :test 'equal))
        (backlinks (make-hash-table :test 'equal)))
    (dolist (arq (nr/arquivos-org))
      (let ((n (nr/nota-ler arq)))
        (unless (equal (nr/nota-tipo n) "indice")
          (puthash arq n notas)
          (dolist (id (nr/nota-ids n))
            (puthash id n por-id)))))
    (maphash
     (lambda (_ n)
       (dolist (id (delete-dups (copy-sequence (nr/nota-links-id n))))
         (when-let* ((alvo (gethash id por-id)))
           (unless (eq alvo n)
             (cl-pushnew n (gethash (nr/nota-arquivo alvo) backlinks) :test #'eq)))))
     notas)
    (setq nr/indice notas
          nr/indice--por-id por-id
          nr/indice--backlinks backlinks)
    (nr/--sincronizar-org-id)
    (when (called-interactively-p 'interactive)
      (message "Índice: %d notas, %d publicáveis"
               (hash-table-count notas) (length (nr/notas t))))
    notas))

(defun nr/--sincronizar-org-id ()
  "Informa ao org-id onde está cada :ID:, para links id: resolverem no export."
  (require 'org-id)
  (unless (hash-table-p org-id-locations)
    (setq org-id-locations (make-hash-table :test 'equal)))
  (maphash (lambda (id n) (puthash id (nr/nota-arquivo n) org-id-locations))
           nr/indice--por-id))

(defun nr/indice ()
  "Hash de notas; constrói na primeira chamada."
  (or nr/indice (nr/indice-construir)))

(defun nr/notas (&optional so-publicaveis)
  "Lista de notas ordenada por caminho; só as publicáveis se SO-PUBLICAVEIS."
  (let (res)
    (maphash (lambda (_ n)
               (when (or (not so-publicaveis) (nr/nota-publicavel n))
                 (push n res)))
             (nr/indice))
    (sort res (lambda (a b) (string< (nr/nota-rel a) (nr/nota-rel b))))))

(defun nr/nota-por-arquivo (arquivo)
  (gethash (expand-file-name arquivo) (nr/indice)))

(defun nr/nota-por-id (id)
  (nr/indice)
  (gethash id nr/indice--por-id))

(defun nr/backlinks (nota)
  "Notas *publicáveis* que linkam para NOTA, em ordem de título."
  (nr/indice)
  (sort (seq-filter #'nr/nota-publicavel
                    (gethash (nr/nota-arquivo nota) nr/indice--backlinks))
        (lambda (a b) (string< (downcase (nr/nota-titulo a))
                               (downcase (nr/nota-titulo b))))))

(defun nr/url (nota)
  "URL (relativa à raiz do site) da página HTML de NOTA."
  (concat "/"
          (mapconcat #'url-hexify-string
                     (split-string (file-name-sans-extension (nr/nota-rel nota)) "/")
                     "/")
          ".html"))

(defun nr/tags-visiveis (nota)
  "Tags de NOTA sem a tag de publicação."
  (remove nr/tag-publicar (nr/nota-tags nota)))

(provide 'nr-index)
;;; nr-index.el ends here
