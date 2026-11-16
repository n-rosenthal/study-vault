;;; nr-template.el --- Cabeçalhos e notas novas por tipo -*- lexical-binding: t; -*-

;; Gerado por tangle de org/05-extras.org. Edite o .org, não este arquivo.

;;; Code:

(require 'nr-core)
(require 'org-id)
(require 'ucs-normalize)

(defconst nr/modelos
  '(("estudo"  :dir "estudos/"  :status t   :corpo nr/--corpo-estudo)
    ("sintese" :dir "sinteses/" :status t   :corpo nr/--corpo-sintese)
    ("diario"  :dir "diario/"   :status nil :corpo nr/--corpo-diario))
  "Modelos por #+NOTE_TYPE: diretório, se leva #+STATUS: e função do corpo.")

(defun nr/--corpo-estudo (_tags)
  (concat "* Ideia central\n\n* Detalhes\n\n* Exemplos\n\n* Perguntas em aberto\n"))

(defun nr/--corpo-sintese (tags)
  (let ((tag (or (car (remove nr/tag-publicar tags)) "TAG")))
    (concat "* Notas de estudo\n"
            (format "#+BEGIN: notas-por-tag :tag \"%s\" :tipo \"estudo\" :resumo t\n" tag)
            "#+END:\n\n"
            "* Relacionadas\n"
            "#+BEGIN: notas-relacionadas :limite 5\n"
            "#+END:\n")))

(defun nr/--corpo-diario (_tags) "* ")

(defun nr/cabecalho (tipo titulo &rest plist)
  "Texto do cabeçalho de uma nota de TIPO com TITULO.
PLIST aceita :tags (lista), :descricao, :status, :data (AAAA-MM-DD), :id
e :publicar (non-nil adiciona a tag de publicação)."
  (let* ((cfg (cdr (or (assoc tipo nr/modelos)
                       (error "Tipo de nota desconhecido: %s" tipo))))
         (tags (delete-dups
                (append (plist-get plist :tags)
                        (unless (equal tipo "diario") (list tipo))
                        (and (plist-get plist :publicar) (list nr/tag-publicar)))))
         (id (or (plist-get plist :id) (org-id-uuid)))
         (data (or (plist-get plist :data) (format-time-string "%Y-%m-%d")))
         (status (or (plist-get plist :status) "semente")))
    (concat ":PROPERTIES:\n"
            (format ":ID:       %s\n" id)
            ":END:\n"
            (format "#+TITLE: %s\n" titulo)
            (format "#+DATE: [%s]\n" (format-time-string
                                      "%Y-%m-%d %a"
                                      (date-to-time (concat data " 12:00:00"))))
            (format "#+DESCRIPTION: %s\n" (or (plist-get plist :descricao) ""))
            (if (plist-get cfg :status) (format "#+STATUS: %s\n" status) "")
            (format "#+NOTE_TYPE: %s\n" tipo)
            (if tags (format "#+FILETAGS: :%s:\n" (string-join tags ":")) "")
            "\n")))

(defun nr/--slug (texto)
  "TEXTO em minúsculas, sem acentos, com hífens (slug de arquivo)."
  (let* ((s (ucs-normalize-NFD-string (downcase texto)))
         (s (replace-regexp-in-string "[\u0300-\u036f]" "" s))
         (s (replace-regexp-in-string "[^a-z0-9]+" "-" s)))
    (string-trim s "-" "-")))

(defun nr/criar-nota (tipo titulo &rest plist)
  "Cria o arquivo de uma nota nova e devolve o caminho. PLIST: `nr/cabecalho'."
  (let* ((cfg (cdr (assoc tipo nr/modelos)))
         (dir (expand-file-name (plist-get cfg :dir) nr/vault))
         (base (if (equal tipo "diario")
                   (format-time-string "%Y-%m")
                 (nr/--slug titulo)))
         (arq (expand-file-name (concat base ".org") dir)))
    (when (string-empty-p base) (user-error "Título inválido: %S" titulo))
    (when (file-exists-p arq) (user-error "Já existe: %s" arq))
    (make-directory dir t)
    (with-temp-file arq
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert (apply #'nr/cabecalho tipo titulo plist)
              (funcall (plist-get cfg :corpo) (plist-get plist :tags))))
    arq))

;;;###autoload
(defun nr/nova-nota (tipo titulo tags publicar)
  "Cria uma nota nova (e a abre). Sem confirmação explícita, ela fica privada."
  (interactive
   (list (completing-read "Tipo: " (mapcar #'car nr/modelos) nil t nil nil "estudo")
         (read-string "Título: ")
         (split-string (read-string "Tags (separadas por espaço): ") " " t)
         (y-or-n-p "Publicar no site? ")))
  (find-file (nr/criar-nota tipo titulo :tags tags :publicar publicar)))

(provide 'nr-template)
;;; nr-template.el ends here
