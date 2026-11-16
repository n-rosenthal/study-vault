;;; nr-template.el --- Cabeçalhos e notas novas por tipo -*- lexical-binding: t; -*-

;; Gerado por tangle de org/05-extras.org. Edite o .org, não este arquivo.

;;; Code:

(require 'nr-core)
(require 'org-id)
(require 'ucs-normalize)
(require 'nr-diario)

(defconst nr/modelos
  '(("study"     :dir "estudos/"  :status t   :corpo nr/--corpo-estudo)
    ("synthesis" :dir "sinteses/" :status t   :corpo nr/--corpo-sintese)
    ("journal"   :dir "diario/"   :status nil :corpo nr/--corpo-diario))
  "Modelos por #+NOTE_TYPE: diretório, se leva #+STATUS: e função do corpo.")

(defun nr/--corpo-estudo (_tags)
  (concat "* Ideia central\n\n* Detalhes\n\n* Exemplos\n\n* Perguntas em aberto\n"))

(defun nr/--corpo-sintese (tags)
  (let ((tag (or (car (remove nr/tag-publicar tags)) "TAG")))
    (concat "* Notas de estudo\n"
            (format "#+BEGIN: notas-por-tag :tag \"%s\" :tipo \"study\" :resumo t\n" tag)
            "#+END:\n\n"
            "* Relacionadas\n"
            "#+BEGIN: notas-relacionadas :limite 5\n"
            "#+END:\n")))

(defun nr/--corpo-diario (_tags) "")

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
         (status (or (plist-get plist :status) "seed")))
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
         (dir (if (equal tipo "journal")
                  nr/diario-dir
                (expand-file-name (plist-get cfg :dir) nr/vault)))
         (base (if (equal tipo "journal")
                   (or (plist-get plist :data) (format-time-string "%Y-%m-%d"))
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
   (list (completing-read "Tipo: " (mapcar #'car nr/modelos) nil t nil nil "study")
         (read-string "Título: ")
         (split-string (read-string "Tags (separadas por espaço): ") " " t)
         (y-or-n-p "Publicar no site? ")))
  (find-file (nr/criar-nota tipo titulo :tags tags :publicar publicar)))

;;;###autoload
(defun nr/nova-nota-estudo (titulo &optional tags)
  "Cria uma nota de estudo privada com TITULO e a abre.
Com prefixo (\\[universal-argument]) pergunta também se ela é publicada."
  (interactive
   (list (read-string "Título: ")
         (split-string (read-string "Tags (espaço): ") " " t)))
  (find-file (nr/criar-nota "study" titulo :tags tags
                            :publicar (and current-prefix-arg
                                           (y-or-n-p "Publicar no site? ")))))

(defun nr/--entrada-diario (&optional titulo data)
  "Texto de uma entrada nova do diário; :CREATED: é DATA (padrão hoje) + hora."
  (format "* %s%s\n:PROPERTIES:\n:CREATED:  %s\n:END:\n\n"
          (format-time-string "%H:%M")
          (if (and titulo (not (string-empty-p titulo))) (concat " — " titulo) "")
          (format-time-string
           "[%Y-%m-%d %a %H:%M]"
           (date-to-time (concat (or data (format-time-string "%Y-%m-%d"))
                                 (format-time-string " %H:%M:%S"))))))

;;;###autoload
(defun nr/nova-nota-diario (&optional data titulo)
  "Abre o diário do dia DATA (AAAA-MM-DD; padrão hoje) e inicia uma entrada.
O arquivo é `nr/diario-dir'/AAAA-MM-DD.org; se já existe, só acrescenta
uma entrada com a hora atual. Com prefixo, pergunta a data."
  (interactive
   (list (when current-prefix-arg (org-read-date nil nil nil "Data: "))
         (read-string "Título da entrada (opcional): ")))
  (let* ((data (or data (format-time-string "%Y-%m-%d")))
         (arq (expand-file-name (concat data ".org") nr/diario-dir)))
    (unless (file-exists-p arq)
      (nr/criar-nota "journal" data :data data))
    (find-file arq)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (nr/--entrada-diario titulo data))
    (save-buffer)))

;;;; Atalhos e org-capture

(defvar nr/mapa-nova (make-sparse-keymap) "Atalhos para criar notas.")
(define-key nr/mapa-nova (kbd "e") #'nr/nova-nota-estudo)
(define-key nr/mapa-nova (kbd "d") #'nr/nova-nota-diario)
(define-key nr/mapa-nova (kbd "n") #'nr/nova-nota)

(defun nr/instalar-atalhos (&optional prefixo)
  "Liga PREFIXO (padrão \"C-c n\") ao mapa: e = estudo, d = diário, n = qualquer."
  (global-set-key (kbd (or prefixo "C-c n")) nr/mapa-nova))

(defun nr/capture-diario ()
  "Para `org-capture': vai ao diário de hoje (criando-o) e posiciona no fim."
  (let ((arq (expand-file-name (format-time-string "%Y-%m-%d.org") nr/diario-dir)))
    (unless (file-exists-p arq)
      (nr/criar-nota "journal" (format-time-string "%Y-%m-%d")))
    (set-buffer (find-file-noselect arq))
    (goto-char (point-max))))

(defun nr/capture-templates ()
  "Modelo de `org-capture' para uma entrada de diário (tecla \"d\").
Uso: (add-to-list \='org-capture-templates (car (nr/capture-templates)))."
  (list `("d" "Entrada de diário" plain (function nr/capture-diario)
          ,(concat "* %<%H:%M> — %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n")
          :empty-lines-before 1 :unnarrowed t)))

(provide 'nr-template)
;;; nr-template.el ends here
