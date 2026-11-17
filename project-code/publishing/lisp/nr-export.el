;;; nr-export.el --- Export por tipo de nota (PDF e HTML) -*- lexical-binding: t; -*-

;; Gerado por tangle de org/02-export.org. Edite o .org, não este arquivo.

;;; Code:

(require 'nr-core)
(require 'nr-index)
(require 'nr-og)
(require 'ox)
(require 'ox-latex)
(require 'ox-html)

;;;; Tipos de nota

(defconst nr/tipos-de-nota
  '(
    ("conceito"  :latex "notas-estudo" :css "estudo.css"  :opcoes nil)
    ("estudo"    :latex "notas-estudo" :css "estudo.css"  :opcoes nil)
    ("sintese"   :latex "sintese"      :css "sintese.css" :opcoes "toc:3")
    ("diario"    :latex "diario"       :css nil           :opcoes "toc:nil num:nil")
    ("indice"    :latex nil            :css nil           :opcoes nil)
    )
  "Configuração de export por valor de #+NOTE_TYPE:.")

(defconst nr/classes-latex
  '(("notas-estudo" . "estudo")
    ("sintese"      . "sintese")
    ("diario"       . "diario"))
  "Classe do org-latex-classes → pacote (.sty) que a define.")

;;;; Configuração global

;; O dicionário do Org só conhece "pt_BR" ("Índice", "Notas de rodapé"...).
;; O atributo lang do HTML é corrigido para "pt-BR" por `nr/--lang-html'.
(setq org-export-default-language "pt_BR"
      org-export-with-broken-links 'mark)

(defun nr/--lang-html (saida backend _info)
  "Filtro final: <html lang=\"pt_BR\"> → <html lang=\"pt-BR\"> (BCP 47)."
  (if (org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string "<html lang=\"pt_BR\"" "<html lang=\"pt-BR\"" saida t t)
    saida))

(add-to-list 'org-export-filter-final-output-functions #'nr/--lang-html)

;; LaTeX ----------------------------------------------------------------

(setq org-latex-compiler "lualatex"
      org-latex-pdf-process
      '("latexmk -%latex -interaction=nonstopmode -output-directory=%o %f")
      org-latex-prefer-user-labels t
      org-latex-hyperref-template nil          ; o estudo.sty carrega o hyperref
      org-latex-default-packages-alist
      '(("" "graphicx" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "capt-of" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "amssymb" t)))

(if (boundp 'org-latex-src-block-backend)
    (setq org-latex-src-block-backend 'listings)
  (with-no-warnings (setq org-latex-listings t)))   ; Org < 9.6

(defconst nr/secoes-latex
  '(("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")))

(dolist (classe nr/classes-latex)
  (setf (alist-get (car classe) org-latex-classes nil nil #'equal)
        (cons (format "\\documentclass[11pt,a4paper]{scrartcl}\n\\usepackage{%s}"
                      (cdr classe))
              nr/secoes-latex)))

(defun nr/--configurar-texinputs ()
  "Faz o LaTeX achar os .sty de assets/latex/, mantendo os caminhos padrão."
  (let ((dir (expand-file-name "latex" nr/assets-dir))
        (atual (or (getenv "TEXINPUTS") "")))
    (unless (string-search dir atual)
      (setenv "TEXINPUTS" (concat dir "//:" atual)))))

(nr/--configurar-texinputs)

;; HTML -----------------------------------------------------------------

(setq org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-validation-link nil
      org-html-htmlize-output-type 'css
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-mathjax-options
      (cons '(path "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
            (assq-delete-all 'path (copy-sequence org-html-mathjax-options))))

;;;; Utilidades do gancho

(defun nr/--limite-do-cabecalho ()
  "Posição do primeiro heading (ou fim do buffer)."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\*+ " nil t) (match-beginning 0) (point-max))))

(defun nr/--id-do-buffer ()
  "Primeiro :ID: do cabeçalho do buffer, ou nil."
  (save-excursion
    (let ((limite (nr/--limite-do-cabecalho)))
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*:ID:[ \t]+\\([^ \t\n]+\\)" limite t)
        (match-string 1)))))

(defun nr/--id-no-buffer-p (id)
  "Non-nil se o buffer atual define o :ID: ID (em qualquer heading)."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^[ \t]*:ID:[ \t]+" (regexp-quote id) "[ \t]*$") nil t)))

(defun nr/--inserir-no-topo (linhas)
  "Insere LINHAS no topo do buffer, depois do drawer de propriedades."
  (when linhas
    (save-excursion
      (goto-char (point-min))
      (when (looking-at-p "[ \t]*:PROPERTIES:")
        (re-search-forward "^[ \t]*:END:[ \t]*\n" nil t))
      (insert (mapconcat #'identity linhas "\n") "\n"))))

;;;; Passo 1: links id: para outras notas

(defun nr/--reescrever-links-id (html)
  "Reescreve [[id:...]] para notas de outros arquivos. HTML: t ou nil (LaTeX)."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\[\\[id:\\([^]:\n]+\\)\\(?:::[^]\n]*\\)?\\]\\(?:\\[\\([^]\n]*\\)\\]\\)?\\]"
            nil t)
      (let* ((ini (match-beginning 0))
             (fim (match-end 0))
             (id (match-string 1))
             (desc (match-string 2))
             (alvo (nr/nota-por-id id)))
        (unless (or (null alvo)
                    (nr/--id-no-buffer-p id)
                    (save-excursion
                      (goto-char ini)
                      (org-in-block-p '("src" "example" "export" "comment" "verse"))))
          (let* ((texto (or desc (nr/nota-titulo alvo)))
                 (publicavel (nr/nota-publicavel alvo))
                 (novo
                  (cond
                   ((not publicavel) texto)
                   (html (format "@@html:<a href=\"%s\">@@%s@@html:</a>@@"
                                 (nr/escapar-html (nr/url alvo)) texto))
                   (t (format "[[%s%s][%s]]" nr/site-url (nr/url alvo) texto)))))
            (goto-char ini)
            (delete-region ini fim)
            (insert novo)))))))

;;;; Passo 2: topo

(defun nr/--linhas-do-topo (backend kw tipo cfg nota)
  "Linhas #+KEYWORD: do topo. NOTA é a `nr/nota' do arquivo (ou nil)."
  (let ((val (lambda (k) (cadr (assoc k kw))))
        (publica (and nota (nr/nota-publicavel nota)))
        linhas)
    (cond
     ((org-export-derived-backend-p backend 'latex)
      (when-let* ((classe (plist-get cfg :latex)))
        (push (format "#+LATEX_CLASS: %s" classe) linhas))
      ;; URL da versão web (usada pelo QR code do estudo.sty). Os catcodes
      ;; deixam %, _ e # passarem sem escape.
      (when publica
        (push (format "#+LATEX_HEADER: \\begingroup\\catcode`\\%%=12 \\catcode`\\_=12 \\catcode`\\#=12 \\gdef\\nrurl{%s%s}\\endgroup"
                      nr/site-url (nr/url nota))
              linhas)))
     ((org-export-derived-backend-p backend 'html)
      ;; O #+HTML_HEAD: de um arquivo *substitui* o :html-head do projeto
      ;; (não soma), então o CSS base também entra por aqui.
      (push "#+HTML_HEAD: <link rel=\"stylesheet\" href=\"/css/site.css\">" linhas)
      (when-let* ((css (plist-get cfg :css)))
        (push (format "#+HTML_HEAD: <link rel=\"stylesheet\" href=\"/css/%s\">" css) linhas))
      (push (format "#+HTML_HEAD: <link rel=\"alternate\" type=\"application/atom+xml\" title=\"%s\" href=\"/feed.xml\">"
                    (nr/escapar-html nr/titulo-do-site))
            linhas)
      (push "#+HTML_HEAD: <script defer src=\"/js/grafo-local.js\"></script>" linhas)
      (unless (equal tipo "indice")
        (push "#+HTML_HEAD: <meta property=\"og:type\" content=\"article\">" linhas)
        (when-let* ((titulo (funcall val "TITLE")))
          (push (format "#+HTML_HEAD: <meta property=\"og:title\" content=\"%s\">"
                        (nr/escapar-html titulo))
                linhas))
        (when-let* ((desc (funcall val "DESCRIPTION")))
          (push (format "#+HTML_HEAD: <meta property=\"og:description\" content=\"%s\">"
                        (nr/escapar-html desc))
                linhas))
        (when publica
          (push (format "#+HTML_HEAD: <meta property=\"og:url\" content=\"%s%s\">"
                        nr/site-url (nr/escapar-html (nr/url nota)))
                linhas)
          (when nr/og-imagens
            (push (format "#+HTML_HEAD: <meta property=\"og:image\" content=\"%s%s\">"
                          nr/site-url (nr/escapar-html (nr/og-caminho nota)))
                  linhas)
            (push "#+HTML_HEAD: <meta name=\"twitter:card\" content=\"summary_large_image\">"
                  linhas))))))
    (when-let* ((op (plist-get cfg :opcoes)))
      (push (concat "#+OPTIONS: " op) linhas))
    (unless (funcall val "AUTHOR")      ; sem isso o Org usa `user-full-name'
      (push (concat "#+AUTHOR: " nr/autor) linhas))
    (nreverse linhas)))

;;;; Passo 3: fim (só HTML)

(defun nr/--bloco-backlinks ()
  "Org com a seção \"Citada em\" (links para as notas que citam esta), ou nil."
  (when-let* ((id (nr/--id-do-buffer))
              (nota (nr/nota-por-id id))
              (bl (nr/backlinks nota)))
    (concat "\n* Citada em\n#+begin_export html\n<ul class=\"backlinks\">\n"
            (mapconcat (lambda (n)
                         (format "<li><a href=\"%s\">%s</a></li>"
                                 (nr/escapar-html (nr/url n))
                                 (nr/escapar-html (nr/nota-titulo n))))
                       bl "\n")
            "\n</ul>\n#+end_export\n")))

(defun nr/--filtros-pagefind (kw tipo)
  "Spans ocultos lidos pelo Pagefind como filtros (tipo, tag, status)."
  (let* ((val (lambda (k) (cadr (assoc k kw))))
         (tags (remove nr/tag-publicar
                       (apply #'append
                              (mapcar #'nr/--tags (cdr (assoc "FILETAGS" kw))))))
         (status (funcall val "STATUS"))
         ;; Span *vazio* com o valor num atributo: o texto não entra no índice.
         (span (lambda (filtro valor)
                 (format "<span class=\"pf-meta\" data-pagefind-filter=\"%s[data-v]\" data-v=\"%s\"></span>"
                         filtro (nr/escapar-html valor)))))
    (unless (equal tipo "indice")
      (concat "\n#+begin_export html\n"
              (mapconcat #'identity
                         (append (list (funcall span "tipo" tipo))
                                 (mapcar (lambda (tg) (funcall span "tag" tg)) tags)
                                 (and status (list (funcall span "status" (downcase status)))))
                         "\n")
              "\n#+end_export\n"))))

;;;; Passo 4: data

(defun nr/--normalizar-data ()
  "Troca #+DATE: [2026-11-12 Thu 10:00] por #+DATE: 2026-11-12."
  (save-excursion
    (goto-char (point-min))
    (let ((limite (nr/--limite-do-cabecalho)) (case-fold-search t))
      (when (re-search-forward "^#\\+DATE:[ \t]*\\(.*\\)$" limite t)
        ;; nr/data-curta usa string-match e apagaria o match-data: guarde antes.
        (let ((ini (match-beginning 1))
              (fim (match-end 1))
              (curta (nr/data-curta (match-string 1))))
          (when curta
            (delete-region ini fim)
            (goto-char ini)
            (insert curta)))))))

;;;; O gancho

(defun nr/pre-export (backend)
  "Gancho único de pré-processamento; veja org/02-export.org."
  (let ((html (org-export-derived-backend-p backend 'html))
        (latex (org-export-derived-backend-p backend 'latex)))
    (when (or html latex)
      (let* ((kw (org-collect-keywords
                  '("NOTE_TYPE" "TITLE" "DESCRIPTION" "STATUS" "FILETAGS" "AUTHOR")))
             (tipo (nr/normalizar-tipo (cadr (assoc "NOTE_TYPE" kw))))
             (cfg (cdr (assoc tipo nr/tipos-de-nota)))
             (id (nr/--id-do-buffer))
             (nota (and id (nr/nota-por-id id))))
        (nr/--normalizar-data)
        (nr/--reescrever-links-id html)
        (nr/--inserir-no-topo (nr/--linhas-do-topo backend kw tipo cfg nota))
        (when html
          (goto-char (point-max))
          (insert (or (nr/--bloco-backlinks) "")
                  (or (nr/--filtros-pagefind kw tipo) "")))))))

(nr/adicionar-hook-pre-export #'nr/pre-export)

(defun nr/tipo-da-nota ()
  "Valor de #+NOTE_TYPE: do buffer atual (padrão \"estudo\")."
  (nr/normalizar-tipo (cadr (assoc "NOTE_TYPE" (org-collect-keywords '("NOTE_TYPE"))))))

(provide 'nr-export)
;;; nr-export.el ends here
