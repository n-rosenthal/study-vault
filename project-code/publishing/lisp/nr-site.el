;;; nr-site.el --- Publicação do site (ox-publish), feed e verificação -*- lexical-binding: t; -*-

;; Gerado por tangle de org/03-site.org. Edite o .org, não este arquivo.

;;; Code:

(require 'nr-core)
(require 'nr-index)
(require 'nr-export)
(require 'nr-sinteses)
(require 'nr-relacionadas)
(require 'nr-grafo)
(require 'nr-og)
(require 'ox-publish)

(defcustom nr/usar-cache nil
  "Se non-nil, usa o cache do ox-publish (mais rápido, menos correto)."
  :type 'boolean :group 'nr)

(defcustom nr/atualizar-dblocks-ao-publicar nil
  "Se non-nil, atualiza os blocos dinâmicos do vault antes de publicar.
No CI (variável de ambiente CI) isso sempre acontece."
  :type 'boolean :group 'nr)

(defcustom nr/feed-limite 30
  "Número de entradas do feed Atom."
  :type 'integer :group 'nr)

(defconst nr/status-validos '("semente" "crescendo" "madura"))

(defun nr/ci-p ()
  "Non-nil se rodando em integração contínua (variável CI não vazia)."
  (let ((v (getenv "CI"))) (and v (not (string-empty-p v)))))

;;;; Metadados de página

(defun nr/git-data (arquivo)
  "Data (AAAA-MM-DD) do último commit de ARQUIVO; senão, a da modificação."
  (let ((default-directory (file-name-directory arquivo)))
    (or (ignore-errors
          (with-temp-buffer
            (when (zerop (call-process "git" nil '(t nil) nil "log" "-1" "--format=%cs"
                                       "--" (file-name-nondirectory arquivo)))
              (let ((s (string-trim (buffer-string))))
                (and (not (string-empty-p s)) s)))))
        (format-time-string
         "%Y-%m-%d" (file-attribute-modification-time (file-attributes arquivo))))))

(defun nr/minutos-de-leitura (nota)
  (max 1 (ceiling (nr/nota-palavras nota) 200.0)))

(defconst nr/nav-html
  (concat "<nav class=\"nav\"><a href=\"/\">Início</a>"
          "<a href=\"/grafo.html\">Grafo</a>"
          "<a href=\"/feed.xml\">Feed</a></nav>"))

(defun nr/preamble (info)
  "Preâmbulo HTML: navegação e linha de metadados da nota."
  (let* ((arq (plist-get info :input-file))
         (nota (and arq (nr/nota-por-arquivo arq))))
    (if (null nota)
        nr/nav-html
      (let ((status (nr/nota-status nota)))
        (concat
         nr/nav-html
         "<div class=\"meta-nota\">"
         (when status
           (format "<span class=\"selo selo-%s\">%s</span>"
                   (if (member status nr/status-validos) status "outro")
                   (nr/escapar-html status)))
         (format "<span>%d min de leitura</span>" (nr/minutos-de-leitura nota))
         (format "<span>criada em %s</span>" (nr/nota-data nota))
         (format "<span>atualizada em %s</span>" (nr/git-data arq))
         "</div>")))))

;;;; Índice do site (sitemap)

(defconst nr/busca-html
  "#+begin_export html
<link href=\"/pagefind/pagefind-ui.css\" rel=\"stylesheet\">
<script src=\"/pagefind/pagefind-ui.js\"></script>
<div id=\"search\"></div>
<script>
window.addEventListener('DOMContentLoaded', function () {
  if (typeof PagefindUI === 'undefined') { return; }
  new PagefindUI({
    element: '#search',
    showSubResults: true,
    showImages: false,
    translations: {
      placeholder: 'Buscar nas notas',
      zero_results: 'Nenhum resultado para [SEARCH_TERM]',
      one_result: '[COUNT] resultado para [SEARCH_TERM]',
      many_results: '[COUNT] resultados para [SEARCH_TERM]'
    }
  });
});
</script>
#+end_export
")

(defun nr/--item-do-indice (nota)
  (format "- [[file:%s][%s]] — %s%s"
          (nr/nota-rel nota)
          (nr/--sem-colchetes (nr/nota-titulo nota))
          (nr/nota-data nota)
          (if (nr/nota-status nota) (format " · %s" (nr/nota-status nota)) "")))

(defun nr/sitemap (titulo _lista)
  "Conteúdo de index.org, montado a partir do índice do vault (ignora _LISTA)."
  (let ((notas (nr/--ordenar-notas (nr/notas t) 'recentes)))
    (concat
     "#+TITLE: " titulo "\n"
     "#+NOTE_TYPE: indice\n"
     "#+OPTIONS: toc:nil num:nil\n\n"
     nr/busca-html "\n"
     "#+begin_export html\n<div data-pagefind-ignore>\n#+end_export\n"
     (mapconcat
      (lambda (grupo)
        (let ((itens (seq-filter
                      (lambda (n)
                        (if (car grupo)
                            (equal (nr/nota-tipo n) (car grupo))
                          ;; "Outras": nada fica escondido por causa de um tipo inesperado
                          (not (member (nr/nota-tipo n) '("sintese" "estudo")))))
                      notas)))
          (when itens
            (concat "* " (cdr grupo) "\n"
                    (mapconcat #'nr/--item-do-indice itens "\n") "\n\n"))))
      '(("sintese" . "Sínteses") ("estudo" . "Estudos") (nil . "Outras"))
      "")
     "#+begin_export html\n</div>\n#+end_export\n")))

;;;; Feed Atom

(defun nr/gerar-feed ()
  "Gera feed.xml (Atom) com as notas publicáveis mais recentes."
  (interactive)
  (when (string-search "SEU-USUARIO" nr/site-url)
    (message "AVISO: nr/site-url ainda é o padrão (%s); defina NR_SITE_URL" nr/site-url))
  (let* ((notas (seq-take (nr/--ordenar-notas (nr/notas t) 'recentes) nr/feed-limite))
         (destino (expand-file-name "feed.xml" nr/saida))
         (atualizado (if notas (nr/nota-data (car notas)) (format-time-string "%Y-%m-%d"))))
    (make-directory nr/saida t)
    (with-temp-file destino
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
              "<feed xmlns=\"http://www.w3.org/2005/Atom\" xml:lang=\"pt\">\n"
              (format "  <title>%s</title>\n" (nr/escapar-html nr/titulo-do-site))
              (format "  <link href=\"%s/feed.xml\" rel=\"self\"/>\n" nr/site-url)
              (format "  <link href=\"%s/\"/>\n" nr/site-url)
              (format "  <id>%s/</id>\n" nr/site-url)
              (format "  <updated>%sT00:00:00Z</updated>\n" atualizado))
      (dolist (n notas)
        (let ((url (concat nr/site-url (nr/url n))))
          (insert "  <entry>\n"
                  (format "    <title>%s</title>\n" (nr/escapar-html (nr/nota-titulo n)))
                  (format "    <link href=\"%s\"/>\n" (nr/escapar-html url))
                  (format "    <id>%s</id>\n" (nr/escapar-html url))
                  (format "    <updated>%sT00:00:00Z</updated>\n" (nr/nota-data n))
                  (if (nr/nota-desc n)
                      (format "    <summary>%s</summary>\n" (nr/escapar-html (nr/nota-desc n)))
                    "")
                  "  </entry>\n")))
      (insert "</feed>\n"))
    (message "Feed: %d entradas → %s" (length notas) destino)
    destino))

;;;; Verificador de links

(defun nr/links-problemas ()
  "Lista de problemas de link nas notas publicáveis (strings; vazia se tudo OK)."
  (let (problemas)
    (dolist (n (nr/notas t))
      (let ((rel (nr/nota-rel n)))
        (dolist (id (delete-dups (copy-sequence (nr/nota-links-id n))))
          (let ((alvo (nr/nota-por-id id)))
            (cond
             ((null alvo)
              (push (format "%s: id desconhecido (%s)" rel id) problemas))
             ((not (nr/nota-publicavel alvo))
              (push (format "%s: aponta para nota não publicada \"%s\""
                            rel (nr/nota-titulo alvo))
                    problemas)))))
        (dolist (caminho (delete-dups (copy-sequence (nr/nota-links-arq n))))
          (let ((abs (expand-file-name caminho (file-name-directory (nr/nota-arquivo n)))))
            (cond
             ((not (file-exists-p abs))
              (push (format "%s: arquivo inexistente (%s)" rel caminho) problemas))
             ((and (string-suffix-p ".org" abs)
                   (let ((alvo (nr/nota-por-arquivo abs)))
                     (or (null alvo) (not (nr/nota-publicavel alvo)))))
              (push (format "%s: aponta para arquivo não publicado (%s)" rel caminho)
                    problemas)))))))
    (nreverse problemas)))

(defun nr/verificar-links ()
  "Procura links quebrados ou que apontam para notas não publicadas.
Em modo batch (CI), sinaliza erro se achar algum."
  (interactive)
  (let ((problemas (nr/links-problemas)))
    (cond
     ((null problemas) (message "Links OK") nil)
     (noninteractive
      (message "LINKS COM PROBLEMA:\n%s" (mapconcat #'identity problemas "\n"))
      (error "%d link(s) com problema" (length problemas)))
     (t
      (with-current-buffer (get-buffer-create "*nr-links*")
        (erase-buffer)
        (insert (mapconcat #'identity problemas "\n") "\n")
        (pop-to-buffer (current-buffer)))
      problemas))))

;;;; Projeto ox-publish

(defun nr/regexp-exclusao ()
  "Regexp (para :exclude) de diretórios proibidos e de todo arquivo não publicável.
O ox-publish compara o :exclude com caminhos *relativos* ao vault."
  (let ((fora (mapcar #'nr/nota-rel (seq-remove #'nr/nota-publicavel (nr/notas)))))
    (mapconcat #'identity
               (delq nil
                     (list (nr/regexp-diretorios-proibidos)
                           (when fora (concat "\\`" (regexp-opt fora) "\\'"))))
               "\\|")))

(defun nr/projetos-definir ()
  "Define `org-publish-project-alist'. Chame depois de (re)construir o índice."
  (setq org-publish-project-alist
        `(("estudos-org"
           :base-directory ,nr/vault
           :base-extension "org"
           :recursive t
           :publishing-directory ,nr/saida
           :exclude ,(nr/regexp-exclusao)
           :publishing-function org-html-publish-to-html
           :with-toc t
           :section-numbers nil
           :html-doctype "html5"
           :html-html5-fancy t
           :html-validation-link nil
           :html-head-include-default-style nil
           :html-head-include-scripts nil
           :html-preamble nr/preamble
           :html-postamble nil
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title ,nr/titulo-do-site
           :sitemap-function nr/sitemap)
          ("estudos-anexos"
           :base-directory ,nr/vault
           :base-extension "png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\|pdf"
           :recursive t
           :publishing-directory ,nr/saida
           :exclude ,(nr/regexp-diretorios-proibidos)
           :publishing-function org-publish-attachment)
          ("estudos-static"
           :base-directory ,(expand-file-name "site/" nr/assets-dir)
           :base-extension "css\\|js\\|png\\|jpg\\|svg\\|woff2\\|html"
           :recursive t
           :publishing-directory ,nr/saida
           :publishing-function org-publish-attachment)
          ("estudos" :components ("estudos-org" "estudos-anexos" "estudos-static")))))

;;;; Busca (Pagefind)

(defcustom nr/pagefind-versao "1.3.0"
  "Versão do Pagefind usada via npx (fixada para builds reprodutíveis)."
  :type 'string :group 'nr)

(defun nr/indexar-busca ()
  "Roda o Pagefind sobre o site gerado (configuração em pagefind.yml)."
  (interactive)
  (if-let ((npx (executable-find "npx")))
      (let ((default-directory nr/projeto-dir))
        (message "Indexando a busca (Pagefind %s)..." nr/pagefind-versao)
        (unless (zerop (call-process npx nil "*nr-pagefind*" nil
                                     "-y" (concat "pagefind@" nr/pagefind-versao)
                                     "--site" (directory-file-name nr/saida)))
          (error "Pagefind falhou; veja o buffer *nr-pagefind*")))
    (message "npx não encontrado: busca não indexada")))

;;;; Publicação

(defun nr/publicar (&optional forcar)
  "Publica o site completo. Com FORCAR, ignora o cache do ox-publish."
  (interactive "P")
  (nr/indice-construir)
  (when (or (nr/ci-p) nr/atualizar-dblocks-ao-publicar)
    (nr/atualizar-dblocks)
    (nr/indice-construir))
  (nr/verificar-links)
  (nr/projetos-definir)
  (make-directory nr/saida t)
  (let ((org-publish-timestamp-directory (file-name-as-directory
                                          (expand-file-name "timestamps/" nr/cache)))
        (org-publish-cache nil))   ; não reaproveita cache de outro diretório
    (make-directory org-publish-timestamp-directory t)
    (org-publish "estudos" (or forcar (not nr/usar-cache))))
  (nr/gerar-grafo)
  (nr/gerar-feed)
  (nr/gerar-og)
  (unless (nr/ci-p)
    (nr/indexar-busca))
  (message "Site publicado em %s" nr/saida))

(provide 'nr-site)
;;; nr-site.el ends here
