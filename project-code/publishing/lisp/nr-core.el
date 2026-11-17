;;; nr-core.el --- Configuração, caminhos e utilidades -*- lexical-binding: t; -*-

;; Gerado por tangle de org/01-nucleo.org. Edite o .org, não este arquivo.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup nr nil
  "Pipeline de publicação org → PDF e web."
  :group 'org
  :prefix "nr/")

(defconst nr/projeto-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name default-directory))))
  "Raiz deste projeto (diretório pai de lisp/).")

(defconst nr/assets-dir (expand-file-name "assets/" nr/projeto-dir)
  "Diretório com estilos LaTeX, CSS, JS e páginas estáticas.")

(defun nr/--diretorio-env (variavel padrao)
  "Diretório de VARIAVEL de ambiente, ou PADRAO, sempre com barra final."
  (file-name-as-directory (expand-file-name (or (getenv variavel) padrao))))

(defcustom nr/vault (nr/--diretorio-env "NR_VAULT" "~/org/roam/")
  "Raiz do vault de notas."
  :type 'directory :group 'nr)

(defcustom nr/saida (nr/--diretorio-env "NR_SAIDA" "~/site/public/")
  "Diretório onde o site é gerado."
  :type 'directory :group 'nr)

(defcustom nr/exports (nr/--diretorio-env "NR_EXPORTS" "~/org/exports/")
  "Onde ficam os PDFs avulsos (mantenha fora do vault)."
  :type 'directory :group 'nr)

(defcustom nr/cache (nr/--diretorio-env "NR_CACHE" "~/.cache/nr-publicacao/")
  "Cache do ox-publish."
  :type 'directory :group 'nr)

(defcustom nr/site-url
  (string-remove-suffix "/" (or (getenv "NR_SITE_URL") "https://SEU-USUARIO.github.io"))
  "URL base do site, sem barra final."
  :type 'string :group 'nr)

(defcustom nr/autor (or (getenv "NR_AUTOR") "Nicolas")
  "Autor usado nos documentos gerados."
  :type 'string :group 'nr)

(defcustom nr/titulo-do-site "Notas de estudo"
  "Título do índice e do feed."
  :type 'string :group 'nr)

(defcustom nr/diretorios-proibidos
  '("diario" "private" "templates" "project-code" "_site" "node_modules")
  "Diretórios (relativos ao vault) cujo conteúdo nunca é publicado."
  :type '(repeat string) :group 'nr)

(defcustom nr/tag-publicar "publish"
  "Tag em #+FILETAGS: que torna uma nota publicável."
  :type 'string :group 'nr)

(defcustom nr/tag-privada "private"
  "Tag em #+FILETAGS: que impede a publicação, mesmo com a tag de publicar."
  :type 'string :group 'nr)

;;;; Caminhos e privacidade

(defun nr/diretorio-proibido-p (arquivo)
  "Non-nil se ARQUIVO está em diretório proibido ou oculto do vault."
  (let ((rel (file-relative-name arquivo nr/vault)))
    (and (not (string-prefix-p ".." rel))
         (seq-some (lambda (d)
                     (or (member d nr/diretorios-proibidos)
                         (string-prefix-p "." d)))
                   (butlast (split-string rel "/" t))))))

(defun nr/regexp-diretorios-proibidos ()
  "Regexp (para :exclude do ox-publish) dos diretórios proibidos do vault.
O ox-publish compara o :exclude com o caminho *relativo* à raiz do projeto
(por exemplo \"private/x.org\"), então a regexp não contém o caminho do vault."
  (concat "\\`\\(?:.*/\\)?"
          "\\(?:" (mapconcat #'regexp-quote nr/diretorios-proibidos "\\|")
          "\\|\\.[^/]+\\)/"))

(defun nr/arquivos-org ()
  "Todos os .org do vault, sem descer em diretórios ocultos nem pegar backups."
  (directory-files-recursively
   nr/vault "\\`[^.#][^/]*\\.org\\'" nil
   (lambda (dir)
     (not (string-prefix-p "." (file-name-nondirectory (directory-file-name dir)))))))

;;;; Texto

(defconst nr/sinonimos-tipo
  '(
    ("concept" . "conceito") 
    ("study" . "estudo") 
    ("synthesis" . "sintese") 
    ("journal" . "diario") 
    ("síntese" . "sintese") 
    ("diário" . "diario") 
    )
  "Nomes alternativos de #+NOTE_TYPE: e seu nome canônico.")

(defconst nr/sinonimos-status
  '(("seed" . "semente") ("growing" . "crescendo") ("mature" . "madura")
    ("evergreen" . "madura"))
  "Nomes alternativos de #+STATUS: e seu nome canônico.")

(defun nr/normalizar-tipo (tipo)
  "TIPO em minúsculas, com sinônimos (study, journal...) trocados pelo canônico."
  (let ((tipo (downcase (string-trim (or tipo "estudo")))))
    (or (cdr (assoc tipo nr/sinonimos-tipo)) tipo)))

(defun nr/normalizar-status (status)
  "STATUS em minúsculas com sinônimos trocados pelo canônico; nil se vazio."
  (when (and status (not (string-empty-p (string-trim status))))
    (let ((s (downcase (string-trim status))))
      (or (cdr (assoc s nr/sinonimos-status)) s))))

(defun nr/escapar-html (texto)
  "Escapa TEXTO para uso em HTML/XML, inclusive em atributos."
  (let ((s (or texto "")))
    (dolist (par '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;") ("\"" . "&quot;")) s)
      (setq s (replace-regexp-in-string (car par) (cdr par) s t t)))))

(defun nr/data-curta (texto)
  "AAAA-MM-DD contido em TEXTO, ou nil."
  (when (and texto (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" texto))
    (match-string 0 texto)))

(defun nr/adicionar-hook-pre-export (funcao)
  "Registra FUNCAO (recebe BACKEND) antes do processamento de export.
Org >= 9.7 usa `org-export-before-processing-functions'; antes, `-hook'."
  (require 'ox)
  (add-hook (if (boundp 'org-export-before-processing-functions)
                'org-export-before-processing-functions
              'org-export-before-processing-hook)
            funcao))

(provide 'nr-core)
;;; nr-core.el ends here
