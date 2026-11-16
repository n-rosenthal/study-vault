;;; nr-diario.el --- PDF único do diário por intervalo -*- lexical-binding: t; -*-

;; Gerado por tangle de org/02-export.org. Edite o .org, não este arquivo.

;;; Code:

(require 'nr-core)
(require 'nr-export)
(require 'org)
(require 'ox-latex)
(require 'calendar)

(defcustom nr/diario-dir (expand-file-name "journal/" nr/vault)
  "Diretório com os arquivos do diário."
  :type 'directory :group 'nr)

(defcustom nr/diario-propriedade-data "CREATED"
  "Propriedade com a data de cada entrada."
  :type 'string :group 'nr)

(defun nr/diario-data-da-entrada ()
  "Tempo (float) da entrada no heading atual, ou nil."
  (let ((ts (or (org-entry-get (point) nr/diario-propriedade-data nil)
                (let ((titulo (org-get-heading t t t t)))
                  (when (string-match org-ts-regexp-both titulo)
                    (match-string 0 titulo))))))
    (when ts
      (ignore-errors (float-time (org-time-string-to-time ts))))))

(defun nr/diario-coletar (de ate)
  "Lista de (TEMPO . TEXTO) das entradas entre DE e ATE (AAAA-MM-DD), em ordem."
  (let ((ini (float-time (org-time-string-to-time de)))
        (fim (+ (float-time (org-time-string-to-time ate)) 86399))
        res)
    (when (file-directory-p nr/diario-dir)
      (dolist (arq (directory-files-recursively nr/diario-dir "\\`[^.#][^/]*\\.org\\'"))
        (with-temp-buffer
          (insert-file-contents arq)
          (delay-mode-hooks (org-mode))
          (let ((ultimo-fim 0))
            (org-map-entries
             (lambda ()
               (let ((inicio (point)))
                 ;; sub-headings de uma entrada já capturada não contam de novo
                 (when (>= inicio ultimo-fim)
                   (when-let ((tempo (nr/diario-data-da-entrada)))
                     (when (<= ini tempo fim)
                       (let ((fim-sub (save-excursion (org-end-of-subtree t t) (point))))
                         (push (cons tempo (buffer-substring-no-properties inicio fim-sub))
                               res)
                         (setq ultimo-fim fim-sub)))))))
             nil nil)))))
    (sort res (lambda (a b) (< (car a) (car b))))))

(defun nr/diario-normalizar-nivel (texto)
  "Rebaixa os asteriscos de TEXTO para que o heading raiz tenha nível 1."
  (if (string-match "\\`\\(\\*+\\) " texto)
      (let ((extra (1- (length (match-string 1 texto)))))
        (if (zerop extra)
            texto
          (replace-regexp-in-string
           (format "^\\*\\{%d\\}\\(\\*+ \\)" extra) "\\1" texto)))
    texto))

(defun nr/diario-montar (de ate entradas)
  "Texto Org do documento com ENTRADAS (lista de (TEMPO . TEXTO))."
  (concat (format "#+TITLE: Diário\n#+SUBTITLE: %s a %s\n#+AUTHOR: %s\n"
                  de ate nr/autor)
          "#+NOTE_TYPE: journal\n#+OPTIONS: broken-links:mark\n\n"
          (mapconcat (lambda (e) (nr/diario-normalizar-nivel (cdr e))) entradas "\n")
          "\n"))

;;;###autoload
(defun nr/exportar-diario (de ate &optional so-tex)
  "Exporta as entradas do diário entre DE e ATE (AAAA-MM-DD) para um PDF.
Com SO-TEX, gera apenas o .tex (sem compilar). Devolve o caminho do arquivo."
  (interactive (list (org-read-date nil nil nil "De: ")
                     (org-read-date nil nil nil "Até: ")))
  (let ((entradas (nr/diario-coletar de ate)))
    (unless entradas
      (user-error "Nenhuma entrada entre %s e %s" de ate))
    (let* ((dir (make-temp-file "diario-" t))
           (arq (expand-file-name "diario.org" dir))
           (ext (if so-tex "tex" "pdf"))
           (destino (expand-file-name (format "diario-%s_%s.%s" de ate ext) nr/exports)))
      (unwind-protect
          (progn
            (with-temp-file arq
              (insert (nr/diario-montar de ate entradas)))
            (let ((gerado (with-current-buffer (find-file-noselect arq)
                            (prog1 (if so-tex
                                       (org-latex-export-to-latex)
                                     (org-latex-export-to-pdf))
                              (set-buffer-modified-p nil)
                              (kill-buffer)))))
              (make-directory nr/exports t)
              (copy-file (expand-file-name gerado dir) destino t)
              (message "Diário exportado: %s (%d entradas)" destino (length entradas))
              destino))
        (delete-directory dir t)))))

;;;###autoload
(defun nr/exportar-diario-mes (ano-mes)
  "Exporta o diário do mês ANO-MES (AAAA-MM)."
  (interactive "sMês (AAAA-MM): ")
  (unless (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}\\'" ano-mes)
    (user-error "Use o formato AAAA-MM"))
  (let* ((ano (string-to-number (substring ano-mes 0 4)))
         (mes (string-to-number (substring ano-mes 5 7)))
         (ultimo (calendar-last-day-of-month mes ano)))
    (nr/exportar-diario (concat ano-mes "-01") (format "%s-%02d" ano-mes ultimo))))

(provide 'nr-diario)
;;; nr-diario.el ends here
