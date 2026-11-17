;;; nr-og.el --- Imagens de pré-visualização (og:image) -*- lexical-binding: t; -*-

;; Gerado por tangle de org/05-extras.org. Edite o .org, não este arquivo.

;;; Code:

(require 'nr-core)
(require 'nr-index)

(defun nr/--conversor-svg ()
  "Símbolo do conversor SVG→PNG disponível (rsvg, magick, convert) ou nil."
  (cond ((executable-find "rsvg-convert") 'rsvg)
        ((executable-find "magick") 'magick)
        ((executable-find "convert") 'convert)))

(defcustom nr/og-imagens (and (nr/--conversor-svg) t)
  "Se non-nil, gera uma imagem og:image por nota publicável."
  :type 'boolean :group 'nr)

(defun nr/og-caminho (nota)
  "Caminho (relativo à raiz do site) da imagem og de NOTA."
  (concat "/og/"
          (mapconcat #'url-hexify-string
                     (split-string (file-name-sans-extension (nr/nota-rel nota)) "/")
                     "/")
          ".png"))

(defun nr/--quebrar-linhas (texto largura max-linhas)
  "Quebra TEXTO em linhas de até LARGURA caracteres; no máximo MAX-LINHAS."
  (let (linhas atual)
    (dolist (p (split-string texto "[ \t\n]+" t))
      (if (and atual (> (+ (length atual) 1 (length p)) largura))
          (progn (push atual linhas) (setq atual p))
        (setq atual (if atual (concat atual " " p) p))))
    (when atual (push atual linhas))
    (setq linhas (nreverse linhas))
    (if (<= (length linhas) max-linhas)
        linhas
      (append (seq-take linhas (1- max-linhas))
              (list (concat (nth (1- max-linhas) linhas) "…"))))))

(defun nr/og-svg (nota)
  "Texto SVG do cartão de NOTA."
  (let* ((tipo (nr/nota-tipo nota))
         (cor (pcase tipo
                ("sintese"  "#2e7d4f")
                ("conceito" "#6a3d9a")
                (_          "#1f4e8c")))
         (rotulo (upcase (string-join (delq nil (list tipo (nr/nota-status nota))) " · ")))
         (linhas (nr/--quebrar-linhas (nr/nota-titulo nota) 24 3))
         (tags (mapconcat (lambda (tg) (concat "#" tg))
                          (seq-take (nr/tags-visiveis nota) 4) "   ")))
    (concat
     "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1200\" height=\"630\" viewBox=\"0 0 1200 630\">\n"
     "<rect width=\"1200\" height=\"630\" fill=\"#fdfdfc\"/>\n"
     (format "<rect width=\"28\" height=\"630\" fill=\"%s\"/>\n" cor)
     (format "<text x=\"96\" y=\"120\" font-family=\"sans-serif\" font-size=\"30\" letter-spacing=\"3\" fill=\"#666666\">%s</text>\n"
             (nr/escapar-html rotulo))
     (let ((i -1))
       (mapconcat
        (lambda (l)
          (setq i (1+ i))
          (format "<text x=\"96\" y=\"%d\" font-family=\"sans-serif\" font-size=\"72\" font-weight=\"bold\" fill=\"%s\">%s</text>"
                  (+ 250 (* i 88)) cor (nr/escapar-html l)))
        linhas "\n"))
     "\n"
     (format "<text x=\"96\" y=\"528\" font-family=\"sans-serif\" font-size=\"30\" fill=\"%s\">%s</text>\n"
             cor (nr/escapar-html tags))
     (format "<text x=\"96\" y=\"588\" font-family=\"sans-serif\" font-size=\"28\" fill=\"#666666\">%s</text>\n"
             (nr/escapar-html nr/titulo-do-site))
     "</svg>\n")))

(defun nr/--svg-para-png (svg png)
  "Converte o arquivo SVG em PNG com o conversor disponível."
  (let ((conv (nr/--conversor-svg)))
    (unless conv (error "Nenhum conversor SVG→PNG encontrado"))
    (unless (zerop (pcase conv
                     ('rsvg (call-process "rsvg-convert" nil nil nil "-w" "1200" "-o" png svg))
                     ('magick (call-process "magick" nil nil nil svg png))
                     ('convert (call-process "convert" nil nil nil svg png))))
      (error "Falha ao converter %s" svg))))

(defun nr/gerar-og ()
  "Gera as og:image das notas publicáveis em og/. Devolve a contagem."
  (interactive)
  (if (not (and nr/og-imagens (nr/--conversor-svg)))
      (progn (message "og:image desligado (sem conversor SVG→PNG)") 0)
    (let ((n 0) (tmp (make-temp-file "nr-og-" nil ".svg")))
      (unwind-protect
          (dolist (nota (nr/notas t))
            (let ((png (expand-file-name
                        (concat "og/" (file-name-sans-extension (nr/nota-rel nota)) ".png")
                        nr/saida)))
              (make-directory (file-name-directory png) t)
              (with-temp-file tmp
                (set-buffer-file-coding-system 'utf-8-unix)
                (insert (nr/og-svg nota)))
              (nr/--svg-para-png tmp png)
              (setq n (1+ n))))
        (delete-file tmp))
      (message "Imagens og:image: %d" n)
      n)))

(provide 'nr-og)
;;; nr-og.el ends here
