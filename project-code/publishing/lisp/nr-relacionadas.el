;;; nr-relacionadas.el --- Bloco dinâmico notas-relacionadas -*- lexical-binding: t; -*-

;; Gerado por tangle de org/05-extras.org. Edite o .org, não este arquivo.

;;; Code:

(require 'nr-core)
(require 'nr-index)
(require 'nr-sinteses)

(defconst nr/tags-genericas '("estudo" "sintese" "diario"))

(defun nr/--tags-relevantes (nota)
  (seq-remove (lambda (tg) (or (equal tg nr/tag-publicar) (member tg nr/tags-genericas)))
              (nr/nota-tags nota)))

(defun nr/--linka-p (a b)
  "Non-nil se A linka para alguma das ids de B."
  (seq-some (lambda (id) (member id (nr/nota-links-id a))) (nr/nota-ids b)))

(defun nr/afinidade (a b)
  "Plist (:pontos N :tags (...)) da relação entre as notas A e B."
  (let* ((comuns (seq-intersection (nr/--tags-relevantes a) (nr/--tags-relevantes b) #'equal))
         (direto (or (nr/--linka-p a b) (nr/--linka-p b a)))
         (alvos (seq-difference
                 (seq-intersection (delete-dups (copy-sequence (nr/nota-links-id a)))
                                   (delete-dups (copy-sequence (nr/nota-links-id b)))
                                   #'equal)
                 (append (nr/nota-ids a) (nr/nota-ids b)) #'equal)))
    (list :pontos (+ (* 2 (length comuns)) (if direto 3 0) (length alvos))
          :tags comuns)))

(defun nr/relacionadas (nota &optional tipo so-publicaveis)
  "Lista de (NOTA-RELACIONADA . AFINIDADE), da mais para a menos relacionada."
  (let (res)
    (dolist (outra (nr/notas))
      (unless (or (eq outra nota)
                  (and tipo (not (equal (nr/nota-tipo outra) tipo)))
                  (and so-publicaveis (not (nr/nota-publicavel outra))))
        (let ((af (nr/afinidade nota outra)))
          (when (> (plist-get af :pontos) 0)
            (push (cons outra af) res)))))
    (sort res (lambda (x y)
                (let ((px (plist-get (cdr x) :pontos)) (py (plist-get (cdr y) :pontos)))
                  (or (> px py)
                      (and (= px py)
                           (string< (downcase (nr/nota-titulo (car x)))
                                    (downcase (nr/nota-titulo (car y)))))))))))

(defun org-dblock-write:notas-relacionadas (params)
  "Lista notas relacionadas à nota atual. Parâmetros: veja org/05-extras.org."
  (let* ((arq (and (buffer-file-name) (expand-file-name (buffer-file-name))))
         (nota (and arq (nr/nota-por-arquivo arq)))
         (limite (or (plist-get params :limite) 5))
         (minimo (or (plist-get params :min) 1))
         (tipo (plist-get params :tipo))
         (so-publicaveis (if (plist-member params :somente-publicaveis)
                             (plist-get params :somente-publicaveis)
                           t)))
    (unless nota (user-error "Esta nota não está no índice (rode `nr/indice-construir')"))
    (let ((itens (seq-take
                  (seq-filter (lambda (x) (>= (plist-get (cdr x) :pontos) minimo))
                              (nr/relacionadas nota tipo so-publicaveis))
                  limite)))
      (insert
       (if itens
           (mapconcat
            (lambda (x)
              (let ((tags (plist-get (cdr x) :tags)))
                (concat (nr/--linha-nota (car x) nil)
                        (when tags (format " (em comum: %s)" (string-join tags ", "))))))
            itens "\n")
         "/(nenhuma nota relacionada)/")))))

(provide 'nr-relacionadas)
;;; nr-relacionadas.el ends here
