.RECIPEPREFIX := >
EMACS ?= emacs
VAULT ?= $(HOME)/org/roam

.PHONY: tangle test compile build clean instalar-ci

tangle:
> $(EMACS) -Q --batch -l scripts/tangle.el

test: tangle
> $(EMACS) -Q --batch -l test/nr-test.el -f ert-run-tests-batch-and-exit

compile: tangle
> $(EMACS) -Q --batch -L lisp --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile lisp/*.el
> rm -f lisp/*.elc

build: tangle
> $(EMACS) -Q --batch -l scripts/build.el

clean:
> rm -f lisp/*.elc

# Copia o workflow para o repositório do vault (o GitHub só lê .github/ da raiz).
instalar-ci:
> mkdir -p $(VAULT)/.github/workflows
> cp .github/workflows/publicar.yml $(VAULT)/.github/workflows/publicar.yml
> @echo "Workflow copiado para $(VAULT)/.github/workflows/. Ajuste PUBLISHING_DIR e NR_SITE_URL."
