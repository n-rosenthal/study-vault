#!/usr/bin/env bash
# Executa um comando com o ambiente da vault privada.
set -euo pipefail
PRIV="${NR_PRIVADA:-$HOME/private-vault}"
export NR_MODO=privado
export NR_VAULT="$PRIV/"
export NR_SAIDA="$PRIV/_site"
export NR_EXPORTS="$PRIV/exports"
export NR_CACHE="$PRIV/.cache-nr"
export NR_SITE_URL="${NR_PRIVADA_URL:-http://localhost:8080}"
exec "$@"
