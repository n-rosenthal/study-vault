#!/usr/bin/env python3
"""tokenizer.py — reads text from stdin, emits a JSON array of tokens on stdout.

Contract with Emacs:
  stdin  : raw text (UTF-8)
  stdout : JSON array of strings, e.g. ["Hello", ",", "world", "!"]
  exit 0 on success; non-zero with the error message on stderr on failure.
"""
from __future__ import annotations

import json
import re
import sys

# Unicode-aware word tokenizer: a "word" run, or a single non-space,
# non-word character (punctuation). Handles accented letters correctly.
_TOKEN_RE = re.compile(r"\w+|[^\w\s]", re.UNICODE)


def tokenize(text: str) -> list[str]:
    return _TOKEN_RE.findall(text)


def main() -> int:
    # Force UTF-8 regardless of the locale Emacs spawned us under.
    sys.stdin.reconfigure(encoding="utf-8")
    sys.stdout.reconfigure(encoding="utf-8")
    try:
        text = sys.stdin.read()
        tokens = tokenize(text)
        json.dump(tokens, sys.stdout, ensure_ascii=False)
        sys.stdout.write("\n")
        sys.stdout.flush()
        return 0
    except Exception as exc:  # noqa: BLE001
        print(f"tokenizer error: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
