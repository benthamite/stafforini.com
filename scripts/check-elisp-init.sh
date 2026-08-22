#!/usr/bin/env bash
# Batch-load scripts/sa-lp-init.el and verify its block lists for both
# note prefixes ("sa" and "vara"). Used as commit evidence for changes
# to the refresh init file.

set -euo pipefail

INIT_FILE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/sa-lp-init.el"

emacs -Q --batch --load "$INIT_FILE" --eval '
(progn
  (let ((sa (sa-lp-blocks t)))
    (unless (equal sa (list "sa-data" "sa-perf" "sa-chart" "sa-chart-ais"
                            "sa-sensitivity" "sa-delay" "sa-calc"))
      (error "Unexpected sa block list: %S" sa)))
  (setq sa-lp-block-prefix "vara")
  (let ((vara (sa-lp-blocks t)))
    (unless (equal vara (list "vara-data" "vara-perf" "vara-chart"
                              "vara-chart-salp" "vara-sensitivity"
                              "vara-delay" "vara-calc"))
      (error "Unexpected vara block list: %S" vara)))
  (message "sa-lp-init.el OK: block lists verified for both prefixes"))'
