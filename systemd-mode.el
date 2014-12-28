;;; systemd-mode.el --- Major mode for editing systemd units

;; Copyright (C) 2014  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Keywords: tools, unix

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defgroup systemd ()
  "Major mode for editing systemd units."
  :group 'tools)

(defcustom systemd-comment-start "#"
  "String to insert to start a new comment."
  :group 'systemd
  :type '(choice (string :tag "Comment sign" "#")
                 (string :tag "Semicolon" ";")))

(defvar systemd-font-lock-keywords
  `(
    ("^\\([#;]\\(.*\\)\\)$"
     (1 'font-lock-comment-delimiter-face)
     (2 'font-lock-comment-face))
    ("\\\\$" 0 'font-lock-warning-face) ; line break
    ;; sections
    ("^\\(\\[[[:upper:]][[:alnum:]]+\\]\\)"
     1 'font-lock-type-face)
    ;; keys
    ("^\\([[:upper:]][[:alnum:]]+\\)="
     1 'font-lock-keyword-face)
    ;; boolean arguments
    ("=\\(1\\|yes\\|true\\|on\\|0\\|no\\|false\\|off\\)$"
     1 'font-lock-constant-face)
    ;; specifiers
    ("%[nNpPiIfcrRtuUhsmbHv%]" 0 'font-lock-constant-face)
    )
  "Default expressions to highlight in `systemd-mode'. See systemd.unit(5)
for details on unit file syntax.")

(defvar systemd-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " table)
    (modify-syntax-entry ?\n ">   " table)
    (modify-syntax-entry ?\% "\\   " table)
    table)
  "Syntax table used in `systemd-mode' buffers.")

;;;###autoload (add-to-list 'auto-mode-alist '("\\.automount\\'" . systemd-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.busname\\'" . systemd-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.mount\\'" . systemd-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.socket\\'" . systemd-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.target\\'" . systemd-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.timer\\'" . systemd-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.link\\'" . systemd-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.netdev\\'" . systemd-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.network\\'" . systemd-mode))

;;;###autoload
(define-derived-mode systemd-mode fundamental-mode "Systemd"
  "Major mode for editing systemd unit files. See
http://www.freedesktop.org/wiki/Software/systemd/ for more
information about systemd.  The hook `systemd-mode-hook' is run
at mode initialization."
  (set-syntax-table systemd-mode-syntax-table)
  (setq-local comment-start systemd-comment-start)
  (setq-local font-lock-defaults '(systemd-font-lock-keywords))
  (run-mode-hooks 'systemd-mode-hook))

(provide 'systemd-mode)

;;; systemd-mode.el ends here
