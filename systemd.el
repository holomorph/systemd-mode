;;; systemd.el --- Major mode for editing systemd units -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 1.5
;; Package-Requires: ((emacs "24.4"))
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

;; Major mode for editing systemd units.

;; Similar to `conf-mode' but with added highlighting; e.g. for
;; specifiers and booleans.  Employs strict regex for whitespace.
;; Features a facility for browsing documentation: use C-c C-o to open
;; links to documentation in a unit (cf. systemctl help).

;; Supports completion of directives and sections in either units or
;; network configuration.  Both a completer for
;; `completion-at-point-functions' and a company backend are provided.
;; The latter can be enabled by adding `company-mode' to
;; `systemd-mode-hook'.

;;; Code:

(require 'conf-mode)
(require 'thingatpt)
(require 'url-parse)

(declare-function company-begin-backend "company")
(declare-function company-grab-symbol "company")

(defgroup systemd ()
  "Major mode for editing systemd units."
  :link '(url-link "http://www.freedesktop.org/wiki/Software/systemd/")
  :group 'tools)

(defcustom systemd-browse-url-function 'browse-url
  "Browser to use for HTTP(S) documentation."
  :type `(radio (function-item browse-url)
                ,@(when (fboundp 'eww) '((function-item eww)))
                ,@(when (fboundp 'w3m-browse-url) '((function-item w3m-browse-url)))
                (function :tag "Other function"))
  :group 'systemd)

(defcustom systemd-comment-start "#"
  "String to insert to start a new comment."
  :type '(choice (string :tag "Comment sign" "#")
                 (string :tag "Semicolon" ";"))
  :group 'systemd)

(defcustom systemd-man-function 'man
  "Pager to use for system manual pages."
  :type '(radio (function-item man)
                (function-item woman)
                (function :tag "Other function"))
  :group 'systemd)

(defcustom systemd-mode-hook nil
  "Hook run after entering `systemd-mode'."
  :type 'hook
  :options '(company-mode flycheck-mode)
  :group 'systemd)

(defconst systemd-unit-sections
  '("Unit" "Install" "Service")
  "Configuration sections for systemd 225.")

(defconst systemd-unit-directives
  (eval-when-compile
    (with-temp-buffer
      (insert-file-contents
       (let ((f "unit-directives.txt"))
         (if (null load-file-name) f
           (expand-file-name f (file-name-directory load-file-name)))))
      (split-string (buffer-string))))
  "Configuration directives for systemd.")

(defconst systemd-network-sections
  '("Match" "Link" "NetDev" "VLAN" "MACVLAN" "MACVTAP" "IPVLAN" "VXLAN"
    "Tunnel" "Peer" "Tun" "Tap" "Bond" "Network" "Address" "Route" "DHCP"
    "Bridge" "BridgeFDB")
  "Network configuration sections for systemd 225.")

(defconst systemd-network-directives
  (eval-when-compile
    (with-temp-buffer
      (insert-file-contents
       (let ((f "network-directives.txt"))
         (if (null load-file-name) f
           (expand-file-name f (file-name-directory load-file-name)))))
      (split-string (buffer-string))))
  "Network configuration directives for systemd.")

(defconst systemd-nspawn-sections
  '("Exec" "Files" "Network")
  "Namespace container configuration sections for systemd 232.")

(defconst systemd-nspawn-directives
  (eval-when-compile
    (with-temp-buffer
      (insert-file-contents
       (let ((f "nspawn-directives.txt"))
         (if (null load-file-name) f
           (expand-file-name f (file-name-directory load-file-name)))))
      (split-string (buffer-string))))
  "Namespace container configuration directives for systemd.")

;;;###autoload
(defconst systemd-autoload-regexp
  (eval-when-compile
    (rx (+? (any "a-zA-Z0-9-_.@\\")) "."
        (or "automount" "busname" "mount" "service" "slice"
            "socket" "swap" "target" "timer" "link" "netdev" "network")
        string-end))
  "Regexp for file buffers in which to autoload `systemd-mode'.")

;;;###autoload
(defconst systemd-tempfn-autoload-regexp
  (eval-when-compile
    (rx ".#"
        (or (and (+? (any "a-zA-Z0-9-_.@\\")) "."
                 (or "automount" "busname" "mount" "service" "slice"
                     "socket" "swap" "target" "timer" "link" "netdev" "network"))
            "override.conf")
        (= 16 (char hex-digit)) string-end))
  "Regexp for temp file buffers in which to autoload `systemd-mode'.")

;;;###autoload
(defconst systemd-dropin-autoload-regexp
  (eval-when-compile
    (rx "/systemd/" (+? anything) ".d/" (+? (not (any ?/))) ".conf" string-end))
  "Regexp for dropin config file buffers in which to autoload `systemd-mode'.")

(defun systemd-get-value (start)
  "Return the value of the key whose value begins at position START.
Lines ending in a backslash are concatenated with the next
according to systemd.unit(5)."
  (let (end)
    (save-excursion
      (while (= (char-before (line-end-position)) ?\\)
        (forward-line))
      (setq end (line-end-position))
      (replace-regexp-in-string "\\\\\n" " " (buffer-substring start end)))))

(defun systemd-doc-find ()
  "Find the value of the unit's “Documentation” keys.
Return values in a list of strings, otherwise nil."
  (let ((key "^Documentation=")
        string)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward key nil t)
        (setq string (concat string " " (systemd-get-value (point))))))
    (when string
      (remove "\\" (split-string string)))))

(defun systemd-doc-man (page)
  "Open a manual page PAGE with `systemd-man-function'."
  (pcase (symbol-name systemd-man-function)
    ("woman" (woman (replace-regexp-in-string "([[:alnum:]]+)" "" page)))
    ("man" (man page))
    (_ (apply 'systemd-man-function page))))

(defun systemd-doc-open (url)
  "Prompt to open URL.
Interactively completes the documentation in the current unit
file, defaulting to the link under point, if any."
  (interactive
   (let* ((completion-cycle-threshold t)
          (collection (systemd-doc-find))
          (uri (or (thing-at-point-url-at-point)
                   (car-safe collection)))
          (prompt (concat "URL"
                          (when uri (format " (default %s)" uri))
                          ": ")))
     (list (completing-read prompt collection nil nil nil nil uri))))
  (let ((link (url-generic-parse-url url)))
    (pcase (url-type link)
      ("file" (find-file (url-filename link)))
      ("man" (systemd-doc-man (url-filename link)))
      ("info" (url-info link))
      ((or "http" "https") (funcall systemd-browse-url-function url))
      (_ (user-error "Invalid link")))))

(defun systemd-doc-directives ()
  "Open systemd.directives(7)."
  (interactive)
  (systemd-doc-man "systemd.directives(7)"))

(defun systemd-buffer-section-p ()
  "Return t if current line begins with \"[\", otherwise nil."
  (save-excursion
    (beginning-of-line)
    (= (following-char) ?\[)))

(defun systemd-file-network-p (filename)
  "Return non-nil if FILENAME has a network-type extension, otherwise nil."
  (string-match-p (eval-when-compile
                    (rx "." (or "link" "netdev" "network") string-end))
                  filename))

(defun systemd-file-nspawn-p (filename)
  "Return non-nil if FILENAME has an nspawn extension, otherwise nil."
  (string-match-p (eval-when-compile (rx ".nspawn" string-end)) filename))

(defun systemd-completion-table (&rest _ignore)
  "Return a list of completion candidates."
  (let ((sectionp (systemd-buffer-section-p))
        (name (buffer-name)))
    (cond
     ((systemd-file-nspawn-p name)
      (if sectionp systemd-nspawn-sections systemd-nspawn-directives))
     ((systemd-file-network-p name)
      (if sectionp systemd-network-sections systemd-network-directives))
     (t (if sectionp systemd-unit-sections systemd-unit-directives)))))

(defun systemd-complete-at-point ()
  "Complete the symbol at point."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (list (or (car bounds) (point))
          (or (cdr bounds) (point))
          (completion-table-dynamic #'systemd-completion-table))))

(defun systemd-company-backend (command &optional arg &rest ignored)
  "Backend for `company-mode' in `systemd-mode' buffers."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'systemd-company-backend))
    (`prefix (and (eq major-mode 'systemd-mode) (company-grab-symbol)))
    (`candidates (all-completions arg (systemd-completion-table nil)))
    (`post-completion (if (not (systemd-buffer-section-p)) (insert "=")))))

(defvar systemd-font-lock-keywords
  (eval-when-compile
    `(("^[[:space:]]*?\\([#;]\\)\\(.*\\)$"
       (1 'font-lock-comment-delimiter-face)
       (2 'font-lock-comment-face))
      ("\\\\$" 0 'font-lock-warning-face) ; line break
      ;; sections
      ("^\\(\\[\\([[:upper:]][[:alnum:]]+\\|X-.*?\\)\\]\\)"
       1 'font-lock-type-face)
      ;; keys
      ("^\\([[:upper:]][[:alnum:]]+\\)="
       1 'font-lock-keyword-face)
      ;; boolean arguments
      (,(rx "=" (group (or "yes" "true" "on" "0" "no" "false" "off")) eol)
       1 'font-lock-constant-face)
      ;; specifiers
      ("%[nNpPiIfcrRtuUhsmbHv%]" 0 'font-lock-constant-face)
      ;; exec prefixes
      ;; TODO account for @ being a prefix now
      ("=\\(-@\\|@-\\|[@-]\\)" 1 'font-lock-negation-char-face)))
  "Default expressions to highlight in `systemd-mode'.
See systemd.unit(5) for details on unit file syntax.")

(defvar systemd-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\% "\\" table)
    table)
  "Syntax table used in `systemd-mode' buffers.")

(defvar systemd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'systemd-doc-directives)
    (define-key map (kbd "C-c C-o") 'systemd-doc-open)
    map)
  "Keymap used in `systemd-mode' buffers.")

(easy-menu-define systemd-mode-menu systemd-mode-map
  "Menu used in `systemd-mode' buffers."
  '("Systemd"
    ["Open Unit File help" systemd-doc-open
     :help "Documentation referenced in current buffer"]
    ["Open systemd.directives(7)" systemd-doc-directives
     :help "Index of configuration directives"]))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.nspawn\\'" . systemd-mode))
;;;###autoload (add-to-list 'auto-mode-alist `(,systemd-autoload-regexp . systemd-mode))
;;;###autoload (add-to-list 'auto-mode-alist `(,systemd-tempfn-autoload-regexp . systemd-mode))
;;;###autoload (add-to-list 'auto-mode-alist `(,systemd-dropin-autoload-regexp . systemd-mode))

;;;###autoload
(define-derived-mode systemd-mode conf-mode "Systemd"
  "Major mode for editing systemd unit files.
See http://www.freedesktop.org/wiki/Software/systemd/ for more
information about systemd.

In addition to any hooks its parent mode might have run, this
mode runs the hook `systemd-mode-hook' at mode initialization.

Key bindings:
\\{systemd-mode-map}"
  (set-keymap-parent systemd-mode-map nil)
  (conf-mode-initialize systemd-comment-start)
  (add-hook 'company-backends #'systemd-company-backend)
  (add-hook 'completion-at-point-functions #'systemd-complete-at-point nil t)
  (setq font-lock-defaults '(systemd-font-lock-keywords t)))

(provide 'systemd)

;;; systemd.el ends here
