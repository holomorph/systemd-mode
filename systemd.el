;;; systemd.el --- Major mode for editing systemd units -*- lexical-binding: t -*-

;; Copyright (C) 2014-2023  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 1.6.1
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

;; Similar to `conf-mode' but with enhanced highlighting; e.g. for
;; specifiers and booleans.  Employs strict regex for whitespace.
;; Features a facility for browsing documentation: use C-c C-o to open
;; links to documentation in a unit (cf. systemctl help).

;; Supports completion of directives and sections in either units or
;; network configuration.  Both a completer for
;; `completion-at-point-functions' and a company backend are provided.
;; The latter can be enabled by adding `company-mode' to
;; `systemd-mode-hook' and adding `systemd-company-backend' to
;; `company-backends'.

;;; Code:

(require 'conf-mode)
(require 'thingatpt)
(require 'url-parse)

(declare-function company-begin-backend "company")
(declare-function company-grab-symbol "company")

(defvar font-lock-beg)
(defvar font-lock-end)

(defgroup systemd ()
  "Major mode for editing systemd units."
  :link '(url-link "https://www.freedesktop.org/wiki/Software/systemd/")
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
  "Configuration sections for systemd 244.")

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
  '("Match" "Link" "NetDev" "VLAN" "MACVLAN" "MACVTAP" "IPVLAN" "IPVTAP" "VXLAN"
    "GENEVE" "L2TP" "L2TPsession" "MACsec" "FooOverUDP"
    "Tunnel" "Peer" "Tun" "Tap" "Bond" "Network" "Address" "Route" "DHCP"
    "Neighbor" "IPv6AddressLabel" "RoutingPolicyRule" "NextHop" "DHCPv4"
    "DHCPv6" "IPv6AcceptRA" "DHCPServer" "IPv6Prefix" "CAN" 
    "Bridge" "BridgeFDB" "BridgeVLAN" "VXCAN" "WireGuard" "WireGuardPeer")
  "Network configuration sections for systemd 244 (not exhaustive).")

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
  "Namespace container configuration sections for systemd 244.")

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
  (rx (+? (any "a-zA-Z0-9-_.@\\")) "."
      (or "automount" "busname" "mount" "service" "slice"
          "socket" "swap" "target" "timer" "link" "netdev" "network")
      string-end)
  "Regexp for file buffers in which to autoload `systemd-mode'.")

;;;###autoload
(defconst systemd-tempfn-autoload-regexp
  (rx ".#"
      (or (and (+? (any "a-zA-Z0-9-_.@\\")) "."
               (or "automount" "busname" "mount" "service" "slice"
                   "socket" "swap" "target" "timer" "link" "netdev" "network"))
          "override.conf")
      (= 16 (char hex-digit)) string-end)
  "Regexp for temp file buffers in which to autoload `systemd-mode'.")

;;;###autoload
(defconst systemd-dropin-autoload-regexp
  (rx "/systemd/" (+? anything) ".d/" (+? (not (any ?/))) ".conf" string-end)
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
  (= (char-after (line-beginning-position)) ?\[))

(defun systemd-file-network-p (filename)
  "Return non-nil if FILENAME has a network-type extension, otherwise nil."
  (string-match-p (rx "." (or "link" "netdev" "network") string-end)
                  filename))

(defun systemd-file-nspawn-p (filename)
  "Return non-nil if FILENAME has an nspawn extension, otherwise nil."
  (string-match-p (rx ".nspawn" string-end) filename))

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

(defun systemd-company-backend (command &optional arg &rest _ignored)
  "Backend for `company-mode' in `systemd-mode' buffers."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'systemd-company-backend))
    (`prefix (and (eq major-mode 'systemd-mode) (company-grab-symbol)))
    (`candidates (all-completions arg (systemd-completion-table nil)))
    (`post-completion (if (not (systemd-buffer-section-p)) (insert "=")))))

(defun systemd-construct-start-p ()
  "Return non-nil if the current line is the first in a multi-line construct."
  (let ((flag t))
    (save-excursion
      (while (and (zerop (forward-line -1))
                  (eq ?\\ (char-before (line-end-position)))
                  (skip-chars-forward " \t")
                  (setq flag (memq (following-char) '(?# ?\;))))))
    flag))

(defun systemd-syntax-propertize (start end)
  "`systemd-propertize-function' for `systemd-mode' buffers."
  (let ((case-fold-search nil))
    (goto-char start)
    (funcall
     (syntax-propertize-rules
      ("^[ \t]*\\([;#]\\)$?"
       (1 (when (systemd-construct-start-p) (string-to-syntax "<")))))
     start end)))

(defun systemd-value-extend-region ()
  "Return the EOL position of the last line of the construct at point."
  (while (and (= (char-before (line-end-position)) ?\\)
              (skip-chars-forward " \t")
              (not (memq (following-char) '(?# ?\;)))
              (zerop (forward-line))))
  (line-end-position))

(defun systemd-font-lock-extend-region ()
  (goto-char font-lock-beg)
  (while (and (zerop (forward-line -1))
              (= (char-before (line-end-position)) ?\\)
              (skip-chars-forward " \t")
              (not (memq (following-char) '(?# ?\;)))))
  (setq font-lock-beg (point-marker))
  (goto-char font-lock-end)
  (setq font-lock-end (systemd-value-extend-region)))

(defmacro define-systemd-matcher (name regexp &optional docstring)
  "Define a new function NAME that matches REGEXP in a multi-line construct.
Only returns matches of REGEXP on lines passing `systemd-construct-start-p'."
  (declare (debug (symbolp stringp &optional stringp))
           (indent 2) (doc-string 3))
  `(defun ,name (limit)
     ,docstring
     (let (match)
       (while (and (setq match (re-search-forward ,regexp limit t))
                   (not (systemd-construct-start-p))))
       match)))

(define-systemd-matcher systemd-section-matcher
    "^\\(\\[\\([[:upper:]][[:alnum:]]+\\|X-.*?\\)\\]\\)"
  "Matcher for section titles.")

(define-systemd-matcher systemd-key-matcher "^\\([[:upper:]][[:alnum:]]+\\)="
  "Matcher for keys (unit directives).")

(defun systemd-exec-prefix-anchored-matcher (limit)
  "Matcher for the exec prefix in anchored font-lock rule.
See `font-lock-keywords' and (info \"(elisp) Search-based Fontification\")."
  (let ((pos (car (match-data)))
        (prefixes '(?- ?@ ?+))
        char end res)
    (while (and (memq (setq char (following-char)) prefixes)
                (< (point) limit))
      (forward-char)
      (setq prefixes (remq char prefixes))
      (setq end (point-marker)))
    (when end
      (prog1 (setq res (list (1+ pos) end))
        (set-match-data res)))))

(defconst systemd-font-lock-keywords-1
  '((systemd-section-matcher 1 'font-lock-type-face)
    (systemd-key-matcher 1 'font-lock-keyword-face))
  "Minimal expressions to highlight in `systemd-mode'.")

(defconst systemd-font-lock-keywords-2
  `(,@systemd-font-lock-keywords-1
    ("\\\\$" 0 'font-lock-warning-face) ; line break
    ;; boolean arguments
    (,(rx "=" (group (or "yes" "true" "on" "0" "no" "false" "off")) eol)
     1 'font-lock-constant-face)
    ("="
     ;; exec prefixes
     (systemd-exec-prefix-anchored-matcher
      nil nil (0 'font-lock-negation-char-face))
     ;; environment variables
     ("\\$[A-Z_]+\\>"
      (systemd-value-extend-region) nil (0 'font-lock-variable-name-face))
     ;; specifiers
     ("%[abBCEfgGhHiIjJlLmnNopPrRsStTuUvVw%]"
      (systemd-value-extend-region) nil (0 'font-lock-constant-face))))
  "Extended expressions to highlight in `systemd-mode'.")

(defconst systemd-font-lock-keywords-3
  `(,@systemd-font-lock-keywords-2
    ("^Type=\\(simple\\|forking\\|oneshot\\|dbus\\|notify\\|idle\\)$"
     1 'font-lock-builtin-face)
    (,(rx bol "Restart="
          (group (or "no" "on-success" "on-failure"
                     "on-abnormal" "on-watchdog" "on-abort" "always"))
          eol)
     1 'font-lock-builtin-face)
    ("^KillMode=\\(control-group\\|process\\|mixed\\|none\\)$"
     1 'font-lock-builtin-face)
    (,(rx bol "KillSignal="
          (group
           (or "SIGHUP" "SIGINT" "SIGQUIT" "SIGILL" "SIGABRT" "SIGFPE" "SIGKILL"
               "SIGSEGV" "SIGPIPE" "SIGALRM" "SIGTERM" "SIGUSR1" "SIGUSR2"
               "SIGCHLD" "SIGCONT" "SIGSTOP" "SIGTSTP" "SIGTTIN" "SIGTTOU"))
          eol)
     1 'font-lock-constant-face))
  "Flamboyant expressions to highlight in `systemd-mode'.")

(defvar systemd-font-lock-keywords 'systemd-font-lock-keywords-2
  "Default expressions to highlight in `systemd-mode'.
See systemd.unit(5) for details on unit file syntax.")

(defvar systemd-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?% "/" table)
    (modify-syntax-entry ?$ "'" table)
    (modify-syntax-entry ?\; "." table)
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
See https://www.freedesktop.org/wiki/Software/systemd/ for more
information about systemd.

In addition to any hooks its parent mode might have run, this
mode runs the hook `systemd-mode-hook' at mode initialization.

Key bindings:
\\{systemd-mode-map}"
  (set-keymap-parent systemd-mode-map nil)
  (conf-mode-initialize systemd-comment-start)
  (setq-local auto-fill-inhibit-regexp "^[ \t]*?[^;#]")
  (add-hook 'completion-at-point-functions #'systemd-complete-at-point nil t)
  (add-hook 'font-lock-extend-region-functions
            'systemd-font-lock-extend-region nil t)
  (setq-local syntax-propertize-function #'systemd-syntax-propertize)
  (setq font-lock-defaults
        '((systemd-font-lock-keywords
           systemd-font-lock-keywords-1
           systemd-font-lock-keywords-2
           systemd-font-lock-keywords-3))))

(provide 'systemd)

;;; systemd.el ends here
