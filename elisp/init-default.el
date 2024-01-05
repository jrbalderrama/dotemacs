;;; init-default.el --- Global configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; global modes on Emacs
;;
;; ;; autocomplete everywhere DEPRECATED: company
;; (global-auto-complete-mode t)
;; refresh buffers automatically
(global-auto-revert-mode)
;; fontify text by default
(global-font-lock-mode)
;; regexp highlight in buffers
(global-hi-lock-mode)
;; show buffer changes differently
(global-highlight-changes-mode)
;; highlight current line
(global-hl-line-mode)
;; ;; show line numbers DEPRECATED: display-line-numbers-mode
;; (global-linum-mode)
;; show symbols of special words
(global-prettify-symbols-mode)
;; easily navigate silly cased words
(global-subword-mode)
;; ;; treat symbols as words
;; (global-superword-mode)
;; auto-wrapp lines in buffers
(global-visual-line-mode)


;;; actived minor modes
;;
;; get rid off bars
;; (dolist (mode '(scroll-bar-mode
;;                 ;; menu-bar-mode ; do not disable this on Mac
;;                 tool-bar-mode))
;;   (funcall mode -1))

;; transparently open compressed files
(auto-compression-mode t)
;; show column numbers
(column-number-mode t)
;; remove text in active region if inserting text
(delete-selection-mode t)
;; being smart about filenames in mbuf
(file-name-shadow-mode t)
;; show line numbers
(line-number-mode t)
;; ;; move mouse pointer when is close to the cursor
;; (mouse-avoidance-mode 'jump)
;; highlight matching parentheses
(show-paren-mode t)
;; highlight region between point and mark
(transient-mark-mode t)


;;; default buffer-local modes
;;
(setq-default
 ;; enable abbreviation replacements
 abbrev-mode t
 ;; use spaces instead of tabs
 indent-tabs-mode nil)


;;; default buffer-local variables
;;
(setq-default
 ;; fill line ending with colon with two spaces (M-q)
 colon-double-space t
 ;; max fill width per line (chars)
 fill-column 80
 ;; show me empty lines after buffer end
 indicate-empty-lines t
 ;; ;; default mode for unknown files
 ;; major-mode 'org-mode
 ;; ;; disable mode-line
 ;; mode-line-format nil
 ;; end files with a newline
 require-final-newline t
 ;;
 ;; scroll-up-aggressively 0
 ;;
 ;; scroll-down-aggressively 0
 ;; sentences do not need double spaces at end
 sentence-end-double-space nil
 ;; set tab with 4 spaces
 tab-width 4)


;;; default global variables
;;
(setq
 ;; stop creating #autosave# files
 auto-save-default nil
 ;; input events between auto-saves
 auto-save-interval 140
 ;; backup files copying them
 backup-by-copying t
 ;; delete excess backups silently
 delete-old-versions t
 ;; delete trailing 'with delete-trailing-whitespace'
 delete-trailing-lines t
 ;; comment also empty lines
 comment-empty-lines 'eol
 ;; cycle file completions up to the limit of options
 completion-cycle-threshold 5
 ;; disabling prompt asking for confirmation with new files
 confirm-nonexistent-file-or-buffer nil
 ;; ;; show keystrokes in progress quicker
 ;; echo-keystrokes 0.1
 ;; resize frames pixelwise
 frame-resize-pixelwise t
 ;; increase garbage collector to 10M
 gc-cons-threshold (* 10 1024 1024)
 ;; ;; do not load built-in init config
 ;; inhibit-default-init t
 ;; don't show splash screen
 inhibit-startup-screen t
 ;; no message for *scratch* buffer
 initial-scratch-message nil
 ;; load newer instead of .elc preference
 load-prefer-newer t
 ;; ;; insert newline to avoid end-of-buffer error (adds lots ot trailing lines)
 ;; next-line-add-newlines t
 ;; highlight when replacing
 query-replace-highlight t
 ;; ignore case when completing functions
 read-buffer-completion-ignore-case t
 ;; ignore case when completing files
 read-file-name-completion-ignore-case t
 ;; do not ask to save abbrevs in file
 save-abbrevs nil
 ;; highlight when searching
 search-highlight t
 ;;
 ;; scroll-conservatively 1000
 ;;
 ;; scroll-margin 0
 ;;
 ;; scroll-preserve-screen-position t
 ;;
 ;; ask for textual confirmation instead of GUI
 use-file-dialog nil
 ;; don't ask before open a symlink
 vc-follow-symlinks t
 ;; backup files even when they're in vc
 vc-make-backup-files t
 ;; make backup versions unconditionally
 version-control t
 ;; no beep when reporting errors
 visible-bell t
 ;; copy-and-paste with other programs
 select-enable-clipboard t)


;;; literal & compound settings
;;
;; disable messages from startup
(setq inhibit-startup-echo-area-message "javier")

;; backups
(setq backup-directory-alist
      `((".*" . ,user-backup-directory)))
;; (list (cons ".*" user-backup-directory))

;; auto-save
(setq auto-save-list-file-prefix user-auto-save-directory
      auto-save-file-name-transforms `((".*" ,user-auto-save-directory t)))

;; keep Emacs custom-settings in separate file
(setq custom-file
      (expand-file-name "custom.el" user-elisp-directory))

(when (file-exists-p custom-file)
  (load custom-file))


;;; Emacs advanced commands
;;
;; ;; overwrite contents of the active region
;; (put 'overwrite-mode 'disabled t)
;; ;; delete the entire contents of the current buffer
;; (put 'erase-buffer 'disabled nil)
;; evaluate expresion in the echo area (M-:)
(put 'eval-expression 'disabled nil)
;; narrow visible content to a defun (C-x n d / C-x n w)
(put 'narrow-to-defun 'disabled nil)
;; ;; narrow visible content to a page (C-x n p / C-x n w)
;; (put 'narrow-to-page 'disabled nil)
;; narrow visible content to a region (C-x n n / C-x n w)
(put 'narrow-to-region 'disabled nil)
;; downcase active region or buffer (C-x C-l)
(put 'downcase-region 'disabled t)
;; upcase active region or buffer (C-x C-u)
(put 'upcase-region 'disabled t)
;; set goal column (C-x C-n / C-u C-x C-n)
(put 'set-goal-column 'disabled nil)
;; horizontal scroll (C-x < / C-x >)
(put 'scroll-left 'disabled nil)


;;; force encoding
;;
;; use utf-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)


;;; define aliases
;;
;; replace yes-no options with y-n
(defalias 'yes-or-no-p 'y-or-n-p)
;; common command shortcuts
(defalias 'eb 'eval-buffer)


;;; macros
;;
;; enlarge current buffer
(fset 'enlarge-current-buffer
      [?\C-u ?5 ?\C-x ?^])


;; user defined set of global functions: "parentheses/" group
(load (expand-file-name "defun-default.el"
                        (file-name-as-directory
                         user-elisp-directory)))

;; ;;; periodical tasks
;; ;;
;; (run-at-time nil 60 'parentheses/display-on-fullscreen)
;; (run-at-time nil 60 'parentheses/display-on-battery)


;;; advises
;;
(advice-add 'kill-buffer :around  #'parentheses/kill-buffer)


;;; hooks
;;
;; remove highlight changes
(add-hook 'after-save-hook
          'parentheses/remove-highlight-changes)
;; delete trailing white-space/lines
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
;; ;; update user auto-loads NOT WORKING
;; (add-hook 'kill-emacs-hook
;;           'parentheses/update-autoloads-in-current-directory)


;;; additional common packages
;;
;; ;;
;; (use-package ace-jump-mode
;;   :init (ace-jump-mode-enable-mark-sync))


;; ;;
;; (use-package multiple-cursors
;;   :config
;;   (setq mc/list-file
;;         (expand-file-name "mc-list" user-cache-directory)))


;; ;; better undo (no circular)
;; (use-package undo-tree
;;   :config
;;   (global-undo-tree-mode))


;; ;;
;; (use-package iedit
;;   :config
;;   (setq iedit-toggle-key-default nil))


;;; key bindings
;;
;; disable minimise frame
(global-unset-key (kbd "C-z"))
;; disable suspend Emacs
(global-unset-key (kbd "C-x C-z"))
;; disable default kill Emacs
(global-unset-key (kbd "C-x C-c"))
;; change <RET> definition
(substitute-key-definition #'newline
                           #'newline-and-indent global-map)
;;
(substitute-key-definition #'isearch-forward
                           #'isearch-forward-regexp global-map)
;;
(substitute-key-definition #'isearch-backward
                           #'isearch-backward-regexp global-map)
;;
(substitute-key-definition #'query-replace
                           #'query-replace-regexp global-map)
;; override kill Emacs
(define-key global-map (kbd "C-x C-c C-v")
  'save-buffers-kill-terminal)
;; activate primary selection
(define-key global-map (kbd "C-x C-y")
  'parentheses/yank-primary)


;; ;;
;; (with-eval-after-load  "multiple-cursors"
;;   (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;   (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;   (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;   (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))
;; ;;
;; (with-eval-after-load "iedit"
;;   (define-key global-map (kbd "C-!") 'parentheses/iedit-narrow-to-defun))
;; ;;
;; ;; (with-eval-after-load "ace-jump-mode"
;; ;;   (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; ;;   (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark))
;;
;; (require 'org-install)
;; (define-key global-map (kbd "C-c a") 'org-agenda)

;; use doom-modeline instead
(use-package diminish
  :disabled
  :demand t
  :config
  (diminish 'highlight-changes-mode)
  (diminish 'subword-mode)
  (diminish 'visual-line-mode)
  (diminish 'whitespace-mode))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-env-enable-python nil)
  (doom-modeline-total-line-number t))

;; set symbols replacement and ligatures with typeface
;; install the fonts with the 'M-x fira-code-mode-install-fonts'
(use-package fira-code-mode
  :diminish
  :if (display-graphic-p)
  :config
  ;; ligatures everywhere
  (global-fira-code-mode)
  ;; (when (window-system)
  ;;   (set-frame-font "Fira Code"))
  ;; "Fira Code" everywhere in GUI (different from "Fira Code Symbols")
  (set-face-attribute 'default nil :family "Fira Code")
  (set-face-attribute 'default nil :height 115)
  :custom
  ;; disable some ligatures
  (fira-code-mode-disabled-ligatures '("[]" "x")))

;; install the fonst with 'M-x nerd-icons-install-fonts'
(use-package nerd-icons
  :ensure t)


(provide 'init-default)

;;; init-default.el ends here
