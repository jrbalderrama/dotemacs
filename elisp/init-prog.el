;;; init-prog.el --- Common programming configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package aggressive-indent
;;   :config
;;   (add-to-list 'aggressive-indent-excluded-modes 'yaml-mode))


(use-package company)

(use-package diff-hl)

(use-package flycheck
  :config
  ;; override pop-up font settings
  (add-to-list 'face-remapping-alist
               '(popup-tip-face :height 0.7
                                :box t
                                :background "#D0BF8F"
                                :foreground "#2B2B2B"))
  :bind
  (:map flycheck-command-map
        ;; use 'C-c ! t' to with tip cycle
        ("t" . #'flycheck-tip-cycle)))

(use-package flycheck-tip
  :after flycheck
  :config
  (setq
   ;; function to display on error messages
   flycheck-display-errors-function
   'flycheck-tip-display-current-line-error-message
   ;; disable error messages at point in mini-buffer
   flycheck-display-errors-function nil)
  ;; use pop-up tool-tips instead of echo area
  :hook flycheck-tip-cycle)


(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package yaml-mode
  :mode "\\.ya?ml$"
  :bind
  (:map yaml-mode-map
        ("C-m" . newline-and-indent)))

(with-eval-after-load "prog-mode"

  (setq
   ;; disable ligatures at point
   prettify-symbols-unprettify-at-point t
   ;; events to activate whitespace style
   whitespace-style '(face lines-tail space-before-tab
                           indentation space-after-tab))
  ;; change display-line-numbers-mode face attributes
  (add-to-list 'face-remapping-alist
               '(line-number
                 :inherit (shadow default)
                 :height 0.6))
  ;; (set-face-attribute 'line-number nil :xxx)

  ;; enable flycheck for programming
  (add-hook 'prog-mode-hook 'flycheck-mode)
  ;; show long lines in different style
  (add-hook 'prog-mode-hook #'whitespace-mode)
  ;; spell only strings not source code
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  ;; show line numbers
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  ;; highlights uncommitted changes next to line number
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'prog-mode-hook 'diff-hl-flydiff-mode))

;; (load "~/.emacs.d/elisp/init-elisp.el")
;; (require 'init-python)

(provide 'init-prog)

;;; init-prog.el ends here
