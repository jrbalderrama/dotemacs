;;; init-prog.el --- Common programming configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package aggressive-indent
;;   :config
;;   (add-to-list 'aggressive-indent-excluded-modes 'yaml-mode))

(use-package company
  :hook (prog-mode . company-mode))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-flydiff-mode)
         (vc-dir-mode . diff-hl-flydiff-mode)))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :bind (:map flycheck-command-map
              ;; use 'C-c ! t' to with tip cycle
              ("t" . #'flycheck-tip-cycle)))

(use-package flycheck-pos-tip
  :after flycheck
  :config (flycheck-pos-tip-mode)
  :custom
  ;; TODO 'face-remap-add-relative
  (pos-tip-foreground-color "white")
  (pos-tip-internal-border-width 10)
  (pos-tip-background-color "black"))

(use-package jinja2-mode
  :disabled
  :defer t
  :mode "\\.j2\\'")

(use-package lua-mode
  :disabled
  :defer t
  :mode "\\.lua\\'")

(use-package yaml-mode
  :disabled
  :defer t
  :mode "\\.ya?ml$"
  :bind
  (:map yaml-mode-map
        ("C-m" . newline-and-indent)))

(with-eval-after-load "prog-mode"
  (setq
   ;; disable ligatures at point
   prettify-symbols-unprettify-at-point t
   ;; events to activate white space style
   whitespace-style '(face lines-tail space-before-tab
                           indentation space-after-tab))

  ;; change display-line-numbers-mode face attributes
  (add-to-list 'face-remapping-alist
               '(line-number
                 ;;:inherit (shadow default)
                 :height 0.75))

  ;; (set-face-attribute 'line-number nil :xxx)

  ;; show long lines in different style
  (add-hook 'prog-mode-hook #'whitespace-mode)
  ;; spell only strings not source code
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  ;; show line numbers
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

;; (load "~/.emacs.d/elisp/init-elisp.el")
;; (require 'init-python)

(provide 'init-prog)

;;; init-prog.el ends here
