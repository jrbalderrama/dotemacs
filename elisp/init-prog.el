;;; init-prog.el --- Common programming configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package aggressive-indent
;;   :config
;;   (add-to-list 'aggressive-indent-excluded-modes 'yaml-mode))

;; TODO
;; - ein (jupyter)
;; - ruff linter in two years
;; - refactoring
;; - pydoc edoc
;; - Vertico and Corfu and Orderless + (embark, consult)
(use-package company
  :diminish
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :hook (prog-mode . company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; (use-package company-quickhelp
;;   :hook (company-mode . company-quickhelp-mode))

(use-package dap-mode
  :disabled
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package diff-hl
  :config (diff-hl-flydiff-mode)
  :hook ((prog-mode . turn-on-diff-hl-mode)
         (vc-dir-mode . turn-on-diff-hl-mode)))

(use-package direnv
  :after exec-path-from-shell
  :hook (python-mode . direnv-mode))

(use-package envrc
  :disabled
  :after exec-path-from-shell
  :config (envrc-global-mode))

(use-package flycheck
  :custom
  (flycheck-idle-buffer-switch-delay 0)
  (flycheck-check-syntax-automatically
   '(save idle-buffer-switch idle-change new-line mode-enabled))
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :config (flycheck-pos-tip-mode)
  :custom
  ;; TODO 'face-remap-add-relative
  (pos-tip-foreground-color "white")
  (pos-tip-internal-border-width 10)
  (pos-tip-background-color "black"))

(use-package flycheck-pycheckers
  :after (:all direnv flycheck)
  :custom (flycheck-pycheckers-checkers '(mypy3 flake8))
  ;; :config (add-to-list 'direnv-non-file-modes 'flycheck-mode)
  ;; (setq flycheck-pycheckers-ignore-codes (append flycheck-pycheckers-ignore-codes '("E0731" "E0741")))
  :hook (flycheck-mode . flycheck-pycheckers-setup))

(use-package flycheck-mypy
  :disabled
  :after flycheck
  :config
  (load (expand-file-name
         "flycheck-python-ruff.el"
         (file-name-as-directory
          user-elisp-directory)))
  :hook ((python-mode python-ts-mode) . flycheck-python-ruff-setup))

(use-package flycheck-python-ruff
  :disabled
  :after flycheck
  :requires flycheck-mypy
  :hook ((python-mode python-ts-mode) . flycheck-python-ruff-setup))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :config (require 'lsp-ido)
  :custom
  (lsp-diagnostics-provider :none)
  (lsp-enable-snippet nil)
  (lsp-modeline-diagnostics-scope nil)
  :bind (:map lsp-mode-map
              ("<tab>" . company-indent-or-complete-common))
  :hook
  (python-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-pyright
  :after (:all lsp-mode direnv)
  :custom
  (lsp-pyright-disable-language-service nil)
  (lsp-pyright-disable-organize-imports t))

(use-package lsp-treemacs
  :defer t
  :custom
  (lsp-treemacs-sync-mode t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-doc-delay 1)
  :bind (:map lsp-ui-mode-map
              ("C-c i" . lsp-ui-imenu))
  :hook (lsp-mode . lsp-ui-mode))

(use-package reformatter
  :config
  (reformatter-define darker-reformat
    :program "darker"
    :stdin nil
    :stdout nil
    :args (list "-q" "-i" input-file))
  :hook (python-mode . darker-reformat-on-save-mode))

(use-package pyenv-mode
  :disabled
  :after direnv
  ;; :init (setq exec-path (append exec-path '("~/.pyenv/bin")))
  :hook (python-mode . pyenv-mode))

(use-package python-pytest
  :after direnv)

(use-package python
  :custom
  ;; Remove guess indent python message
  (python-indent-guess-indent-offset-verbose nil)
  :config
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))

(use-package pyvenv
  :disabled
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package treemacs
  :defer t
  :custom
  ;(treemacs-no-png-images t)
  (treemacs-width 24)
  :bind ("C-c t" . treemacs))

(use-package with-venv
  :disabled)

(use-package which-key
  :diminish
  :after lsp-mode
  :custom (which-key-sort-order 'which-key-local-then-key-order)
  :hook (lsp-mode . which-key-mode))

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
