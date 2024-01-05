;;; early-init.el --- Emacs init configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;  - maximize initial frame on startup
;;  - remove useless decoration
;;  - remove message 'Package cl is deprecated'

;;
;;; Code:
(push '(fullscreen . fullboth) initial-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq byte-compile-warnings '(cl-functions))
;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8
;; End:
;;; early-init.el ends here
