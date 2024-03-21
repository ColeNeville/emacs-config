(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)


(use-package org-babel-eval-in-repl
  :ensure t
  :config
  (org-babel-load-file
   (expand-file-name
    "config.org"
    user-emacs-directory)))


(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x S") 'ace-window))


(use-package docker
  :ensure t)


(use-package magit
  :ensure t)


(use-package vterm
  :ensure t)


;; Language major modes

(use-package ledger-mode
  :ensure t
  :config
  ;; ledger mode isn't considered a programming mode
  (add-hook 'ledger-mode-hook 'flymake-mode))


(use-package nix-mode
  :ensure t)


(use-package dockerfile-mode
  :ensure t)


(use-package docker-compose-mode
  :ensure t)


(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode))


;; Language utility packages

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


;; Search and code completion


(use-package company
  :ensure t
  :commands (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
	company-tooltip-offset-display 'lines
	company-tooltip-flip-when-above t
	company-tooltip-margin 3
	company-tooltip-maximum-width 60
	company-tooltip-width-grow-only t
	company-frontends '(company-pseudo-tooltip-frontend
			    company-preview-if-just-one-frontend))
  (global-company-mode))


(use-package counsel
  :ensure t
  :commands (counsel-mode
	     ivy-mode)
  :bind (("C-f" . councel-grep)
	 ("C-s" . counsel-projectile-grep))
  :init
  (setq ivy-dynamic-exhibit-delay-ms 250)
  :config
  (counsel-mode))


;; Project functionality

(use-package projectile
  :ensure t
  :commands (projectile-mode)
  :bind (:map projectile-mode-map
	      ("C-x p" . projectile-command-map)))


(use-package treemacs
  :ensure t
  :commands (treemacs
	     treemas-mode)
  :init
  (setq treemacs-width 45)
  :config
  (treemacs-follow-mode 1)
  (treemacs-git-commit-diff-mode 1))


(use-package counsel-projectile
  :ensure t
  :commands (counsel-projectile-mode)
  :config
  (counsel-projectile-mode 1))


(use-package treemacs-projectile
  :ensure t)


(projectile-mode 1)
(company-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-babel-eval-in-repl treemacs-projectile use-package which-key vterm treemacs robe rainbow-delimiters nix-mode neotree modus-themes magit ledger-mode helm flycheck dockerfile-mode docker-compose-mode docker counsel-projectile company auto-package-update all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
