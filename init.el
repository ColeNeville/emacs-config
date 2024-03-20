(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(require 'use-package)
(setq auto-package-always-ensure)


(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))


;; Remove the annoying ringing
(setq visible-bell t
      ring-bell-function 'ignore)

;; This shouldn't even be on by default...
(setq inhibit-startup-screen t
      inhibit-startup-message t)


;; Be explicit/and enable some minor modes
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode -1)


;; Line numbers in the editors
(setq display-line-numbers-minor-tick 5
      display-line-numbers-major-tick 25
      display-line-numbers-width 4)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(line-number-mode -1)


(add-hook 'prog-mode-hook 'flymake-mode)


(use-package modus-themes
  :config
  (setq modus-themes-common-palette-overrides
	`((border-mode-line-active bg-mode-line-active)
	  (border-mode-line-inactive bg-mode-line-inactive)))
  (setq modus-vivendi-palette-overrides
	`((bg-main "#161616")))
  (load-theme 'modus-vivendi t))


(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode)
  (global-set-key (kbd "M-h") 'which-key-show-top-level))


(use-package ace-window
  :config
  (global-set-key (kbd "C-x S") 'ace-window))


(use-package docker)


(use-package magit)


(use-package vterm)


;; Language major modes

(use-package ledger-mode
  :config
  ;; ledger mode isn't considered a programming mode
  (add-hook 'ledger-mode-hook 'flymake-mode))


(use-package nix-mode)


(use-package dockerfile-mode)


(use-package docker-compose-mode)


(use-package robe
  :hook (ruby-mode . robe-mode))


;; Language utility packages

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Search and code completion


(use-package company
  :commands global-company-mode
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
  :commands (counsel-mode, ivy-mode)
  :bind (("C-f" . councel-grep)
	 ("C-s" . counsel-projectile-grep))
  :init
  (setq ivy-dynamic-exhibit-delay-ms 250)
  :config
  (counsel-mode))


;; Project functionality

(use-package projectile
  :commands (projectile-mode)
  :bind (:map projectile-mode-map
	      ("C-x p" . projectile-command-map))
  :config
  (projectile-mode 1))


(use-package treemacs
  :commands (treemacs, treemas-mode)
  :init
  (setq treemacs-width 45)
  :config
  (treemacs-follow-mode 1)
  (treemacs-git-commit-diff-mode 1))


(use-package counsel-projectile
  :commands (counsel-projectile-mode)
  :config
  (counsel-projectile-mode 1))


(use-package treemacs-projectile)

