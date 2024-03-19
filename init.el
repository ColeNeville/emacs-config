(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)


(setq visible-bell t
      ring-bell-function 'ignore)
(setq inhibit-startup-screen t
      inhibit-startup-message t)


;; Be explicit/and enable some minor modes

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(setq display-line-numbers-minor-tick 5
      display-line-numbers-major-tick 25
      display-line-numbers-width 4)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(line-number-mode -1)


(add-hook 'prog-mode-hook 'flymake-mode)


(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))
  (ivy-mode 1))


(use-package company
  :ensure t
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


(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-common-palette-overrides
	`((border-mode-line-active bg-mode-line-active)
	  (border-mode-line-inactive bg-mode-line-inactive)))
  (setq modus-vivendi-palette-overrides
	`((bg-main "#161616")))
  (load-theme 'modus-vivendi t))


(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(use-package all-the-icons
  :ensure t)


(use-package treemacs
  :ensure t
  :config
  (treemacs))


(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x S") 'ace-window))

(use-package docker
  :ensure t)


(use-package magit
  :ensure t)


;; Language major modes

(use-package ledger-mode
  :ensure t
  :config
  ;; ledger mode isn't considered a programming mode
  (add-hook 'ledger-mode-hook 'flymake-mode))


(use-package nix-mode
  :ensure t)


(use-package docker-compose-mode
  :ensure t)


(use-package robe
  :ensure t
  :defer t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))


;; Language utility packages

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

