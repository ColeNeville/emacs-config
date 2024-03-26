(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-common-palette-overrides
   `((border-mode-line-active bg-mode-line-active)
     (border-mode-line-inactive bg-mode-line-inactive)))
  (modus-vivendi-palette-overrides
   `((bg-main "#161616")))
  :config
    (load-theme 'modus-vivendi t))

(use-package which-key
  :ensure t
  :commands (which-key-mode)
  :bind (("M-h" . which-key-show-top-level))
  :custom
  (which-key-idle-delay 0.5))

(use-package ace-window
  :ensure t
  :bind (("C-x S" . ace-window)))

(use-package docker :ensure t)

(use-package magit :ensure t)

(use-package vterm :ensure t)

(use-package ledger-mode
  :ensure t
  :hook ((ledger-mode . flymake-mode)
	 (ledger-mode . display-line-numbers-mode)))

(use-package nix-mode :ensure t)

(use-package dockerfile-mode :ensure t)

(use-package docker-compose-mode
  :ensure t
  :commands (docker-compose-mode))

(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package company
  :ensure t
  :custom
  (company-tooltip-align-annotations t)
  (company-tooltip-display 'lines)
  (company-tooltip-flip-when-above t)
  (company-tooltip-margin 3)
  (company-tooltip-maximum-width 60)
  (company-frontends '(company-pseudo-tooltip-frontend
		       company-preview-if-just-one-frontend))
  :config
  (global-company-mode))

(use-package counsel
  :ensure t
  :commands (ivy-mode
	     counsel-mode)
  :custom
  (ivy-dynamic-exhibit-delay-ms 250)
  :bind (("C-f" . counsel-grep)
	 ("C-s" . counsel-git-grep)))

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :custom
  (treemacs-width 45)
  :config
  (treemacs-follow-mode 1)
  (treemacs-git-commit-diff-mode 1))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/notes")
  :commands (org-roam-setup)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package eglot :ensure t)

(use-package apheleia
  :ensure t
  :commands (apheleia-global-mode))

(use-package terraform-mode :ensure t)

(use-package editorconfig :ensure t)

(setq visible-bell t
      ring-bell-function 'ignore)

(setq inhibit-startup-screen t
      inhibit-startup-message t)

(setq display-line-numbers-minor-tick 5
	display-line-numbers-major-tick 25
	display-line-numbers-width 4)

(setq org-support-shift-select t)


;; This is required for eglot to load in cases where the folder is not a git repo
(add-to-list 'project-vc-extra-root-markers "tsconfig.json")

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode -1)
(line-number-mode -1)

(menu-bar-mode 1)

(ivy-mode 1)
(counsel-mode 1)
(which-key-mode 1)
(apheleia-global-mode 1)

;; Load treemacs last so ivy and counsel are setup
(treemacs 1)

(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-set-key (kbd "C-z") 'ignore)
(global-set-key (kbd "C-x C-z") 'ignore)
