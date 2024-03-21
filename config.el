(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe)
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t))

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-common-palette-overrides
   `((border-mode-line-active bg-mode-line-active)
     (border-mode-line-inactive bg-mode-line-inactive)))
  (modus-vivendi-palette-overrides
   `((bg-main "#161616"))))

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
  :commands (ledger-mode)
  :hook (ledger-mode . flymake-mode))

(use-package nix-mode
  :ensure t
  :commands (nix-mode))

(use-package dockerfile-mode
  :ensure t
  :commands (dockerfile-mode))

(use-package docker-compose-mode
  :ensure t
  :commands (docker-compose-mode))

(use-package robe
  :ensure
  :commands (robe-mode))

(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode))

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
  :commands (global-company-mode))

(use-package counsel
  :ensure t
  :custom
  (ivy-dynamic-exhibit-delay-ms 250)
  :commands (counsel-mode
	     ivy-mode)
  :bind (("C-f" . counsel-grep)
	 ("C-s" . counsel-projectile-grep)))

(use-package counsel-projectile
  :ensure t
  :commands (counsel-projectile-mode))

(use-package projectile
  :ensure t
  :commands (projectile-mode)
  :bind (:map projectile-mode-map
	      ("C-x p" . projectile-command-map)))

(use-package treemacs
  :ensure t
  :custom
  (treemacs-width 45)
  :commands (treemacs)
  :config
  (treemacs-follow-mode 1)
  (treemacs-git-commit-diff-mode 1))

(use-package treemacs-projectile :ensure t)

(setq visible-bell t
      ring-bell-function 'ignore)

(setq inhibit-startup-screen t
      inhibit-startup-message t)

(setq display-line-numbers-minor-tick 5
	display-line-numbers-major-tick 25
	display-line-numbers-width 4)

(load-theme 'modus-vivendi t)
(auto-package-update-maybe)

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode -1)
(line-number-mode -1)

(which-key-mode)
(global-company-mode)

(projectile-mode)
(counsel-mode)
(counsel-projectile-mode)

(treemacs)

(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook 'ruby-mode-hook 'robe-mode)

(add-hook 'ledger-mode-hook 'flymake-mode)

(global-set-key (kbd "C-z") 'ignore)
