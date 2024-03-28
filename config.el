(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe)
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t))

(auto-package-update-maybe)

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-common-palette-overrides
   `((border-mode-line-active bg-mode-line-active)
     (border-mode-line-inactive bg-mode-line-inactive)))
  (modus-vivendi-palette-overrides
   `((bg-main "#161616"))))

(load-theme 'modus-vivendi t)

(setq visible-bell t
      ring-bell-function 'ignore)

(setq inhibit-startup-screen t
      inhibit-startup-message t)

(setq display-line-numbers-minor-tick 5
      display-line-numbers-major-tick 25
      display-line-numbers-width 4)

(setq org-support-shift-select t)

(setq custom-file "~/.config/custom.el")

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode -1)
(line-number-mode -1)

(menu-bar-mode 1)

(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-unset-key (kbd "C-x C-z"))

(global-set-key [escape] 'keyboard-escape-quit)

(define-prefix-command 'personal-prefix-map)
(global-set-key (kbd "C-z") 'personal-prefix-map)

(use-package ace-window
  :ensure t
  :bind (:map personal-prefix-map
              ("s" . ace-window)
              ("S" . ace-swap-window)))

(use-package diminish :ensure t)

(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package treemacs
  :ensure t
  :after (counsel)
  :commands (treemacs
             treemacs-follow-mode
             treemacs-git-commit-diff-mode)
  :custom
  (treemacs-width 45)
  :config
  (treemacs-follow-mode 1)
  (treemacs-git-commit-diff-mode 1))

;; Treemacs loads after ivy and counsel so the workspace
;; picker has counsel support

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-z l")
  :commands (lsp
	         lsp-enable-which-key-integration)
  :hook ((tsx-ts-mode . lsp)
	     (typescript-ts-mode . lsp)
	     (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-mode))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode
	  ivy)
  :commands (lsp-ivy-workspace-symbol))

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode
	  treemacs)
  :commands (lsp-treemacs-errors-list))

(use-package ivy
  :ensure t
  :diminish t
  :commands (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :bind (:map ivy-minibuffer-map
	      ("S-SPC" . nil)))

(use-package counsel
  :ensure t
  :diminish t
  :after ivy
  :commands (counsel-mode))

(ivy-mode 1)
(counsel-mode 1)
(treemacs 1)

(use-package which-key
  :ensure t
  :diminish t
  :commands (which-key-mode)
  :bind (("M-h" . which-key-show-top-level))
  :custom
  (which-key-idle-delay 0.5))

(which-key-mode 1)

(use-package company
  :ensure t
  :commands (global-company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-tooltip-display 'lines)
  (company-tooltip-flip-when-above t)
  (company-tooltip-margin 3)
  (company-tooltip-maximum-width 60)
  (company-frontends '(company-pseudo-tooltip-frontend
                       company-preview-if-just-one-frontend)))

(global-company-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package marginalia
  :ensure t
  :commands (marginalia-mode))

(marginalia-mode)

(use-package tree-sitter
  :ensure t
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.rb\\'" . ruby-ts-mode))
  :commands (global-tree-sitter-mode
             tree-sitter-hl-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package treesit-auto
  :ensure t
  :commands (global-treesit-auto-mode))

(global-tree-sitter-mode)
(global-treesit-auto-mode)

(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger\\'" "\\.journal\\'")
  :hook ((ledger-mode . flymake-mode)
         (ledger-mode . display-line-numbers-mode)))

(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'"))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile"))

(use-package docker-compose-mode
  :ensure t
  :mode ("docker-compose\\.yml"))

(use-package terraform-mode
  :ensure t
  :mode ("\\.tf//'"))

(require 'org-tempo)

(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
(eval-after-load 'org-indent '(diminsh 'org-indent-mode))

(use-package toc-org
  :ensure t
  :commands (toc-org-enable)
  :hook ((org-mode . toc-org-enable)))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :hook ((org-mode . (lambda () (org-bullets-mode 1)))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/notes")
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-template
   '(("d" "default" entry
      (file "~/.config/emacs/org-roam/templates/daily.org")
      :target (file+head "%<%Y-%m-%d>.org"
			 "#+TITLE: %<%Y-%m-%d>\n"))))
  :commands (org-roam-setup))

(defun cn/org-roam-dailies-goto-today
    (org-roam-dailies-capture-today :goto t))

(org-roam-setup)
(define-key personal-prefix-map
	    "nb" 'org-roam-buffer-toggle)
(define-key personal-prefix-map
	    "ni" 'org-roam-node-insert)
(define-key personal-prefix-map
	    "nf" 'org-roam-node-find)
(define-key personal-prefix-map
	    "nd" 'cn/org-roam-dailies-goto-today)

(use-package robe
  :ensure t
  :hook ((ruby-mode . robe-mode)
         (ruby-ts-mode . robe-mode)))
