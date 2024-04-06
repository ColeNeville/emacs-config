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

(use-package ace-window
  :ensure t
  :commands (ace-window
             ace-swap-window))

(use-package diminish :ensure t)

(use-package magit
  :ensure t
  :commands (magit-status
             magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package treemacs
  :ensure t
  :commands (treemacs
             treemacs-follow-mode
             treemacs-git-commit-diff-mode
             treemacs-select-window)
  :custom
  (treemacs-width 45)
  :config
  ;; These are "modes" but more specific to treemacs and its experience
  ;; I am considering these as configuration for treemacs
  (treemacs-follow-mode 1)
  (treemacs-git-commit-diff-mode 1))

(use-package ivy
  :ensure t
  :commands (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-selectable-prompt t)
  :bind (:map ivy-minibuffer-map
              ("S-SPC" . nil))
  :config
  (diminish 'ivy-mode))

(use-package counsel
  :ensure t
  :after ivy
  :commands (counsel-mode
             counsel-grep
             counsel-git-grep)
  :config
  (diminish 'counsel-mode))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-z l")
  :commands (lsp lsp-enable-which-key-integration)
  :hook ((tsx-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands (lsp-ui-mode))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode ivy)
  :commands (lsp-ivy-workspace-symbol))

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :commands (lsp-treemacs-errors-list))

(use-package which-key
  :ensure t
  :commands (which-key-mode
             which-key-show-top-level)
  :custom
  (which-key-idle-delay 0.5)
  (diminish 'which-key-mode))

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
                       company-preview-if-just-one-frontend))
  :config
  (diminish 'company-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package marginalia
  :ensure t
  :commands (marginalia-mode))

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
  :mode ("\\.tf\\'"))

(use-package robe
  :ensure t
  :hook ((ruby-mode . robe-mode)
         (ruby-ts-mode . robe-mode)))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :hook ((org-mode . (lambda () (org-bullets-mode 1)))))

(use-package toc-org
  :ensure t
  :commands (toc-org-enable)
  :hook ((org-mode . toc-org-enable)))

(use-package org-roam
  :ensure t
  :after (org)
  :custom
  (org-roam-directory "~/notes/")
  (org-roam-capture-templates
   (let ((head "#+TITLE: ${title}")
         (filename "%<%Y%m%d%H%M%S>-${slug}.org"))
     `(("n" "note" plain "* %?"
        :target (file+head ,filename ,head)
        :unnarrowed t))))
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   (let ((head "#+TITLE: %<%Y-%m-%d>\n\n* [/] Do today\n\n* Journal")
         (filename "%<%Y-%m-%d>.org"))
     `(("j" "journal" item "%<%H:%M> - %?"
        :target (file+head+olp ,filename ,head ("Journal"))
        :unarrowed t)
       ("t" "todo" item "- [ ] %?"
        :target (file+head+olp ,filename ,head ("Do today"))
        :unarrowed t)
       ("n" "note" entry "* %?"
        :target (file+head ,filename ,head)
        :unarrowed t)
       ("m" "meeting" entry
        "* %?\n** Attending\n- \n** Notes\n*** \n** Takeaways [/]\n- [ ] "
        :target (file+head ,filename ,head)
        :unarrowed t))))
  :commands (org-roam-setup
             org-roam-buffer-toggle
             org-roam-dailies-goto-today
             org-roam-dailies-capture-today
             org-roam-dailies-goto-date
             org-roam-dailies-capture-date))

(use-package vulpea
  :ensure t
  :after (org-roam)
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable))
  :commands (vulpea-find
             vulpea-insert))

(use-package deft
  :ensure t
  :after (org-roam)
  :commands (deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(use-package org-noter
  :ensure t
  :defer t)

(use-package org-roam-ui
  :ensure t
  :after (org-roam)
  :commands (org-roam-ui-open))

(use-package org-ql
  :ensure t
  :defer t)

(use-package org-roam-ql
  :ensure t
  :after (org-roam)
  :commands (org-roam-ql-search))

(use-package org-roam-ql-ql
  :ensure t
  :after (org-ql org-roam-ql))

(require 'org-tempo)

;; Remove the annoying ding on actions
(setq visible-bell t
      ring-bell-function 'ignore)

;; Remove that annoying startup/welcome screen and just give me the scratchpad
(setq inhibit-startup-screen t
      inhibit-startup-message t)

;; Configure the display line number on the left side of the buffer mode
(setq display-line-numbers-minor-tick 5
      display-line-numbers-major-tick 25)

(setq-default display-line-numbers-width 4)

;; Org mode options
(setq org-support-shift-select t
      org-startup-truncated nil)

;; Opening a link between org notes should open in the same frame rather than a new one
;; Frame spawning in emacs seems to be unpredictable (i need to look more into it)
(add-to-list 'org-link-frame-setup '(file . find-file))

;; This is nice when using ivy and counsel, but can also be a pain point
(setq enable-recursive-minibuffers t)

;; Move our custom files and keep init.el clean
(setq custom-file "~/.config/emacs_custom.el")

;; I keep pressing this trying to undo... Minimize doesn't really work on my personal laptop with qtile
;; I would also unbind C-z too but I commendeer it for my personal prefix.
(global-unset-key (kbd "C-x C-z"))

(define-prefix-command 'personal-prefix-map)

;; Define a prefix that is useful for modes that can get in the way until you want them
(define-prefix-command 'personal-mode-toggle-prefix-map)

(define-prefix-command 'personal-org-roam-prefix-map)

(global-set-key (kbd "C-z") 'personal-prefix-map)

(define-key personal-prefix-map
            "m" 'personal-mode-toggle-prefix-map)

(define-key personal-prefix-map
            "n" 'personal-org-roam-prefix-map)

(global-set-key (kbd "M-h") 'which-key-show-top-level)

(define-key personal-prefix-map
            "f" 'counsel-grep)
(define-key personal-prefix-map
            "F" 'counsel-git-grep)

(define-key personal-prefix-map
            "s" 'ace-window)
(define-key personal-prefix-map
            "S" 'ace-swap-window)

(define-key personal-prefix-map
            "t" 'treemacs-select-window)

(define-key personal-org-roam-prefix-map
            "b" 'org-roam-buffer-toggle)

(define-key personal-org-roam-prefix-map
            "d" 'org-roam-dailies-capture-date)
(define-key personal-org-roam-prefix-map
            "D" 'org-roam-dailies-goto-date)

(define-key personal-org-roam-prefix-map
            "f" 'vulpea-find)

(define-key personal-org-roam-prefix-map
            "i" 'vulpea-insert)

(define-key personal-org-roam-prefix-map
            "s" 'deft) ;; "s" for search

(define-key personal-org-roam-prefix-map
            "t" 'org-roam-dailies-capture-today)
(define-key personal-org-roam-prefix-map
            "T" 'org-roam-dailies-goto-today)

(define-key personal-org-roam-prefix-map
            "q" 'org-roam-ql-search)

(define-key personal-mode-toggle-prefix-map
            "c" 'highlight-changes-mode)
(define-key personal-mode-toggle-prefix-map
            "w" 'whitespace-mode)

;; Programming modes hooks
(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)



(load-theme 'modus-vivendi t)

;; Disable some default minor modes
(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode -1)

;; Enable some minor modes
(menu-bar-mode 1)

;; Enable some modes from packages above

(global-company-mode 1)

(which-key-mode 1)

(ivy-mode 1)
(counsel-mode 1)

(marginalia-mode 1)

;; Treemacs needs to be initialized after ivy and counsel
(treemacs 1)

(global-tree-sitter-mode)
(global-treesit-auto-mode)

(org-roam-setup)

(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
(eval-after-load 'org-indent '(diminish 'org-indent-mode))
