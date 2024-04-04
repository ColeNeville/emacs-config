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
      display-line-numbers-major-tick 25)

(setq-default display-line-numbers-width 4)

(setq org-support-shift-select t
      org-startup-truncated nil)

(setq enable-recursive-minibuffers t)

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
(define-prefix-command 'personal-mode-toggle-prefix-map)

(define-key personal-mode-toggle-prefix-map
            "c" 'highlight-changes-mode)
(define-key personal-mode-toggle-prefix-map
            "w" 'whitespace-mode)

(define-key personal-prefix-map
            "m" 'personal-mode-toggle-prefix-map)

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
  :after (ivy counsel)
  :commands (treemacs
             treemacs-follow-mode
             treemacs-git-commit-diff-mode)
  :custom
  (treemacs-width 45)
  :bind (:map personal-prefix-map
              ("t" . treemacs-select-window))
  :config
  (treemacs-follow-mode 1)
  (treemacs-git-commit-diff-mode 1))

;; Treemacs loads after ivy and counsel so the workspace
;; picker has counsel support

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
  :commands (lsp-ui-mode))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode ivy)
  :commands (lsp-ivy-workspace-symbol))

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :commands (lsp-treemacs-errors-list))

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
  :commands (counsel-mode)
  :bind (:map personal-prefix-map
              ("f" . counsel-grep)
              ("F" . counsel-git-grep))
  :config
  (diminish 'counsel-mode))

(ivy-mode 1)
(counsel-mode 1)
(treemacs 1) ;; Treemacs needs to be initialized after ivy and counsel

(use-package which-key
  :ensure t
  :commands (which-key-mode)
  :bind (("M-h" . which-key-show-top-level))
  :custom
  (which-key-idle-delay 0.5)
  (diminish 'which-key-mode))

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
                       company-preview-if-just-one-frontend))
  :config
  (diminish 'company-mode))

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
  :mode ("\\.tf\\'"))

(require 'org-tempo)

(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(add-to-list 'org-link-frame-setup '(file . find-file))

(use-package toc-org
  :ensure t
  :commands (toc-org-enable)
  :hook ((org-mode . toc-org-enable)))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :hook ((org-mode . (lambda () (org-bullets-mode 1)))))

(define-prefix-command 'personal-org-roam-prefix-map)
(define-key personal-prefix-map
            "n" 'personal-org-roam-prefix-map)

;; ;; These aren't working as expected...
;; (defun cn/org-roam-dailies-journal-today ()
;;   "Create a journal entry in the today daily note"
;;   (interactive)
;;   (org-roam-dailies-capture-today :key "j"))

;; (defun cn/org-roam-dailies-todo-today ()
;;   "Create a todo entry in the today daily note"
;;   (interactive)
;;   (org-roam-dailies-capture-today :key "t"))
;; ;;

(use-package org-roam
  :ensure t
  :after (org)
  :custom
  (org-roam-directory "~/notes/")
  (org-roam-capture-templates
   '(("n" "note" plain "* %?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}")
      :unnarrowed t)))
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   (let ((head "#+TITLE: %<%Y-%m-%d>\n\n* [/] Do today\n\n* Journal")
         (filename "%<%Y-%m-%d>.org"))
     `(("j" "journal" item
        "%<%H:%M> - %?"
        :target (file+head+olp ,filename ,head ("Journal"))
        :unarrowed t)
       ("t" "todo" entry
        "** TODO %?"
        :target (file+head+olp ,filename ,head ("Do today"))
        :unarrowed t)
       ("n" "note" entry
        "* %?"
        :target (file+head ,filename ,head)
        :unarrowed t)
       ("m" "meeting" entry
        "* %{meeting_name}\n** Attending\n- %?\n** Notes\n*** \n** Takeaways [/]\n- [ ] "
        :target (file+head ,filename ,head)
        :unarrowed t))))
  :commands (org-roam-setup)
  :bind (:map personal-org-roam-prefix-map
              ("b" . org-roam-buffer-toggle)
              ;; Create a journal capture
              ;; ("j" . cn/org-roam-dailies-journal-today)
              ;; ("t" . cn/org-roam-dailies-todo-today)
              ;; (T)oday
              ("T" . org-roam-dailies-goto-today)
              ("t" . org-roam-dailies-capture-today)
              ;; Select (d)ate
              ("D" . org-roam-dailies-goto-date)
              ("d" . org-roam-dailies-capture-date)))

(use-package vulpea
  :ensure t
  :after (org-roam)
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable))
  :bind (:map personal-org-roam-prefix-map
              ("f" . vulpea-find)
              ("i" . vulpea-insert)))

(use-package deft
  :ensure t
  :after (org-roam)
  :bind (:map personal-org-roam-prefix-map
              ("s" . deft))
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(use-package org-noter :ensure t)

(org-roam-setup)

(use-package robe
  :ensure t
  :hook ((ruby-mode . robe-mode)
         (ruby-ts-mode . robe-mode)))
