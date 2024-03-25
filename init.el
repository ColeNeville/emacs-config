(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(require 'use-package)


(use-package org-babel-eval-in-repl
  :ensure t
  :config
  (org-babel-load-file
   (expand-file-name
    "config.org"
    user-emacs-directory)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-complete-in-steps t)
 '(ledger-default-date-format "%Y-%m-%d")
 '(ledger-schedule-file "~/finance/scheduled.ledger")
 '(package-selected-packages
   '(terraform-mode treesit-auto marginalia consult-org-roam org-roam docker-file-mode org-babel-eval-in-repl treemacs-projectile use-package which-key vterm treemacs robe rainbow-delimiters nix-mode neotree modus-themes magit ledger-mode helm flycheck dockerfile-mode docker-compose-mode docker counsel-projectile company auto-package-update all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
