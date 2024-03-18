(setq visible-bell t)
(setq ring-bell-function 'ignore)

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(use-package company
  :ensure t
  :config
  (company-mode))

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-common-palette-overrides
	`(
	  (border-mode-line-active bg-mode-line-active)
	  (border-mode-line-inactive bg-mode-line-inactive)

	  ;; ,@modus-themes-preset-overrides-faint
	))
  (setq modus-vivendi-palette-overrides
	`((bg-main "#171717")))
  (load-theme 'modus-vivendi))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (setq neo-window-width 45)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neotree))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x S") 'ace-window))

(use-package helm
  :ensure t
  :config
  (helm-mode)
  (global-set-key (kbd "M-x") 'helm-M-x))

(use-package docker
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package robe
  :ensure t
  :defer t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; (use-package multi-term
;;   :ensure t)
  ;; (bind-keys :map global-map
  ;; 	     :prefix "C-x T"
  ;; 	     :prefix-map terminal-commands
  ;; 	     ("n" . multi-term)
  ;; 	     ("left" . multi-term-prev)
  ;; 	     ("right" . multi-term-next)))
  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" default))
 '(package-selected-packages
   '(projectile multi-term helm docker-compose-mode which-key robe neotree modus-themes magit docker company all-the-icons ace-window)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
