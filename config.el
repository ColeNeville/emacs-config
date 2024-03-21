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
  :bind (("M-h" . 'which-key-show-top-level))
  :custom
  (which-key-idle-delay 0.5))

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

(add-hook 'prog-mode-hook 'fly-make)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
