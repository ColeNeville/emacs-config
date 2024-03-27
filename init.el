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
