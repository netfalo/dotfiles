;;;; init.el -- my emacs configuration
;-*-Emacs-lisp-*-

;;;; Comments
;;
;; My emacs init.el after checking out Aaron Biebers @ https://github.com/aaronbieber/dotfiles
;;
;;;; Code:
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Also add all directories within "lisp"
;; I use this for packages I'm actively working on, mostly.
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'init-utils)
(require 'init-elpa)

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Essential settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
(eval-after-load "vc" '(setq vc-handled-backends nil))
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(put 'narrow-to-region 'disabled nil)
(column-number-mode t)

(setq visible-bell t)

(defun my-minibuffer-setup-hook ()
  "Increase GC cons threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Set GC cons threshold to its default value."
  (setq gc-cons-threshold 1000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;;Additional packages
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(use-package elpy
  :ensure t
  :config
  (add-to-list 'python-mode-hook
               (lambda ()
                 (elpy-mode))))

(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'"
  :config
  (c-set-offset 'label 4))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode)
  :mode ("\\.yaml\\'" . yaml-mode)
  :config (add-hook 'yaml-mode-hook
                    (lambda ()
                      (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-enable-undo-in-region t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-relative-timestamps nil))

(use-package flycheck
  :ensure t
  :commands flycheck-mode)

;; Trying helm only a bit
;; (use-package ido
;;   :ensure t
;;   :config
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-everywhere t)
;;   (ido-mode t))

;; (use-package ido-yes-or-no
;;   :ensure t
;;   :config
;;   (ido-yes-or-no-mode))

(use-package multiple-cursors
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (add-hook 'markdown-mode-hook (lambda ()
                                  (yas-minor-mode t)
                                  (set-fill-column 80)
                                  (turn-on-auto-fill)
                                  (flyspell-mode))))

(require 'init-org)

(global-set-key (kbd "C-x C-u") 'undo-tree-visualize)

(global-set-key (kbd "C-{") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-}") 'mc/mark-next-like-this)

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package flyspell
  :ensure t)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(setq-default major-mode 'text-mode)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package flymake-json
  :ensure t
  :config
  (add-hook 'json-mode-hook 'flymake-json-load))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open")

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(electric-pair-mode 1)

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :diminish
  :config
  (setq
   git-gutter:update-interval 0.02))

(column-number-mode)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(setq-default show-trailing-whitespace t)

(add-hook 'sql-mode-hook 'lsp)

(use-package expand-region
  :ensure t
  :bind ("C-c +" . 'er/expand-region))

(pending-delete-mode t)

(require 'init-lsp)
(require 'init-metals)
(require 'init-lsp-react)

(load-theme 'zenburn)

(defun uniq-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniq-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

(require 'init-move-region)

(use-package helm
  :ensure t
  :diminish
  :bind
  ("M-x" . 'helm-M-x)
  ("C-x C-f" . 'helm-find-files)
  ("C-x b" . 'helm-buffers-list)
  :config
  (helm-mode 1)
  (require 'helm-config)
  (setq helm-M-x-show-short-doc t))

(use-package helm-swoop
  :ensure t
  :bind ("C-s" . 'helm-swoop)
  :config
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line))

(use-package projectile
  :ensure t
  :after helm
  :custom ((projectile-completion-system 'helm))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/git/")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-switch-project-action #'projectile-dired)
  :config (projectile-mode))

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . #'helpful-callable)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key))

(provide 'init)
;;; init.el ends here
