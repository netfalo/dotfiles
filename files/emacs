(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this 

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))

(setq-default indent-tabs-mode nil)
(setq tab-width 4) ; or any other preferred value

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(setq-default fill-column 80)
