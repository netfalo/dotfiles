;;; init-org.el --- Set up Org Mode
(require 'org)

;;Log time when done
(setq org-log-done 'time)


(setq org-capture-templates
      ;; other capture templates
      '(("s" "Slipbox" entry  (file "~/Documents/org/braindump.org")
       "* %?\n")))

;; Try to use a slipbox for braindumps
(defun netfalo/org-capture-slipbox ()
  (interactive)
  (org-capture nil "s"))

(global-set-key (kbd "C-c c") 'netfalo/org-capture-slipbox)

;; Configure org-roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%H:%M>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n\n"))
     ("m" "meeting-notes" entry "** %?"
      :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n\n" ("%^{Meeting Subject}")))
     ("w" "work-log" entry "* %<%H:%M>: %? :daily:"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n\n"))
     ))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n I" . org-roam-node-insert-immediate)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-enable))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(provide 'init-org)
;;; init-org.el ends here
