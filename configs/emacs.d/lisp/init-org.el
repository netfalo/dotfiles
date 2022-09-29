;;; init-org.el --- Set up Org Mode
(require 'org)

;;Log time when done
(setq org-log-done 'time)

(setq org-agenda-files (directory-files-recursively "~/Documents/notes" "\\.org$"))


(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/Documents/notes/journal")
  (setq org-journal-file-format "%Y%m%d.org"))

(add-hook
 'org-journal-after-entry-create-hook
 (lambda ()
   (org-agenda-file-to-front (buffer-name))))

(defun org-journal-save-entry-and-exit()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit)

(provide 'init-org)
;;; init-org.el ends here
