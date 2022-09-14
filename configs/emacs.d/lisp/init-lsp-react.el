;;; package --- Summary
;;; Commentary:
;;; Code:
(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))

(add-to-list 'auto-mode-alist
'("\\.tsx\\'" . javascript-mode))

(provide 'init-lsp-react)
;;; init-lsp-react.el ends here
