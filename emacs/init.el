(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; package management
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; See: https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html
;; In short, we set this so that we can upgrade packages that are included
;; with the emacs distribution
(setq package-install-upgrade-built-in t)

;; If it's our first time (and we don't have packages downloaded) then
;; refresh
(unless package-archive-contents (package-refresh-contents))

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;; Always install a package if not found
(setq use-package-always-ensure t)

;;; end package management

;;; core editor setup
;; turn off the menubar and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; hide welcome screen
(setq inhibit-startup-message t)

;; indents
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; font
(add-to-list 'default-frame-alist '(font . "Hack-12"))

;; useless whitespace
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)

;; mute echo
(setq visible-bell t)

;; always highlight parenthesis
(show-paren-mode t)

;; turn on column number
(setq column-number-mode t)

;; magit
(use-package magit)

;;; end core editor setup

;;; text modes
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Spell checking
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

;; Note: copied from Prelude
(defun ta42-enable-flyspell ()
  "Enable command `flyspell-mode'."
  (flyspell-mode +1))

(add-hook 'markdown-mode-hook 'ta42-enable-flyspell)
(add-hook 'text-mode-hook 'ta42-enable-flyspell)
(add-hook 'org-mode-hook 'ta42-enable-flyspell)

;;; end text modes

;;; begin prelude-to-programming
;; use environment variables from shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; end prelude-to-programming

;;; begin serialized
(use-package yaml-mode
  :ensure t)

;;; end serialized

;;; begin programming

;;; begin lsp things
;; Company mode for completions
(use-package company
  :ensure t
  :hook
  (go-mode . company-mode)
  (python-mode . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))


;; LSP Mode for code intelligence
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (go-mode . lsp-deferred)
  (pipenv-mode . (lambda ()
                   ;; Activate pipenv first
                   (pipenv-activate)
                   ;; Then start LSP
                   (lsp-deferred)))
  :config
  (setq lsp-enable-file-watchers nil)
  (setq lsp-gopls-staticcheck t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-gopls-complete-unimported t)
  :init
  (setq lsp-keymap-prefix "C-c l"))


;; Optional: For better UI
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t))

;;; end lsp things

;;; project management
;; Optional: For project management
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;;; begin golang
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred)
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

;;; end golang

;;; begin python
(use-package pyenv-mode
  :ensure t
  :hook (python-mode . pyenv-mode)
  :config
  ;; Automatically switch to the correct pyenv version based on .python-version file
  (pyenv-mode))

(use-package pipenv
  :ensure t
  :hook (pyenv-mode . pipenv-mode)
  :config
  (setq pipenv-with-projectile t)
  :custom
  (pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

(defun ta42-python-format-with-black ()
  "Format the current buffer with black from the current pipenv environment."
  (interactive)
  (when (derived-mode-p 'python-mode)
    (let ((file-path (buffer-file-name)))
      (when file-path
        (if (projectile-project-p)
            (let ((default-directory (projectile-project-root)))
              (message "Formatting %s with Black..." (file-name-nondirectory file-path))
              (shell-command (format "pipenv run black %s" file-path))
              (revert-buffer t t t))
          (message "Not in a Projectile project"))))))

(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'ta42-python-format-with-black nil t)))

;;; begin terraform
(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'"
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;; Optional: Add company completion support for Terraform
(use-package company-terraform
  :ensure t
  :after (terraform-mode company)
  :config
  (company-terraform-init))
;;; end terraform


;;; end programming


;;; init.el ends here
