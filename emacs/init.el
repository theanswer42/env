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
(use-package magit
  :ensure t)

;;; end core editor setup

;;; text modes
;; markdown
(use-package markdown-mode
  :ensure t)

;; auto-fill-mode (limit to 80 cols)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Spell checking
;; Note: copied from Prelude
(defun ta42-enable-flyspell ()
  "Enable command `flyspell-mode'."
  (flyspell-mode +1))

(use-package flyspell
  :ensure t
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  :hook ((text-mode markdown-mode org-mode) . ta42-enable-flyspell))

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

;;; begin programming
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  )

;; pyenv
(use-package pyenv-mode
  :ensure t
  :hook (python-mode . pyenv-mode)
  :config
  ;; Automatically switch to the correct pyenv version based on .python-version file
  (pyenv-mode))

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :custom
  (pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))  ; or lsp-deferred

;; yaml
(use-package yaml-mode
  :ensure t)

;;; begin lsp things
(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-python-ms-python-executable-cmd "pipenv run python")
  :hook
  ((python-mode . lsp)))


;; Optional but recommended: lsp-ui for fancy sideline, docs, etc.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t))

;; Optional: for completion with lsp
;; (use-package company-lsp
;;   :ensure t
;;   :after (lsp-mode company)
;;   :config
;;   (push 'company-lsp company-backends))

;; ;; Optional: which-key integration (shows key binding hints)
;; (use-package which-key
;;   :ensure t
;;   :config
;;   (which-key-mode))

;; company for completion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.4
        company-minimum-prefix-length 4))

(use-package company-quickhelp
  :ensure t
  :config
  (add-hook 'company-mode-hook #'company-quickhelp-mode-enable-in-buffer))

;;; end lsp things

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
