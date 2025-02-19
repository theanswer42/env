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

;;; begin programming

;;; begin lsp things
(use-package company-mode)


;;; end lsp things


;;; end programming


;;; init.el ends here
