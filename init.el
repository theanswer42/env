(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; melpa setup
;; At some point, I had to do this to fix an error while
;; fetching packages. I'm not sure if I need this now.
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
;; TODO: I don't what to do this every time.
;; Is there a good workaround?
;; (package-refresh-contents)

;; copied from prelude
(defun require-package (package)
  "Install PACKAGE unless already installed."
  (unless (package-installed-p package)
    (package-install package)))

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
(require-package 'zenburn-theme)
(load-theme 'zenburn)

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

;;; end core editor setup

;;; text modes
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

;; also copied from prelude
(defun ta42-enable-flyspell ()
  "Enable command `flyspell-mode'."
  (flyspell-mode +1))


(add-hook 'markdown-mode-hook 'ta42-enable-flyspell)
(add-hook 'text-mode-hook 'ta42-enable-flyspell)
(add-hook 'org-mode-hook 'ta42-enable-flyspell)
;;; end text modes

;;; programming
;; javascript, typescript
(require-package 'js2-mode)
(require-package 'json-mode)
(require-package 'tide)

(require 'typescript-mode)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'"    . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(defun ta42-js-indent-hook ()
  "Set javascript basic offset."
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'ta42-js-indent-hook)

(defun ta42-ts-indent-hook ()
  "Set javascript basic offset."
  (setq typescript-indent-level 2))
(add-hook 'typescript-mode-hook 'ta42-ts-indent-hook)

;; web-mode (html and templates)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))

(defun ta42-set-web-mode-indents ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'ta42-set-web-mode-indents)

;; ruby, rails
(add-to-list 'auto-mode-alist '("\\.rake$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$"      . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile"    . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile"     . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile"     . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
;; I used to have this. Do I need it?
;; (setq ruby-insert-encoding-magic-comment nil)

;; python - pyenv
(require 'pyenv-mode)

;; golang customizations
(add-hook 'before-save-hook 'gofmt-before-save)
(require 'lsp-mode)

(setq lsp-keymap-prefix "C-l")

(add-hook 'go-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)

;; flycheck
(global-flycheck-mode)

;; yamllint does not get selected as the default
;; linter for yaml-mode. Lets make it so.
(defun ta42-yaml-mode-linter-hook ()
  "Add yamllint as a linter for yaml-mode."
  (flycheck-select-checker 'yaml-yamllint)
  )
(add-hook 'yaml-mode-hook 'ta42-yaml-mode-linter-hook)
;; Taken from prelude --v
(add-hook 'yaml-mode-hook 'whitespace-mode)
(add-hook 'yaml-mode-hook 'subword-mode)

;;; end programming


; turn on ido-mode
(ido-mode t)
(setq ido-use-vertual-buffers t)

; clean up indents over regions and buffers
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;org-mode customizations
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

; make org-mode the default text mode
(setq default-major-mode 'org-mode)

; magit - enable global magit file mode
(global-magit-file-mode)

