(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

; melpa
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

; turn off the menubar and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

; hide welcome screen
(setq inhibit-startup-message t)

; indents
(setq tab-width 4)
(setq c-basic-offset 4)
(setq indent-tabs-mode nil)

; font - set default font to droid sans mono
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono-11"))

; make zenburn the default: 
(load-theme 'zenburn)

; indicate empty lines
(setq-default indicate-empty-lines t)

; Some shortcuts: 
(global-set-key (kbd "RET") 'newline-and-indent)

; echo's and bells
(setq visible-bell t)

; always highlight parenthesis
(show-paren-mode t)

; turn on column number
(setq column-number-mode t)

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


; ruby customizations
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))

; rvm - not sure what this is for
; (rvm-use-default)

; yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

; default extensions for markdown-mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))

;org-mode customizations
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

; make org-mode the default text mode
(setq default-major-mode 'org-mode)

