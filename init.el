;;;; Nabin's .emacs file.
; Nabin Sharma
; Dec 10, 2012

;;;; Default directory (home).
(setq default-directory "~")


;;;; Load required path and start server.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
(server-start)

;;;; Turn on ido mode.
(require 'ido)
(ido-mode t)

;;;; Cygwin.
;; Learn Cygwin styles and mount points.
(if (eq system-type 'windows-nt)
    (require 'cygwin-mount))

;;;; Key bindings.
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-O") '(lambda ()(interactive)(other-window -1)))
(global-set-key (kbd "M-o") 'other-window)

;;;; Auto load changes in disk.
(global-auto-revert-mode t)

;;;; Key remappings.
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)

;;;; Appearence.
;; Font.
(set-face-attribute 'default nil :font "Inconsolata Medium")
;; Default indentation of 2 spaces.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode)(tool-bar-mode -1))
;; Turn off blinking curser.
(blink-cursor-mode 0)
;; Add column numbering to the status bar.
(column-number-mode 1)
;; Single line scrolling.
(setq scroll-step 1)
(setq transient-mark-mode t)
;; Default to line wrapping.
(setq truncate-partial-width-windows nil)
;; Use a visible instead of a beep when a error occurs.
(setq visible-bell t)
;; Remove scrollbar.
(if (fboundp 'scroll-bar-mode)(scroll-bar-mode -1))

;;;; Cygwin shell
(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
    (call-interactively 'shell)))

;;;; Grin.
(require 'grin)

;;;; C/C++.
;; File association.
(add-to-list 'auto-mode-alist '("\\.cxx$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.$hpp" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.idl$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp$" . c++-mode))
;; Google C style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;;;; Python.
;; Default indentation: 4 for home (Mac) and we use 2 in work.
(add-hook 'python-mode-hook
          (lambda () 
            ((if (eq system-type "darwin")
                 (setq python-indent 4)
               (setq python-indent 2)))))

;;;; Protobuf mode.
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

;;;; Octave mode.
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))
(setq octave-comment-start "%")


;;;; YAML mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Unlike python-mode, this mode follows the Emacs convention of not
;; binding the ENTER key to newline-and-indent.  To get this
;; behavior, add the key definition to yaml-mode-hook:
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;; Backups.
(setq
 backup-by-copying t ; dont clobber symlinks
 backup-directory-alist
 '(("."."~/.emacs-backups"))
 delete-old-versions t
 kept-new-versions 2
 kept-old-versions 2
 version-control t)

;;;; Org mode.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;;;; Tags.
(require 'etags-select)
(global-set-key (kbd "M-.")'etags-select-find-tag)
(global-set-key (kbd "M-?")'etags-select-find-tag-at-point)
