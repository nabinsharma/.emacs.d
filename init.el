;;;; Nabin's .emacs file.
; Nabin Sharma
; Dec 10, 2012

;;;; For backward compatibility.
(require 'cl)

;;;; Package manager.
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;;; Default directory (home).
(setq default-directory "~")

;;;; Theme.
(load-theme 'hickey t)

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
;; Google C style when not in work and indentation of 3
;; when in work.
(if (eq system-type 'windows-nt)
    (setq c-default-style "linux" c-basic-offset 3)
  ((require 'google-c-style)
   (add-hook 'c-mode-common-hook 'google-set-c-style)))

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
            (auto-fill-mode 0)
            (if (eq window-system 'x)
                (font-lock-mode 1))))
(setq octave-comment-start "%")
(setq octave-continuation-string "...")
(setq octave-block-offset 3)


;;;; YAML mode.
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Unlike python-mode, this mode follows the Emacs convention of not
;; binding the ENTER key to newline-and-indent.  To get this
;; behavior, add the key definition to yaml-mode-hook:
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;; Perl mode.
(require 'perl-mode)
(setq perl-indent-level 3)
(add-to-list 'auto-mode-alist '("\\.pl$" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.tune$" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.symfun$" . perl-mode))


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
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;;; Tags.
(require 'etags-select)
(global-set-key (kbd "M-.")'etags-select-find-tag)
(global-set-key (kbd "M-?")'etags-select-find-tag-at-point)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "1a85b8ade3d7cf76897b338ff3b20409cb5a5fbed4e45c6f38c98eee7b025ad4" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
