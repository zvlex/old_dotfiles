;; MELPA
(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Hide menu and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(scroll-bar-mode -1)
(setq-default frame-title-format '(buffer-file-name "%b"))

;; Disable extra files
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving
;;
(global-auto-revert-mode t)
(setq auto-revert-verbose nil)

(global-set-key (kbd "<f5>") 'revert-buffer)

;; turn bell off
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
          (ding))
        ))
(setq ring-bell-function 'ignore)

;;(defalias 'yas/get-snippet-tables 'yas--get-snippet-tables)
;;(defalias 'yas/table-hash 'yas--table-hash)
;; (require 'whitespace-cleanup-mode)
;; (global-whitespace-cleanup-mode)
;; Auto Complete Mode
(add-to-list 'load-path "~/.emacs.d/lisp")    ; This may not be appeared if you have already added.
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20150408.1132/dict")
(ac-config-default)

;; AutoPairs
(require 'autopair)
(autopair-global-mode)

;; http://code.google.com/p/dea/source/browse/trunk/my-lisps/linum%2B.el
(require 'linum+)
(setq linum-format "%d")
(global-linum-mode 1)

;; built-in
;; Turn on auto complete directory
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'grizzl)
;; define a search index
(setq projectile-completion-system 'grizzl)

(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#333333")

 (add-hook 'ruby-mode-hook
           (lambda () (highlight-indentation-current-column-mode)))

 (add-hook 'coffee-mode-hook
           (lambda () (highlight-indentation-current-column-mode)))

(setq ruby-insert-encoding-magic-comment nil)

;; Robe
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; Project manajer
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
(setq speedbar-use-images nil)
(setq speedbar-show-unknown-files 1)
(setq sr-speedbar-right-side nil)
(add-hook 'speedbar-mode-hook '(lambda () (linum-mode 0)))

;; (require 'neotree)
;; (global-set-key [f12] 'neotree-toggle)
;; (setq projectile-switch-project-action 'neotree-projectile-action)

;; Lisp REPL
(require 'slime)
(slime-setup '(slime-fancy))
(setq slime-lisp-implementations
      '((clisp ("/usr/bin/clisp" "-q -I"))
        (sbcl  ("/usr/local/bin/sbcl") :coding-system utf-8-unix)))

;; Ruby
(require 'inf-ruby)

;; Yasnippet
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20141223.303")
(require 'yasnippet)
(yas-global-mode 1)

;; Rinari
(add-to-list 'load-path "~/.emacs.d/elpa/rinari")
(require 'rinari)
(global-rinari-mode)

;; Rspec
;;
(add-to-list 'load-path "/path/to/rspec-mode")
(require 'rspec-mode)
(eval-after-load 'rspec-mode
 '(rspec-install-snippets))

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

;; RVM
(require 'rvm)
(rvm-use-default)

;;
(require 'ruby-hash-syntax)

;; Projectile
(require 'projectile)
(projectile-global-mode)

;; Hypertext

;; HAML
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$'" . haml-mode))

;; SASS
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.scss$'" . sass-mode))

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; web-mode
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; indents
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(setq web-mode-extra-snippets '(("erb" . (("name" . ("beg" . "end"))))
                                ))
(setq web-mode-extra-auto-pairs '(("erb" . (("open" "close")))
                                  ))
;; highlights paired brackets
(show-paren-mode t)

;; highlight current item
(setq web-mode-enable-current-element-highlight t)

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; JavaScript
(require 'json-mode)

(require 'js2-mode)

(require 'ac-js2)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; Comment
(require 'comment-dwim-2)
(global-set-key (kbd "M-/") 'comment-dwim-2)

;; Git Gutter
(require 'git-gutter-fringe)
(global-git-gutter-mode t)
(setq git-gutter-fr:side 'left-fringe)
(add-hook 'git-gutter:update-hooks 'magit-revert-buffer-hook)

;; Git Gutter Customize
(set-face-foreground 'git-gutter-fr:modified "yellow")
(set-face-foreground 'git-gutter-fr:added    "green")
(set-face-foreground 'git-gutter-fr:deleted  "red")

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(server-start t)

;; Org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;-----------------------
;; Emacs appearance setup
;;-----------------------

(setq display-time t ; display day and date
      display-time-24hr-format t ; use 24hr format
      display-time-interval 55 ; redisplay every 55 seconds
      display-time-default-load-average nil) ; don't display the system load average
(display-time)

(setq-default indent-tabs-mode nil)
(setq tab-width 2
      c-default-style "stroustrup"
      js-indent-level 2
      css-indent-offset 2)

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq tab-width 2)
            ))

;; Color Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'gruvbox t)

;; Font ;; Monaco-10 ;;
(add-to-list 'default-frame-alist '(font . "Monaco-10"))
(set-default-font "Monaco-10")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
