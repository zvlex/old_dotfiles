;; MELPA
(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Hide menu and tool bar
(menu-bar-mode 1)
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

(global-set-key (kbd "<f6>") 'server-force-delete)
(global-set-key (kbd "<f9>") 'server-start)

(global-set-key (kbd "C-`") 'term)

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f10>") 'ruby-send-region)

;;
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)


;; turn bell off
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
          (ding))
        ))
(setq ring-bell-function 'ignore)

(load "server")
(unless (server-running-p) (server-start))

;;(defalias 'yas/get-snippet-tables 'yas--get-snippet-tables)
;;(defalias 'yas/table-hash 'yas--table-hash)
(require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Auto Complete Mode
(add-to-list 'load-path "~/.emacs.d/lisp")    ; This may not be appeared if you have already added.
(require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20150618.1949/dict")
(ac-config-default)

(global-auto-complete-mode t)

;; (add-to-list 'ac-modes 'web-mode)

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
;; (add-hook 'ruby-mode-hook 'robe-mode)
;; (add-hook 'robe-mode-hook 'ac-robe-setup)

;; Project manajer
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
(setq speedbar-use-images nil)
(setq sr-speedbar-auto-refresh t)
(setq speedbar-show-unknown-files 1)
(setq sr-speedbar-right-side nil)
(add-hook 'speedbar-mode-hook '(lambda () (linum-mode 0)))


;; Lisp REPL
(require 'slime)
(slime-setup '(slime-fancy))
(setq slime-lisp-implementations
      '((clisp ("/usr/bin/clisp" "-q -I"))
        (sbcl  ("/usr/local/bin/sbcl") :coding-system utf-8-unix)))

;; Ruby
(require 'inf-ruby)

;; Yasnippet
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20151126.518")
(require 'yasnippet)
(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

;; Rinari
(add-to-list 'load-path "~/.emacs.d/elpa/rinari")
(require 'rinari)
(global-rinari-mode)

;; bindind.pry and press C-x C-q
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

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
;;(require 'rvm)
;;(rvm-use-default)


(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;
(require 'ruby-hash-syntax)
;; Projectile
(require 'projectile)
(projectile-global-mode)

;; Hypertext

;; web-mode
(require 'web-mode)
(require 'coffee-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("5d1434865473463d79ee0523c1ae60ecb731ab8d134a2e6f25c17a2b497dd459" "dc261a3614777e08f8c8f70d7cf63215786eafeedae45fc7f062c614eabf584c" "2916d16e583c17bb2a1a9d231ea8ddcb3577f8cb97179eea689e91036213ff03" "fa11ec1dbeb7c54ab1a7e2798a9a0afa1fc45a7b90100774d7b47d521be6bfcf" "7dd0db710296c4cec57c39068bfffa63861bf919fb6be1971012ca42346a417f" "94146ac747852749e9444b184eb1e958f0e546072f66743929a05c3af62de473" "0542fbd0a5a636ff3000d77c5ddf5da6c375976c921efb83960f425e4b399a99" "7b4d9b8a6ada8e24ac9eecd057093b0572d7008dbd912328231d0cada776065a" "4c028a90479b9ad4cbb26ae7dc306dded07718749fe7e4159621a8aebac40213" "bac3f5378bc938e96315059cd0488d6ef7a365bae73dac2ff6698960df90552d" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" default)))
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(linum-format "%d"))
;; HAML
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml" . haml-mode))

;; SASS
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass" . sass-mode))

(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))

;; с какими файлами ассоциировать web-mode
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; настройка отступов
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; сниппеты и автозакрытие парных скобок
(setq web-mode-extra-snippets '(("erb" . (("name" . ("beg" . "end"))))
                                ))
(setq web-mode-extra-auto-pairs '(("erb" . (("open" "close")))
                                  ))
;; подсвечивает парные скобки
(show-paren-mode t)

;; подсвечивать текущий элемент
(setq web-mode-enable-current-element-highlight t)

(add-to-list 'auto-mode-alist '("\\.jsx" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; JavaScript
;;(require 'react-snippets)
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

;; Org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Gnus
(setq gnus-select-method
      '(nntp "localhost"))

 (setq gnus-secondary-select-methods '((nnml ""))
       mail-sources '((pop :server "pop.yandex.ru"
                           :user "zvlexnum"
                           :port 995)))

;;-----------------------
;; Emacs appearance setup ( вынести в отдельный файл )
;;-----------------------

(setq display-time t ; display day and date
      display-time-24hr-format t ; use 24hr format
      display-time-interval 55 ; redisplay every 55 seconds
      display-time-default-load-average nil) ; don't display the system load average
(display-time)
;;(display-battery-mode 1)

(setq-default indent-tabs-mode nil)
(setq tab-width 2
      c-default-style "stroustrup"
      js-indent-level 2
      css-indent-offset 2)

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq tab-width 2)
            ))
(require 'go-mode)

;; Color Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'darcula t)
;;(load-theme 'darkburn t) ;;
;;(load-theme 'colorsarenice-dark t) ;; +
;;(load-theme 'warm-night t)
;;(load-theme 'base16-railscasts t)
;;(load-theme 'firecode t)
;;(load-theme 'gruvbox t)
;;(load-theme 'stekene-dark)
;;(load-theme 'sunburst)
(load-theme 'darktooth t)

;; Font ;; MesloLGS-10.9;; Monaco-10;;
;; Source Code Pro-10
;; Hack-10
;; Linux Libertine Mono-10
;; Oxygen Mono
(add-to-list 'default-frame-alist '(font . "Luxi Mono-10"))
;; (set-default-font "CPMono_v07-10")

;; (set-face-attribute 'default nil :family "CPMono_v07" :height 100)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :family "CPMono_v07"))))
 '(mode-line ((t (:family "CPMono_v07" :background "gray13" :foreground "goldenrod4" :box (:line-width 1 :color "gray7") :weight light))))
 '(mode-line-buffer-id ((t (:foreground "brown" :weight bold))))
 '(mode-line-emphasis ((t (:weight bold :family "CPMono_v07"))))
 '(mode-line-highlight ((t (:foreground "brown" :box nil :weight bold))))
 '(mode-line-inactive ((t (:background "gray13" :foreground "#A89984" :box (:line-width 1 :style pressed-button) :weight light :family "CPMono_v07"))))
 '(speedbar-button-face ((t (:foreground "DarkOrange2"))))
 '(speedbar-directory-face ((t (:foreground "SteelBlue3"))))
 '(speedbar-faces ((t (:family "CPMono_v07" :weight bold))))
 '(speedbar-file-face ((t (:foreground "gray75"))))
 '(speedbar-highlight-face ((t (:background "SlateGray4"))))
 '(speedbar-selected-face ((t (:foreground "red green" :underline t)))))

(make-face 'speedbar-face)
(set-face-font 'speedbar-face "CPMono_v07-10")
(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))

;; Dash
(setq helm-dash-browser-func 'eww)
(setq helm-dash-common-docsets '("Ruby"))

;; (setq helm-dash-common-docsets '("JavaScript"))

(require 'crystal-mode)
(add-to-list 'auto-mode-alist '("\\.cr'" . crystal-mode))

(require 'elixir-mode)
(add-to-list 'auto-mode-alist '("\\.exs'" . elixir-mode))


(require 'rainbow-mode)
(dolist (hook '(css-mode-hook sass-mode-hook scss-mode-hook))
  (add-hook hook 'rainbow-mode))

;; PostgreSQL
(setq sql-postgres-login-params
      '((user :default "zvlex")
        (database :default "zvample_developmnet")
        (password x)
        (server :default "localhost")
        (port :default 5432)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(setq sql-connection-alist
      '((zvample-dev (sql-product 'postgres)
                  (sql-port 5432)
                  (sql-server "localhost")
                  (sql-user "zvlex")
                  (sql-password "")
                  (sql-database "zvample_development"))
        (testdb (sql-product 'postgres)
                  (sql-port 5432)
                  (sql-server "localhost")
                  (sql-user "zvlex")
                  (sql-password "")
                  (sql-database "testdb"))))

;; Customized from sql.el source
(defun sql-read-connection (prompt)
  "Read a connection name."
  (let ((completion-ignore-case t))
    (completing-read prompt
                     (mapcar #'(lambda (c) (car c))
                             sql-connection-alist))))

(defun pg-connect (connection)
  (interactive
   (if sql-connection-alist
       (list (sql-read-connection "Connection: "))
     (user-error "No SQL Connections defined")))

  (setq sql-product 'postgres)
  (sql-connect connection "PostgreSQL"))

(add-hook 'sql-mode-hook
          (lambda ()
            (setq-local ac-ignore-case t)
            (auto-complete-mode)))

;; Rbenv config
(require 'rbenv)
(global-rbenv-mode)

(require 'workgroups2)
(setq wg-session-file "~/.emacs.d/.emacs_workgroups")
(workgroups-mode 1)
