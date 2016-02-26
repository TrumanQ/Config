;;=========================
;;=       Configure for Theme
;;=========================
(require 'color-theme)
(color-theme-initialize)
(color-theme-vim-colors)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9db09837597dacb8e6f38eae5df8f39fd28c98420a0b1626da1954b7a423d8b5" default)))
 '(org-agenda-files
   (quote
    ("~/workspace/TEX/Org/agenda1" "~/workspace/TEX/Org/calendar"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;================================
;;= Configure for Line number column leftside
;;================================
(require 'linum)
(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode 1)

;;==========================
;;=            Configure for Git
;;= here, I used magit package
;;==========================
;;(require 'git)
(autoload 'magit-status "magit" nil t)

;;==========================
;;=    Configure of Tramp for SSH
;;==========================
(setq tramp-default-method "ssh")

;;==========================
;;=    Configure to umcompress
;;==========================
(auto-compression-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add for Org-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(add-hook 'org-mode-hook (lambda() (setq truncate-lines nil))) ;; for truncate-line auto


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for w3m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load-file "~/.emacs.d/lisp/web-mode.el")
(require 'web-mode)
(require 'w3m)
(require 'mime-w3m)
;(require 'org-w3m)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

;; 设置w3m主页
(setq w3m-home-page "http://www.baidu.com")
;; 默认显示图片
(setq w3m-default-display-inline-images t)
(setq w3m-default-toggle-inline-images t)
;; 使用cookies
(setq w3m-use-cookies t)
;;设定w3m运行的参数，分别为使用cookie和使用框架  
(setq w3m-command-arguments '("-cookie" "-F"))               
;; 使用w3m作为默认浏览器
(setq browse-url-browser-function 'w3m-browse-url)                
(setq w3m-view-this-url-new-session-in-background t)
;;显示图标                                                      
(setq w3m-show-graphic-icons-in-header-line t)                  
(setq w3m-show-graphic-icons-in-mode-line t) 


;;=============================
;;=               Configure for AucTex
;;=       espacially for XeLaTeX compile
;;=============================
;(add-hook 'LaTeX-mode-hook 
;          (lambda () 
;            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
;            (setq TeX-auto-untabify t     ; remove all tabs before saving 
;                  TeX-engine 'xetex       ; use xelatex default 
;                  TeX-show-compilation t ; display compilation windows 
;                  TeX-command-default "XeLaTeX")
;            (TeX-global-PDF-mode t)       ; PDF mode enable, not plain 
;            (setq TeX-save-query nil) 
;            (imenu-add-menubar-index) 
;            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol))) 
