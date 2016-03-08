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
(add-hook 'LaTeX-mode-hook 
          (lambda () 
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
            (setq TeX-auto-untabify t     ; remove all tabs before saving 
                  TeX-engine 'xetex       ; use xelatex default 
                  TeX-show-compilation t ; display compilation windows 
                  TeX-command-default "XeLaTeX")
            (TeX-global-PDF-mode t)       ; PDF mode enable, not plain 
            (setq TeX-save-query nil) 
            (imenu-add-menubar-index) 
            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol))) 


;;============================
;;=              Package Repositories
;;============================
(require 'package)
;;(add-to-list 'package-archives
;;	     '("melpa" . "http://melpa.org/packages/")
;;	     t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;;=============================
;;=       Configure for Fonts
;;=============================
(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
	  nil t))

(defvar font-list '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))
(require 'cl) ;; find-if is in common list package
(find-if #'qiang-font-existsp font-list)

(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
		   (equal ":" (string (elt font-size 0))))
	  (format "%s%s" font-name font-size)
	(format "%s %s" font-name font-size)))

(defun qiang-set-font (english-fonts
					   english-font-size
					   chinese-fonts
					   &optional chinese-font-size)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl)						 ; for find if
  (let ((en-font (qiang-make-font-string
				  (find-if #'qiang-font-existsp english-fonts)
				  english-font-size))
		(zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
							:size chinese-font-size)))
 
	;; Set the default English font
	;;
	;; The following 2 method cannot make the font settig work in new frames.
	;; (set-default-font "Consolas:pixelsize=18")
	;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
	;; We have to use set-face-attribute
	(message "Set English Font to %s" en-font)
	(set-face-attribute
	 'default nil :font en-font)
 
	;; Set Chinese font
	;; Do not use 'unicode charset, it will cause the english font setting invalid
	(message "Set Chinese Font to %s" zh-font)
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
						charset
						zh-font))))

(qiang-set-font
 '("Consolas" "Monaco" "DejaVu Sans Mono" "Monospace" "Courier New") ":pixelsize=11"
 '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))
