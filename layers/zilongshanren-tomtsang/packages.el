                                        ; -*- lexical-binding: t -*-
;;; packages.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defconst zilongshanren-tomtsang-packages
  '(
    ;; (go :variables gofmt-command "goimports"
    ;;     go-use-gometalinter t
    ;;     go-tab-width 4)
    ;; go-mode
    ac-ispell
    command-log-mode
    easy-hugo
    ein
    elpy
    flycheck
    django-mode
    (org :location built-in)
    ox-hugo
    py-autopep8
    pyim-wbdict
    pyim-basedict
    pyim
    sdcv
    bug-hunter
    proxy-mode
    w3m
    ;; (sdcv-mode :location (recipe
    ;;                       :fetcher github
    ;;                       :repo "gucong/emacs-sdcv"))

    ;; vmd-mode
    ;; (ox-hugo :location (recipe :fetcher github :repo "kaushalmodi/ox-hugo"))
    ;; (zilongshanren-org :location built-in)
    ;; (blog-admin :location (recipe
    ;;                        :fetcher github
    ;;                        :repo "codefalling/blog-admin"))
    ;; org-tree-slide
    ;; ox-reveal
    ;; worf
    ;; org-download
    ;; plain-org-wiki
    ))

;; w3m start
(defun zilongshanren-tomtsang/init-w3m ()
  (use-package w3m
    :init
    )
  )

(defun zilongshanren-tomtsang/post-init-w3m ()

  ;; w3m post start
  ;; https://forum.suse.org.cn/t/emacs-w3m/2918

  ;; 使用 w3m 作为默认浏览器
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-view-this-url-new-session-in-background t)

  ;; 显示图标
  (setq w3m-show-graphic-icons-in-header-line t)
  (setq w3m-show-graphic-icons-in-mode-line t)

  ;; 这行代码是抄了一个比较老的 Emacs 配置的，貌似现在没有什么效果了
  ;;(setq w3m-view-this-url-new-session-in-background t)

  (add-hook 'w3m-fontify-after-hook 'remove-w3m-output-garbages)
  (defun remove-w3m-output-garbages ()
    " 去掉 w3m 输出的垃圾."
    (interactive)
    (let ((buffer-read-only))
      (setf (point) (point-min))
      (while (re-search-forward "\200-\240]" nil t)
        (replace-match " "))
      (set-buffer-multibyte t))
    (set-buffer-modified-p nil))

  ;; (setq w3m-search-default-engine "google")

  (eval-after-load "w3m-search" '(progn
    (add-to-list 'w3m-search-engine-alist '("baidu"
     "http://www.baidu.com/baidu?wd=%s" utf-8))
    (add-to-list 'w3m-search-engine-alist '("wz"
     "http://zh.wikipedia.org/wiki/Special:Search?search=%s" utf-8))
    (add-to-list 'w3m-search-engine-alist '("q"
     "http://www.google.com/search?hl=en&q=%s+site:stackoverflow.com" utf-8))
    (add-to-list 'w3m-search-engine-alist '("s"
     "http://code.google.com/codesearch?q=%s" utf-8))))

  ;; Proxy Gateway
  (setq w3m-command-arguments
    (nconc w3m-command-arguments
      '("-o" "http_proxy=http://127.0.0.1:8118/")))
  (setq w3m-no-proxy-domains '("local.com" "192.168.168.137" "127.0.0.1" "baidu.com" "qq.com"))
  ;; w3m post end
)
;; w3m end

;; proxy-mode
(defun zilongshanren-tomtsang/init-proxy-mode ()
  (use-package proxy-mode
    :init
    :config
    (setq url-gateway-local-host-regexp
          (concat "\\`" (regexp-opt '("localhost" "127.0.0.1")) "\\'"))
    )
  )

(defun zilongshanren-tomtsang/init-bug-hunter ()
  (use-package bug-hunter
    :init))

(defun zilongshanren-tomtsang/init-django-mode ()
  (use-package django-mode
    :init))
;; (defun zilongshanren-tomtsang/init-sdcv-mode ()
;;   (use-package sdcv-mode
;;     :demand t
;;     :config
;;     (add-hook 'sdcv-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

;;     (defun sdcv-search-at-point ()
;;       (interactive)
;;       (sdcv-search (ag/region-or-word-at-point-str) nil nil t))

;;     (spacemacs/set-leader-keys "xll" #'sdcv-search-at-point)

;;     (evil-define-key 'normal sdcv-mode-map "q" #'sdcv-return-from-sdcv)
;;     (evil-define-key 'normal sdcv-mode-map "n" #'sdcv-next-entry)
;;     (evil-define-key 'normal sdcv-mode-map "p" #'sdcv-previous-entry)
;;     (evil-define-key 'normal sdcv-mode-map (kbd "RET") #'sdcv-search-at-point)
;;     (evil-define-key 'normal sdcv-mode-map "a" #'sdcv-search-at-point)))

(defun zilongshanren-tomtsang/init-sdcv ()
  (use-package sdcv
    ;; :demand t
    :init
    :config

    (add-to-list 'load-path (expand-file-name "~/.spacemacs.d/layers/manaul-install/"))
    (require 'company-english-helper)
    (require 'insert-translated-name)

    (load-file "~/.spacemacs.d/layers/manaul-install/posframe.el")
    (load-file "~/.spacemacs.d/layers/manaul-install/sdcv.el")

    ;; (require 'sdcv)

    ;; (setq sdcv-dictionary-data-dir lazycat-emacs-sdcv-data-dir) ;设置星际译王本地词典的目录
    (setq sdcv-dictionary-data-dir "~/.stardict/dic")

    (setq sdcv-say-word-p t)	     ;say word after search
    (setq sdcv-dictionary-simple-list    ;星际译王屏幕取词词典, 简单, 快速
          '("懒虫简明英汉词典"
            "懒虫简明汉英词典"
            "KDic11万英汉词典"))
    (setq sdcv-dictionary-complete-list     ;星际译王的词典, 完全, 详细
          '(
            "懒虫简明英汉词典"
            "英汉汉英专业词典"
            "XDICT英汉辞典"
            "stardict1.3英汉辞典"
            "WordNet"
            "XDICT汉英辞典"
            "Jargon"
            "懒虫简明汉英词典"
            "FOLDOC"
            "新世纪英汉科技大词典"
            "KDic11万英汉词典"
            "朗道汉英字典5.0"
            "CDICT5英汉辞典"
            "新世纪汉英科技大词典"
            "牛津英汉双解美化版"
            "21世纪双语科技词典"
            "quick_eng-zh_CN"
            ))

    )
  )

(defun zilongshanren-tomtsang/init-pyim-wbdict ()
  (use-package pyim-wbdict
    :ensure nil
    :config (pyim-wbdict-v98-enable))
  :init)

(defun zilongshanren-tomtsang/init-pyim-basedict ()
  (use-package pyim-basedict
    :ensure nil)
    :init)

(defun zilongshanren-tomtsang/init-pyim()
  "Initialize pyim"
  (use-package pyim
    :ensure nil
    :demand t
    :init
    (setq pyim-punctuation-translate-p '(no yes auto))
    :config
    (setq default-input-method "pyim")
    (setq pyim-default-scheme 'wubi)

    ;; 让 Emacs 启动时自动加载 pyim 词库
    (add-hook 'emacs-startup-hook
              #'(lambda () (pyim-restart-1 t)))

    (setq pyim-page-tooltip 'posframe)
    (setq pyim-dicts '((:name "基础词库" :file "~/.spacemacs.d/layers/zilongshanren-tomtsang/utils/wbdict.pyim")))
    (global-set-key (kbd "C-9") 'toggle-input-method)
    ;; (global-set-key (kbd "C-,") 'toggle-input-method)
    ))

;; (defun zilongshanren-tomtsang/init-pyim ()
;;   (use-package pyim
;;     :ensure
;;     ;; :ensure nil
;;     :config
;; ;    ;; 激活 basedict 拼音词库
;;     (use-package pyim-basedict
;;       :ensure nil
;;       :config (pyim-basedict-enable))

;;     ;; 五笔用户使用 wbdict 词库
;;     (use-package pyim-wbdict
;;       :ensure nil
;;       :config (pyim-wbdict-gbk-enable))

;;    (setq default-input-method "pyim")

;; ;    ;; 我使用全拼
;;    (setq pyim-default-scheme 'quanpin)
;;     ;; (setq pyim-default-scheme 'wubi)

;;     ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;;     ;; 我自己使用的中英文动态切换规则是：
;;     ;; 1. 光标只有在注释里面时，才可以输入中文。
;;     ;; 2. 光标前是汉字字符时，才能输入中文。
;;     ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;;     (setq-default pyim-english-input-switch-functions
;;                   '(pyim-probe-dynamic-english
;;                     pyim-probe-isearch-mode
;;                     pyim-probe-program-mode
;;                     pyim-probe-org-structure-template))

;;     (setq-default pyim-punctuation-half-width-functions
;;                   '(pyim-probe-punctuation-line-beginning
;;                     pyim-probe-punctuation-after-punctuation))

;;     ;; 开启拼音搜索功能
;;     (pyim-isearch-mode 1)

;;     ;; 使用 pupup-el 来绘制选词框
;;     (setq pyim-page-tooltip 'popup)

;;     ;; 选词框显示5个候选词
;;     (setq pyim-page-length 5)

;;     ;; 让 Emacs 启动时自动加载 pyim 词库
;;     (add-hook 'emacs-startup-hook
;;               #'(lambda () (pyim-restart-1 t)))
;; ;    :bind
;; ;    (("M-j" . pyim-convert-code-at-point) ;;与 pyim-probe-dynamic-english 配合
;; ;     ("C-;" . pyim-delete-word-from-personal-buffer)))


;; ;让选词框跟随光标
;; ;用户可以通过下面的设置让 pyim 在光标处显示一个选词框：
;; ;
;; ;使用 popup 包来绘制选词框 （emacs overlay 机制）
;; ;(setq pyim-page-tooltip 'popup)
;; ;使用 pos-tip 包来绘制选词框（emacs tooltip 机制）
;; ;(setq pyim-page-tooltip 'pos-tip)
;; ;注：Linux 平台下，emacs 可以使用 GTK 来绘制选词框：
;; ;
;; ;(setq pyim-page-tooltip 'pos-tip)
;; ;(setq x-gtk-use-system-tooltips t)


;; ;调整 tooltip 选词框的显示样式
;; ;pyim 的 tooltip 选词框默认使用双行显示的样式，在一些特殊的情况下（比如：popup 显示的菜单错位），用户可以使用单行显示 的样式：
;; ;
;; ;(setq pyim-page-style 'one-line)
;;     ;; (global-set-key (kbd "C-\\") 'toggle-input-method)
;;     ;; (global-set-key (kbd "M-C-\\") 'my-cycle-input-method)
;;     :init))

(defun zilongshanren-tomtsang/init-ein ()
  (use-package ein
    :init))

(defun zilongshanren-tomtsang/init-py-autopep8 ()
  (use-package py-autopep8
    :init))

(defun zilongshanren-tomtsang/init-flycheck ()
  (use-package flycheck
    :init))


(defun zilongshanren-tomtsang/post-init-flycheck-mode ()
  (global-flycheck-mode t)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  )

(defun zilongshanren-tomtsang/init-elpy ()
  (use-package elpy
    :init
    (elpy-enable)
    ;; (elpy-use-ipython) ;; can't use, ref: https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt")
    ;; use flycheck not flymake with elpy
    (when (require 'flycheck nil t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode)
      (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))))

;; (defun zilongshanren-tomtsang/init-go ()
;;   (use-package go
;;     :init
;;     )
;;   )

;; (defun zilongshanren-tomtsang/init-go-mode ()
;;   (use-package go-mode
;;     :init
;;     )
;;   )




;; (defun zilongshanren-tomtsang/init-vmd-mode ()
;;   (use-package vmd-mode
;;     :init
;;     )
;;   )

(defun zilongshanren-tomtsang/post-init-vmd-mode ()
  (global-set-key (kbd "M-m p") 'vmd-mode) ;; make a global binding for that,
  (add-hook 'markdown-mode-hook 'vmd-mode) ;; or add a hook...
  )


(defun zilongshanren-tomtsang/pre-init-org ()
  (setq org-enable-hugo-support t) ; 这个参数，放在 post-init-org 下，是失效的。
  )

(defun zilongshanren-tomtsang/init-ox-hugo ()
  ;; ox-hugo config
  (use-package ox-hugo
    :ensure t                    ;Auto-install the package from Melpa (optional)
    :after ox
    :init
    )

  )

(defun zilongshanren-tomtsang/init-ox-hugo-auto-export ()
  (use-package ox-hugo-auto-export
    :init) ;If you want the auto-exporting on file saves
  )

;; (defun zilongshanren-tomtsang/init-easy-hugo ()
;;   (use-package easy-hugo
;;     :init
;;     (setq easy-hugo-basedir "~/bitbucket/qqbb/tomtsang-rootsongjc-hugo/content/posts/")
;;     ;; (setq easy-hugo-postdir-list ("content/post/" "content/posts/"))
;;     (setq easy-hugo-postdir "content/posts")
;;     (setq easy-hugo-select-postdir t)
;;     ;; (setq easy-hugo-select-postdir "~/bitbucket/qqbb/tomtsang-rootsongjc-hugo/content/posts/")
;;     (setq easy-hugo-url "https://b.qqbb.app")
;;     ;; (setq easy-hugo-sshdomain "blogdomain")
;;     (setq easy-hugo-root "/home/tomtsang/")
;;     (setq easy-hugo-previewtime "300")
;;     (setq easy-hugo-default-ext ".org")
;;     :bind ("C-c C-e" . easy-hugo)
;;     ))

;; (defun zilongshanren-tomtsang/easy-hugo ()
;;   ;; (defun zilongshanren-tomtsang/post-init-easy-hugo ()
;;   (interactive)
;;   (evil-define-key
;;     (list 'normal 'insert 'visual 'motion)
;;     easy-hugo-mode-map
;;     "n" 'easy-hugo-newpost
;;     "D" 'easy-hugo-article
;;     "p" 'easy-hugo-preview
;;     "P" 'easy-hugo-publish
;;     "o" 'easy-hugo-open
;;     "d" 'easy-hugo-delete
;;     "e" 'easy-hugo-open
;;     "c" 'easy-hugo-open-config
;;     "f" 'easy-hugo-open
;;     "N" 'easy-hugo-no-help
;;     "v" 'easy-hugo-view
;;     "r" 'easy-hugo-refresh
;;     "g" 'easy-hugo-refresh
;;     "s" 'easy-hugo-sort-time
;;     "S" 'easy-hugo-sort-char
;;     "G" 'easy-hugo-github-deploy
;;     "A" 'easy-hugo-amazon-s3-deploy
;;     "C" 'easy-hugo-google-cloud-storage-deploy
;;     "q" 'evil-delete-buffer
;;     (kbd "TAB") 'easy-hugo-open
;;     (kbd "RET") 'easy-hugo-preview)
;;   ;; (define-key global-map (kbd "C-c C-e") 'easy/hugo)
;;   )

(defun zilongshanren-tomtsang/init-easy-hugo ()
  (use-package easy-hugo
    ;; :after (helm-ag)
    :ensure t
    :config
    (setq easy-hugo-basedir "~/bitbucket/qqbb/tomtsang-rootsongjc-hugo/"
          easy-hugo-postdir "content/post"
          easy-hugo-url "https://b.qqbb.app"
          easy-hugo-preview-url "http://127.0.0.1:1313/"
          ;; easy-hugo-sshdomain "me"
          easy-hugo-root "/Users/tomtsang"
          easy-hugo-previewtime "20"
          easy-hugo-default-ext ".org"
          )
    ;; (add-hook 'easy-hugo-mode-hook 'zilongshanren-tomtsang/easy-hugo)
    :init
    :bind ("C-c e" . easy-hugo)
    ))

(defun zilongshanren-tomtsang/init-ac-ispell ()
  (use-package ac-ispell
    :init
    )
  )

(defun zilongshanren-tomtsang/init-command-log-mode ()
  (use-package command-log-mode
    :init
    )
  )

(defun zilongshanren-tomtsang/post-init-command-log ()
  (with-eval-after-load 'global-command-log-mode
    (setq open-command-log-buffer t)
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line
                                                evil-previous-visual-line)))))

(defun zilongshanren-tomtsang/post-init-ac-ispell ()
  (with-eval-after-load 'global-ac-ispell
    (add-hook 'org-mode 'ac-ispell-ac-setup)
    )
  )

(defun zilongshanren-tomtsang/post-init-org ()
  (add-hook 'org-mode-hook (lambda () (spacemacs/toggle-line-numbers-off)) 'append)
  (with-eval-after-load 'org
    (progn
      (setq org-todo-keywords
            (quote ((sequence "TODO(t!)" "STARTED(s!)" "|" "DONE(d!/!)")
                    (sequence  "NEXT(n!)" "SOMEDAY(S)" "WAITING(w@/!)"  "|" "CANCELLED(c@/!)" "ABORT(a@/!)" "PHONE(p)"))))
      ;; (quote ((sequence "TODO(t!)" "STARTED(s)" "NEXT(n!)" "WAITING(w@/!)" "SOMEDAY(S)" "|" "DONE(d!/!)" "CANCELLED(c@/!)" "ABORT(a@/!)"))))

      ;; define the refile targets
      (setq org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir))
      (setq org-agenda-file-task (expand-file-name "task.org" org-agenda-dir))
      (setq org-agenda-file-finished (expand-file-name "finished.org" org-agenda-dir))
      (setq org-agenda-file-trash (expand-file-name "trash.org" org-agenda-dir))
      (setq org-agenda-file-project (expand-file-name "project.org" org-agenda-dir))
      (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
      (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
      (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
      (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
      ;; (setq org-agenda-files (list org-agenda-dir))

      ;; https://github.com/15cm/spacemacs-config/blob/master/layers/my-writing/packages.el
      ;; agenda files for tag search
      (
       ;; let ((notes-dir "~/org-notes"))
       let ((notes-dir "~/org"))
       (if (file-exists-p notes-dir)
           (progn
             (load-library "find-lisp")
             (setq org-agenda-files (find-lisp-find-files "~/org" "\.org$")
                   ;; (setq org-agenda-files (find-lisp-find-files "~/org-notes" "\.org$")
                   )
             )))


      (with-eval-after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
        (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
          "." 'spacemacs/org-agenda-transient-state/body)
        )
      ;; the %i would copy the selected text into the template
      ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;;add multi-file journal
      (setq org-capture-templates
            '(("i" "Todo" entry (file+headline org-agenda-file-inbox "Ideas")
               "* TODO [#B] %?\n  %i\n"
               :empty-lines 1)
              ("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
               "* TODO [#B] %?\n  %i\n"
               :empty-lines 1)
              ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
               "* %?\n  %i\n %U"
               :empty-lines 1)
              ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
               :empty-lines 1)
              ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n  %i\n %a \n %U"
               :empty-lines 1)
              ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
               "* TODO [#B] %?\n  %i\n %U"
               :empty-lines 1)
              ("s" "Code Snippet" entry
               (file org-agenda-file-code-snippet)
               "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
              ("w" "work" entry (file+headline org-agenda-file-gtd "WORK")
               "* TODO [#A] %?\n  %i\n %U"
               :empty-lines 1)
              ("j" "Journal Entry"
               entry (file+datetree org-agenda-file-journal)
               "* %?"
               :empty-lines 1)))
      ;;An entry without a cookie is treated just like priority ' B '.
      ;;So when create new task, they are default 重要且紧急
      (setq org-agenda-custom-commands
            '(
              ("w" . "任务安排")
              ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
              ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
              ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
              ("b" "Blog" tags-todo "BLOG")
              ("e" "Emacs" tags-todo "emacs")
              ("g" . "golang")
              ("gl" "Golang learn" tags-todo "golang")
              ("k" . "kubernetes")
              ("kl" "k8" tags-todo "k8s")
              ("d" "docker" tags-todo "docker")
              ("p" . "项目安排")
              ("pf" tags-todo "PROJECT+WORK+CATEGORY=\"future\"")
              ("pe" tags-todo "PROJECT=\"emacs\"")
              ("W" "Weekly Review"
               ((stuck "") ;; review stuck projects as designated by org-stuck-projects
                (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                ))))


      ;; Of course I’m using a handy capture template, as provided by the ox-hugo docs.
      ;; https://www.baty.net/2018/lets-try-using-ox-hugo-again/
      ;; This lets me type C-c c h to quickly create a new draft post.
      (with-eval-after-load 'org-capture
        (defun org-hugo-new-subtree-post-capture-template ()
          "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
          (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
                 (fname (org-hugo-slug title)))
            (mapconcat #'identity
                       `(
                         ,(concat "* TODO " title)
                         ":PROPERTIES:"
                         ,(concat ":EXPORT_FILE_NAME: " fname)
                         ":END:"
                         "%?\n")        ;Place the cursor here finally
                       "\n")))

        (add-to-list 'org-capture-templates
                     '("h"              ;`org-capture' binding + h
                       "Hugo post"
                       entry
                       ;; It is assumed that below file is present in `org-directory'
                       ;; and that it has a "Blog Ideas" heading. It can even be a
                       ;; symlink pointing to the actual location of all-posts.org!
                       (file+olp "all-posts.org" "Blog Ideas")
                       (function org-hugo-new-subtree-post-capture-template))))

      ;; (with-eval-after-load 'ox-hugo
      ;;   ()
      )))
