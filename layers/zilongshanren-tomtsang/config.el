;;; config.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(define-abbrev-table 'global-abbrev-table
  '(

    ;; start tomtsang
    ;; hello world
    ("zhw" "hello world")
    ;; kubernetes
    ("zk8s" "kubernetes")
    ;; end tomtsang

    ;; start xah lee part 1
    ;; http://ergoemacs.org/emacs/emacs_abbrev_mode.html

    ;; net abbrev
    ("afaik" "as far as i know" )
    ("atm" "at the moment" )
    ("dfb" "difference between" )
    ("ty" "thank you" )
    ("ui" "user interface" )
    ("uns" "understand" )
    ("ur" "you are" )
    ("btw" "by the way" )

    ("cnt" "can't" )
    ("ddnt" "didn't" )
    ("dnt" "don't" )

    ;; english word abbrev
    ("ann" "announcement" )
    ("arg" "argument" )
    ("autom" "automatic" )
    ("bc" "because" )
    ("bg" "background" )
    ("bt" "between" )
    ("math" "mathematics" )

    ;; computing
    ("ahk" "AutoHotkey" )
    ("cfg" "context-free grammar" )
    ("cj" "Clojure" )
    ("cs" "computer science" )

    ;; tech company
    ("gc" "Google Chrome" )
    ("gm" "Google Map" )
    ("macos" "Mac OS" )
    ("msw" "Microsoft Windows" )

    ;; programing
    ("ev" "environment variable" )
    ("ipa" "IP address" )
    ("jvm" "Java Virtual Machine" )
    ("rsi" "Repetitive Strain Injury" )
    ("subdir" "sub-directory" )
    ("wd" "web development" )

    ("db" "database" )
    ("gui3" "graphical user interface" )
    ("oop3" "object oriented programing" )

    ("os3" "operating system" )

    ;; programing
    ("eq" "==" )
    ("r" "return" )
    ("utf8" "-*- coding: utf-8 -*-" )

    ;; regex
    ("xaz" "\\([A-Za-z0-9]+\\)" )

    ;; unicode
    ("md" "" )

    ("hr" "--------------------------------------------------" )
    ("bu" "" )
    ("catface" "😸" )
    ("hearts" "♥💕💓💔💖💗💘💝💞💟💙💚💛💜" )
    ("ra" "→" )

    ;; url
    ("urlemacs" "http://ergoemacs.org/" )
    ;; end xah lee part 1

    ;; start xah lee part 2
    ;; http://ergoemacs.org/emacs/emacs_abbrev_mode_tutorial.html

    ;; math/unicode symbols
    ("zin"    "∈")
    ("znin"    "∉")
    ("zinf"    "∞")
    ("zluv"    "♥")
    ("zsmly"    "☺")

    ;; email
    ("zme"  "273412935@qq.com")

    ;; computing tech
    ("zwp"    "Wikipedia")
    ("zms"    "Microsoft")
    ("zg"    "Google")
    ("zit"    "IntelliType")
    ("zmsw"    "Microsoft Windows")
    ("zwin"    "Windows")
    ("zie"    "Internet Explorer")
    ("zahk"    "AutoHotkey")

    ;; normal english words
    ("zalt"    "alternative")
    ("zchar"    "character")
    ("zdef"    "definition")
    ("zbg"    "background")
    ("zkb"    "keyboard")
    ("zex"    "example")
    ("zkbd"    "keybinding")
    ("zenv"    "environment")
    ("zvar"    "variable")
    ("zev"    "environment variable")
    ("zcp"    "computer")

    ;; signature
    ("zxl"    "Xah Lee")

    ;; url
    ("zuxl"    "http://xahlee.info/")

    ;; emacs regex
    ("zd"    "\\([0-9]+?\\)")
    ("zstr"    "\\([^\"]+?\\)\"")

    ;; shell commands
    ("zf"    "find . -type f -size 0 -exec rm {} ';'")
    ;; end xah lee

    ;; start zilongshanren
    ;; math/unicode symbols
    ("8in" "∈")
    ("8nin" "∉")
    ("8inf" "∞")
    ("8luv" "♥")
    ("8smly" "☺")
    ("8en" "@~english")
    ("8zh" "@~chinese")
    ("8sp" "spacemacs")
    ;; email
    ("8me" "273412935@qq.com")

    ;; computing tech
    ("8wp" "Wikipedia")
    ("8ms" "Microsoft")
    ("8g" "Google")
    ("8it" "IntelliType")
    ("8msw" "Microsoft Windows")
    ("8win" "Windows")
    ("8ie" "Internet Explorer")
    ("8ahk" "AutoHotkey")
    ("82dx" "Cocos2D-X")

    ;; signature
    ("8zl" "zilongshanren")
    ;; emacs regex
    ("8d" "\\([0-9]+?\\)")
    ;; end zilongshanren
    ("8str" "\\([^\"]+?\\)\"")))

;; http://ergoemacs.org/emacs/emacs_abbrev_mode.html
(when (boundp 'go-mode-abbrev-table)
  (clear-abbrev-table go-mode-abbrev-table))

(define-abbrev-table 'go-mode-abbrev-table
  '(
    ("go" "package main
import \"fmt\"
func main() {
        fmt.Println(\"3\")
}")

    ("p" "fmt.Printf(\"%v\\n\", hh▮)")
    ("pl" "fmt.Println(hh▮)")
    ("r" "return")
    ("st" "string")
    ("eq" "==")
    ("v" "var x = 3")
    ("df" "x := 3")
    ("c" "const x = 3")
    ("f" "func ff(x int) int {
    return nil
}")
    ("if" "if 4 { 3 }")
    ("ie" " if err != nil { panic(err) }")
    ("ei" "else if x > 0 { 3 }")
    ("else" "else { 3 }")
    ("for" "for i := 0; i < 4; i++ { i }")
    ("fr" "for k, v := range xxx {
▮
    }
")
    ;;

    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)

(setq user-mail-address "273412935@qq.com")
