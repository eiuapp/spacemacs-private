;;; funcs.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun zilongshanren-tomtsang/my-pclip (str-val)
  (if simpleclip-works (simpleclip-set-contents str-val)
    (cond
     ((eq system-type 'darwin)
      (with-temp-buffer
        (insert str-val)
        (call-process-region (point-min) (point-max) "/usr/bin/pbcopy")))
     ((eq system-type 'cygwin)
      (with-temp-buffer
        (insert str-val)
        (call-process-region (point-min) (point-max) "putclip")))
     ((memq system-type '(gnu gnu/linux gnu/kfreebsd))
      (with-temp-buffer
        (insert str-val)
        (call-process-region (point-min) (point-max) "xsel" nil nil nil "--clipboard" "--input"))))))
;; }}