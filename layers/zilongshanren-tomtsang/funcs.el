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

;; start lisha's ergoemacs: copy from http://ergoemacs.org/emacs/elisp.html
(defun my-insert-p-tag ()
  "Insert <p></p> at cursor point."
  (interactive)
  (insert "<p></p>")
  (backward-char 4))

(defun my-wrap-markup-region ()
  "Insert a markup <b></b> around a region."
  (interactive)
  (save-excursion
    (goto-char (region-end))
    (insert "</b>")
    (goto-char (region-beginning))
    (insert "<b>")))

;; turn on highlight selection
;; (transient-mark-mode 1)
(defun my-select-current-word ()
  "Select the word under cursor.
“word” here is considered any alphanumeric sequence with “_” or “-”."
  (interactive)
  (let (pt)
    (skip-chars-backward "-_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "-_A-Za-z0-9")
    (set-mark pt)))


;; turn on highlight selection
;; (transient-mark-mode 1)

(defun my-select-current-line ()
  "Select the current line"
  (interactive)
  (let ((pos (line-beginning-position)))
    (end-of-line)
    (set-mark pos)))


(defun my-replace-greek-region ()
  "Replace “alpha” to “α” and other greek letters in current region."
  (interactive)
  (let (
        (p1 (region-beginning))
        (p2 (region-end)))
    (save-restriction
      (narrow-to-region p1 p2)
      (goto-char (point-min))
      (while (search-forward " alpha" nil t)
        (replace-match " α" nil t))
      (goto-char (point-min))
      (while (search-forward " beta" nil t)
        (replace-match " β" nil t))
      (goto-char (point-min))
      (while (search-forward " gamma" nil t)
        (replace-match " γ" nil t)))))

(defun my-delete-enclosed-text ()
  "Delete texts between any pair of delimiters."
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-chars-backward "^([<>“")
      (setq p1 (point))
      (skip-chars-forward "^)]<>”")
      (setq p2 (point))
      (delete-region p1 p2))))

(defun my-remove-line-breaks ()
  "Remove line endings in current paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


(defun my-insert-random-number ()
  "Insert a random number between 0 to 999999."
  (interactive)
  (random t) ; seed it randomly
  (insert (number-to-string (random 999999))) )

(defun my-word-definition-lookup ()
  "Look up the word under cursor in a browser."
  (interactive)
  (browse-url
   (concat "http://www.answers.com/main/ntquery?s=" (thing-at-point 'symbol))))


(defun my-to-unix-eol (fPath)
  "Change file's line ending to unix convention."
  (let ((myBuffer (find-file fPath)))
    (set-buffer-file-coding-system 'unix) ; or 'mac or 'dos
    (save-buffer)
    (kill-buffer myBuffer)))


(defun my-dired-2unix-marked-files ()
  "Change to unix line ending for marked (or next arg) files."
  (interactive)
  (mapc 'to-unix-eol (dired-get-marked-files))
  )


(defun my-delete-current-file ()
  "Delete the file associated with the current buffer.
Delete the current buffer too.
If no file is associated, just close buffer without prompt for save."
  (interactive)
  (let ((currentFile (buffer-file-name)))
    (when (yes-or-no-p (concat "Delete file?: " currentFile))
      (kill-buffer (current-buffer))
      (when currentFile
        (delete-file currentFile)))))

(defun my-insert-column-counter (n)
  "Insert a sequence of numbers vertically.
For example:

a▮b
c d
e f

becomes:

a1 b
c2 d
e3 f

If there are not enough existing lines after the cursor
when this function is called, it aborts at the last line.

This command is conveniently used together with `kill-rectangle' and `string-rectangle'.
Version 2019-01-27"
  (interactive "nEnter the max integer: ")
  (let ((i 1) colpos )
    (setq colpos (- (point) (line-beginning-position)))
    (while (<= i n)
      (insert (number-to-string i))
      (forward-line)
      (beginning-of-line)
      (forward-char colpos)
      (setq i (1+ i)))))

(defun my-get-word ()
  "print the word under cursor.
Word here is any A to Z, a to z, and low line _"
  (interactive)
  (let (
        p1
        p2
        (case-fold-search t))
    (save-excursion
      (skip-chars-backward "_a-z0-9" )
      (setq p1 (point))
      (skip-chars-forward "_a-z0-9" )
      (setq p2 (point))
      (message "%s" (buffer-substring-no-properties p1 p2)))))

(defun my-print-current-word ()
  "print current word."
  (interactive)
  (message "%s" (thing-at-point 'word)))


(defun my-get-boundary-and-thing ()
  "example of using `bounds-of-thing-at-point'"
  (interactive)
  (let (bounds pos1 pos2 mything)
    (setq bounds (bounds-of-thing-at-point 'symbol))
    (setq pos1 (car bounds))
    (setq pos2 (cdr bounds))
    (setq mything (buffer-substring-no-properties pos1 pos2))

    (message
     "thing begin at [%s], end at [%s], thing is [%s]"
     pos1 pos2 mything)))


(defun my-select-inside-quotes ()
  "Select text between double straight quotes on each side of cursor.

the cursor is the beginning-of-quotes."
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "^\"")
    (setq p1 (point))
    (skip-chars-forward "^\"")
    (setq p2 (point))

    (goto-char p1)
    (push-mark p2)
    (setq mark-active t)))

(defun my-select-text-in-quote ()
  "Select text between the nearest left and right quotes.

the cursor is the end-of-quotes."
  (interactive)
  (let ($pos
        ($skipChars "^\""))
    (skip-chars-backward $skipChars)
    (setq $pos (point))
    (skip-chars-forward $skipChars)
    (push-mark $pos)
    (setq mark-active t)))


(defun my-save-to-test-txt ()
  "write whole buffer to a file. overwrites the file content."
  (interactive)
  (write-region (point-min) (point-max) "text.txt" ))


(defun my-save-to-file-path (filePath)
  "write whole buffer to a file. overwrites the file content."
  (interactive "sEnter your file path: ")
  (write-region (point-min) (point-max) filePath)
  (message "Buffer had save to: %s" filePath))


(defun my-make-backup ()
  "Make a backup copy of current buffer's file.
Create a backup of current buffer's file.
The new file name is the old file name with trailing “~”, in the same dir.
If such a file already exist, append more “~”.
If the current buffer is not associated with a file, its a error."
  (interactive)
  (let ((fName (spacemacs/copy-file-name))
        ;; buffer-file-name
        backupName )
    (message "%s" fName)
    (if (not fName)
        (error "current buffer is not a file." )
      (progn
        (setq backupName (concat fName "~"))
        (while (file-exists-p backupName)
          (setq backupName (concat backupName "~")))
        (copy-file fName backupName t)
        (message (concat "Backup saved as: " (file-name-nondirectory backupName)))))))

(defun my-command ()
  "One sentence summary of what this command do.

More details here. Be sure to mention the return value if relevant.
Lines here should not be longer than 70 chars,
and don't indent them."
  (interactive)
  (let (var1 var2 …)
    (setq var1 …)
    (setq var2 …)
    ;; do something …
    ))


(defun my-region-beginning-to-end ()
  "sample code to show region begin/end positions"
  (interactive)
  (message "begin at %s\nend at %s"
           (region-beginning)
           (region-end)))

(defun my-is-region-active ()
  "print whether region is active."
  (interactive)
  (if (use-region-p)
      (message "region active")
    (message "region not active")))


(defun my-select-line ()
  "Select current line."
  (interactive)
  (let (p1 p2)
    (setq p1 (line-beginning-position))
    (setq p2 (line-end-position))
    (goto-char p1)
    (push-mark p2)
    (setq mark-active t)))


(defun my-select-line ()
  "Select current line."
  (interactive)
  (let (p1 p2)
    (setq p1 (line-beginning-position))
    (setq p2 (line-end-position))
    (goto-char p1)
    (push-mark p2)
    (setq mark-active t)))


(defun my-downcase-word-or-region ()
  "Downcase current word or region."
  (interactive)
  (let (pos1 pos2 bds)
    (if (use-region-p)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))

    ;; now, pos1 and pos2 are the starting and ending positions of the
    ;; current word, or current text selection if exist.
    (downcase-region pos1 pos2)
    ))

(defun my-current-line ()
  "Downcase current word or region."
  (interactive)
  (setq myLine
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)
       ))
  (message "%s" myLine))

(defun my-prompt-file-name ()
  "Prompt user to enter a file name, with completion and history support."
  (interactive)
  (message "String is %s" (read-file-name "Enter file name:")))

(defun my-prompt-directory ()
  "Prompt user to enter a dir path, with path completion and input history support."
  (interactive)
  (message "Path is %s" (read-directory-name "Directory:")))


(defun my-prompt-string ()
  "Prompt user to enter a string, with input history support."
  (interactive)
  (message "String is %s" (read-string "Enter your name:")))

(defun my-prompt-regex-string ()
  "Prompt user to enter a elisp regex, with input history support."
  (interactive)
  (message "Regex is %s" (read-regexp "Type a regex:")))

;; (require 'ido)

(defun my-pick-one ()
  "Prompt user to pick a choice from a list."
  (interactive)
  (let ((choices '("cat" "dog" "dragon" "tiger")))
    (message "%s" (ido-completing-read "Open bookmark:" choices ))))


(defun my-pick-y-or-n-p ()
  "Prompt user to pick a choice from y-or-n."
  (interactive)
  (if (y-or-n-p "Do it?")
      (progn
        (message "yes, you will do it.")
        ;; code to do something here
        )
    (progn
      (message "no, you will not do it.")
      ;; code if user answered no.
      )
    )
  )


(defun my-ask-name (x)
  "Ask name."
  (interactive "sEnter your name: ")
  (message "Name: %s" x))


(defun my-ask-age (x)
  "Ask age."
  (interactive "nEnter your age: ")
  (message "Age: %d" x))


(defun my-print-region-boundary (x y)
  "Prints region start and end positions"
  (interactive "r")
  (message "Region begin at: %d, end at: %d" x y))


(defun my-interactive-list-no-input (x y)
  "Ask name and age"
  (interactive
   ;; complex code here that returns a list
   (list "Mary" 22))
  (message "Name is: %s, Age is: %d" x y))


(defun my-interactive-list-ask-name-and-age (x y)
  "Ask name and age"
  (interactive "sEnter you name:
nEnter your age: ")
  (message "Name is: %s, Age is: %d" x y))


(defun my-print-argument-received (x)
  "print argument received"
  (interactive "P")
  (message "%s" x)
  ;; value of x is from universal argument, or nil if universal-argument isn't called
  )

(defun my-current-prefix-arg ()
  "print `current-prefix-arg'

;; try
;; M-x g
;; C-u M-x g
;; C-u C-u M-x g
;; C-u 1 M-x g
;; C-u 2 M-x g

"
  (interactive )
  (message "%s" current-prefix-arg))


(defun my-utest (arg1 &optional arg2 arg3)
  "Sample command to test `universal-argument'."
  (interactive
   (cond
    ((equal current-prefix-arg nil) ; no C-u
     (list 1 nil nil))
    ((equal current-prefix-arg '(4)) ; C-u
     (list 1 2 nil))
    ((equal current-prefix-arg 2) ; C-u 2
     (list 1 2 3))
    ;; more special case here

    (t ; all other cases, prompt
     (list
      (read-string "arg1:" )
      (read-string "arg2:" )
      (read-string "arg3:" )))))

  ;; now, all the parameters of your function is filled.
  ;; code body here

  (message "args are: %s %s %s" arg1 arg2 arg3)
  ;;
  )

(defun my-find-replace-all-current-buffer-type-a (x y)
  "idiom for string replacement in current buffer

"
  (interactive "sEnter your find string:
sEnter your replacement: ")
   (let ((case-fold-search t)) ; or nil

    (goto-char (point-min))
    (while (search-forward x nil t)
      (replace-match y))
    ;; repeat for other string pairs
    ))

(defun my-find-replace-regexp-all-current-buffer-type-a (x y)
  "idiom for string replacement in current buffer, if you need regexp

"
  (interactive "sEnter your find string:
sEnter your replacement: ")
  (let ((case-fold-search t)) ; or nil

    (goto-char (point-min))
    (while (search-forward-regexp x nil t)
      (replace-match y))
    ;; repeat for other string pairs
    ))









;; end lisha's ergoemacs: copy from http://ergoemacs.org/emacs/elisp.html
