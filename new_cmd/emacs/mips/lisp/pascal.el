;;; Modified by Mosur Mohan 15-Apr-88 <uunet!mntgfx!mosurm> 
;;; Pascal editing support package, based on:

;;; Originally, Modula-2 editing support package
;;; Author Mick Jordan
;;; Amended Peter Robinson
;;; Ported to GNU Michael Schmidt
;;; From: "Michael Schmidt" <michael@pbinfo.UUCP>
;;; Modified by Tom Perrine <Perrin@LOGICON.ARPA> (TEP)
;;; Grossly modified by Earl Killian

(defvar pascal-mode-syntax-table nil
  "Syntax table in use in Pascal-mode buffers.")
(defvar pascal-mode-abbrev-table nil
  "Abbrev table in use in pascal-mode buffers.")
(define-abbrev-table 'pascal-mode-abbrev-table ())

(if pascal-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\( "() 1" table)
    (modify-syntax-entry ?\) ")( 4" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?{ "<" table)
    (modify-syntax-entry ?} ">" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\' "\"" table)
    (setq pascal-mode-syntax-table table)))

(defvar pascal-mode-map nil
  "Keymap used in Pascal-mode.")

(if pascal-mode-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-i"     'pascal-indent-line)
    (define-key map "\C-c\C-i" 'pascal-tab-to-tab-col)
    (define-key map "\C-j"     'pascal-newline)
    (define-key map "\C-cb"    'pascal-begin)
    (define-key map "\C-c\C-b" 'pascal-backward-block)
    (define-key map "\C-c\C-f" 'pascal-forward-block)
    (define-key map "\C-c\C-d" 'pascal-down-block)
    (define-key map "\C-c\C-e" 'pascal-up-block)
    (define-key map "\C-c\C-u" 'pascal-back-pascal-up-block)
    (define-key map "\C-c\C-@" 'pascal-mark-block)
    (define-key map "\C-c\C-n" 'pascal-narrow-to-block)
    (define-key map "\C-c~"    'pascal-self-assign-stmt)
    (define-key map "\C-c\["   'pascal-open-comment-box)
    (define-key map "\C-c\C-m" 'pascal-continue-comment-box)
    (define-key map "\C-c\>"   'pascal-set-end-comment-col)
    (define-key map "\C-c}"    'pascal-end-comment)
    (setq pascal-mode-map map)))

(defvar pascal-indent 2 "*This variable gives the indentation in Pascal-mode")

(defun pascal-mode ()
  "Mode to support program development in Pascal.
The prefix-key for pascal-mode is C-c.

  TAB      pascal-indent-line       C-c TAB     pascal-tab-to-tab-col
  C-j      pascal-newline           C-c b       pascal-begin
  C-c C-f  pascal-forward-block     C-c C-b     pascal-backward-block
  C-c C-d  pascal-down-block        C-c C-u     pascal-back-pascal-up-block
  C-c C-e  pascal-up-block          C-c C-@     pascal-mark-block
  C-c C-n  pascal-narrow-to-block   C-c ~       pascal-self-assign-stmt
  C-c C-[  pascal-open-comment-box  C-c C-m     pascal-continue-comment-box
  C-c }    pascal-end-comment       C-c >       pascal-set-end-comment-column

  pascal-indent controls the number of spaces for each indentation."
  (interactive)
  (kill-all-local-variables)
  (use-local-map pascal-mode-map)
  (setq major-mode 'pascal-mode)
  (setq mode-name "Pascal")
  (setq local-abbrev-table pascal-mode-abbrev-table)
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'box-com-col)
  (setq box-com-col 2)
  (make-local-variable 'end-comment-column)
  (setq end-comment-column 66)
  (set-syntax-table pascal-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "{ ")
  (make-local-variable 'comment-end)
  (setq comment-end " }")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "{ *")
  (setq indent-tabs-mode nil)
  (make-local-variable 'pascal-tab-col)
  (setq pascal-tab-col 20)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'pascal-mode-hook))


(defun pascal-indent-line ()
  "Indent the current line based on the indentation of the
surrounding Pascal block, and on whether the previous line
ended a Pascal statement."
  (interactive)
  (let (blk-ind blk-beg prev-ind prev-beg shift-amt keep-going fishy)
    (save-excursion
      (beginning-of-line)
      (setq fishy (not (pascal-backward-scan-blocks 1 nil nil)))
      (beginning-of-line)
      (setq blk-beg (point))
      (setq blk-ind (current-indentation))
      (if fishy
        (setq indent (+ blk-ind pascal-indent)) ))
    (if fishy nil
      (save-excursion
        (forward-line -1)
        (setq prev-beg (point))
        (setq prev-ind (current-indentation))
        (if (<= prev-beg blk-beg)       ; prev line is containing block
	    (setq indent (+ blk-ind pascal-indent))
          (skip-chars-forward " \t")
          (if (looking-at "\\<if\\>\\|\\<case\\>\\|\\<with\\>\\|\\<for\\>\\|\\<while\\>\\|\\<repeat\\>")
	      (setq indent (+ prev-ind pascal-indent)) ; then
            (setq indent (+ blk-ind pascal-indent)) ; else
            (end-of-line)
            (if (or
		 (re-search-backward ";[ \t]*\\((\\*.*\\*)\\)*$" prev-beg t 1)
		 (re-search-backward "^ *(\\*.*\\*)$" prev-beg t 1)
		 (re-search-backward "^$" prev-beg t 1)
		 (= (point) prev-beg) )
		nil                       ; then block-indent will do
              (setq indent (+ prev-ind pascal-indent)) ; else use previous-line indent
              )))
        ))
    (save-excursion
      (beginning-of-line)
      (setq prev-beg (point))
      (skip-chars-forward " \t")
      (if (and (not fishy) (looking-at "end\\|until"))
	  (setq indent blk-ind)
        (save-excursion
          (cond ((looking-at "then")
                 (pascal-backward-find-kwd "\\<if\\>" nil)
                 (setq indent (+ (current-indentation) pascal-indent)) )
                ((looking-at "else")
                 (setq then-cnt 1)
                 (setq keep-going t)
                 (while keep-going
                   (pascal-backward-find-kwd "\\<then\\>\\|\\<else\\>" nil)
                   (if (looking-at "then")
                       (setq then-cnt (1- then-cnt))
                     (setq then-cnt (1+ then-cnt)) )
                   (if (> then-cnt 0)
		       nil
                     (setq keep-going nil)
                     (setq indent (current-indentation)) ))
                 )
                (t nil) )
          ))
      ;; install the right indentation
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
	  nil
        (delete-region prev-beg (point))
        (indent-to indent) )
      )
    (if (bolp) (back-to-indentation))
    ))

(defun pascal-tab-to-tab-col (&optional arg)
  "Insert space to force indent to specified ARG column,
or to pascal-tab-col."
  (interactive "P")
  (if arg
    (if (integerp arg)
	(setq pascal-tab-col arg)
      (setq pascal-tab-col (current-column))
      ))
  (indent-to pascal-tab-col))

(defun pascal-newline ()
  "Insert a newline and indent it appropriately."
  (interactive)
  (newline)
  (pascal-indent-line) )

(defun pascal-end-comment ()
  "Finish this comment correctly right-aligned."
  (interactive)
  (if (not (bolp))
      (indent-to end-comment-column 1))
  (insert "*)"))

(defun pascal-set-end-comment-column ()
  "Set the Pascal mode local variable end-comment-column
   to the column that point is on."
  (interactive)
  (message (concat "end-comment-column set to "
    (setq end-comment-column (current-column)) )))

(defun pascal-open-comment-box (arg)
  "Open a box comment: set box-com-col to the current
column.  Now, read the char to use for the comment line,
then insert two lines and open an aligned comment box."
  (interactive "cComment-line char: ")
  (setq box-com-col (current-column))
  (insert "(*")
  (let ( (counter 1)
         (lsize (- end-comment-column box-com-col)) )
    (while (< (setq counter (1+ counter)) lsize)
      (insert arg) )
    (insert "*)\n")
    (indent-to box-com-col 0)
    (insert "(*")
    (setq counter 1)
    (while (< (setq counter (1+ counter)) lsize)
      (insert arg) )
    (insert "*)")
    (beginning-of-line)
    (open-line 1)
    (indent-to box-com-col)
    (insert "(*  ") )
  )


(defun pascal-continue-comment-box ()
  "Close current-line comment correctly right-aligned, open a new
indented comment on the next line, and indent to pascal-tab-col."
  (interactive)
  (indent-to end-comment-column 1)
  (insert "*)\n")
  (indent-to box-com-col)
  (insert "(*")
  (indent-to pascal-tab-col 2) )

(defun pascal-forward-find-kwd (target lim)
  "Leave point at the end of a keyword and return the position
of the beginning of the matched keyword, skipping comments
and literal strings en route.  If TARGET is specified, find it
outside comments & strings until limit LIM is reached.  If not
found, return NIL."
  (let ( (keep-looking t)
         (reg-str
           (concat (or target "\\<begin\\>\\|\\<end\\>\\|\\<record\\>\\|\\<case\\>\\|\\<repeat\\>\\|\\<until\\>")
             "\\|(\\*\\|{\\|""\\|'"))
         found mbeg mend next-target)
    (while keep-looking
      (setq found (re-search-forward reg-str lim t 1))
      (if (not found)
        ;;; then... didn't find any of the REG-STR components
        (setq keep-looking nil)
        ;;; else... goto beginning of match, check it out
        (setq mend (match-end 0))
        (goto-char (match-beginning 0))
        (setq mbeg (point))
        (cond
          ((and target (looking-at target))
            (setq keep-looking nil) )
          ((looking-at "(\\*") (setq next-target "*)"))
          ((looking-at "{") (setq next-target "}"))
          ((looking-at "'") (setq next-target "'"))
          ((looking-at """") (setq next-target """"))
          (t  (setq keep-looking nil)) )
        (goto-char mend)
        (if keep-looking (search-forward next-target nil t 1)) )
      )
    (and found mbeg)                    ; return-value = match-beginning
    )
  )

(defun pascal-backward-find-kwd (target lim)
  "Leave point at the beginning of a keyword and return the
position of the end of the matched keyword, skipping comments
and literal strings en route.  If TARGET is specified, find it
outside comments & strings until limit LIM is reached.  If not
found, return NIL."
  (let ( (keep-looking t)
         (reg-str
           (concat (or target "\\<begin\\>\\|\\<end\\>\\|\\<record\\>\\|\\<case\\>\\|\\<repeat\\>\\|\\<until\\>")
             "\\|\\*)\\|}\\|""\\|'"))
         found mbeg mend next-target)
    (while keep-looking
      (setq found (re-search-backward reg-str lim t 1))
      (if (not found)
        ;;; then... didn't find any of the REG-STR components
        (setq keep-looking nil)
        ;;; else... we're at beginning of match, check it out
        (setq mend (match-end 0))
        (setq mbeg (point))
        (cond
          ((and target (looking-at target))
            (setq keep-looking nil) )
          ((looking-at "\\*)") (setq next-target "(*"))
          ((looking-at "}") (setq next-target "{"))
          ((looking-at "'") (setq next-target "'"))
          ((looking-at """") (setq next-target """"))
          (t  (setq keep-looking nil)) )
        (if keep-looking (search-backward next-target nil t 1)) )
      )
    (and found mend)                    ; return-value = match-end
    )
  )


(defun pascal-forward-scan-blocks (depth target lim)
  "Move forward:
   down into blocks if DEPTH < 0,
   across one block if DEPTH = 0,
   up out of blocks if DEPTH > 0.
Second arg TARGET = nil initially, used internally
to distinguish between until and end.
LIM bounds the search."
  (or target (setq target ""))
  (let (mbeg mend done fishy)
    (if (not (setq mbeg (pascal-forward-find-kwd nil lim)))
      (setq fishy t)                    ; bad location
      (setq mend (point))               ; else process kwd
      (goto-char mbeg)
      (cond
        ((looking-at "begin\\|case\\|record\\|repeat")
          (setq depth (1+ depth))
          (if (= depth 0) (setq done t)
            (if (looking-at "repeat")
              (setq target "until")     ; then
              (setq target "end") ))    ; else
          (goto-char mend) )
        ((looking-at "end\\|until")
          (if (<= depth 0)
            (setq fishy t)              ; bad location
            (setq depth (1- depth))     ; else...
            (if (and (= depth 0) (looking-at target))
              (setq done t) )
            (goto-char mend)
            (setq target nil) ))
        )
      (if fishy nil                     ; return bad status
        (or done (pascal-forward-scan-blocks depth target lim)) ) ; else recurse
      )
    )
  )

(defun pascal-backward-scan-blocks (depth target lim)
  "Move backward:
   down into blocks if DEPTH < 0,
   across one block if DEPTH = 0,
   up out of blocks if DEPTH > 0.
Second arg TARGET = nil initially, used internally
to distinguish between until and end.
LIM bounds the search."
  (or target (setq target ""))
  (or lim (setq lim nil))
  (let (mend done fishy)
    (if (not (setq mend (pascal-backward-find-kwd nil lim)))
	(setq fishy t)			; bad location
      (cond                             ; else process kwd
        ((looking-at "end\\|until")
          (setq depth (1+ depth))
          (if (= depth 0)
	      (setq done t)
            (if (looking-at "until")
		(setq target "repeat")
              (setq target "begin\\|case\\|record\\|repeat") )))
        ((looking-at "begin\\|case\\|record\\|repeat")
          (if (<= depth 0)
	      (setq fishy t)
            (setq depth (1- depth))
            (if (and (= depth 0) (looking-at target))
		(setq done t) )
            (setq target nil) ))
        )
      (if fishy nil                     ; return bad status
        (or done (pascal-backward-scan-blocks depth target lim)) ) ; else recurse
      )
    )
  )


(defun pascal-forward-block (&optional numblks)
  "Move forward across NUMBLKS balanced begin-end blocks."
  (interactive "p")
  (or numblks (setq numblks 1))
  (if (< numblks 0) (pascal-backward-block (- numblks))
    (let (found-pos fishy)
      (save-excursion
        (while (> numblks 0)
          (if (pascal-forward-scan-blocks 0 nil nil)
            (setq numblks (1- numblks)) ; then... all's well
            (setq fishy t)              ; else exit
            (setq numblks 0) )
          )
        (setq found-pos (point)) )
      (if (not fishy)
        (goto-char found-pos)           ; happy ending
        (push-mark (point) t)           ; else mark and warn
        (goto-char found-pos)
        (send-string-to-terminal "")
        (message "Bad block structure, mark set.") )
      ))
  )

(defun pascal-backward-block (&optional numblks)
  "Move backward across NUMBLKS balanced begin-end block."
  (interactive "p")
  (or numblks (setq numblks 1))
  (if (< numblks 0) (pascal-forward-block (- numblks))
    (let (found-pos fishy)
      (save-excursion
        (while (> numblks 0)
          (if (pascal-backward-scan-blocks 0 nil nil)
            (setq numblks (1- numblks)) ; then... all's well
            (setq fishy t)              ; else exit
            (setq numblks 0) )
          )
        (setq found-pos (point)) )
      (if (not fishy)
        (goto-char found-pos)           ; happy ending
        (push-mark (point) t)           ; else mark and warn
        (goto-char found-pos)
        (send-string-to-terminal "")
        (message "Bad block structure, mark set.") )
      ))
  )

(defun pascal-down-block (&optional arg)
  "Move forward down ARG levels of begin-end block.
A negative argument means move backward but still down."
  (interactive "p")
  (or arg (setq arg 1))
  (let (found-pos all-swell)
    (save-excursion
      (setq all-swell
        (if (> arg 0)
          (pascal-forward-scan-blocks (- arg) nil nil) ; then
          (pascal-backward-scan-blocks arg nil nil) ; else
          ))
      (setq found-pos (point)) )
      (if all-swell
        (goto-char found-pos)           ; happy ending
        (push-mark (point) t)           ; else mark and warn
        (goto-char found-pos)
        (send-string-to-terminal "")
        (message "Bad block structure, mark set.") )
    )
  )

(defun pascal-back-pascal-up-block (&optional arg)
  "Move backward out of ARG levels of begin-end blocks.
   A negative argument means move forward but still up."
  (interactive "p")
  (or arg (setq arg 1))
  (pascal-up-block (- arg)))

(defun pascal-up-block (&optional arg)
  "Move forward out of ARG levels of begin-end blocks.
   A negative argument means move backward but still up."
  (interactive "p")
  (or arg (setq arg 1))
  (let (found-pos all-swell)
    (save-excursion
      (setq all-swell
        (if (> arg 0)
          (pascal-forward-scan-blocks arg nil nil) ; then
          (pascal-backward-scan-blocks (- arg) nil nil)
          ))
      (setq found-pos (point)) )
      (if all-swell
        (goto-char found-pos)           ; happy ending
        (push-mark (point) t)           ; else mark and warn
        (goto-char found-pos)
        (send-string-to-terminal "")
        (message "Bad block structure, mark set.") )
    )
  )


(defun pascal-mark-block (&optional arg)
  "Set mark at the end of the next block from point.
With argument, do this that many blocks away.  Leave
the cursor at top-of-region."
  (interactive "p")
  (or arg (setq arg 1))
  (let (save-loc all-swell)
    (save-excursion
      (setq all-swell
            (pascal-forward-block arg))
      (end-of-line)
      (setq save-loc (point)) )
    (push-mark save-loc 1)
    (if all-swell
        (message "Block marked.")
      (send-string-to-terminal "")
      (message "Bad block structure, mark set.") )
    )
  )

(defun pascal-narrow-to-block (&optional arg)
  "Narrow window down to the next block ahead from the cursor.
   With argument, do this that many blocks ahead (or back)."
  (interactive "p")
  (or arg (setq arg 1))
  (let ( (reg-beg (point))
         (reg-end 0)
         all-swell)
    (save-excursion
      (cond
        ((< arg 0)
          (setq all-swell (pascal-backward-block (- arg)))
          (beginning-of-line)
          (setq reg-end (point)) )
        (t
          (setq all-swell (pascal-forward-block arg))
          (end-of-line)
          (setq reg-end (point)) ))
      )
    (cond
      (all-swell
        (narrow-to-region reg-beg reg-end)
        (goto-char (min reg-beg reg-end)) )
      (t
        (push-mark reg-end)
        (send-string-to-terminal "")
        (message "Bad block structure, mark set.") )
      )
    )
  )

(defun pascal-self-assign-stmt ()
  "Given variable X typed in, generate X := X."
  (interactive)
  (let (cur-pt var-end tmpstr)
    (setq cur-pt (point))
    (skip-chars-backward " \t")
    (setq var-end (point))
    (skip-chars-backward "^ \t\n")
    (setq tmpstr (buffer-substring (point) var-end))
    (goto-char cur-pt)
    (insert " := " tmpstr " ") ))
