;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;									     ;;
;;	File:     sup-mouse.el						     ;;
;;	Author:   Wolfgang Rupprecht					     ;;
;;	Created:  Fri Nov 21 19:22:22 1986				     ;;
;;	Contents: supdup mouse support for lisp machines		     ;;
;;									     ;;
;;     (from code originally written by John Robinson@bbn for the bitgraph)  ;;
;;									     ;;
;;	$Log:	sup-mouse.el,v $
; Revision 1.1.2.1  89/11/27  23:39:00  wje
; Branch RISCOS_4_50_FIXES off of trunk.
; 
; Revision 1.1  89/03/24  18:53:31  wje
; Initial revision
; 								     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GNU Emacs code for lambda/supdup mouse
;; Copyright (C) Free Software Foundation 1985, 1986

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;;  User customization option:

(defvar sup-mouse-fast-select-window nil
  "*Non-nil for mouse hits to select new window, then execute; else just select.")

(defconst mouse-left 0)
(defconst mouse-center 1)
(defconst mouse-right 2)

(defconst mouse-2left 4)
(defconst mouse-2center 5)
(defconst mouse-2right 6)

(defconst mouse-3left 8)
(defconst mouse-3center 9)
(defconst mouse-3right 10)

;;;  Defuns:

(defun sup-mouse-report ()
  "This function is called directly by the mouse, it parses and
executes the mouse commands.

 L move point          *  |---- These apply for mouse click in a window.
2L delete word            |
3L copy word		  | If sup-mouse-fast-select-window is nil,
 C move point and yank *  | just selects that window.
2C yank pop		  |
 R set mark            *  |
2R delete region	  |
3R copy region		  |

on modeline		    on \"scroll bar\"	in minibuffer
 L scroll-up		    line to top		execute-extended-command
 C proportional goto-char   line to middle	mouse-help
 R scroll-down		    line to bottom	eval-expression"
  
  (interactive)
  (let*
;; expect a string of <esc>:<buttons>;<x-pos>;<y-pos>c
      ((buttons (sup-get-tty-num ?\;))
       (x (sup-get-tty-num ?\;))
       (y (sup-get-tty-num ?c))
       (window (sup-pos-to-window x y))
       (edges (window-edges window))
       (old-window (selected-window))
       (in-minibuf-p (eq y (1- (screen-height))))
       (same-window-p (and (not in-minibuf-p) (eq window old-window)))
       (in-modeline-p (eq y (1- (nth 3 edges))))
       (in-scrollbar-p (>= x (1- (nth 2 edges)))))
    (setq x (- x (nth 0 edges)))
    (setq y (- y (nth 1 edges)))

;    (error "mouse-hit %d %d %d" buttons x y) ;;;; debug

    (cond (in-modeline-p
	   (select-window window)
	   (cond ((= buttons mouse-left)
		  (scroll-up))
		 ((= buttons mouse-right)
		  (scroll-down))
		 ((= buttons mouse-center)
		  (goto-char (/ (* x
				   (- (point-max) (point-min)))
				(1- (window-width))))
		  (beginning-of-line)
		  (what-cursor-position)))
	   (select-window old-window))
	  (in-scrollbar-p
	   (select-window window)
	   (scroll-up
	    (cond ((= buttons mouse-left)
		   y)
		  ((= buttons mouse-right)
		   (+ y (- 2 (window-height))))
		  ((= buttons mouse-center)
		   (/ (+ 2 y y (- (window-height))) 2))
		  (t
		   0)))
	   (select-window old-window))
	  (same-window-p
	   (cond ((= buttons mouse-left)
		  (sup-move-point-to-x-y x y))
		 ((= buttons mouse-2left)
		  (sup-move-point-to-x-y x y)
		  (kill-word 1))
		 ((= buttons mouse-3left)
		  (sup-move-point-to-x-y x y)
		  (save-excursion
		    (copy-region-as-kill
		     (point) (progn (forward-word 1) (point))))
		  (setq this-command 'yank)
		  )
		 ((= buttons mouse-right)
		  (push-mark)
		  (sup-move-point-to-x-y x y)
		  (exchange-point-and-mark))
		 ((= buttons mouse-2right)
		  (push-mark)
		  (sup-move-point-to-x-y x y)
		  (kill-region (mark) (point)))
		 ((= buttons mouse-3right)
		  (push-mark)
		  (sup-move-point-to-x-y x y)
		  (copy-region-as-kill (mark) (point))
		  (setq this-command 'yank))
		 ((= buttons mouse-center)
		  (sup-move-point-to-x-y x y)
		  (setq this-command 'yank)
		  (yank))
		 ((= buttons mouse-2center)
		  (yank-pop 1))
		 )
	   )
	  (in-minibuf-p
	   (cond ((= buttons mouse-right)
		  (call-interactively 'eval-expression))
		 ((= buttons mouse-left)
		  (call-interactively 'execute-extended-command))
		 ((= buttons mouse-center)
		  (describe-function 'sup-mouse-report)); silly self help 
		 ))
	  (t				;in another window
	   (select-window window)
	   (cond ((not sup-mouse-fast-select-window))
		 ((= buttons mouse-left)
		  (sup-move-point-to-x-y x y))
		 ((= buttons mouse-right)
		  (push-mark)
		  (sup-move-point-to-x-y x y)
		  (exchange-point-and-mark))
		 ((= buttons mouse-center)
		  (sup-move-point-to-x-y x y)
		  (setq this-command 'yank)
		  (yank))
		 ))
	  )))


(defun sup-get-tty-num (term-char)
  "Read from terminal until TERM-CHAR is read, and return intervening number.
Upon non-numeric not matching TERM-CHAR signal an error."
  (let
      ((num 0)
       (char (read-char)))
    (while (and (>= char ?0)
		(<= char ?9))
      (setq num (+ (* num 10) (- char ?0)))
      (setq char (read-char)))
    (or (eq term-char char)
	(error "Invalid data format in mouse command"))
    num))

(defun sup-move-point-to-x-y (x y)
  "Position cursor in window coordinates.
X and Y are 0-based character positions in the window."
  (move-to-window-line y)
  (move-to-column x)
  )

(defun sup-pos-to-window (x y)
  "Find window corresponding to screen coordinates.
X and Y are 0-based character positions on the screen."
  (let ((edges (window-edges))
	(window nil))
    (while (and (not (eq window (selected-window)))
		(or (<  y (nth 1 edges))
		    (>= y (nth 3 edges))
		    (<  x (nth 0 edges))
		    (>= x (nth 2 edges))))
      (setq window (next-window window))
      (setq edges (window-edges window))
      )
    (or window (selected-window))
    )
  )
