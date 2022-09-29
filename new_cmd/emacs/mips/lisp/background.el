;; Fun with background jobs.
;; Copyright (C) 1988 Joe Keane <jk3k+@andrew.cmu.edu>
;; Refer to the GNU Emacs General Public License for copyright info

;; user variables
(defvar background-show t
  "*If non-nil, background jobs' buffers are shown when they're started.")
(defvar background-select nil
  "*If non-nil, background jobs' buffers are selected when they're started.")

;; patches to shell-mode
(require 'shell)
(define-key shell-mode-map "\C-c\C-f" 'continue-shell-subjob)
(define-key shell-mode-map "\C-c\C-k" 'kill-shell-subjob)
(defun continue-shell-subjob ()
  "Continue this shell's current subjob."
  (interactive)
  (continue-process nil t))

(defun background (command)
  "Run COMMAND in the background like csh.  A message is displayed when
the job starts and finishes.  The buffer is in shell mode, so among
other things you can control the job and send input to it.  The
process object is returned if anyone cares.  See also shell-mode and
the variables background-show and background-select."
  (interactive "s%% ")
  (let*
      ((job-number 1)
       (process
	(let ((job-name "%1"))
	  (while (process-status job-name)
	    (setq job-name (concat "%" (setq job-number (1+ job-number)))))
	  (setq default-directory
	   (prog1
	       (if (string-match
		    "^cd[\t ]+\\([^\t ;]+\\)[\t ]*;[\t ]*"
		    command)
		   (prog1
		       (file-name-as-directory
			(expand-file-name
			 (substring command
			  (match-beginning 1) (match-end 1))))
		     (setq command (substring command (match-end 0))))
		 default-directory)
	     (if background-select (pop-to-buffer job-name)
	       (and background-show (with-output-to-temp-buffer job-name))
	       (set-buffer (get-buffer-create job-name)))))
	  (start-process job-name job-name shell-file-name "-c" command))))
    (message "[%d] %d" job-number (process-id process))
    (erase-buffer)
    (insert "% cd " default-directory "\n% " command ?\n)
    (set-marker (process-mark process) (point))
    (set-process-sentinel process 'background-sentinel)
    (shell-mode)
    (setq mode-name "Background")
    process))

(defun background-sentinel (process msg)
  "Called when a background job changes state."
  (message
   "[%s] %s %s"
   (substring (process-name process) 1)
   (setq msg
    (cond
     ((string= msg "finished\n") "Done")
     ((string-match "^exited" msg)
      (concat "Exit " (substring msg 28 -1)))
     ((zerop (length msg)) "Continuing") ;;Emacs bug
     (t (concat (upcase (substring msg 0 1)) (substring msg 1 -1)))))
   (nth 2 (process-command process)))
  (if (buffer-name (process-buffer process))
      (and
       (memq (process-status process) '(signal exit))
       (set-buffer
	(prog1 (current-buffer)
	  (set-buffer (process-buffer process))
	  (and
	   (prog1 (eobp)
	     (save-excursion
	       (goto-char (point-max))
	       (insert ?\n msg ? 
		(substring (current-time-string) 11 19) ?\n)))
	   (goto-char (point-max)))
	  (set-buffer-modified-p nil))))
    (set-process-buffer process nil)))
