;;; semantic/complete.el --- Routines for performing tag completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2005, 2007-2025 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Completion of tags by name using tables of semantic generated tags.
;;
;; While it would be a simple matter of flattening all tag known
;; tables to perform completion across them using `all-completions',
;; or `try-completion', that process would be slow.  In particular,
;; when a system database is included in the mix, the potential for a
;; ludicrous number of options becomes apparent.
;;
;; As such, dynamically searching across tables using a prefix,
;; regular expression, or other feature is needed to help find symbols
;; quickly without resorting to "show me every possible option now".
;;
;; In addition, some symbol names will appear in multiple locations.
;; If it is important to distinguish, then a way to provide a choice
;; over these locations is important as well.
;;
;; Beyond brute force offers for completion of plain strings,
;; using the smarts of semantic-analyze to provide reduced lists of
;; symbols, or fancy tabbing to zoom into files to show multiple hits
;; of the same name can be provided.
;;
;;; How it works:
;;
;; There are several parts of any completion engine.  They are:
;;
;; A.  Collection of possible hits
;; B.  Typing or selecting an option
;; C.  Displaying possible unique completions
;; D.  Using the result
;;
;; Here, we will treat each section separately (excluding D)
;; They can then be strung together in user-visible commands to
;; fulfill specific needs.
;;
;; COLLECTORS:
;;
;; A collector is an object which represents the means by which tags
;; to complete on are collected.  It's first job is to find all the
;; tags which are to be completed against.  It can also rename
;; some tags if needed so long as `semantic-tag-clone' is used.
;;
;; Some collectors will gather all tags to complete against first
;; (for in buffer queries, or other small list situations).  It may
;; choose to do a broad search on each completion request.  Built in
;; functionality automatically focuses the cache in as the user types.
;;
;; A collector choosing to create and rename tags could choose a
;; plain name format, a postfix name such as method:class, or a
;; prefix name such as class.method.
;;
;; DISPLAYERS
;;
;; A displayer is in charge if showing the user interesting things
;; about available completions, and can optionally provide a focus.
;; The simplest display just lists all available names in a separate
;; window.  It may even choose to show short names when there are
;; many to choose from, or long names when there are fewer.
;;
;; A complex displayer could opt to help the user 'focus' on some
;; range.  For example, if 4 tags all have the same name, subsequent
;; calls to the displayer may opt to show each tag one at a time in
;; the buffer.  When the user likes one, selection would cause the
;; 'focus' item to be selected.
;;
;; CACHE FORMAT
;;
;; The format of the tag lists used to perform the completions are in
;; semanticdb "find" format, like this:
;;
;; ( ( DBTABLE1 TAG1 TAG2 ...)
;;   ( DBTABLE2 TAG1 TAG2 ...)
;;   ... )
;;
;; INLINE vs MINIBUFFER
;;
;; Two major ways completion is used in Emacs is either through a
;; minibuffer query, or via completion in a normal editing buffer,
;; encompassing some small range of characters.
;;
;; Structure for both types of completion are provided here.
;; `semantic-complete-read-tag-engine' will use the minibuffer.
;; `semantic-complete-inline-tag-engine' will complete text in
;; a buffer.

(require 'semantic)
(require 'eieio-opt)
(require 'semantic/analyze)
(require 'semantic/ctxt)
(require 'semantic/decorate)
(require 'semantic/format)
(require 'semantic/idle)

(eval-when-compile
  ;; For the semantic-find-tags-for-completion macro.
  (require 'semantic/find))
(require 'semantic/db-find)          ;For type semanticdb-find-result-with-nil.

;;; Code:

(defvar semantic-complete-inline-overlay nil
  "The overlay currently active while completing inline.")

(defun semantic-completion-inline-active-p ()
  "Non-nil if inline completion is active."
  (when (and semantic-complete-inline-overlay
	     (not (overlay-buffer semantic-complete-inline-overlay)))
    (delete-overlay semantic-complete-inline-overlay)
    (setq semantic-complete-inline-overlay nil))
  semantic-complete-inline-overlay)

;;; ------------------------------------------------------------
;;; MINIBUFFER or INLINE utils
;;
(defun semantic-completion-text ()
  "Return the text that is currently in the completion buffer.
For a minibuffer prompt, this is the minibuffer text.
For inline completion, this is the text wrapped in the inline completion
overlay."
  (if semantic-complete-inline-overlay
      (semantic-complete-inline-text)
    (minibuffer-contents)))

(defun semantic-completion-delete-text ()
  "Delete the text that is actively being completed.
Presumably if you call this you will insert something new there."
  (if semantic-complete-inline-overlay
      (semantic-complete-inline-delete-text)
    (delete-minibuffer-contents)))

(defun semantic-completion-message (fmt &rest args)
  "Display the string FMT formatted with ARGS at the end of the minibuffer."
  (if semantic-complete-inline-overlay
      (apply #'message fmt args)
    (apply #'message (concat "%s" fmt) (buffer-string) args)))

;;; ------------------------------------------------------------
;;; MINIBUFFER: Option Selection harnesses
;;
(defvar semantic-completion-collector-engine nil
  "The tag collector for the current completion operation.
Value should be an object of a subclass of
`semantic-completion-engine-abstract'.")

(defvar semantic-completion-display-engine nil
  "The tag display engine for the current completion operation.
Value should be a ... what?")

(defvar semantic-complete-key-map
  (let ((km (make-sparse-keymap)))
    (define-key km " " #'semantic-complete-complete-space)
    (define-key km "\t" #'semantic-complete-complete-tab)
    (define-key km "\C-m" #'semantic-complete-done)
    (define-key km "\C-g" #'abort-recursive-edit)
    (define-key km "\M-n" #'next-history-element)
    (define-key km "\M-p" #'previous-history-element)
    (define-key km "\M-v" #'switch-to-completions)
    (define-key km "\C-n" #'next-history-element)
    (define-key km "\C-p" #'previous-history-element)
    ;; Add history navigation
    km)
  "Keymap used while completing across a list of tags.")

(defvar semantic-completion-default-history nil
  "Default history variable for any unhistoried prompt.
Keeps STRINGS only in the history.")

(defvar semantic-complete-active-default)
(defvar semantic-complete-current-matched-tag)

(defun semantic-complete-read-tag-engine (collector displayer prompt
						    default-tag initial-input
						    history)
  "Read a semantic tag, and return a tag for the selection.
Argument COLLECTOR is an object which can be used to calculate
a list of possible hits.  See `semantic-completion-collector-engine'
for details on COLLECTOR.
Argument DISPLAYER is an object used to display a list of possible
completions for a given prefix.  See `semantic-completion-display-engine'
for details on DISPLAYER.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to story the history in."
  (let* ((semantic-completion-collector-engine collector)
	 (semantic-completion-display-engine displayer)
	 (semantic-complete-active-default nil)
	 (semantic-complete-current-matched-tag nil)
	 (default-as-tag (semantic-complete-default-to-tag default-tag))
	 (default-as-string (when (semantic-tag-p default-as-tag)
			      (semantic-tag-name default-as-tag)))
	 )

    (when default-as-string
      ;; Add this to the prompt.
      ;;
      ;; I really want to add a lookup of the symbol in those
      ;; tags available to the collector and only add it if it
      ;; is available as a possibility, but I'm too lazy right
      ;; now.
      ;;

      ;; @todo - move from () to into the editable area
      (if (string-match ":" prompt)
          (setq prompt (format-prompt
                        (substring prompt 0 (match-beginning 0))
                        default-as-string))
        (setq prompt (format-prompt prompt default-as-string))))
    ;;
    ;; Perform the Completion
    ;;
    (unwind-protect
	(read-from-minibuffer prompt
			      initial-input
			      semantic-complete-key-map
			      nil
			      (or history
				  'semantic-completion-default-history)
			      default-tag)
      (semantic-collector-cleanup semantic-completion-collector-engine)
      (semantic-displayer-cleanup semantic-completion-display-engine)
      )
    ;;
    ;; Extract the tag from the completion machinery.
    ;;
    semantic-complete-current-matched-tag
    ))


;;; Util for basic completion prompts
;;

(defvar semantic-complete-active-default nil
  "The current default tag calculated for this prompt.")

(defun semantic-complete-default-to-tag (default)
  "Convert a calculated or passed in DEFAULT into a tag."
  (if (semantic-tag-p default)
      ;; Just return what was passed in.
      (setq semantic-complete-active-default default)
    ;; If none was passed in, guess.
    (if (null default)
	(setq default (semantic-ctxt-current-thing)))
    (if (null default)
	;; Do nothing
	nil
      ;; Turn default into something useful.
      (let ((str
	     (cond
	      ;; Semantic-ctxt-current-symbol will return a list of
	      ;; strings.  Technically, we should use the analyzer to
	      ;; fully extract what we need, but for now, just grab the
	      ;; first string
	      ((and (listp default) (stringp (car default)))
	       (car default))
	      ((stringp default)
	       default)
	      ((symbolp default)
	       (symbol-name default))
	      (t
	       (signal 'wrong-type-argument
		       (list default 'semantic-tag-p)))))
	    (tag nil))
	;; Now that we have that symbol string, look it up using the active
	;; collector.  If we get a match, use it.
	(save-excursion
	  (semantic-collector-calculate-completions
	   semantic-completion-collector-engine
	   str nil))
	;; Do we have the perfect match???
	(let ((ml (semantic-collector-current-exact-match
		   semantic-completion-collector-engine)))
	  (when ml
	    ;; We don't care about uniqueness.  Just guess for convenience
	    (setq tag (semanticdb-find-result-nth-in-buffer ml 0))))
	;; save it
	(setq semantic-complete-active-default tag)
	;; Return it.. .whatever it may be
	tag))))


;;; Prompt Return Value
;;
;; Getting a return value out of this completion prompt is a bit
;; challenging.  The read command returns the string typed in.
;; We need to convert this into a valid tag.  We can exit the minibuffer
;; for different reasons.  If we purposely exit, we must make sure
;; the focused tag is calculated... preferably once.
(defvar semantic-complete-current-matched-tag nil
  "Variable used to pass the tags being matched to the prompt.")



;; Abstract baseclass for any displayer which supports focus

(defclass semantic-displayer-abstract ()
  ((table :type (or null semanticdb-find-result-with-nil)
	  :initform nil
	  :protection :protected
	  :documentation "List of tags this displayer is showing.")
   (last-prefix :type string
		:protection :protected
		:documentation "Prefix associated with slot `table'.")
   )
  "Abstract displayer baseclass.
Manages the display of some number of tags.
Provides the basics for a displayer, including interacting with
a collector, and tracking tables of completion to display."
  :abstract t)

(defclass semantic-displayer-focus-abstract (semantic-displayer-abstract)
  ((focus :type number
	  :protection :protected
	  :documentation "A tag index from `table' which has focus.
Multiple calls to the display function can choose to focus on a
given tag, by highlighting its location.")
   (find-file-focus
    :allocation :class
    :initform nil
    :documentation
    "Non-nil if focusing requires a tag's buffer be in memory.")
   )
  "Abstract displayer supporting `focus'.
A displayer which has the ability to focus in on one tag.
Focusing is a way of differentiating among multiple tags
which have the same name."
  :abstract t)


(defun semantic-complete-current-match ()
  "Calculate a match from the current completion environment.
Save this in our completion variable.  Make sure that variable
is cleared if any other keypress is made.
Return value can be:
  tag - a single tag that has been matched.
  string - a message to show in the minibuffer."
  ;; Query the environment for an active completion.
  (let ((collector semantic-completion-collector-engine)
	(displayer semantic-completion-display-engine)
	(contents (semantic-completion-text))
	matchlist
	answer)
    (if (string= contents "")
	;; The user wants the defaults!
	(setq answer semantic-complete-active-default)
      ;; This forces a full calculation of completion on CR.
      (save-excursion
	(semantic-collector-calculate-completions collector contents nil))
      (semantic-complete-try-completion)
      (cond
       ;; Input match displayer focus entry
       ((setq answer (semantic-displayer-current-focus displayer))
	;; We have answer, continue
	)
       ;; One match from the collector
       ((setq matchlist (semantic-collector-current-exact-match collector))
	(if (= (semanticdb-find-result-length matchlist) 1)
	    (setq answer (semanticdb-find-result-nth-in-buffer matchlist 0))
	  (if (cl-typep displayer 'semantic-displayer-focus-abstract)
	      ;; For focusing displayers, we can claim this is
	      ;; not unique.  Multiple focuses can choose the correct
	      ;; one.
	      (setq answer "Not Unique")
	    ;; If we don't have a focusing displayer, we need to do something
	    ;; graceful.  First, see if all the matches have the same name.
	    (let ((allsame t)
		  (firstname (semantic-tag-name
			      (car
			       (semanticdb-find-result-nth matchlist 0)))
			     )
		  (cnt 1)
		  (max (semanticdb-find-result-length matchlist)))
	      (while (and allsame (< cnt max))
		(if (not (string=
			  firstname
			  (semantic-tag-name
			   (car
			    (semanticdb-find-result-nth matchlist cnt)))))
		    (setq allsame nil))
		(setq cnt (1+ cnt))
		)
	      ;; Now we know if they are all the same.  If they are, just
	      ;; accept the first, otherwise complain.
	      (if allsame
		  (setq answer (semanticdb-find-result-nth-in-buffer
				matchlist 0))
		(setq answer "Not Unique"))
	      ))))
       ;; No match
       (t
	(setq answer "No Match")))
      )
    ;; Set it into our completion target.
    (when (semantic-tag-p answer)
      (setq semantic-complete-current-matched-tag answer)
      ;; Make sure it is up to date by clearing it if the user dares
      ;; to touch the keyboard.
      (add-hook 'pre-command-hook
		(lambda () (setq semantic-complete-current-matched-tag nil)))
      )
    ;; Return it
    answer
    ))


;;; Keybindings
;;
;; Keys are bound to perform completion using our mechanisms.
;; Do that work here.
(defun semantic-complete-done ()
  "Accept the current input."
  (interactive)
  (let ((ans (semantic-complete-current-match)))
    (if (stringp ans)
	(semantic-completion-message (concat " [" ans "]"))
      (exit-minibuffer)))
  )

(defun semantic-complete-complete-space ()
  "Complete the partial input in the minibuffer."
  (interactive)
  (semantic-complete-do-completion t))

(defun semantic-complete-complete-tab ()
  "Complete the partial input in the minibuffer as far as possible."
  (interactive)
  (semantic-complete-do-completion))

;;; Completion Functions
;;
;; Thees routines are functional entry points to performing completion.
;;
(defun semantic-complete-hack-word-boundaries (original new)
  "Return a string to use for completion.
ORIGINAL is the text in the minibuffer.
NEW is the new text to insert into the minibuffer.
Within the difference bounds of ORIGINAL and NEW, shorten NEW
to the nearest word boundary, and return that."
  (save-match-data
    (let* ((diff (substring new (length original)))
	   (end (string-match "\\>" diff))
	   (start (string-match "\\<" diff)))
      (cond
       ((and start (> start 0))
	;; If start is greater than 0, include only the new
	;; white-space stuff
	(concat original (substring diff 0 start)))
       (end
	(concat original (substring diff 0 end)))
       (t new)))))

(defun semantic-complete-try-completion (&optional partial)
  "Try a completion for the current minibuffer.
If PARTIAL, do partial completion stopping at spaces."
  (let ((comp (semantic-collector-try-completion
               semantic-completion-collector-engine
	       (semantic-completion-text))))
    (cond
     ((null comp)
      (semantic-completion-message " [No Match]")
      (ding)
      )
     ((stringp comp)
      (if (string= (semantic-completion-text) comp)
	  (when partial
	    ;; Minibuffer isn't changing AND the text is not unique.
	    ;; Test for partial completion over a word separator character.
	    ;; If there is one available, use that so that SPC can
	    ;; act like a SPC insert key.
	    (let ((newcomp (semantic-collector-current-whitespace-completion
			    semantic-completion-collector-engine)))
	      (when newcomp
		(semantic-completion-delete-text)
		(insert newcomp))
	      ))
	(when partial
	  (let ((orig (semantic-completion-text)))
	    ;; For partial completion, we stop and step over
	    ;; word boundaries.  Use this nifty function to do
	    ;; that calculation for us.
	    (setq comp
		  (semantic-complete-hack-word-boundaries orig comp))))
	;; Do the replacement.
	(semantic-completion-delete-text)
        (insert comp))
      )
     ((and (listp comp) (semantic-tag-p (car comp)))
      (unless (string= (semantic-completion-text)
		       (semantic-tag-name (car comp)))
        ;; A fully unique completion was available.
        (semantic-completion-delete-text)
        (insert (semantic-tag-name (car comp))))
      ;; The match is complete
      (if (= (length comp) 1)
          (semantic-completion-message " [Complete]")
        (semantic-completion-message " [Complete, but not unique]"))
      )
     (t nil))))

(defun semantic-complete-do-completion (&optional partial _inline)
  "Do a completion for the current minibuffer.
If PARTIAL, do partial completion stopping at spaces.
if INLINE, then completion is happening inline in a buffer."
  (let* ((collector semantic-completion-collector-engine)
	 (displayer semantic-completion-display-engine)
	 (contents (semantic-completion-text))
	 (ans nil))

    (save-excursion
      (semantic-collector-calculate-completions collector contents partial))
    (let* ((na (semantic-complete-next-action partial)))
      (cond
       ;; We're all done, but only from a very specific
       ;; area of completion.
       ((eq na 'done)
	(semantic-completion-message " [Complete]")
	(setq ans 'done))
       ;; Perform completion
       ((or (eq na 'complete)
	    (eq na 'complete-whitespace))
	(semantic-complete-try-completion partial)
	(setq ans 'complete))
       ;; We need to display the completions.
       ;; Set the completions into the display engine
       ((or (eq na 'display) (eq na 'displayend))
	(semantic-displayer-set-completions
	 displayer
	 (or
	  ;; For the below - This caused problems for Chong Yidong
	  ;; when experimenting with the completion engine.  I don't
	  ;; remember what the problem was though, and I wasn't sure why
	  ;; the below two lines were there since they obviously added
	  ;; some odd behavior.  -EML
	  ;; (and (not (eq na 'displayend))
	  ;;      (semantic-collector-current-exact-match collector))
	  (semantic-collector-all-completions collector contents))
	 contents)
	;; Ask the displayer to display them.
	(semantic-displayer-show-request displayer))
       ((eq na 'scroll)
	(semantic-displayer-scroll-request displayer)
	)
       ((eq na 'focus)
	(semantic-displayer-focus-next displayer)
	(semantic-displayer-focus-request displayer)
	)
       ((eq na 'empty)
	(semantic-completion-message " [No Match]"))
       (t nil)))
    ans))


;;; ------------------------------------------------------------
;;; INLINE: tag completion harness
;;
;; Unlike the minibuffer, there is no mode nor other traditional
;; means of reading user commands in completion mode.  Instead
;; we use a pre-command-hook to inset in our commands, and to
;; push ourselves out of this mode on alternate keypresses.
(defvar semantic-complete-inline-map
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-i" #'semantic-complete-inline-TAB)
    (define-key km "\M-p" #'semantic-complete-inline-up)
    (define-key km "\M-n" #'semantic-complete-inline-down)
    (define-key km "\C-m" #'semantic-complete-inline-done)
    (define-key km "\C-\M-c" #'semantic-complete-inline-exit)
    (define-key km "\C-g" #'semantic-complete-inline-quit)
    (define-key km "?"
      (lambda () (interactive)
	(describe-variable 'semantic-complete-inline-map)))
    km)
  "Keymap used while performing Semantic inline completion.")

(defface semantic-complete-inline-face
  '((((class color) (background dark))
     (:underline "yellow"))
    (((class color) (background light))
     (:underline "brown")))
  "Face used to show the region being completed inline.
The face is used in `semantic-complete-inline-tag-engine'."
  :group 'semantic-faces)

(defun semantic-complete-inline-text ()
  "Return the text that is being completed inline.
Similar to `minibuffer-contents' when completing in the minibuffer."
  (let ((s (overlay-start semantic-complete-inline-overlay))
	(e (overlay-end semantic-complete-inline-overlay)))
    (if (= s e)
	""
      (buffer-substring-no-properties s e ))))

(defun semantic-complete-inline-delete-text ()
  "Delete the text currently being completed in the current buffer."
  (delete-region
   (overlay-start semantic-complete-inline-overlay)
   (overlay-end semantic-complete-inline-overlay)))

(defun semantic-complete-inline-done ()
  "This completion thing is DONE, OR, insert a newline."
  (interactive)
  (let* ((displayer semantic-completion-display-engine)
	 (tag (semantic-displayer-current-focus displayer)))
    (if tag
	(let ((txt (semantic-completion-text)))
	  (insert (substring (semantic-tag-name tag)
			     (length txt)))
	  (semantic-complete-inline-exit))

      ;; Get whatever binding RET usually has.
      (let ((fcn
	     (condition-case nil
		 (lookup-key (current-active-maps) (this-command-keys))
	       (error
		;; I don't know why, but for some reason the above
		;; throws an error sometimes.
		(lookup-key (current-global-map) (this-command-keys))
		))))
	(when fcn
	  (funcall fcn)))
      )))

(defun semantic-complete-inline-quit ()
  "Quit an inline edit."
  (interactive)
  (semantic-complete-inline-exit)
  (keyboard-quit))

(defun semantic-complete-inline-exit ()
  "Exit inline completion mode."
  (interactive)
  ;; Remove this hook FIRST!
  (remove-hook 'pre-command-hook #'semantic-complete-pre-command-hook)

  (condition-case nil
      (progn
	(when semantic-completion-collector-engine
	  (semantic-collector-cleanup semantic-completion-collector-engine))
	(when semantic-completion-display-engine
	  (semantic-displayer-cleanup semantic-completion-display-engine))

	(when semantic-complete-inline-overlay
	  (let ((wc (overlay-get semantic-complete-inline-overlay
					  'window-config-start))
		(buf (overlay-buffer semantic-complete-inline-overlay))
		)
	    (delete-overlay semantic-complete-inline-overlay)
	    (setq semantic-complete-inline-overlay nil)
	    ;; DON'T restore the window configuration if we just
	    ;; switched windows!
	    (when (eq buf (current-buffer))
	      (set-window-configuration wc))
	    ))

	(setq semantic-completion-collector-engine nil
	      semantic-completion-display-engine nil))
    (error nil))

  ;; Remove this hook LAST!!!
  ;; This will force us back through this function if there was
  ;; some sort of error above.
  (remove-hook 'post-command-hook #'semantic-complete-post-command-hook)

  ;;(message "Exiting inline completion.")
  )

(defun semantic-complete-pre-command-hook ()
  "Used to redefine what commands are being run while completing.
When installed as a `pre-command-hook' the special keymap
`semantic-complete-inline-map' is queried to replace commands normally run.
Commands which edit what is in the region of interest operate normally.
Commands which would take us out of the region of interest, or our
quit hook, will exit this completion mode."
  (let ((fcn (lookup-key semantic-complete-inline-map
			 (this-command-keys) nil)))
    (cond ((commandp fcn)
	   (setq this-command fcn))
	  (t nil)))
  )

(defun semantic-complete-post-command-hook ()
  "Used to determine if we need to exit inline completion mode.
If completion mode is active, check to see if we are within
the bounds of `semantic-complete-inline-overlay', or within
a reasonable distance."
  (condition-case nil
      ;; Exit if something bad happened.
      (if (not semantic-complete-inline-overlay)
	  (progn
	    ;;(message "Inline Hook installed, but overlay deleted.")
	    (semantic-complete-inline-exit))
	;; Exit if commands caused us to exit the area of interest
	(let ((os (overlay-get semantic-complete-inline-overlay 'semantic-original-start))
	      (s (overlay-start semantic-complete-inline-overlay))
	      (e (overlay-end semantic-complete-inline-overlay))
	      (b (overlay-buffer semantic-complete-inline-overlay))
	      (txt nil)
	      )
	  (cond
	   ;; EXIT when we are no longer in a good place.
	   ((or (not (eq b (current-buffer)))
		(< (point) s)
		(< (point) os)
		(> (point) e)
		)
	    ;;(message "Exit: %S %S %S" s e (point))
	    (semantic-complete-inline-exit)
	    )
	   ;; Exit if the user typed in a character that is not part
	   ;; of the symbol being completed.
	   ((and (setq txt (semantic-completion-text))
		 (not (string= txt ""))
		 (and (/= (point) s)
		      (save-excursion
			(forward-char -1)
			(not (looking-at "\\(\\w\\|\\s_\\)")))))
	    ;;(message "Non symbol character.")
	    (semantic-complete-inline-exit))
	   ((lookup-key semantic-complete-inline-map
			(this-command-keys) nil)
	    ;; If the last command was one of our completion commands,
	    ;; then do nothing.
	    nil
	    )
	   (t
	    ;; Else, show completions now
	    (semantic-complete-inline-force-display)
	    ))))
    ;; If something goes terribly wrong, clean up after ourselves.
    (error (semantic-complete-inline-exit))))

(defun semantic-complete-inline-force-display ()
  "Force the display of whatever the current completions are.
DO NOT CALL THIS IF THE INLINE COMPLETION ENGINE IS NOT ACTIVE."
  (condition-case e
      (save-excursion
	(let ((collector semantic-completion-collector-engine)
	      (displayer semantic-completion-display-engine)
	      (contents (semantic-completion-text)))
	  (when collector
	    (semantic-collector-calculate-completions
	     collector contents nil)
	    (semantic-displayer-set-completions
	     displayer
	     (semantic-collector-all-completions collector contents)
	     contents)
	    ;; Ask the displayer to display them.
	    (semantic-displayer-show-request displayer))
	  ))
    (error (message "Bug Showing Completions: %S" e))))

(defun semantic-complete-inline-tag-engine
  (collector displayer buffer start end)
  "Perform completion based on semantic tags in a buffer.
Argument COLLECTOR is an object which can be used to calculate
a list of possible hits.  See `semantic-completion-collector-engine'
for details on COLLECTOR.
Argument DISPLAYER is an object used to display a list of possible
completions for a given prefix.  See `semantic-completion-display-engine'
for details on DISPLAYER.
BUFFER is the buffer in which completion will take place.
START is a location for the start of the full symbol.
If the symbol being completed is \"foo.ba\", then START
is on the \"f\" character.
END is at the end of the current symbol being completed."
  ;; Set us up for doing completion
  (setq semantic-completion-collector-engine collector
	semantic-completion-display-engine displayer)
  ;; Create an overlay
  (setq semantic-complete-inline-overlay
	(make-overlay start end buffer nil t))
  (overlay-put semantic-complete-inline-overlay
	       'face
	       'semantic-complete-inline-face)
  (overlay-put semantic-complete-inline-overlay
	       'window-config-start
	       (current-window-configuration))
  ;; Save the original start.  We need to exit completion if START
  ;; moves.
  (overlay-put semantic-complete-inline-overlay
	       'semantic-original-start start)
  ;; Install our command hooks
  (add-hook 'pre-command-hook #'semantic-complete-pre-command-hook)
  (add-hook 'post-command-hook #'semantic-complete-post-command-hook)
  ;; Go!
  (semantic-complete-inline-force-display)
  )

;;; Inline Completion Keymap Functions
;;
(defun semantic-complete-inline-TAB ()
  "Perform inline completion."
  (interactive)
  (let ((cmpl (semantic-complete-do-completion nil t)))
    (cond
     ((eq cmpl 'complete)
      (semantic-complete-inline-force-display))
     ((eq cmpl 'done)
      (semantic-complete-inline-done))
     ))
  )

(defun semantic-complete-inline-down()
  "Focus forwards through the displayer."
  (interactive)
  (let ((displayer semantic-completion-display-engine))
    (semantic-displayer-focus-next    displayer)
    (semantic-displayer-focus-request displayer)
    ))

(defun semantic-complete-inline-up ()
  "Focus backwards through the displayer."
  (interactive)
  (let ((displayer semantic-completion-display-engine))
    (semantic-displayer-focus-previous displayer)
    (semantic-displayer-focus-request  displayer)
    ))


;;; ------------------------------------------------------------
;;; Interactions between collection and displaying
;;
;; Functional routines used to help collectors communicate with
;; the current displayer, or for the previous section.

(defun semantic-complete-next-action (partial)
  "Determine what the next completion action should be.
PARTIAL is non-nil if we are doing partial completion.
First, the collector can determine if we should perform a completion or not.
If there is nothing to complete, then the displayer determines if we are
to show a completion list, scroll, or perhaps do a focus (if it is capable.)
Expected return values are:
  done -> We have a singular match
  empty -> There are no matches to the current text
  complete -> Perform a completion action
  complete-whitespace -> Complete next whitespace type character.
  display -> Show the list of completions
  scroll -> The completions have been shown, and the user keeps hitting
            the complete button.  If possible, scroll the completions
  focus -> The displayer knows how to shift focus among possible completions.
           Let it do that.
  displayend -> Whatever options the displayer had for repeating options, there
           are none left.  Try something new."
  (let ((ans1 (semantic-collector-next-action
		semantic-completion-collector-engine
		partial))
	(ans2 (semantic-displayer-next-action
		semantic-completion-display-engine))
	)
    (cond
     ;; No collector answer, use displayer answer.
     ((not ans1)
      ans2)
     ;; Displayer selection of 'scroll, 'display, or 'focus trumps
     ;; 'done
     ((and (eq ans1 'done) ans2)
      ans2)
     ;; Use ans1 when we have it.
     (t
      ans1))))



;;; ------------------------------------------------------------
;;; Collection Engines
;;
;; Collection engines can scan tags from the current environment and
;; provide lists of possible completions.
;;
;; General features of the abstract collector:
;; * Cache completion lists between uses
;; * Cache itself per buffer.  Handle reparse hooks
;;
;; Key Interface Functions to implement:
;; * semantic-collector-next-action
;; * semantic-collector-calculate-completions
;; * semantic-collector-try-completion
;; * semantic-collector-all-completions

(defvar-local semantic-collector-per-buffer-list nil
  "List of collectors active in this buffer.")

(defvar semantic-collector-list nil
  "List of global collectors active this session.")

(defclass semantic-collector-abstract ()
  ((buffer :initarg :buffer
	   :type buffer
	   :documentation "Originating buffer for this collector.
Some collectors use a given buffer as a starting place while looking up
tags.")
   (cache :initform nil
	  :type (or null semanticdb-find-result-with-nil)
	  :documentation "Cache of tags.
These tags are reused during a completion session.
Sometimes these tags are cached between completion sessions.")
   (last-all-completions :initarg nil
			 :type semanticdb-find-result-with-nil
			 :documentation "Last result of `all-completions'.
This result can be used for refined completions as `last-prefix' gets
closer to a specific result.")
   (last-prefix :type string
		:protection :protected
		:documentation "The last queried prefix.
This prefix can be used to cache intermediate completion offers.
making the action of homing in on a token faster.")
   (last-completion :type (or null string)
		    :documentation "The last calculated completion.
This completion is calculated and saved for future use.")
   (last-whitespace-completion :type (or null string)
			       :documentation "The last whitespace completion.
For partial completion, SPC will disambiguate over whitespace type
characters.  This is the last calculated version.")
   (current-exact-match :type list
			:protection :protected
			:documentation "The list of matched tags.
When tokens are matched, they are added to this list.")
   )
  "Root class for completion engines.
The baseclass provides basic functionality for interacting with
a completion displayer object, and tracking the current progress
of a completion."
  :abstract t)

;;; Smart completion collector
(defclass semantic-collector-analyze-completions (semantic-collector-abstract)
  ((context :initarg :context
	    :type semantic-analyze-context
	    :documentation "An analysis context.
Specifies some context location from whence completion lists will be drawn."
	    )
   (first-pass-completions :type list
			   :documentation "List of valid completion tags.
This list of tags is generated when completion starts.  All searches
derive from this list.")
   )
  "Completion engine that uses the context analyzer to provide options.
The only options available for completion are those which can be logically
inserted into the current context.")

(cl-defmethod semantic-collector-calculate-completions-raw
  ((obj semantic-collector-analyze-completions) prefix _completionlist)
  "Calculate the completions for prefix from COMPLETIONLIST."
  ;; if there are no completions yet, calculate them.
  (if (not (slot-boundp obj 'first-pass-completions))
      (oset obj first-pass-completions
	    (semantic-analyze-possible-completions (oref obj context))))
  ;; search our cached completion list.  make it look like a semanticdb
  ;; results type.
  (list (cons (with-current-buffer (oref (oref obj context) buffer)
		semanticdb-current-table)
	      (semantic-find-tags-for-completion
	       prefix
	       (oref obj first-pass-completions)))))

(cl-defmethod semantic-collector-cleanup ((_obj semantic-collector-abstract))
  "Clean up any mess this collector may have."
  nil)

(cl-defmethod semantic-collector-next-action
  ((obj semantic-collector-abstract) partial)
  "What should we do next?  OBJ can be used to determine the next action.
PARTIAL indicates if we are doing a partial completion."
  (if (and (slot-boundp obj 'last-completion)
	   (string= (semantic-completion-text) (oref obj last-completion)))
      (let* ((cem (semantic-collector-current-exact-match obj))
	     (cemlen (semanticdb-find-result-length cem))
	     (cac (semantic-collector-all-completions
		   obj (semantic-completion-text)))
	     (caclen (semanticdb-find-result-length cac)))
	(cond ((and cem (= cemlen 1)
		    cac (> caclen 1)
		    (eq last-command this-command))
	       ;; Defer to the displayer...
	       nil)
	      ((and cem (= cemlen 1))
	       'done)
	      ((and (not cem) (not cac))
	       'empty)
	      ((and partial (semantic-collector-try-completion-whitespace
			     obj (semantic-completion-text)))
	       'complete-whitespace)))
    'complete))

(cl-defmethod semantic-collector-last-prefix= ((obj semantic-collector-abstract)
					    last-prefix)
  "Return non-nil if OBJ's prefix matches PREFIX."
  (and (slot-boundp obj 'last-prefix)
       (string= (oref obj last-prefix) last-prefix)))

(cl-defmethod semantic-collector-get-cache ((obj semantic-collector-abstract))
  "Get the raw cache of tags for completion.
Calculate the cache if there isn't one."
  (or (oref obj cache)
      (semantic-collector-calculate-cache obj)))

(cl-defmethod semantic-collector-calculate-completions-raw
  ((obj semantic-collector-abstract) prefix completionlist)
  "Calculate the completions for prefix from completionlist.
Output must be in semanticdb Find result format."
  ;; Must output in semanticdb format
  (unless completionlist
    (setq completionlist
	  (or (oref obj cache)
	      (semantic-collector-calculate-cache obj))))
  (let ((table (with-current-buffer (oref obj buffer)
		 semanticdb-current-table))
	(result (semantic-find-tags-for-completion
		 prefix
		 ;; To do this kind of search with a pre-built completion
		 ;; list, we need to strip it first.
		 (semanticdb-strip-find-results completionlist))))
    (if result
	(list (cons table result)))))

(cl-defmethod semantic-collector-calculate-completions
  ((obj semantic-collector-abstract) prefix _partial)
  "Calculate completions for prefix as setup for other queries."
  (let* ((case-fold-search semantic-case-fold)
	 (same-prefix-p (semantic-collector-last-prefix= obj prefix))
	 (last-prefix (and (slot-boundp obj 'last-prefix)
			   (oref obj last-prefix)))
	 (completionlist
	  (cond ((or same-prefix-p
		     (and last-prefix (string-prefix-p last-prefix prefix t)))
		 ;; We have the same prefix, or last-prefix is a
		 ;; substring of the of new prefix, in which case we are
		 ;; refining our symbol so just reuse cache.
		 (oref obj last-all-completions))
		((and last-prefix
		      (> (length prefix) 1)
		      (string-prefix-p prefix last-prefix t))
		   ;; The new prefix is a substring of the old
		   ;; prefix, and it's longer than one character.
		   ;; Perform a full search to pull in additional
		   ;; matches.
		 (let ((context (semantic-analyze-current-context (point))))
		   ;; Set new context and make first-pass-completions
		   ;; unbound so that they are newly calculated.
		   (oset obj context context)
		   (when (slot-boundp obj 'first-pass-completions)
		     (slot-makeunbound obj 'first-pass-completions)))
		 nil)))
	 ;; Get the result
	 (answer (if same-prefix-p
		     completionlist
		   (semantic-collector-calculate-completions-raw
		    obj prefix completionlist)))
	 (completion nil)
	 (complete-not-uniq nil)
	 )
    ;;(semanticdb-find-result-test answer)
    (when (not same-prefix-p)
      ;; Save results if it is interesting and beneficial
      (oset obj last-prefix prefix)
      (oset obj last-all-completions answer))
    ;; Now calculate the completion.
    (setq completion (try-completion
		      prefix
		      (semanticdb-strip-find-results answer)))
    (oset obj last-whitespace-completion nil)
    (oset obj current-exact-match nil)
    ;; Only do this if a completion was found.  Letting a nil in
    ;; could cause a full semanticdb search by accident.
    (when completion
      (oset obj last-completion
	    (cond
	     ;; Unique match in AC.  Last completion is a match.
	     ;; Also set the current-exact-match.
	     ((eq completion t)
	      (oset obj current-exact-match answer)
	      prefix)
	     ;; It may be complete (a symbol) but still not unique.
	     ;; We can capture a match
	     ((setq complete-not-uniq
		    (semanticdb-find-tags-by-name
		     prefix
		     answer))
	      (oset obj current-exact-match
		    complete-not-uniq)
	      prefix
	      )
	     ;; Non unique match, return the string that handles
	     ;; completion
	     (t (or completion prefix))
	     )))
    ))

(cl-defmethod semantic-collector-try-completion-whitespace
  ((obj semantic-collector-abstract) prefix)
  "For OBJ, do whitespace completion based on PREFIX.
This implies that if there are two completions, one matching
the test \"prefix\\>\", and one not, the one matching the full
word version of PREFIX will be chosen, and that text returned.
This function requires that `semantic-collector-calculate-completions'
has been run first."
  (let* ((ac (semantic-collector-all-completions obj prefix))
	 (matchme (concat "^" prefix "\\>"))
	 (compare (semanticdb-find-tags-by-name-regexp matchme ac))
	 (numtag (semanticdb-find-result-length compare))
	 )
    (if compare
	(let* ((idx 0)
	       (cutlen (1+ (length prefix)))
	       (twws (semanticdb-find-result-nth compare idx)))
	  ;; Is our tag with whitespace a match that has whitespace
	  ;; after it, or just an already complete symbol?
	  (while (and (< idx numtag)
		      (< (length (semantic-tag-name (car twws))) cutlen))
	    (setq idx (1+ idx)
		  twws (semanticdb-find-result-nth compare idx)))
	  (when (and twws (car-safe twws))
	    ;; If COMPARE has succeeded, then we should take the very
	    ;; first match, and extend prefix by one character.
	    (oset obj last-whitespace-completion
		  (substring (semantic-tag-name (car twws))
			     0 cutlen))))
      )))


(cl-defmethod semantic-collector-current-exact-match ((obj semantic-collector-abstract))
  "Return the active valid MATCH from the semantic collector.
For now, just return the first element from our list of available
matches.  For semanticdb based results, make sure the file is loaded
into a buffer."
  (when (slot-boundp obj 'current-exact-match)
    (oref obj current-exact-match)))

(cl-defmethod semantic-collector-current-whitespace-completion ((obj semantic-collector-abstract))
  "Return the active whitespace completion value."
  (when (slot-boundp obj 'last-whitespace-completion)
    (oref obj last-whitespace-completion)))

(cl-defmethod semantic-collector-get-match ((obj semantic-collector-abstract))
  "Return the active valid MATCH from the semantic collector.
For now, just return the first element from our list of available
matches.  For semanticdb based results, make sure the file is loaded
into a buffer."
  (when (slot-boundp obj 'current-exact-match)
    (semanticdb-find-result-nth-in-buffer (oref obj current-exact-match) 0)))

(cl-defmethod semantic-collector-all-completions
  ((obj semantic-collector-abstract) _prefix)
  "For OBJ, retrieve all completions matching PREFIX.
The returned list consists of all the tags currently
matching PREFIX."
  (when (slot-boundp obj 'last-all-completions)
    (oref obj last-all-completions)))

(cl-defmethod semantic-collector-try-completion
  ((obj semantic-collector-abstract) _prefix)
  "For OBJ, attempt to match PREFIX.
See `try-completion' for details on how this works.
Return nil for no match.
Return a string for a partial match.
For a unique match of PREFIX, return the list of all tags
with that name."
  (if (slot-boundp obj 'last-completion)
      (oref obj last-completion)))

(cl-defmethod semantic-collector-calculate-cache
  ((_obj semantic-collector-abstract))
  "Calculate the completion cache for OBJ."
  nil
  )

(cl-defmethod semantic-collector-flush ((this semantic-collector-abstract))
  "Flush THIS collector object, clearing any caches and prefix."
  (oset this cache nil)
  (slot-makeunbound this 'last-prefix)
  (slot-makeunbound this 'last-completion)
  (slot-makeunbound this 'last-all-completions)
  (slot-makeunbound this 'current-exact-match)
  )

;;; PER BUFFER
;;
(defclass semantic-collector-buffer-abstract (semantic-collector-abstract)
  ()
  "Root class for per-buffer completion engines.
These collectors track themselves on a per-buffer basis."
  :abstract t)

(cl-defmethod make-instance ((this (subclass semantic-collector-buffer-abstract))
			     &rest _args)
  "Reuse previously created objects of this type in buffer."
  (let ((old nil)
	(bl semantic-collector-per-buffer-list))
    (while (and bl (null old))
      (if (eq (eieio-object-class (car bl)) this)
	  (setq old (car bl))))
    (unless old
      (let ((new (cl-call-next-method)))
	(add-to-list 'semantic-collector-per-buffer-list new)
	(setq old new)))
    (slot-makeunbound old 'last-completion)
    (slot-makeunbound old 'last-prefix)
    (slot-makeunbound old 'current-exact-match)
    old))

;; Buffer specific collectors should flush themselves
(defun semantic-collector-buffer-flush (_newcache)
  "Flush all buffer collector objects.
NEWCACHE is the new tag table, but we ignore it."
  (condition-case nil
      (let ((l semantic-collector-per-buffer-list))
	(while l
	  (if (car l) (semantic-collector-flush (car l)))
	  (setq l (cdr l))))
    (error nil)))

(add-hook 'semantic-after-toplevel-cache-change-hook
	  #'semantic-collector-buffer-flush)

;;; DEEP BUFFER SPECIFIC COMPLETION
;;
(defclass semantic-collector-buffer-deep
  (semantic-collector-buffer-abstract)
  ()
  "Completion engine for tags in the current buffer.
When searching for a tag, uses semantic deep search functions.
Basics search only in the current buffer.")

(cl-defmethod semantic-collector-calculate-cache
  ((obj semantic-collector-buffer-deep))
  "Calculate the completion cache for OBJ.
Uses `semantic-flatten-tags-table'."
  (oset obj cache
	;; Must create it in SEMANTICDB find format.
	;; ( ( DBTABLE TAG TAG ... ) ... )
	(list
	 (cons semanticdb-current-table
	       (semantic-flatten-tags-table (oref obj buffer))))))

;;; PROJECT SPECIFIC COMPLETION
;;
(defclass semantic-collector-project-abstract (semantic-collector-abstract)
  ((path :initarg :path
	 :initform nil
	 :documentation "List of database tables to search.
At creation time, it can be anything accepted by
`semanticdb-find-translate-path' as a PATH argument.")
   )
  "Root class for project wide completion engines.
Uses semanticdb for searching all tags in the current project."
  :abstract t)

;;; Project Search
(defclass semantic-collector-project (semantic-collector-project-abstract)
  ()
  "Completion engine for tags in a project.")


(cl-defmethod semantic-collector-calculate-completions-raw
  ((obj semantic-collector-project) prefix _completionlist)
  "Calculate the completions for prefix from COMPLETIONLIST."
  (semanticdb-find-tags-for-completion prefix (oref obj path)))

;;; Brutish Project search
(defclass semantic-collector-project-brutish (semantic-collector-project-abstract)
  ()
  "Completion engine for tags in a project.")

(declare-function semanticdb-brute-deep-find-tags-for-completion
		  "semantic/db-find")

(cl-defmethod semantic-collector-calculate-completions-raw
  ((obj semantic-collector-project-brutish) prefix _completionlist)
  "Calculate the completions for prefix from COMPLETIONLIST."
  (require 'semantic/db-find)
  (semanticdb-brute-deep-find-tags-for-completion prefix (oref obj path)))

;;; Current Datatype member search.
(defclass semantic-collector-local-members (semantic-collector-project-abstract)
  ((scope :initform nil
	  :type (or null semantic-scope-cache)
	  :documentation
	  "The scope the local members are being completed from."))
  "Completion engine for tags in a project.")

(cl-defmethod semantic-collector-calculate-completions-raw
  ((obj semantic-collector-local-members) prefix _completionlist)
  "Calculate the completions for prefix from COMPLETIONLIST."
  (let* ((scope (or (oref obj scope)
		    (oset obj scope (semantic-calculate-scope))))
	 (localstuff (oref scope scope)))
    (list
     (cons
      (oref scope table)
      (semantic-find-tags-for-completion prefix localstuff)))))
    ;(semanticdb-brute-deep-find-tags-for-completion prefix (oref obj path))))


;;; ------------------------------------------------------------
;;; Tag List Display Engines
;;
;; A typical displayer accepts a pre-determined list of completions
;; generated by a collector.  This format is in semanticdb search
;; form.  This vaguely standard form is a bit challenging to navigate
;; because the tags do not contain buffer info, but the file associated
;; with the tags precedes the tag in the list.
;;
;; Basic displayers don't care, and can strip the results.
;; Advanced highlighting displayers need to know when they need
;; to load a file so that the tag in question can be highlighted.
;;
;; Key interface methods to a displayer are:
;; * semantic-displayer-next-action
;; * semantic-displayer-set-completions
;; * semantic-displayer-current-focus
;; * semantic-displayer-show-request
;; * semantic-displayer-scroll-request
;; * semantic-displayer-focus-request

(define-obsolete-function-alias 'semantic-displayor-cleanup
  #'semantic-displayer-cleanup "27.1")
(cl-defmethod semantic-displayer-cleanup ((_obj semantic-displayer-abstract))
  "Clean up any mess this displayer may have."
  nil)

(define-obsolete-function-alias 'semantic-displayor-next-action
  #'semantic-displayer-next-action "27.1")
(cl-defmethod semantic-displayer-next-action ((obj semantic-displayer-abstract))
  "The next action to take on the minibuffer related to display."
  (if (and (slot-boundp obj 'last-prefix)
	   (or (eq this-command 'semantic-complete-inline-TAB)
	       (and (string= (oref obj last-prefix) (semantic-completion-text))
		    (eq last-command this-command))))
      'scroll
    'display))

(define-obsolete-function-alias 'semantic-displayor-set-completions
  #'semantic-displayer-set-completions "27.1")
(cl-defmethod semantic-displayer-set-completions ((obj semantic-displayer-abstract)
					       table prefix)
  "Set the list of tags to be completed over to TABLE."
  (oset obj table table)
  (oset obj last-prefix prefix))

(define-obsolete-function-alias 'semantic-displayor-show-request
  #'semantic-displayer-show-request "27.1")
(cl-defmethod semantic-displayer-show-request ((_obj semantic-displayer-abstract))
  "A request to show the current tags table."
  (ding))

(define-obsolete-function-alias 'semantic-displayor-focus-request
  #'semantic-displayer-focus-request "27.1")
(cl-defmethod semantic-displayer-focus-request ((_obj semantic-displayer-abstract))
  "A request to for the displayer to focus on some tag option."
  (ding))

(define-obsolete-function-alias 'semantic-displayor-scroll-request
  #'semantic-displayer-scroll-request "27.1")
(cl-defmethod semantic-displayer-scroll-request ((_obj semantic-displayer-abstract))
  "A request to for the displayer to scroll the completion list (if needed)."
  (scroll-other-window))

(define-obsolete-function-alias 'semantic-displayor-focus-previous
  #'semantic-displayer-focus-previous "27.1")
(cl-defmethod semantic-displayer-focus-previous ((_obj semantic-displayer-abstract))
  "Set the current focus to the previous item."
  nil)

(define-obsolete-function-alias 'semantic-displayor-focus-next
  #'semantic-displayer-focus-next "27.1")
(cl-defmethod semantic-displayer-focus-next ((_obj semantic-displayer-abstract))
  "Set the current focus to the next item."
  nil)

(define-obsolete-function-alias 'semantic-displayor-current-focus
  #'semantic-displayer-current-focus "27.1")
(cl-defmethod semantic-displayer-current-focus ((_obj semantic-displayer-abstract))
  "Return a single tag currently in focus.
This object type doesn't do focus, so will never have a focus object."
  nil)


;; Traditional displayer
(defcustom semantic-completion-displayer-format-tag-function
  #'semantic-format-tag-name
  "A Tag format function to use when showing completions."
  :group 'semantic
  :type semantic-format-tag-custom-list)

(defclass semantic-displayer-traditional (semantic-displayer-abstract)
  ()
  "Display options in *Completions* buffer.
Traditional display mechanism for a list of possible completions.
Completions are shown in a new buffer and listed with the ability
to click on the items to aid in completion.")

(define-obsolete-function-alias 'semantic-displayor-show-request
  #'semantic-displayer-show-request "27.1")
(cl-defmethod semantic-displayer-show-request ((obj semantic-displayer-traditional))
  "A request to show the current tags table."

  ;; NOTE TO SELF.  Find the character to type next, and emphasize it.

  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list
     (mapcar semantic-completion-displayer-format-tag-function
	     (semanticdb-strip-find-results (oref obj table))))
    )
  )

;;; Methods for any displayer which supports focus

(define-obsolete-function-alias 'semantic-displayor-next-action
  #'semantic-displayer-next-action "27.1")
(cl-defmethod semantic-displayer-next-action ((obj semantic-displayer-focus-abstract))
  "The next action to take on the minibuffer related to display."
  (if (and (slot-boundp obj 'last-prefix)
	   (string= (oref obj last-prefix) (semantic-completion-text))
	   (eq last-command this-command))
      (if (and
	   (slot-boundp obj 'focus)
	   (slot-boundp obj 'table)
	   (<= (semanticdb-find-result-length (oref obj table))
	       (1+ (oref obj focus))))
	  ;; We are at the end of the focus road.
	  'displayend
	;; Focus on some item.
	'focus)
    'display))

(define-obsolete-function-alias 'semantic-displayor-set-completions
  #'semantic-displayer-set-completions "27.1")
(cl-defmethod semantic-displayer-set-completions ((obj semantic-displayer-focus-abstract)
					          _table _prefix)
  "Set the list of tags to be completed over to TABLE."
  (cl-call-next-method)
  (slot-makeunbound obj 'focus))

(define-obsolete-function-alias 'semantic-displayor-focus-previous
  #'semantic-displayer-focus-previous "27.1")
(cl-defmethod semantic-displayer-focus-previous ((obj semantic-displayer-focus-abstract))
  "Set the current focus to the previous item.
Not meaningful return value."
  (when (and (slot-boundp obj 'table) (oref obj table))
    (with-slots (table) obj
      (if (or (not (slot-boundp obj 'focus))
	      (<= (oref obj focus) 0))
	  (oset obj focus (1- (semanticdb-find-result-length table)))
	(oset obj focus (1- (oref obj focus)))
	)
      )))

(define-obsolete-function-alias 'semantic-displayor-focus-next
  #'semantic-displayer-focus-next "27.1")
(cl-defmethod semantic-displayer-focus-next ((obj semantic-displayer-focus-abstract))
  "Set the current focus to the next item.
Not meaningful return value."
  (when (and (slot-boundp obj 'table) (oref obj table))
    (with-slots (table) obj
      (if (not (slot-boundp obj 'focus))
	  (oset obj focus 0)
	(oset obj focus (1+ (oref obj focus)))
	)
      (if (<= (semanticdb-find-result-length table) (oref obj focus))
	  (oset obj focus 0))
      )))

(define-obsolete-function-alias 'semantic-displayor-focus-tag
  #'semantic-displayer-focus-tag "27.1")
(cl-defmethod semantic-displayer-focus-tag ((obj semantic-displayer-focus-abstract))
  "Return the next tag OBJ should focus on."
  (when (and (slot-boundp obj 'table) (oref obj table))
    (with-slots (table) obj
      (semanticdb-find-result-nth table (oref obj focus)))))

(define-obsolete-function-alias 'semantic-displayor-current-focus
  #'semantic-displayer-current-focus "27.1")
(cl-defmethod semantic-displayer-current-focus ((obj semantic-displayer-focus-abstract))
  "Return the tag currently in focus, or call parent method."
  (if (and (slot-boundp obj 'focus)
	   (slot-boundp obj 'table)
	   ;; Only return the current focus IFF the minibuffer reflects
	   ;; the list this focus was derived from.
	   (slot-boundp obj 'last-prefix)
	   (string= (semantic-completion-text) (oref obj last-prefix))
	   )
      ;; We need to focus
      (if (oref obj find-file-focus)
	  (semanticdb-find-result-nth-in-buffer (oref obj table) (oref obj focus))
	;; result-nth returns a cons with car being the tag, and cdr the
	;; database.
	(car (semanticdb-find-result-nth (oref obj table) (oref obj focus))))
    ;; Do whatever
    (cl-call-next-method)))

;;; Simple displayer which performs traditional display completion,
;; and also focuses with highlighting.
(defclass semantic-displayer-traditional-with-focus-highlight
  (semantic-displayer-focus-abstract semantic-displayer-traditional)
  ((find-file-focus :initform t))
  "Display completions in *Completions* buffer, with focus highlight.
A traditional displayer which can focus on a tag by showing it.
Same as `semantic-displayer-traditional', but with selection between
multiple tags with the same name done by focusing on the source
location of the different tags to differentiate them.")

(define-obsolete-function-alias 'semantic-displayor-focus-request
  #'semantic-displayer-focus-request "27.1")
(cl-defmethod semantic-displayer-focus-request
  ((obj semantic-displayer-traditional-with-focus-highlight))
  "Focus in on possible tag completions.
Focus is performed by cycling through the tags and highlighting
one in the source buffer."
  (let* ((tablelength (semanticdb-find-result-length (oref obj table)))
	 (focus (semantic-displayer-focus-tag obj))
	 ;; Raw tag info.
	 (rtag (car focus))
	 (rtable (cdr focus))
	 ;; Normalize
	 (nt (semanticdb-normalize-one-tag rtable rtag))
	 (tag (cdr nt))
	 (table (car nt))
	 (curwin (selected-window)))
    ;; If we fail to normalize, reset.
    (when (not tag) (setq table rtable tag rtag))
    ;; Do the focus.
    (let ((buf (or (semantic-tag-buffer tag)
		   (and table (semanticdb-get-buffer table)))))
      ;; If no buffer is provided, then we can make up a summary buffer.
      (when (not buf)
	(with-current-buffer (get-buffer-create "*Completion Focus*")
	  (erase-buffer)
	  (insert "Focus on tag: \n")
	  (insert (semantic-format-tag-summarize tag nil t) "\n\n")
	  (when table
	    (insert "From table: \n")
	    (insert (eieio-object-name table) "\n\n"))
	  (when buf
	    (insert "In buffer: \n\n")
	    (insert (format "%S" buf)))
	  (setq buf (current-buffer))))
      ;; Show the tag in the buffer.
      (if (get-buffer-window buf)
	  (select-window (get-buffer-window buf))
	(switch-to-buffer-other-window buf t)
	(select-window (get-buffer-window buf)))
      ;; Now do some positioning
      (when (semantic-tag-with-position-p tag)
	;; Full tag positional information available
	(goto-char (semantic-tag-start tag))
	;; This avoids a dangerous problem if we just loaded a tag
	;; from a file, but the original position was not updated
	;; in the TAG variable we are currently using.
	(semantic-momentary-highlight-tag (semantic-current-tag)))
      (select-window curwin)
      ;; Calculate text difference between contents and the focus item.
      (let* ((mbc (semantic-completion-text))
	     (ftn (semantic-tag-name tag))
	     (diff (substring ftn (length mbc))))
	(semantic-completion-message
	 (format "%s [%d of %d matches]" diff (1+ (oref obj focus)) tablelength)))
      )))


;;; Tooltip completion lister
;;
;; Written and contributed by Masatake YAMATO <yamato@redhat.com>
;;
;; Modified by Eric Ludlam for
;; * Safe compatibility for tooltip free systems.
;; * Don't use 'avoid package for tooltip positioning.

;;;###autoload
(defcustom semantic-displayer-tooltip-mode 'standard
  "Mode for the tooltip inline completion.

Standard: Show only `semantic-displayer-tooltip-initial-max-tags'
number of completions initially.  Pressing TAB will show the
extended set.

Quiet: Only show completions when we have narrowed all
possibilities down to a maximum of
`semantic-displayer-tooltip-initial-max-tags' tags.  Pressing TAB
multiple times will also show completions.

Verbose: Always show all completions available.

The absolute maximum number of completions for all mode is
determined through `semantic-displayer-tooltip-max-tags'."
  :group 'semantic
  :version "24.3"
  :type '(choice (const :tag "Standard" standard)
		 (const :tag "Quiet" quiet)
		 (const :tag "Verbose" verbose)))

;;;###autoload
(defcustom semantic-displayer-tooltip-initial-max-tags 5
  "Maximum number of tags to be displayed initially.
See doc-string of `semantic-displayer-tooltip-mode' for details."
  :group 'semantic
  :version "24.3"
  :type 'integer)

(defcustom semantic-displayer-tooltip-max-tags 25
  "The maximum number of tags to be displayed.
Maximum number of completions where we have activated the
extended completion list through typing TAB or SPACE multiple
times.  This limit needs to fit on your screen!

Note: If available, customizing this variable increases
`x-max-tooltip-size' to force over-sized tooltips when necessary.
This will not happen if you directly set this variable via `setq'."
  :group 'semantic
  :version "24.3"
  :type 'integer
  :set (lambda (sym var)
         (set-default sym var)
         (when (boundp 'x-max-tooltip-size)
           (if (not (consp x-max-tooltip-size))
               (setq x-max-tooltip-size '(80 . 40)))
           (setcdr x-max-tooltip-size
                   (max (1+ var) (cdr x-max-tooltip-size))))))

(defclass semantic-displayer-tooltip (semantic-displayer-traditional)
  ((mode :initarg :mode
	 :initform
	 (symbol-value 'semantic-displayer-tooltip-mode)
	 :documentation
	 "See `semantic-displayer-tooltip-mode'.")
   (max-tags-initial :initarg max-tags-initial
		     :initform
		     (symbol-value 'semantic-displayer-tooltip-initial-max-tags)
		     :documentation
		     "See `semantic-displayer-tooltip-initial-max-tags'.")
   (typing-count :type integer
		 :initform 0
		 :documentation
		 "Counter holding how many times the user types space or tab continuously before showing tags.")
   (shown        :type boolean
		 :initform nil
		 :documentation
		 "Flag representing whether tooltip has been shown yet.")
   )
  "Display completions options in a tooltip.
Display mechanism using tooltip for a list of possible completions.")

(cl-defmethod initialize-instance :after ((_obj semantic-displayer-tooltip) &rest _args)
  "Make sure we have tooltips required."
  (require 'tooltip))

(defvar tooltip-mode)

(define-obsolete-function-alias 'semantic-displayor-show-request
  #'semantic-displayer-show-request "27.1")
(cl-defmethod semantic-displayer-show-request ((obj semantic-displayer-tooltip))
  "A request to show the current tags table."
  (if (or (not (featurep 'tooltip)) (not tooltip-mode))
      ;; If we cannot use tooltips, then go to the normal mode with
      ;; a traditional completion buffer.
      (cl-call-next-method)
    (let* ((tablelong (semanticdb-strip-find-results (oref obj table)))
	   (table (semantic-unique-tag-table-by-name tablelong))
	   (completions (mapcar semantic-completion-displayer-format-tag-function table))
	   (numcompl (length completions))
	   ;; (typing-count (oref obj typing-count))
	   (mode (oref obj mode))
	   (max-tags (oref obj max-tags-initial))
	   (matchtxt (semantic-completion-text))
	   msg msg-tail)
      ;; Keep a count of the consecutive completion commands entered by the user.
      (oset obj typing-count
	    (if (equal (this-command-keys) "\C-i")
	        (1+ (oref obj typing-count))
	      0))
      (cond
       ((eq mode 'quiet)
	;; Switch back to standard mode if user presses key more than 5 times.
	(when (>= (oref obj typing-count) 5)
	  (oset obj mode 'standard)
	  (setq mode 'standard)
	  (message "Resetting inline-mode to `standard'."))
	(when (and (> numcompl max-tags)
		   (< (oref obj typing-count) 2))
	  ;; Discretely hint at completion availability.
	  (setq msg "...")))
       ((eq mode 'verbose)
	;; Always show extended match set.
	(oset obj max-tags-initial semantic-displayer-tooltip-max-tags)
	(setq max-tags semantic-displayer-tooltip-max-tags)))
      (unless msg
	(oset obj shown t)
	(cond
	 ((> numcompl max-tags)
	  ;; We have too many items, be brave and truncate 'completions'.
	  (setcdr (nthcdr (1- max-tags) completions) nil)
	  (if (= max-tags semantic-displayer-tooltip-initial-max-tags)
	      (setq msg-tail (concat "\n[<TAB> " (number-to-string (- numcompl max-tags)) " more]"))
	    (setq msg-tail (concat "\n[<n/a> " (number-to-string (- numcompl max-tags)) " more]"))
	    (when (>= (oref obj typing-count) 2)
	      (message "Refine search to display results beyond the `%s' limit"
		       (symbol-name 'semantic-complete-inline-max-tags-extended)))))
	 ((= numcompl 1)
	  ;; two possible cases
	  ;; 1. input text != single match - we found a unique completion!
	  ;; 2. input text == single match - we found no additional matches, it's just the input text!
	  (when (string= matchtxt (semantic-tag-name (car table)))
	    (setq msg "[COMPLETE]\n")))
	 ((zerop numcompl)
	  (oset obj shown nil)
	  ;; No matches, say so if in verbose mode!
	  (when semantic-idle-scheduler-verbose-flag
	    (setq msg "[NO MATCH]"))))
	;; Create the tooltip text.
	(setq msg (concat msg (mapconcat #'identity completions "\n"))))
      ;; Add any tail info.
      (setq msg (concat msg msg-tail))
      ;; Display tooltip.
      (when (not (equal msg ""))
	(semantic-displayer-tooltip-show msg)))))

;;; Compatibility
;;

(defun semantic-displayer-point-position ()
  "Return the location of POINT as positioned on the selected frame.
Return a cons cell (X . Y)."
  (let* ((frame (selected-frame))
	 (toolbarleft
	  (if (eq (cdr (assoc 'tool-bar-position default-frame-alist)) 'left)
	      (tool-bar-pixel-width)
	    0))
	 (left (+ (or (car-safe (cdr-safe (frame-parameter frame 'left)))
		      (frame-parameter frame 'left))
		  toolbarleft))
	 (top (or (car-safe (cdr-safe (frame-parameter frame 'top)))
		  (frame-parameter frame 'top)))
	 (point-pix-pos (posn-x-y (posn-at-point)))
	 (edges (window-inside-pixel-edges (selected-window))))
    (cons (+ (car point-pix-pos) (car edges) left)
          (+ (cdr point-pix-pos) (cadr edges) top))))


(defvar tooltip-frame-parameters)
(declare-function tooltip-show "tooltip" (text &optional use-echo-area
                                               text-face default-face))

(defun semantic-displayer-tooltip-show (text)
  "Display a tooltip with TEXT near cursor."
  (let ((point-pix-pos (semantic-displayer-point-position))
	(tooltip-frame-parameters
	 (append tooltip-frame-parameters nil)))
    (push
     (cons 'left (+ (car point-pix-pos) (frame-char-width)))
     tooltip-frame-parameters)
    (push
     (cons 'top (+ (cdr point-pix-pos) (frame-char-height)))
     tooltip-frame-parameters)
    (tooltip-show text)))

(define-obsolete-function-alias 'semantic-displayor-scroll-request
  #'semantic-displayer-scroll-request "27.1")
(cl-defmethod semantic-displayer-scroll-request ((obj semantic-displayer-tooltip))
  "A request to for the displayer to scroll the completion list (if needed)."
  ;; Do scrolling in the tooltip.
  (oset obj max-tags-initial 30)
  (semantic-displayer-show-request obj)
  )

;; End code contributed by Masatake YAMATO <yamato@redhat.com>


;;; Ghost Text displayer
;;
(defclass semantic-displayer-ghost (semantic-displayer-focus-abstract)

  ((ghostoverlay :type overlay
		 :documentation
		 "The overlay the ghost text is displayed in.")
   (first-show :initform t
	       :documentation
	       "Non-nil if we have not seen our first show request.")
   )
  "Cycle completions inline with ghost text.
Completion displayer using ghost chars after point for focus options.
Whichever completion is currently in focus will be displayed as ghost
text using overlay options.")

(define-obsolete-function-alias 'semantic-displayor-next-action
  #'semantic-displayer-next-action "27.1")
(cl-defmethod semantic-displayer-next-action ((obj semantic-displayer-ghost))
  "The next action to take on the inline completion related to display."
  (let ((ans (cl-call-next-method))
	(table (when (slot-boundp obj 'table)
		       (oref obj table))))
    (if (and (eq ans 'displayend)
	     table
	     (= (semanticdb-find-result-length table) 1)
	     )
	nil
      ans)))

(define-obsolete-function-alias 'semantic-displayor-cleanup
  #'semantic-displayer-cleanup "27.1")
(cl-defmethod semantic-displayer-cleanup ((obj semantic-displayer-ghost))
  "Clean up any mess this displayer may have."
  (when (slot-boundp obj 'ghostoverlay)
    (delete-overlay (oref obj ghostoverlay)))
  )

(define-obsolete-function-alias 'semantic-displayor-set-completions
  #'semantic-displayer-set-completions "27.1")
(cl-defmethod semantic-displayer-set-completions ((obj semantic-displayer-ghost)
					          _table _prefix)
  "Set the list of tags to be completed over to TABLE."
  (cl-call-next-method)
  (semantic-displayer-cleanup obj))


(define-obsolete-function-alias 'semantic-displayor-show-request
  #'semantic-displayer-show-request "27.1")
(cl-defmethod semantic-displayer-show-request ((obj semantic-displayer-ghost))
  "A request to show the current tags table."
;  (if (oref obj first-show)
;      (progn
;	(oset obj first-show nil)
	(semantic-displayer-focus-next obj)
	(semantic-displayer-focus-request obj)
;	)
    ;; Only do the traditional thing if the first show request
    ;; has been seen.  Use the first one to start doing the ghost
    ;; text display.
;    (cl-call-next-method)
;    )
)

(define-obsolete-function-alias 'semantic-displayor-focus-request
  #'semantic-displayer-focus-request "27.1")
(cl-defmethod semantic-displayer-focus-request
  ((obj semantic-displayer-ghost))
  "Focus in on possible tag completions.
Focus is performed by cycling through the tags and showing a possible
completion text in ghost text."
  (let* ((tablelength (semanticdb-find-result-length (oref obj table)))
	 (focus (semantic-displayer-focus-tag obj))
	 (tag (car focus))
	 )
    (if (not tag)
	(semantic-completion-message "No tags to focus on.")
      ;; Display the focus completion as ghost text after the current
      ;; inline text.
      (when (or (not (slot-boundp obj 'ghostoverlay))
		(not (overlay-buffer (oref obj ghostoverlay))))
	(oset obj ghostoverlay
	      (make-overlay (point) (1+ (point)) (current-buffer) t)))

      (let* ((lp (semantic-completion-text))
	     (os (substring (semantic-tag-name tag) (length lp)))
	     (ol (oref obj ghostoverlay))
	     )

	(put-text-property 0 (length os) 'face 'region os)

	(overlay-put
	 ol 'display (concat os (buffer-substring (point) (1+ (point)))))
	)
      ;; Calculate text difference between contents and the focus item.
      (let* ((mbc (semantic-completion-text))
	     (ftn (concat (semantic-tag-name tag)))
	     )
	(put-text-property (length mbc) (length ftn) 'face
			   'bold ftn)
	(semantic-completion-message
	 (format "%s [%d of %d matches]" ftn (1+ (oref obj focus)) tablelength)))
      )))


;;; ------------------------------------------------------------
;;; Specific queries
;;
(defvar semantic-complete-inline-custom-type
  (append '(radio)
	  (mapcar
	   (lambda (class)
	     (let* ((C (intern (car class)))
		    (doc (cl--class-docstring (cl--find-class C)))
		    (doc1 (car (split-string doc "\n")))
		    )
	       (list 'const
		     :tag doc1
		     C)))
	   (eieio-build-class-alist 'semantic-displayer-abstract t))
	  )
  "Possible options for inline completion displayers.
Use this to enable custom editing.")

(defcustom semantic-complete-inline-analyzer-displayer-class
  'semantic-displayer-traditional
  "Class for displayer to use with inline completion."
  :group 'semantic
  :type semantic-complete-inline-custom-type
  )

(defun semantic-complete-read-tag-buffer-deep (prompt &optional
						      default-tag
						      initial-input
						      history)
  "Ask for a tag by name from the current buffer.
Available tags are from the current buffer, at any level.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in."
  (semantic-complete-read-tag-engine
   (semantic-collector-buffer-deep :buffer (current-buffer))
   (semantic-displayer-traditional-with-focus-highlight)
   ;;(semantic-displayer-tooltip)
   prompt
   default-tag
   initial-input
   history)
  )

(defun semantic-complete-read-tag-local-members (prompt &optional
							default-tag
							initial-input
							history)
  "Ask for a tag by name from the local type members.
Available tags are from the current scope.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in."
  (semantic-complete-read-tag-engine
   (semantic-collector-local-members :buffer (current-buffer))
   (semantic-displayer-traditional-with-focus-highlight)
   ;;(semantic-displayer-tooltip)
   prompt
   default-tag
   initial-input
   history)
  )

(defun semantic-complete-read-tag-project (prompt &optional
						  default-tag
						  initial-input
						  history)
  "Ask for a tag by name from the current project.
Available tags are from the current project, at the top level.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in."
  (semantic-complete-read-tag-engine
   (semantic-collector-project-brutish :buffer (current-buffer)
				       :path (current-buffer)
				       )
   (semantic-displayer-traditional-with-focus-highlight)
   prompt
   default-tag
   initial-input
   history)
  )

(defun semantic-complete-inline-tag-project ()
  "Complete a symbol name by name from within the current project.
This is similar to `semantic-complete-read-tag-project', except
that the completion interaction is in the buffer where the context
was calculated from.
Customize `semantic-complete-inline-analyzer-displayer-class'
to control how completion options are displayed.
See `semantic-complete-inline-tag-engine' for details on how
completion works."
  (let* ((collector (semantic-collector-project-brutish
		     :buffer (current-buffer)
		     :path (current-buffer)))
	 (sbounds (semantic-ctxt-current-symbol-and-bounds))
	 (syms (car sbounds))
	 (start (car (nth 2 sbounds)))
	 (end (cdr (nth 2 sbounds)))
	 (rsym (reverse syms))
	 (thissym (nth 1 sbounds))
	 (nextsym (car-safe (cdr rsym)))
	 (complst nil))
    (when (and thissym (or (not (string= thissym ""))
			   nextsym))
      ;; Do a quick calculation of completions.
      (semantic-collector-calculate-completions
       collector thissym nil)
      ;; Get the master list
      (setq complst (semanticdb-strip-find-results
		     (semantic-collector-all-completions collector thissym)))
      ;; Shorten by name
      (setq complst (semantic-unique-tag-table-by-name complst))
      (if (or (and (= (length complst) 1)
		   ;; Check to see if it is the same as what is there.
		   ;; if so, we can offer to complete.
		   (let ((compname (semantic-tag-name (car complst))))
		     (not (string= compname thissym))))
	      (> (length complst) 1))
	  ;; There are several options.  Do the completion.
	  (semantic-complete-inline-tag-engine
	   collector
	   (funcall semantic-complete-inline-analyzer-displayer-class)
	   ;;(semantic-displayer-tooltip)
	   (current-buffer)
	   start end))
      )))

(defun semantic-complete-read-tag-analyzer (prompt &optional
						   context
						   history)
  "Ask for a tag by name based on the current context.
The function `semantic-analyze-current-context' is used to
calculate the context.  `semantic-analyze-possible-completions' is used
to generate the list of possible completions.
PROMPT is the first part of the prompt.  Additional prompt
is added based on the contexts full prefix.
CONTEXT is the semantic analyzer context to start with.
HISTORY is a symbol representing a variable to store the history in.
usually a default-tag and initial-input are available for completion
prompts.  these are calculated from the CONTEXT variable passed in."
  (if (not context) (setq context (semantic-analyze-current-context (point))))
  (let* ((syms (semantic-ctxt-current-symbol (point)))
	 (inp (car (reverse syms))))
    (setq syms (nreverse (cdr (nreverse syms))))
    (semantic-complete-read-tag-engine
     (semantic-collector-analyze-completions
      :buffer (oref context buffer)
      :context context)
     (semantic-displayer-traditional-with-focus-highlight)
     (with-current-buffer (oref context buffer)
       (goto-char (cdr (oref context bounds)))
       (concat prompt (mapconcat #'identity syms ".")
	       (if syms "." "")))
     nil
     inp
     history)))

(defun semantic-complete-inline-analyzer (context)
  "Complete a symbol name by name based on the current context.
This is similar to `semantic-complete-read-tag-analyze', except
that the completion interaction is in the buffer where the context
was calculated from.
CONTEXT is the semantic analyzer context to start with.
Customize `semantic-complete-inline-analyzer-displayer-class'
to control how completion options are displayed.

See `semantic-complete-inline-tag-engine' for details on how
completion works."
  (if (not context) (setq context (semantic-analyze-current-context (point))))
  (if (not context) (error "Nothing to complete on here"))
  (let* ((collector (semantic-collector-analyze-completions
		     :buffer (oref context buffer)
		     :context context))
	 (syms (semantic-ctxt-current-symbol (point)))
	 (rsym (reverse syms))
	 (thissym (car rsym))
	 (nextsym (car-safe (cdr rsym)))
	 (complst nil))
    (when (and thissym (or (not (string= thissym ""))
			   nextsym))
      ;; Do a quick calculation of completions.
      (semantic-collector-calculate-completions
       collector thissym nil)
      ;; Get the master list
      (setq complst (semanticdb-strip-find-results
		     (semantic-collector-all-completions collector thissym)))
      ;; Shorten by name
      (setq complst (semantic-unique-tag-table-by-name complst))
      (if (or (and (= (length complst) 1)
		   ;; Check to see if it is the same as what is there.
		   ;; if so, we can offer to complete.
		   (let ((compname (semantic-tag-name (car complst))))
		     (not (string= compname thissym))))
	      (> (length complst) 1))
	  ;; There are several options.  Do the completion.
	  (semantic-complete-inline-tag-engine
	   collector
	   (funcall semantic-complete-inline-analyzer-displayer-class)
	   ;;(semantic-displayer-tooltip)
	   (oref context buffer)
	   (car (oref context bounds))
	   (cdr (oref context bounds))
	   ))
      )))

(defcustom semantic-complete-inline-analyzer-idle-displayer-class
  'semantic-displayer-ghost
  "Class for displayer to use with inline completion at idle time."
  :group 'semantic
  :type semantic-complete-inline-custom-type
  )

(defun semantic-complete-inline-analyzer-idle (context)
  "Complete a symbol name by name based on the current context for idle time.
CONTEXT is the semantic analyzer context to start with.
This function is used from `semantic-idle-completions-mode'.

This is the same as `semantic-complete-inline-analyzer', except that
it uses `semantic-complete-inline-analyzer-idle-displayer-class'
to control how completions are displayed.

See `semantic-complete-inline-tag-engine' for details on how
completion works."
  (let ((semantic-complete-inline-analyzer-displayer-class
	 semantic-complete-inline-analyzer-idle-displayer-class))
    (semantic-complete-inline-analyzer context)
    ))


;;;###autoload
(defun semantic-complete-jump-local ()
  "Jump to a local semantic symbol."
  (interactive)
  (semantic-error-if-unparsed)
  (let ((tag (semantic-complete-read-tag-buffer-deep "Jump to symbol: ")))
    (when (semantic-tag-p tag)
      (push-mark)
      (when (fboundp 'xref-push-marker-stack)
        (xref-push-marker-stack))
      (goto-char (semantic-tag-start tag))
      (semantic-momentary-highlight-tag tag)
      (message "%S: %s "
	       (semantic-tag-class tag)
	       (semantic-tag-name  tag)))))

;;;###autoload
(defun semantic-complete-jump ()
  "Jump to a semantic symbol."
  (interactive)
  (semantic-error-if-unparsed)
  (let* ((tag (semantic-complete-read-tag-project "Jump to symbol: ")))
    (when (semantic-tag-p tag)
      (push-mark)
      (when (fboundp 'xref-push-marker-stack)
        (xref-push-marker-stack))
      (semantic-go-to-tag tag)
      (pop-to-buffer-same-window (current-buffer))
      (semantic-momentary-highlight-tag tag)
      (message "%S: %s "
	       (semantic-tag-class tag)
	       (semantic-tag-name  tag)))))

;;;###autoload
(defun semantic-complete-jump-local-members ()
  "Jump to a semantic symbol."
  (interactive)
  (semantic-error-if-unparsed)
  (let* ((tag (semantic-complete-read-tag-local-members "Jump to symbol: ")))
    (when (semantic-tag-p tag)
      (let ((start (condition-case nil (semantic-tag-start tag)
		     (error nil))))
	(unless start
	  (error "Tag %s has no location" (semantic-format-tag-prototype tag)))
	(push-mark)
        (when (fboundp 'xref-push-marker-stack)
          (xref-push-marker-stack))
	(goto-char start)
	(semantic-momentary-highlight-tag tag)
	(message "%S: %s "
		 (semantic-tag-class tag)
		 (semantic-tag-name  tag))))))

;;;###autoload
(defun semantic-complete-analyze-and-replace ()
  "Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The minibuffer is used to perform the completion.
The result is inserted as a replacement of the text that was there."
  (interactive)
  (let* ((c (semantic-analyze-current-context (point)))
	 (tag (save-excursion (semantic-complete-read-tag-analyzer "" c))))
    ;; Take tag, and replace context bound with its name.
    (goto-char (car (oref c bounds)))
    (delete-region (point) (cdr (oref c bounds)))
    (insert (semantic-tag-name tag))
    (message "%S" (semantic-format-tag-summarize tag))))

;;;###autoload
(defun semantic-complete-analyze-inline ()
  "Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The function returns immediately, leaving the buffer in a mode that
will perform the completion.
Configure `semantic-complete-inline-analyzer-displayer-class' to change
how completion options are displayed."
  (interactive)
  ;; Only do this if we are not already completing something.
  (if (not (semantic-completion-inline-active-p))
      (semantic-complete-inline-analyzer
       (semantic-analyze-current-context (point))))
  ;; Report a message if things didn't startup.
  (if (and (called-interactively-p 'any)
	   (not (semantic-completion-inline-active-p)))
      (message "Inline completion not needed.")
    ;; Since this is most likely bound to something, and not used
    ;; at idle time, throw in a TAB for good measure.
    (semantic-complete-inline-TAB)))

;;;###autoload
(defun semantic-complete-analyze-inline-idle ()
  "Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The function returns immediately, leaving the buffer in a mode that
will perform the completion.
Configure `semantic-complete-inline-analyzer-idle-displayer-class'
to change how completion options are displayed."
  (interactive)
  ;; Only do this if we are not already completing something.
  (if (not (semantic-completion-inline-active-p))
      (semantic-complete-inline-analyzer-idle
       (semantic-analyze-current-context (point))))
  ;; Report a message if things didn't startup.
  (if (and (called-interactively-p 'interactive)
	   (not (semantic-completion-inline-active-p)))
      (message "Inline completion not needed.")))

;;;###autoload
(defun semantic-complete-self-insert (arg)
  "Like `self-insert-command', but does completion afterwards.
ARG is passed to `self-insert-command'.  If ARG is nil,
use `semantic-complete-analyze-inline' to complete."
  (interactive "p")
  ;; If we are already in a completion scenario, exit now, and then start over.
  (semantic-complete-inline-exit)

  ;; Insert the key
  (self-insert-command arg)

  ;; Prepare for doing completion, but exit quickly if there is keyboard
  ;; input.
  (when (save-window-excursion
	  (save-excursion
            ;; FIXME: Use `while-no-input'?
	    (and (not (semantic-exit-on-input 'csi
			(semantic-fetch-tags)
			(semantic-throw-on-input 'csi)
			nil))
		 (= arg 1)
		 (not (semantic-exit-on-input 'csi
			(semantic-analyze-current-context)
			(semantic-throw-on-input 'csi)
			nil)))))
    (condition-case nil
	(semantic-complete-analyze-inline)
      ;; Ignore errors.  Seems likely that we'll get some once in a while.
      (error nil))
    ))

;;;###autoload
(defun semantic-complete-inline-project ()
  "Perform inline completion for any symbol in the current project.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The function returns immediately, leaving the buffer in a mode that
will perform the completion."
  (interactive)
  ;; Only do this if we are not already completing something.
  (if (not (semantic-completion-inline-active-p))
      (semantic-complete-inline-tag-project))
  ;; Report a message if things didn't startup.
  (if (and (called-interactively-p 'interactive)
	   (not (semantic-completion-inline-active-p)))
      (message "Inline completion not needed."))
  )

(provide 'semantic/complete)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/complete"
;; End:

;;; semantic/complete.el ends here
