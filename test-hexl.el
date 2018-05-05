
(require 'hexl)
(load-file "./hexm-mode.el")


(mapconcat (lambda (c) (regexp-quote (string c))) "pqr"
                             "\\(?:\n\\(?:[:a-f0-9]+ \\)+ \\)?")
"p\\(?:
\\(?:[:a-f0-9]+ \\)+ \\)?q\\(?:
\\(?:[:a-f0-9]+ \\)+ \\)?r"


(overlay-get 'hexl-ascii-overlay 'face)

(custom-set-faces
 `(hexm-ascii-overlay-face ((t (:box (:line-width -1 :color ,(face-background 'cursor)))))))

(custom-set-faces
 `(hexm-ascii-overlay-face ((t (:box (:line-width (-1 . -1) :color ,(face-background 'cursor)))))))

(custom-set-faces
 `(hexm-ascii-overlay-face ((t (:inherit highlight)))))


(defun hexm-follow-ascii (&optional arg)
  "Toggle following ASCII in Hexl buffers.
With prefix ARG, turn on following if and only if ARG is positive.
When following is enabled, the ASCII character corresponding to the
element under the point is highlighted.
Customize the variable `hexl-follow-ascii' to disable this feature."
  (interactive "P")
  (let ((on-p (if arg
		  (> (prefix-numeric-value arg) 0)
	       (not hexl-ascii-overlay))))
    (message "TOTOOTOTOTO")
    (if on-p
      ;; turn it on
      (if (not hexl-ascii-overlay)
	  (progn
	    (setq hexl-ascii-overlay (make-overlay 1 1)
                  hexl-follow-ascii t)
            (overlay-put hexl-ascii-overlay 'face 'hexm-ascii-overlay-face)
	    (add-hook 'post-command-hook 'hexl-follow-ascii-find nil t)))
      ;; turn it off
      (if hexl-ascii-overlay
	  (progn
	    (delete-overlay hexl-ascii-overlay)
	    (setq hexl-ascii-overlay nil
                  hexl-follow-ascii nil)
	    (remove-hook 'post-command-hook 'hexl-follow-ascii-find t)
	    )))))


(advice-add 'hexl-follow-ascii :override 'hexm-follow-ascii)
;; (define-advice hexl-follow-ascii (:override LAMBDA-LIST &optional NAME DEPTH) &rest BODY)
