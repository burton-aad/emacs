
;;; Code:

(defgroup hexm nil
  "Improvments for the hexl mode."
  :group 'hexl)

(defface hexm-ascii-overlay-face
  '((t (:inherit highlight)))
  "Face for ascii overlay."
  :group 'hexm)


(defun hexm-toggle-ascii-overlay-face (&optional arg)
  "Set the hexl-ascii-overlay face to the specific hexm defined face. Reset it if ARG is negative"
  (let ((arg (if arg arg 1)))
    (when (overlayp hexl-ascii-overlay)
      (if (> arg 0)
          (overlay-put hexl-ascii-overlay 'face 'hexm-ascii-overlay-face)
        (overlay-put hexl-ascii-overlay 'face 'highlight)))))


(defun hexm-enable-mode (enable)
  "Enable/Disable hexm mode base on ENABLE arg. Positive enable, other disable."
  (hexm-toggle-ascii-overlay-face enable))

;;;###autoload
(define-minor-mode hexm-mode
  "Toggle the use of the hexm minor mode.
This mode intend to add some functionality to the hexl-mode in order to
improve its usage.

List of functionality:
- Allow to change to face of the ascii overlay with the hexm-ascii-overlay-face."
  :lighter " hexM"
  :group 'hexm

  (message "hexm mode 1")
  (if hexm-mode
      (hexm-enable-mode 1)
    (hexm-enable-mode 0)))

(provide 'hexm-mode)


;; test
(custom-set-faces
 `(hexm-ascii-overlay-face ((t (:box (:line-width -3 :color ,(face-background 'cursor)))))))
