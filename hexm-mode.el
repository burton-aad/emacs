;;; hexm-mode.el --- change some hexl behavior -*- lexical-binding: t -*-

;;; Code:

(defgroup hexm nil
  "Improvments for the hexl mode."
  :group 'hexl)

(defface hexm-ascii-overlay-face
  '((t (:inherit highlight)))
  "Face for ascii overlay."
  :group 'hexm)

(defvar hexm-hex-ascii-mode nil
  "Define the hex mode : hexadecimal mode if nil, ascii mode otherwise")
(make-variable-buffer-local 'hexm-hex-ascii-mode)

(defun hexm-toggle-ascii-overlay-face (&optional arg)
  "Set the hexl-ascii-overlay face to the specific hexm defined face. Reset it if ARG is negative"
  (let ((arg (if arg arg 1)))
    (when (overlayp hexl-ascii-overlay)
      (if (> arg 0)
          (overlay-put hexl-ascii-overlay 'face 'hexm-ascii-overlay-face)
        (overlay-put hexl-ascii-overlay 'face 'highlight)))))

(defun hexm-follow-overlay-find ()
  "Find and highlight the ASCII/Hex element corresponding to current point.
The overlay position depend on the hexm-hex-ascii-mode"
  (let ((pos (+ (- (point) (current-column))
                (if hexm-hex-ascii-mode
                    (let ((N (* (% (hexl-current-address) 16) 2)))
                      (+ 10 (+ N (/ N (/ hexl-bits 4)))))
                  (+ (hexl-ascii-start-column)
                     (mod (hexl-current-address) 16))))))
    (move-overlay hexl-ascii-overlay pos (1+ pos))
    ))

(defun hexm-address-to-ascii-marker (address)
  "Return buffer ascii position for ADDRESS."
  (interactive "nAddress: ")
  (+ (* (/ address 16) (hexl-line-displen)) ; hexl line no * display length
     (hexl-ascii-start-column) ; offset to the ascii part
     (point-min)               ; base offset (point usually starts at 1, not 0)
     (% address 16)))          ; char offset into ascii display line

(defun hexm-address-to-marker (address)
  "Copied from hexl mode."
  (interactive "nAddress: ")
  (let ((N (* (% address 16) 2)))
    (+ (* (/ address 16) (hexl-line-displen)) ; hexl line no * display length
       10                      ; 10 chars for the "address: " prefix
       (point-min)             ; base offset (point usually starts at 1, not 0)
       (+ N (/ N (/ hexl-bits 4))) )) ) ; char offset into hexl display line

(defun hexm-address-advice (address)
  (if hexm-hex-ascii-mode
      (hexm-address-to-ascii-marker address)
    (hexm-address-to-marker address)))

(defun hexm-switch-mode ()
  "Pass from ascii mode to hex mode and vice-versa."
  (interactive)
  (if hexm-hex-ascii-mode
      (setq hexm-hex-ascii-mode nil)
    (setq hexm-hex-ascii-mode t))
  (hexl-forward-char 0))


(defun hexm-enable-mode (enable)
  "Enable/Disable hexm mode base on ENABLE arg. Positive enable, other disable."
  (hexm-toggle-ascii-overlay-face enable)
  (if (> enable 0)
      (progn
        (advice-add 'hexl-follow-ascii :after 'hexm-toggle-ascii-overlay-face)
        (advice-add 'hexl-address-to-marker :override 'hexm-address-advice)
        (advice-add 'hexl-follow-ascii-find :override 'hexm-follow-overlay-find))
    (advice-remove 'hexl-follow-ascii 'hexm-toggle-ascii-overlay-face)
    (advice-remove 'hexl-address-to-marker 'hexm-address-advice)
    (advice-remove 'hexl-follow-ascii-find 'hexm-follow-overlay-find)))

;;;###autoload
(define-minor-mode hexm-mode
  "Toggle the use of the hexm minor mode.
This mode intend to add some functionality to the hexl-mode in order to
improve its usage.

List of functionality:
- Allow to change to face of the ascii overlay with the hexm-ascii-overlay-face."
  :lighter " hexM"
  :group 'hexm
  :keymap (let ((map hexl-mode-map))
            (define-key map (kbd "TAB") 'hexm-switch-mode)
            map)

  (message (format "hexm mode %S" hexm-mode))
  (if hexm-mode
      (hexm-enable-mode 1)
    (hexm-enable-mode 0)))

(provide 'hexm-mode)
