

(defun test_box_around_text (s)
  (save-excursion
    (goto-char (point-min))
    (insert "ABCDE\nABCDE\nABCDE\n")
    ;; (put-text-property 8 11 'font-lock-face `(:box (:line-width ,s :color "red")))
    (put-text-property 8 11 'font-lock-face `(:box (:line-width ,s :color "white" :style released-button)))
    ))

(test_box_around_text 0)
(test_box_around_text 4)
(test_box_around_text -4)
(test_box_around_text '(4 . 4))
(test_box_around_text '(4 . -4))
(test_box_around_text '(-4 . 4))
(test_box_around_text '(-4 . -4))
(test_box_around_text '(-1 . -1))

(test_box_around_text '(4 . 0))
(test_box_around_text '(0 . 4))
(test_box_around_text '(1 . 1))
(test_box_around_text '(4 . 1))
(test_box_around_text '(1 . 4))
(test_box_around_text '(-4 . 1))
(test_box_around_text '(1 . -4))

(defun test_image_relief ()
  (save-excursion
    (goto-char (point-min))
    (insert-image
     (create-image
      "splash.svg" nil nil :relief 12))))

(test_image_relief)
