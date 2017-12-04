
(defun test_box_around_text (s)
  (save-excursion
    (goto-char (point-min))
    (insert "ABCDE\nABCDE\nABCDE\n")
    (put-text-property 8 11 'font-lock-face `(:box (:line-width ,s)))))

(test_box_around_text 0)
(test_box_around_text 4)
(test_box_around_text -4)
(test_box_around_text '(4 . 4))
(test_box_around_text '(4 . -4))
(test_box_around_text '(-4 . 4))
(test_box_around_text '(-4 . -4))

(test_box_around_text '(4 . 0))
(test_box_around_text '(0 . 4))
