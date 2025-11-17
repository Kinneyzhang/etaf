(require 'ert)

(defmacro should-equal (s1 s2)
  `(should (equal ,s1 ,s2)))

(provide 'etaf-ert)
