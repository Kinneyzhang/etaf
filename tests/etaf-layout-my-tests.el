(require 'etaf-ert)
(require 'etaf-layout)
(require 'etaf-render)
(require 'etaf-css)
(require 'etaf-tml)

(etaf-css-parse-rule
 ".bg-white{background-color:white}")

(:selector ".bg-white" :declarations ((background-color "white" nil)) :specificity (0 1 0) :source style-tag)

(etaf-css-parse-stylesheet
 ".bg-white{background-color:white}p-10{padding-left:10;padding-right:10 !important;padding-top:10;padding-bottom:10;}")

((:selector ".bg-white" :declarations ((background-color "white" nil)) :specificity (0 1 0) :source style-tag) (:selector "p-10" :declarations ((padding-left "10" nil) (padding-right "10" t) (padding-top "10" nil) (padding-bottom "10" nil)) :specificity (0 0 1) :source style-tag))

(etaf-css-extract-style-tags etaf-dom-tests-dom)
(etaf-css-extract-inline-styles etaf-dom-tests-dom)

((:selector "p#test-p.ml-3" :declarations ((color "red" nil) (padding "2" nil)) :specificity (1 0 0 0) :source inline :node (p ((style . "color:red;padding:2") (class . "ml-3") (id . "test-p")) "Adding custom utilities with" (code ((class . "text-gray-950")) "@utility"))))

(etaf-css-calculate-specificity
 "p#test-p.ml-3")

((:selector "p.ml-3" :declarations ((color "red" nil) (padding "2" nil)) :specificity (1 0 0 0) :source inline :node (p ((style . "color:red;padding:2") (class . "ml-3")) "Adding custom utilities with" (code ((class . "text-gray-950")) "@utility"))))
