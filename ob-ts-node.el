;;; ob-ts-node.el --- org-babel functions for Typescript evaluation with ts-node.

;; Copyright (C) 2020 Alexandr Timchenko

;; Author: Alexandr Timchenko
;; Keywords: literate programming, typescript, REPL
;; Homepage: https://github.com/atimchenko92/ob-ts-node
;; Version: 0.1
;; Package-Requires: ((emacs "24") (org "8.0"))

;;; License:

;; The MIT License (MIT)

;; Copyright (c) 2020 Alexandr Timchenko

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; Org-babel support for evaluating typescript files, based on support of ts-node.
;; See: https://github.com/TypeStrong/ts-node

;;; Requirements:
;; You need to install node.js and ts-node to use this extension.

;;; Code:
(require 'ob)
(require 'typescript-mode)

(add-to-list 'org-babel-tangle-lang-exts '("typescript" . "ts"))
(defvar org-babel-default-header-args:typescript '((:cli-args . nil)
                                                   (:cli-override . nil)))

;;;###autoload
(defun org-babel-execute:typescript (body params)
  "Execute a block of Typescript code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (let* ((source-file (org-babel-temp-file "ts-" ".ts"))
         (args (cdr (assoc :cli-args params)))
         (override-args (cdr (assoc :cli-override params))))
    (with-temp-file source-file (insert body))
    (org-babel-eval (if override-args
                        (format "ts-node %s" override-args)
                      (if args
                          (format "ts-node %s %s"
                                  args
                                  (org-babel-process-file-name source-file))
                        (format "ts-node %s"
                                (org-babel-process-file-name source-file))))
                    "")))


;;;###autoload
(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("typescript" . typescript)))

(provide 'ob-ts-node)
;;; ob-ts-node.el ends here
