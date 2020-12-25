;;; ob-ts-node.el --- org-babel support for evaluating typescript code.

;; Copyright (C) 2020 Alexandr Timchenko

;; Author: Alexandr Timchenko
;; Keywords: literate programming, typescript, REPL
;; Homepage: https://github.com/atimchenko92/ob-ts-node
;; Version: 0.1
;; Package-Requires: ((emacs "24") (org "8.0"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-babel support for evaluating typescript code, based on support of ts-node.
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
