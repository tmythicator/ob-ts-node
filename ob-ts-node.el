;;; ob-ts-node.el --- Org-Babel support for TypeScript via ts-node -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  Alexandr Timchenko

;; Author: Alexandr Timchenko
;; URL: https://github.com/tmythicator/ob-ts-node
;; Version: 0.2
;; Keywords: literate programming, typescript, org-babel, REPL
;; Package-Requires: ((emacs "25.1") (org "8.0"))

;; This file is not part of GNU Emacs.

;;; License:
;;
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Org-Babel support for evaluating TypeScript code blocks using ts-node.
;; See: https://github.com/TypeStrong/ts-node
;;
;; Requirements:
;; - Node.js
;; - ts-node
;;
;; Integration:
;; - Emacs >=29: uses built-in `typescript-ts-mode`
;; - Emacs <29: falls back to built-in `typescript-mode` (lisp/progmodes/typescript.el)
;; - If neither mode is available (non-standard builds): fallback to `js-mode`.
;;
;; Provides:
;; - Execution of `#+begin_src typescript` and `#+begin_src ts` blocks
;; - Automatic tangling to `.ts` files
;;

;;; Code:
(require 'ob)

(defcustom ob-ts-node-command "ts-node"
  "Default command used to run ts-node."
  :type 'string
  :group 'org-babel)

(defvar org-babel-default-header-args:typescript
  '((:cli-args    . nil)   ;; additional flags for ts-node
    (:cli-override . nil)  ;; completely override the command line
    (:cli-cmd . nil))  ;; use an alternative command instead of `ob-ts-node-command`
  "Default header arguments for TypeScript Babel blocks.")

(defun ob-ts-node--run (body params)
  "Execute BODY as TypeScript code using ts-node with PARAMS."
  (let* ((src (org-babel-temp-file "ts-" ".ts"))
         (args (alist-get :cli-args params))
         (ovr (alist-get :cli-override params))
         (cmd (or (alist-get :cli-cmd params) ob-ts-node-command))
         (file (org-babel-process-file-name src)))
    (with-temp-file src (insert body))
    (let ((command
           (cond
            (ovr  (format "%s %s" cmd ovr))
            (args (format "%s %s %s" cmd args file))
            (t    (format "%s %s"    cmd file)))))
      (org-babel-eval command ""))))

;;;###autoload
(defun org-babel-execute:typescript (body params)
  "Org-Babel executor for #+begin_src typescript blocks."
  (ob-ts-node--run body params))

;;;###autoload
(defun org-babel-execute:ts (body params)
  "Org-Babel executor for #+begin_src ts blocks (alias for typescript)."
  (ob-ts-node--run body params))

(with-eval-after-load 'org
  (add-to-list 'org-babel-tangle-lang-exts '("typescript" . "ts"))
  (add-to-list 'org-babel-tangle-lang-exts '("ts"         . "ts"))
  (add-to-list 'org-src-lang-modes
               (cons "typescript"
                     (cond
                      ((fboundp 'typescript-ts-mode) 'typescript-ts)
                      ((fboundp 'typescript-mode)    'typescript)
                      (t 'js))))
  (add-to-list 'org-src-lang-modes
               (cons "ts"
                     (cond
                      ((fboundp 'typescript-ts-mode) 'typescript-ts)
                      ((fboundp 'typescript-mode)    'typescript)
                      (t 'js)))))

(provide 'ob-ts-node)

;;; ob-ts-node.el ends here