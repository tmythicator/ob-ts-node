;;; ob-ts-node.el --- Org-Babel support for TypeScript via ts-node -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  Alexandr Timchenko

;; Author: Alexandr Timchenko
;; URL: https://github.com/tmythicator/ob-ts-node
;; Version: 0.3
;; Keywords: languages, tools
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
  :safe #'stringp
  :group 'org-babel)

(defcustom ob-ts-node-tsconfig nil
  "Path to a tsconfig.json used by ob-ts-node by default.
Highly recommended to set for stable, reproducible behavior (see ts-node docs)."
  :type '(choice (const :tag "Disabled" nil)
                 (string :tag "Path"))
  :group 'org-babel)

(defvar org-babel-default-header-args:typescript
  '((:cli-args    . nil)
    (:cli-override . nil)
    (:cli-cmd . nil))
  "Default header arguments for TypeScript Babel blocks.

Recognized keys:
- :cli-args     – additional flags passed to ts-node
- :cli-override – completely override the command line
- :cli-cmd      – alternative command instead of `ob-ts-node-command`.")

(defvar org-babel-default-header-args:ts
  org-babel-default-header-args:typescript)

(defun ob-ts-node--run (body params)
  "Execute BODY as TypeScript code using ts-node with PARAMS."
  (let* ((src (org-babel-temp-file "ts-" ".ts"))
         (args (alist-get :cli-args params))
         (ovr (alist-get :cli-override params))
         (cmd-base (if ob-ts-node-tsconfig
                       (concat ob-ts-node-command " --project " ob-ts-node-tsconfig)
                     ob-ts-node-command))
         (cmd (or (alist-get :cli-cmd params) cmd-base))
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
  "Org-Babel executor for #+begin_src typescript blocks.
Argument BODY is the contents of the source block as a string.
Argument PARAMS is an alist of header arguments controlling execution."
  (ob-ts-node--run body params))

;;;###autoload
(defun org-babel-execute:ts (body params)
  "Org-Babel executor for #+begin_src ts blocks (alias for typescript).
Argument BODY is the contents of the source block as a string.
Argument PARAMS is an alist of header arguments controlling execution."
  (ob-ts-node--run body params))

;;;###autoload
(defun ob-ts-node-setup ()
  "Register TypeScript support for Org Babel and src editing."
  (add-to-list 'org-babel-tangle-lang-exts '("typescript" . "ts"))
  (add-to-list 'org-babel-tangle-lang-exts '("ts" . "ts"))
  (let ((mode (cond
               ((fboundp 'typescript-ts-mode) 'typescript-ts)
               ((fboundp 'typescript-mode)    'typescript)
               (t 'js))))
    (dolist (lang '("typescript" "ts"))
      (add-to-list 'org-src-lang-modes (cons lang mode)))))

(provide 'ob-ts-node)
;;; ob-ts-node.el ends here