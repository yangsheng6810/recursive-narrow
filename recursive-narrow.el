;;; recursive-narrow.el --- narrow-to-region that operates recursively

;; Copyright (C) 2010 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; URL: http://github.com/nflath/recursive-narrow
;; Version: 20140811.1546
;; X-Original-Version: 20140801.1400
;; X-Original-Version: 1.3

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package defines two functions, recursive-narrow-to-region and
;; recursive-widen that replace the builtin functions narrow-to-region and
;; widen.  These functions operate the same way, except in the case of multiple
;; calls to recursive-narrow-to-region.  In this case, recursive-widen will go
;; to the previous buffer visibility, not make the entire buffer visible.

;;; Installation:

;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;;
;; (require 'recursive-narrow)
;;

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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

;;; Code:

(defgroup recursive-narrow nil
  "This module contains code for recursively narrowing and widening."
  :tag "User interface"
  :group 'recursive-narrow)

(defcustom recursive-narrow-dwim-functions nil
  "Functions to try for narrowing.
These functions do not get any arguments. They should narrow and
return non-nil if applicable."
  :type 'hook
  :group 'recursive-narrow)

;; Functions that need to be advised.
(setq-default recursive-narrow-funcs-to-advise
              '(org-narrow-to-subtree
                org-narrow-to-block
                org-narrow-to-element
                narrow-to-defun
                narrow-to-region
                bibtex-narrow-to-entry
                LaTeX-narrow-to-environment
                TeX-narrow-to-group
                sp-narrow-to-sexp))

(defvar-local recursive-narrow-settings nil "List of buffer visibility settings.")

(defmacro recursive-narrow-save-position (body &optional unchanged)
  "Execute BODY and save the buffer visibility if changed.
Executes UNCHANGED if the buffer visibility has not changed."
  `(let ((previous-settings (cons (point-min) (point-max))))
     ,body
     (if (and (= (point-min) (car previous-settings))
              (= (point-max) (cdr previous-settings)))
         ,unchanged
       ;; We narrowed, so save the information
       (push previous-settings recursive-narrow-settings))))

(defun recursive-narrow-wrapper (func &rest alist)
  "Wrapper function to advise other func calls to narrow-to-region.
Perform exactly the same task, but push the current narrowing to
`recursive-narrow-settings'"
  (if (boundp 'recursive-narrow-previous-settings)
      (apply func alist)
    (let ((recursive-narrow-previous-settings (cons (point-min) (point-max))))
      (apply func alist)
      (unless (and (= (point-min) (car recursive-narrow-previous-settings))
                   (= (point-max) (cdr recursive-narrow-previous-settings)))
        (push recursive-narrow-previous-settings
              recursive-narrow-settings)))))

(advice-add #'narrow-to-defun :around #'recursive-narrow-wrapper)


(defun recursive-narrow-or-widen-dwim ()
  "If the region is active, narrow to that region.
Otherwise, narrow to the current function. If this has no effect,
widen the buffer. You can add more functions to
`recursive-narrow-dwim-functions'."
  (interactive)
  (cond ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        ((run-hook-with-args-until-success 'recursive-narrow-dwim-functions))
        ((derived-mode-p 'prog-mode) (narrow-to-defun))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree)))
   ;; If we don't narrow
  (progn
    (message "Recursive settings: %d" (length recursive-narrow-settings))
    (widen)))

(defun recursive-widen (func)
  "Replacement of widen that will only pop one level of visibility."
  (interactive)
  (let (widen-to)
    (if recursive-narrow-settings
        (progn
          (setq widen-to (pop recursive-narrow-settings))
          ;; shadow `recursive-narrow-settings' to avoid this call to
          ;; `narrow-to-region' to push another narrowing setting
          (let (recursive-narrow-settings)
            (narrow-to-region (car widen-to) (cdr widen-to)))
          (recenter))
      (funcall func))))

;;;###autoload
(define-minor-mode recursive-narrow-mode
  "Make narrow and widen recursive, i.e. if there are multiple
  calls to `narrow-to-region', then calling `widen' will bring
  the previous buffer visibility"
  :global t
  (if recursive-narrow-mode
      (progn
        (advice-add #'widen :around #'recursive-widen)
        (mapc
         (lambda (func)
           (advice-add func :around #'recursive-narrow-wrapper))
         recursive-narrow-funcs-to-advise))
    (advice-remove #'widen #'recursive-widen)
    (mapc
     (lambda (func)
       (advice-remove func #'recursive-narrow-wrapper))
     recursive-narrow-funcs-to-advise)))

(provide 'recursive-narrow)

;;; recursive-narrow.el ends here
