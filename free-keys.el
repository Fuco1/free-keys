;;; free-keys.el --- Show free keybindings for modkeys or prefixes

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Version: 0.1
;; Created: 3rd November 2013
;; Keywords: convenience
;; Package-Requires: ((cl-lib "0.3"))
;; URL: https://github.com/Fuco1/free-keys

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Show free keybindings for modkeys or prefixes. Based on code
;; located here: https://gist.github.com/bjorne/3796607
;;
;; For complete description see https://github.com/Fuco1/free-keys

;;; Code:

(require 'cl-lib)

(defgroup free-keys ()
  "Free keys."
  :group 'convenience)

(defcustom free-keys-modifiers '("" "C" "M" "C-M")
  "List of modifiers that can be used in front of keys."
  :type '(repeat string)
  :group 'free-keys)

(defcustom free-keys-keys "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*()-=[]{};'\\:\"|,./<>?`~"
  "String of keys that can be used as bindings."
  :type 'string
  :group 'free-keys)

(defvar free-keys-mode-map
  (let ((map (make-keymap)))
    (define-key map "b" 'free-keys-change-buffer)
    (define-key map "p" 'free-keys-set-prefix)
    map)
  "Keymap for Free Keys mode.")

(defvar free-keys-original-buffer nil
  "Buffer from which `free-keys' was called.")

(defun free-keys--print-in-columns (key-list &optional columns)
  "Print the KEY-LIST into as many columns as will fit into COLUMNS characters.

The columns are ordered according to variable `free-keys-keys',
advancing down-right.  The margin between each column is 5 characters."
  (setq columns (or columns 80))
  (let* ((len (+ 5 (length (car key-list))))
         (num-of-keys (length key-list))
         (cols (/ columns len))
         (rows (1+ (/ num-of-keys cols)))
         (rem (mod num-of-keys cols))
         (cur-col 0)
         (cur-row 0))
    (dotimes (i num-of-keys)
      (insert (nth
               (+ (* cur-col rows) cur-row (if (> cur-col rem) (- rem cur-col) 0))
               key-list)
              "     ")
      (cl-incf cur-col)
      (when (= cur-col cols)
        (insert "\n")
        (setq cur-col 0)
        (cl-incf cur-row)))))

(defun free-keys-set-prefix (prefix)
  "Change the prefix in current *Free keys* buffer to PREFIX and
update the display."
  (interactive "sPrefix: ")
  (free-keys prefix free-keys-original-buffer))

(defun free-keys-change-buffer (buffer)
  "Change the buffer for which the bindings are displayed to
BUFFER and update the display."
  (interactive "bShow free bindings for buffer: ")
  (free-keys nil (get-buffer-create buffer)))

(defun free-keys-revert-buffer (_ _)
  "Revert the *Free keys* buffer.

This simply calls `free-keys'."
  (free-keys nil free-keys-original-buffer))

;; TODO: split this function into something cleaner.

;;;###autoload
(defun free-keys (&optional prefix buffer)
  "Display free keys in current buffer.

A free key is a key that has no associated key-binding as
determined by function `key-binding'.

By default, keys on `free-keys-keys' list with no prefix sequence
are considered, possibly together with modifier keys from
`free-keys-modifiers'.  You can change the prefix sequence by
hitting 'p' in the *Free keys* buffer.  Prefix is supplied in
format recognized by `kbd', for example \"C-x\"."
  (interactive (list (when current-prefix-arg
                       (read-from-minibuffer "Prefix: "))))
  (setq prefix (or prefix ""))
  (setq free-keys-original-buffer (or buffer (current-buffer)))
  (let ((buf (get-buffer-create "*Free keys*")))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (if (fboundp 'read-only-mode)
          (read-only-mode -1)
        (setq buffer-read-only nil))
      (erase-buffer)
      (insert "Free keys"
              (if (not (equal prefix "")) (format " with prefix %s" prefix) "")
              " in buffer "
              (buffer-name free-keys-original-buffer)
              " (major mode: " (with-current-buffer free-keys-original-buffer (symbol-name major-mode)) ")\n\n")
      (mapc (lambda (modifier)
              (let (empty-keys)
                (mapc (lambda (key)
                        (let* ((key-name
                                (if (not (equal modifier ""))
                                    (concat modifier "-" (char-to-string key))
                                  (char-to-string key)))
                               (full-name
                                (if (and prefix (not (equal prefix ""))) (concat prefix " " key-name) key-name))
                               (binding
                                (with-current-buffer free-keys-original-buffer (key-binding (read-kbd-macro full-name)))))
                          (when (or (not binding)
                                    (eq binding 'undefined))
                            (push full-name empty-keys))))
                      free-keys-keys)
                (let ((len (length empty-keys)))
                  (when (> len 0)
                    (if (not (equal modifier ""))
                        (insert (format "With modifier %s (%d free)\n=========================\n" modifier len))
                      (insert (format "With no modifier (%d free)\n=========================\n" len)))
                    (free-keys--print-in-columns (nreverse empty-keys))
                    (insert "\n\n")))))
            free-keys-modifiers)
      (setq buffer-read-only t)
      (make-local-variable 'buffer-read-only)
      (goto-char 0)
      (free-keys-mode))))

(define-derived-mode free-keys-mode special-mode "Free Keys"
  "Free keys mode.

Display the free keybindings in current buffer.

\\{free-keys-mode-map}"
  (set (make-local-variable 'revert-buffer-function) 'free-keys-revert-buffer)
  (set (make-local-variable 'header-line-format) "Help: (b) change buffer (p) change prefix (q) quit"))

(provide 'free-keys)
;;; free-keys.el ends here
