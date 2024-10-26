;;; header-progress.el --- Header line based progress indicator -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29") (s "1.13.0"))
;; Keywords:
;; URL: https://github.com/lepisma/header-progress.el

;;; Commentary:

;; Header line based progress indicator
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 's)

(defface hp-bar-complete-face
  '((t (:inherit font-lock-warning-face)))
  "Face for text that denotes completed portion of the progress bar.")

(defface hp-bar-remaining-face
  '((t (:inherit font-lock-comment-face)))
  "Face for text that denotes incomplete portion of the progress bar.")

(defcustom hp-bar-complete-char "█"
  "Character to display complete portion of the bar."
  :type 'string)

(defcustom hp-bar-remaining-char "—"
  "Character to display incomplete portion of the bar."
  :type 'string)

(defvar-local hp-last-bar nil
  "Variable to hold the last bar before header progress kicked-in.

This is used for restoring once the progress bar is finished.")

(defvar-local hp-ratio 0
  "Variable that can be set by programs to update header progress
bar. It is a float between 0 to 1.")

(defun hp--build-bar (ratio)
  "Return a header bar string to show the given RATIO."
  (let* ((total (window-width))
         (complete (round (* ratio total))))
    (concat (s-repeat complete (propertize hp-bar-complete-char 'face 'hp-bar-complete-face))
            (s-repeat (- total complete) (propertize hp-bar-remaining-char 'face 'hp-bar-remaining-face)))))

(defun hp-buffer-progress-bar ()
  "Return progress bar string using the point's position in buffer."
  (hp--build-bar (/ (float (line-number-at-pos)) (count-lines (point-min) (point-max)))))

(defun hp-buffer-progress-bar-wide ()
  "Similar to `hp-buffer-progress-bar' but ignores any narrowing in effect."
  (hp--build-bar (/ (float (line-number-at-pos nil t) (count-lines 1 (+ 1 (buffer-size)))))))

(defun hp-progress-bar ()
  "Build a bar to show progress as indicated in the variable HEADER-PROGRESS-RATIO."
  (hp--build-bar hp-ratio))

(defun hp-bar-start (bar-function)
  "Save the original header bar and initialize the header bar with
given BAR-FUNCTION."
  (setq hp-last-bar header-line-format)
  (setq header-line-format `(:eval (,bar-function))))

(defun hp-bar-finish ()
  "Remove the progress bar and restore the original header bar used
in the buffer."
  (setq header-line-format hp-last-bar))

(provide 'header-progress)

;;; header-progress.el ends here
