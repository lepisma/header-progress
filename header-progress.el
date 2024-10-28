;;; header-progress.el --- Header line based progress indicator -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.1.0
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

(defvar-local hp-gradient-patch nil
  "Patch of colored string to be used for indeterminate bar.")

(defvar-local hp-gradient-offset 0
  "Offset to be used for animating the gradient patch.")

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
  "Similar to `hp-buffer-progress-bar' but ignores any narrowing in
effect."
  (hp--build-bar (/ (float (line-number-at-pos nil t)) (count-lines 1 (+ 1 (buffer-size))))))

(defun hp-progress-bar ()
  "Build a bar to show progress as indicated in the variable HEADER-PROGRESS-RATIO."
  (hp--build-bar hp-ratio))

(defun generate-wrapping-gradient-patch (color1 color2 cycle-length)
  (let* ((r1 (nth 0 (color-values color1)))
         (g1 (nth 1 (color-values color1)))
         (b1 (nth 2 (color-values color1)))
         (r2 (nth 0 (color-values color2)))
         (g2 (nth 1 (color-values color2)))
         (b2 (nth 2 (color-values color2)))
         (vector (make-vector cycle-length nil)))
    (dotimes (j cycle-length)
      (let* ((factor (/ (+ 1 (sin (* 2 float-pi (/ j (float cycle-length))))) 2))
             (r (+ r1 (* factor (- r2 r1))))
             (g (+ g1 (* factor (- g2 g1))))
             (b (+ b1 (* factor (- b2 b1))))
             (color (format "#%02x%02x%02x" (/ r 256) (/ g 256) (/ b 256))))
        (aset vector j (propertize "▀" 'face `(:foreground ,color)))))
    (apply 'concat (append vector nil))))

(defun hp--repeat-with-offset (patch total-length offset)
  "Repeat string PATCH across TOTAL-LENGTH with given OFFSET.
The repeated pattern starts at OFFSET within the repeated sequence."
  ;; TODO: This is a buggy implementation of what the function says it does. But
  ;; for our purpose this works since header-line display does truncation
  ;; automatically.
  (let* ((patch-len (length patch))
         (pre-patch (substring patch offset))
         (remaining-length (- total-length (length pre-patch)))
         (repeat-count (+ 2 (/ remaining-length patch-len)))
         (tiles (apply 'concat (make-list repeat-count patch))))
    (concat pre-patch tiles)))

(defun hp-indeterminate-bar ()
  (let ((width (window-width))
        (patch-length (length hp-gradient-patch)))
    (setq hp-gradient-offset (mod (+ 1 hp-gradient-offset) patch-length))
    (hp--repeat-with-offset hp-gradient-patch width hp-gradient-offset)))

(defun hp-bar-start (bar-function)
  "Save the original header bar and initialize the header bar with
given BAR-FUNCTION."
  (setq hp-last-bar header-line-format)
  (setq header-line-format `(:eval (,bar-function))))

(defun hp-bar-finish ()
  "Remove the progress bar and restore the original header bar used
in the buffer."
  (setq header-line-format hp-last-bar))

(define-minor-mode hp-buffer-progress-mode
  "Minor mode to show buffer progress in using header line.

This uses the widened buffer."
  :global nil
  (if hp-buffer-progress-mode
      (hp-bar-start #'hp-buffer-progress-bar-wide)
    (hp-bar-finish)))

(provide 'header-progress)

;;; header-progress.el ends here
