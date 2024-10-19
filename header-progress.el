;;; header-progress.el --- Header line based progress indicator -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29"))
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

(defface header-progress-bar-complete-face
  '((t (:inherit font-lock-warning-face)))
  "Face for text that denotes completed portion of the progress bar.")

(defface header-progress-bar-remaining-face
  '((t (:inherit font-lock-comment-face)))
  "Face for text that denotes incomplete portion of the progress bar.")

(defun header-progress-build-string ()
  (let* ((ratio (/ (float (line-number-at-pos)) (count-lines (point-min) (point-max))))
         (total (window-width))
         (complete (round (* ratio total))))
    (concat (s-repeat complete (propertize "█" 'face 'header-progress-bar-complete-face))
            (s-repeat (- total complete) (propertize "—" 'face 'header-progress-bar-remaining-face)))))e

(setq header-line-format '(:eval (header-progress-build-string)))
(setq header-line-format nil)

(provide 'header-progress)

;;; header-progress.el ends here
