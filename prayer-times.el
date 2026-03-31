;;; prayer-times.el --- Islamic prayer times application

;; Copyright (C) 2013 Cory Koch

;; Author: Cory Koch <cory.koch@eng.utoledo.edu>
;; Keywords: prayer times, Islam
;; Human-Keywords: prayer times, Islam

;; This file not part of GNU Emacs.

;; License:

;; prayer-times is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; prayer-times is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with prayer-times.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Prayer times currently will get the prayer times for your location
;; for the current date. The goal is to make it behave similar to calendar
;; and also be integrated into calendar. Currently only the current date
;; is displayed but I would like to have it also show the monthly prayer times.
;; As far as integration with calendar goes, I would like to make it so that
;; when you select a date an the calendar that you can hit say p and have it 
;; bring up the prayer times for that date, similar to how you can press a 
;; key combo and have the Islamic date that corresponds to that date displayed

;; Sample of output:
;; date ("September 29, 2013")
;; fajr ("6:15")
;; sunrise ("7:29")
;; dhuhr ("1:25")
;; asr ("4:44")
;; maghrib ("7:20")
;; isha ("8:36")

;;; Installation:
;; Add the following to your init.el
;; (load "~/.emacs.d/prayer-times/prayer-times.el")
;; Then reload your init.el with M-x load-buffer

;;; Setup:
;; See the customization for prayer-times
;; All customization should be straight forward, the only ones that may be a problem are:
;; longitude, latitude and timezone. To get these I would just go to the islamicfinder.org 
;; site and get the prayer times for the week, get the URL and search for the needed values.
;; A sample URL is included below See: URL Samples.
;; I hope eventually make this easier by utilizing the customization from solar.el (part of calendar)


;;; Usage:
;; Once a day perform:
;; M-x prayer-time-get-prayer-times 
;; To update the prayer times cache. I will eventually automate this.
;; To list the prayer times for the day do:
;; M-x prayer-times

;;; TODOs
;; TODO use the custom vars from Calendars Lunar calender/ Solar Calender settings
;; TODO refactor retrieval method
;; TODO refactor display method to allow interchanging of functions (to support different sites)
;; TODO -- OR should we have the data "normalized" to the current JSON format?
;; TODO make the layout/display similar to Calendar mode
;; TODO use a calculation method for the prayer times
;; TODO integrate this into calendar, add ability to select a date and get the prayer time for 
;; the selected date respectively
;; TODO display this in a table date ("September 26, 2013")
;; fajr ("6:11")
;; sunrise ("7:26")
;; dhuhr ("1:26")
;; asr ("4:47")
;; maghrib ("7:25")
;; isha ("8:41")

;;; Code: 
;; Vars
(defconst prayer-times-buffer "*Prayer-Times*"
  "Name of the buffer used for the prayer times.")

;; 16 degree ISNA, 1 is 18 degree time University of Islamic Science, Karachi
(defcustom prayer-times-method "2" 
  "Method of calculation")

;; Shafi, 2 is Hanafi
(defcustom prayer-times-school "1" 
  "School (madhab)")

(defcustom prayer-times-latitude "40.0168996"
  "latitude")

(defcustom prayer-times-longitude "-83.1480781"
  "longitude")

(defcustom prayer-times-api-key "9i165kupa4LdtFOTnCtBe9v76JzEN86lDhG0HOkQG7E3ak50"
  "API key")

(defcustom prayer-times-url (concat "https://islamicapi.com/api/v1/prayer-time/?"
				    "lat=" prayer-times-latitude
				    "&lon=" prayer-times-longitude
				    "&method=" prayer-times-method
				    "&school=" prayer-times-school
				    "&api_key=" prayer-times-api-key
				    )
  "url")

(defcustom prayer-times-prayer-time-json "~/.prayer-times"
  "Location of prayer times cache")

;;;###autoload
(defun prayer-times-get-prayer-times ()
  "Download the prayer times to the cache"
  (interactive)
  (shell-command
   (format (concat "wget -O " prayer-times-prayer-time-json " '%s'") prayer-times-url)))

(defun read-prayer-times (file)
"Reads the prayer times json into memory"
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer)))

(defmacro format-prayer-times (prayer-time)
  "format the prayer times so that they displayed as the name of the prayer on the left
and the time of the prayer on the right."
  `(insert "\n" (pp-to-string (first ,prayer-time)) " " (pp-to-string (last ,prayer-time))))

(defun format-prayer-times (prayer-time)
  (insert (pp-to-string (first prayer-time)) " " (pp-to-string (last prayer-time))))

(defun get-prayer-time (prayer prayer-time-json)
  (gethash prayer prayer-time-json))




(defun get-prayer-time (prayer times-ht)
  "Get a prayer time string from the times hash table.
PRAYER is a string like \"Fajr\", \"Dhuhr\", etc."
  (gethash prayer times-ht))

(defun parse-prayer-times (json-string)
  "Parse the full API response and return the times hash table."
  (let* ((root  (read-prayer-times prayer-times-prayer-time-json))
         (data  (gethash "data" root))
         (times (gethash "times" data)))
    times))




(defun display-prayer-times ()

(let* ((times (parse-prayer-times prayer-times-prayer-time-json)))
  (insert "Fajr:    %s" (get-prayer-time "Fajr"    times))
  (insert "Dhuhr:   %s" (get-prayer-time "Dhuhr"   times))
  (insert "Asr:     %s" (get-prayer-time "Asr"     times))
  (insert "Maghrib: %s" (get-prayer-time "Maghrib" times))
  (insert "Isha:    %s" (get-prayer-time "Isha"    times))))

;;;###autoload
(defun prayer-times ()
  (interactive)
  (prayer-times-basic-setup))

(defun prayer-times-basic-setup ()
  (let ((buff (current-buffer)))
    (set-buffer (get-buffer-create prayer-times-buffer))
    (display-prayer-times)
    (prayer-times-mode)
    (if (window-splittable-p t) (split-window-right))
    (pop-to-buffer prayer-times-buffer)
    ;; Has the window already been split vertically?
    (when (and (not (window-dedicated-p))
	       (window-full-height-p))
      (let ((win (split-window-below)))
	;; In the upper window, show whatever was visible before.
	;; This looks better than using other-buffer.
	(switch-to-buffer buff)
	;; Switch to the lower window with the calendar buffer.
	(select-window win))))
  (prayer-times-generate-window))

(defun prayer-times-generate-window ()
  (let ((in-prayer-time-window (eq (window-buffer (selected-window))
				   (get-buffer prayer-times-buffer))))
  (when in-prayer-time-window
      (if (window-combined-p)
	  ;; Adjust the window to exactly fit the displayed calendar.
	  (fit-window-to-buffer nil nil nil)
	;; For a full height window or a window that is horizontally
	;; combined don't fit height to that of its buffer.
	(set-window-vscroll nil 0))
      (sit-for 0))))

(define-derived-mode prayer-times-mode nil "Prayer Times"
  "A major mode for the prayer-times window.
For a complete description, see the info node `Prayer-times'."
  (setq buffer-read-only t
        buffer-undo-list t
        indent-tabs-mode nil)
  (set (make-local-variable 'scroll-margin) 0))

;;; prayer-times.el ends here


