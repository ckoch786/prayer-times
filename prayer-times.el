;;; (buffer-name)"prayer-times.el"

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

;; URL Samples:
;; http://www.islamicfinder.org/prayerService.php?country=usa&city=toledo&state=OH&zipcode=43607&latitude=41.6484&longitude=-83.6037&timezone=-5.0&HanfiShafi=1&pmethod=5&fajrTwilight1=&fajrTwilight2=&ishaTwilight=0&ishaInterval=0&dhuhrInterval=1&maghribInterval=1&dayLight=1&page_background=&table_background=&table_lines=&text_color=&link_color=&prayerFajr=&prayerSunrise=&prayerDhuhr=&prayerAsr=&prayerMaghrib=&prayerIsha=&lang=&lookChange=1

;; http://www.islamicfinder.org/prayer_service.php?country=usa&city=cambridge&state=MA&zipcode=&latitude=42.3802&longitude=-71.1347&timezone=-5.0&HanfiShafi=1&pmethod=5&fajrTwilight1=&fajrTwilight2=&ishaTwilight=0&ishaInterval=0&dhuhrInterval=1&maghribInterval=1&dayLight=1&simpleFormat=xml


;;; Code: 
;; Vars
;; TODO use the custom vars from Calendars Lunar calender/ Solar Calender settings

(defconst prayer-times-buffer "*Prayer-Times*"
  "Name of the buffer used for the prayer times.")

(defcustom prayer-times-country "usa"
  "Country")

(defcustom prayer-times-city "toledo"
  "City")

(defcustom prayer-times-state "OH"
  "State abbriviation")

(defcustom prayer-times-zip "43607"
  "Five digit zip code")

(defcustom prayer-times-latitude "41.6484"
  "latitude")

(defcustom prayer-times-longitude "-83.6037"
  "longitude")

(defcustom prayer-times-timezone "-5.0"
  "timezone")

(defcustom prayer-times-url (concat "http://www.islamicfinder.org/prayer_service.php?country=" prayer-times-country
			 "&city=" prayer-times-city
			 "&state=" prayer-times-state
			 "&zipcode=" prayer-times-zip
			 "&latitude=" prayer-times-latitude
			 "&longitude=" prayer-times-longitude
			 "&timezone=" prayer-times-timezone
			 "&HanfiShafi=1&pmethod=5"
			 "&fajrTwilight1=&fajrTwilight2=&ishaTwilight=0&ishaInterval=0"
			 "&dhuhrInterval=1&maghribInterval=1&dayLight=1&simpleFormat=xml")
  "url")

(defcustom prayer-times-prayer-time-xml "~/.prayer-times"
  "Location of prayer times cache")

;; TODO refactor retrieval method
;; TODO refactor display method to allow interchanging of functions (to support different sites)
;; TODO -- OR should we have the data "normalized" to the current xml format?
;; TODO make the layout/display similar to Calendar mode
;; TODO use a calculation method for the prayer times
;; TODO integrate this into calendar, add ability to select a date and get the prayer time for 
;; the selected date respectively
(defun get-prayer-times ()
    (shell-command
     (format (concat "wget -O " prayer-times-prayer-time-xml " '%s'") prayer-times-url)))

(get-prayer-times)

(defun read-prayer-times (file)
"Reads the prayer times xml into memory"
  (with-temp-buffer
    (insert-file-contents file)
    (libxml-parse-xml-region (point-min) (point-max))))

(defmacro format-prayer-times (prayer-time)
  `(insert "\n" (pp-to-string (first ,prayer-time)) " " (pp-to-string (last ,prayer-time))))

(defun format-prayer-times (prayer-time)
  (insert (pp-to-string (first prayer-time)) " " (pp-to-string (last prayer-time))))

(defmacro get-prayer-time (prayer prayer-time-xml)
  `(assoc ,prayer prayer-time-xml))

(defun display-prayer-times ()
  (let ((prayer-time-xml (read-prayer-times prayer-times-prayer-time-xml)))
    (dolist (prayer '(date fajr sunrise dhuhr asr maghrib isha))
      (format-prayer-times
       (get-prayer-time prayer prayer-time-xml)))))

;; TODO display this in a tabledate ("September 26, 2013")
;; fajr ("6:11")
;; sunrise ("7:26")
;; dhuhr ("1:26")
;; asr ("4:47")
;; maghrib ("7:25")
;; isha ("8:41")

;;;###autoload
(defun prayer-times ()
  ""
  (interactive)
  (prayer-times-basic-setup))

(defun prayer-times-basic-setup ()
  ""
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
  ""
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
  





