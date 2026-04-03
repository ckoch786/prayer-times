prayer-times
============

Islamic prayer times in Emacs

<img width="994" height="984" alt="image" src="https://github.com/user-attachments/assets/b754c424-94c4-454b-8019-c10074eea1f8" />

* Commentary:
 Prayer times currently will get the prayer times for your location
 for the current date. The goal is to make it behave similar to calendar
 and also be integrated into calendar. Currently only the current date
 is displayed but I would like to have it also show the monthly prayer times.
 As far as integration with calendar goes, I would like to make it so that
 when you select a date an the calendar that you can hit say p and have it 
 bring up the prayer times for that date, similar to how you can press a 
 key combo and have the Islamic date that corresponds to that date displayed

* Installation:
 Add the following to your init.el
     (load "~/.emacs.d/prayer-times/prayer-times.el")
 Then reload your init.el with 
 M-x load-buffer

* Setup:
 See the customization for prayer-times
 Get an api key from https://islamicapi.com/doc/prayer-time/ and set the API Key in the Customization

* Usage:
  Once a day perform:
  M-x prayer-time-get-prayer-times 
  To update the prayer times cache.
  To list the prayer times for the day do:
  M-x prayer-times



Completed as part of [Lisp In Summer Projects 2013 contest](https://lispnyc.github.io/lispinsummerprojects/completed-projects.html)
