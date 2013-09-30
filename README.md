prayer-times
============

Islamic prayer times in Emacs

* Commentary:
 Prayer times currently will get the prayer times for your location
 for the current date. The goal is to make it behave similar to calendar
 and also be integrated into calendar. Currently only the current date
 is displayed but I would like to have it also show the monthly prayer times.
 As far as integration with calendar goes, I would like to make it so that
 when you select a date an the calendar that you can hit say p and have it 
 bring up the prayer times for that date, similar to how you can press a 
 key combo and have the Islamic date that corresponds to that date displayed

<br>
<pre>
 Sample of output:

 date ("September 29, 2013")
 fajr ("6:15")
 sunrise ("7:29")
 dhuhr ("1:25")
 asr ("4:44")
 maghrib ("7:20")
 isha ("8:36")
</pre>

* Installation:
 Add the following to your init.el
     (load "~/.emacs.d/prayer-times/prayer-times.el")
 Then reload your init.el with 
 M-x load-buffer


* Setup:
 See the customization for prayer-times
 All customization should be straight forward, the only ones that may be a problem are:
 longitude, latitude and timezone. To get these I would just go to the islamicfinder.org 
 site and get the prayer times for the week, get the URL and search for the needed values.
 A sample URL is included below See: URL Samples.<br>
 I hope eventually make this easier by utilizing the customization from solar.el (part of calendar)


* Usage:
  Once a day perform:
  M-x prayer-time-get-prayer-times 
  To update the prayer times cache. I will eventually automate this.
  To list the prayer times for the day do:
  M-x prayer-times

 URL Samples:
 http://www.islamicfinder.org/prayerService.php?country=usa&city=toledo&state=OH&zipcode=43607&latitude=41.6484&longitude=-83.6037&timezone=-5.0&HanfiShafi=1&pmethod=5&fajrTwilight1=&fajrTwilight2=&ishaTwilight=0&ishaInterval=0&dhuhrInterval=1&maghribInterval=1&dayLight=1&page_background=&table_background=&table_lines=&text_color=&link_color=&prayerFajr=&prayerSunrise=&prayerDhuhr=&prayerAsr=&prayerMaghrib=&prayerIsha=&lang=&lookChange=1

 http://www.islamicfinder.org/prayer_service.php?country=usa&city=cambridge&state=MA&zipcode=&latitude=42.3802&longitude=-71.1347&timezone=-5.0&HanfiShafi=1&pmethod=5&fajrTwilight1=&fajrTwilight2=&ishaTwilight=0&ishaInterval=0&dhuhrInterval=1&maghribInterval=1&dayLight=1&simpleFormat=xml

* TODOs
- [ ] TODO use the custom vars from Calendars Lunar calender/ Solar Calender settings
- [ ] TODO refactor retrieval method
- [ ] TODO refactor display method to allow interchanging of functions (to support different sites)
- [ ] TODO -- OR should we have the data "normalized" to the current XML format?
- [ ] TODO make the layout/display similar to Calendar mode
- [ ] TODO use a calculation method for the prayer times
- [ ] TODO integrate this into calendar, add ability to select a date and get the prayer time for 
- [ ] the selected date respectively
- [ ] TODO display this in a tabledate 
<pre>("September 26, 2013")
 fajr ("6:11")
 sunrise ("7:26")
 dhuhr ("1:26")
 asr ("4:47")
 maghrib ("7:25")
 isha ("8:41")</pre>