# Wundercal

![logo](/public/wundercal_logo.png?raw=tru)

Flexible calendar export for Wunderlist

## Features

- Separate calendar for each of your lists
- Folders support
- Remainders are exported as alarms
    - Export can be configured to produce several alarms for one reminder
    
## Notes

1. It's my first Plat app also I'm not an experienced Scala developer, so code reviews are welcome :)
2. Application config contains application key for the app that is registered as 'wundercal-test'.   
   For this app Wunderlist authentication will pass only if callback URL is set to `http://localhost:9000`
3. To ran this app on your machine issue `sbt run` in the project root.    
   For more details see Play's and SBT documentation. 
