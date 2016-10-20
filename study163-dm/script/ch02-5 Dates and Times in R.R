#Dates and Times in R
#R has developed a special representation of dates and times
#Dates are represented by the Dateclass
#Times are represented using the POSIXct or the POSIXlt class
#Dates are stored internally as the number of days since 1970-01-01
#Times are stored internally as the number of seconds since 1970-01-01

#Dates in R
#Dates are represented by the Date class and can be coerced from a character string using the as.Date() function.
x <-as.Date("1970-01-01")
x
x <-as.Date("2016年01月01",format="%Y年%m月%d")
x
## [1] "1970-01-01"
unclass(x)
## [1] 0
unclass(as.Date("1970-01-02"))
## [1] 1

#Times in R
#Times are represented using the POSIXctor the POSIXltclass
#POSIXct is just a very large integer under the hood; it use a useful class when you want to store times in something like a data frame
#POSIXlt is a list underneath and it stores a bunch of other useful information like the day of the week, day of the year, month, day of the month
#There are a number of generic functions that work on dates and times
#weekdays: give the day of the week
#months: give the month name
#quarters: give the quarter number (“Q1”, “Q2”, “Q3”, or “Q4”)

#Times in R
#Times can be coerced from a character string using the as.POSIXltor as.POSIXctfunction.
today <-Sys.time()
today
class(today)
unclass(today)
format(today, format="%B %d %Y")
## [1] "2013-01-24 22:04:14 EST"
p <-as.POSIXlt(today)
class(p)
p
names(unclass(p))
## [1] "sec" "min" "hour" "mday" "mon"
## [6] "year" "wday" "yday" "isdst"
p$sec
## [1] 14.34

#Times in R
#You can also use the POSIXct format.
x <-Sys.time()
x ## Already in ‘POSIXct’ format
## [1] "2013-01-24 22:04:14 EST"
x$sec
x$sec
## Error: $ operator is invalid for atomic vectors
unclass(x)
## [1] 1359083054
p <-as.POSIXlt(x)
p
p$sec
## [1] 14.37

#Times in R
#Finally, there is the strptimefunction in case your dates are written in a different format
datestring<-c("2006-01-08 10:07:52", "2006-08-07 19:33:02")
x <-strptime(datestring, "%Y-%m-%d %H:%M:%S")
x
## [1] "2012-01-10 10:40:00 EST" "2011-12-09 09:10:00 EST"
class(x)
## [1] "POSIXlt" "POSIXt"
#I can never remember the formatting strings. Check ?strptimefor details.
?strptime


#Operations on Dates and Times
#You can use mathematical operations on dates and times. Well, really just + and -. You can do comparisons too (i.e. ==, <=)
x <-as.Date("2012-01-01")
y <-strptime("2006-01-08 10:07:52", "%Y-%m-%d %H:%M:%S")
x-y
## Warning: Incompatible methods ("-.Date",
## "-.POSIXt") for "-"
## Error: non-numeric argument to binary operator
x <-as.POSIXlt(x)
x-y
## Time difference of 356.3 days

#Operations on Dates and Times
#Even keeps track of leap years, leap seconds, daylight savings, and time zones.
x <-as.Date("2012-03-01") 
y <-as.Date("2012-02-28")
x-y
### Time difference of 2 days
x <-as.POSIXct("2012-10-25 01:00:00")
x
y <-as.POSIXct("2012-10-25 01:00:00", tz = "GMT")
y
y-x
### Time difference of 1 hours


#Summary
#Dates and times have special classes in R that allow for numerical and statistical calculations
#Dates use the Date class
#Times use the POSIXctand POSIXltclass
#Character strings can be coerced to Date/Time classes using the strptimefunction or the as.Date, as.POSIXlt, or as.POSIXct

















































