#Reading Data
#There are a few principal functions reading data into R.
#read.table, read.csv, for reading tabular data
#readLines, for reading lines of a text file
#source, for reading in R code files (inverse of dump)
#dget, for reading in R code files (inverse of dput)
#load, for reading in saved workspaces
#unserialize, for reading single R objects in binary form

#Writing Data
#There are analogous functions for writing data to files
#write.table
#writeLines
#dump
#dput
#save
#serialize

#Reading Data Files with read.table
#The read.tablefunction is one of the most commonly used functions for reading data. It has a few important arguments:
#file, the name of a file, or a connection
#header, logical indicating if the file has a header line
#sep, a string indicating how the columns are separated
#colClasses, a character vector indicating the class of each column in the dataset
#nrows, the number of rows in the dataset
#comment.char, a character string indicating the comment character
#skip, the number of lines to skip from the beginning
#stringsAsFactors, should character variables be coded as factors?

setwd("D:/R_edu/data")
#read.table
#For small to moderately sized datasets, you can usually call read.table without specifying any other arguments
tab <- read.table("hsb2.txt",header=TRUE)
tab
#R will automatically
#skip lines that begin with a #
#figure out how many rows there are (and how much memory needs to be allocated)
#figure what type of variable is in each column of the table Telling R all these things directly makes R run faster and more efficiently.
#read.csv is identical to read.table except that the default separator is a comma
csv1 <- read.table("hsb2.csv",header=TRUE)
csv2 <- read.csv("hsb2.csv",header=TRUE)
csv2

#install.packages("foreign")
library(foreign)
spss1<-read.spss("hsb2.sav",to.data.frame = TRUE)
stata1<-read.dta("hsb2.dta")

#install.packages("Hmisc")
library(Hmisc)
spss2<- spss.get("hsb2.sav",use.value.labels=TRUE)

#install.packages("openxlsx")
library(openxlsx)
data=read.xlsx("hsb2.xlsx",sheet=1)






#Reading in Larger Datasets with read.table
#With much larger datasets, doing the following things will make your life easier and will prevent R from choking.
#Read the help page for read.table, which contains many hints
#Make a rough calculation of the memory required to store your dataset. If the dataset is larger than the amount of RAM on your computer, you can probably stop right here.
#Set comment.char= "" if there are no commented lines in your file


#Reading in Larger Datasets with read.tabl
#Use the colClassesargument. Specifying this option instead of using the default can make ¡¯read.table¡¯ run MUCH faster, often twice as fast. In order to use this option, you have to know the class of each column in your data frame. If all of the columns are ¡°numeric¡±, for example, then you can just set colClasses= "numeric". A quick an dirty way to figure out the classes of each column is the following:
initial <-read.table("hsb2.txt",header=TRUE, nrows= 10)
sapply(initial, class)

classes <-sapply(initial, class)
tabAll<-read.table("hsb2.txt",header=TRUE, colClasses= classes)
#Set nrows. This doesn¡¯t make R run faster but it helps with memory usage. 

#Know The System
#In general, when using R with larger datasets, it¡¯s useful to know a few things about your system.
#How much memory is available?
#What other applications are in use?
#Are there other users logged into the same system?
#What operating system?
#Is the OS 32 or 64 bit?

#Calculating Memory Requirements
#I have a data frame with 1,500,000 rows and 120 columns, all of which are numeric data. Roughly, how much memory is required to store this data frame?


#Textual Formats
#dumpingand dputingare useful because the resulting textual format is edit-able, and in the case of corruption, potentially recoverable.
#Unlikewriting out a table or csv file, dump and dputpreserve the metadata (sacrificing some readability), so that another user doesn¡¯t have to specify it all over again.
#Textualformats can work much better with version control programs like subversion or gitwhich can only track changes meaningfully in text files
#Textual formats can be longer-lived; if there is corruption somewhere in the file, it can be easier to fix the problem
#Textual formats adhere to the ¡°Unix philosophy¡±
#Downside: The format is not very space-efficient

#dput-ting R Objects
#Another way to pass data around is by deparsingthe R object with dputand reading it back in using dget.
y <-data.frame(a = 1, b = "a")
dput(y)
structure(list(a = 1, b = structure(1L, .Label = "a", class = "factor")), .Names = c("a", "b"), row.names= c(NA, -1L), class = "data.frame")
dput(y, file = "y.R")
new.y<-dget("y.R")
new.y

#Dumping R Objects
#Multiple objects can be deparsedusing the dump function and read back in using source.
x <-"foo"
y <-data.frame(a = 1, b = "a")
dump(c("x", "y"), file = "data.R")
rm(x, y)
source("data.R")
y
x

#Interfaces to the Outside World
#Data are read in using connection interfaces. Connections can be made to files (most common) or to other more exotic things.
#file, opens a connection to a file
#gzfile, opens a connection to a file compressed with gzip
#bzfile, opens a connection to a file compressed with bzip2
#url, opens a connection to a webpage

#Reading Lines of a Text File
con <-gzfile("words.gz")
x <-readLines(con, 10)
x
#writeLinestakes a character vector and writes each element one line at a time to a text file.

#Reading Lines of a Text File
#readLinescan be useful for reading in lines of webpages
## This might take time
con <-url("http://www.jhsph.edu", "r")
x <-readLines(con)
head(x)



















