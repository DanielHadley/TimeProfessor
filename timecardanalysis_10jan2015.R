
## read in the data (change to your own directory of course)
dat <- read.csv("C:/Users/dhadley/Documents/GitHub/TimeProfessor/timecard_10jan2015.csv",as.is=T)
head(dat)

##Some notes about the data
## The first column has either the date (for days) or a lable for the row (GOALS for my weekly goals, Total, etc)
## "day" is the day of the week
## "research" is hours devoted to my own research
## "arabic" is Arabic practice of some kind
## "book" is research directly on my book, though sometimes I forget to mark this column
## "teaching" is any time related to teaching, both in and out of class
## "service" is service to the department: meetings, committees
## "other" is all other work
## "freethink" is ideally staring out the window and thinking creatively.  but sometimes it's just daydreaming
## "total" is the summed total of the row.  This was automaticly calculated in my google doc
## "time" is a column I use to note the time I last updated the spreadsheet.  As such, it also (usually) indicates the time I stopped working on a given day.
## "activities" are things of note I did on this day.  It's not comprehensive.
## "Sleep".  Prior to 2015, this variable has notes about times I stayed up really late or all nigth.  After 1 jan 2015, it has the times I slept and woke up.
## "Exercise" indicates my exercise.

## This is some analysis I did to see how I spent my time in 2014
## get every day of this year in order
## check for duplicate dates
dat$X[duplicated(dat$X)]
## subset to just the days
dat2 <- dat[dat$day %in% c("S","M","T","W","Th","F"),]
## make them dates
as.Date(dat2$X,"%m/%d/%Y")
dat2$date <- as.Date(dat2$X,"%m/%d/%Y")
dat2 <- dat2[order(dat2$date),]
## keep just 2014
dat2 <- dat2[dat2$date >= as.Date("2014-01-01") & dat2$date <= as.Date("2014-12-31"),] 
dim(dat2)
## fix a few vars that aren't numeric
dat2$teaching <- as.numeric(dat2$teaching)
dat2$service <- as.numeric(dat2$service)
## replace the NAs
dat2[is.na(dat2)] <- 0


## this is how many hours are in a year
yrhrs <- 24*365

## calculate how much time I spent on different activities as a fraction of my year
## research
sum(dat2$research+dat2$book)/yrhrs
## arabic
sum(dat2$arabic)/yrhrs
## teaching
sum(as.numeric(dat2$teaching))/yrhrs
## other and service
sum(as.numeric(dat2$other+dat2$service))/yrhrs
## freethink
sum(as.numeric(dat2$freethink))/yrhrs

## How much time did I spend working total?
workhours <- (sum(dat2$research+dat2$book)+
sum(dat2$arabic)+
sum(as.numeric(dat2$teaching))+
sum(as.numeric(dat2$other+dat2$service)) +
sum(as.numeric(dat2$freethink)))
workhours
workhours/365 ##  average hours per day
workhours/yrhrs  ## percentage of total time spent working



## How many late nights and all-nighters?
unique(dat2$Sleep)
badsleep <- c("late","3.50am","no","not much","up early","5 hrs","4.5 hrs","allnighter","late, 2am","up late","2:30:00","6hrs","4hrs","late!")
## number of days where I noted not sleeping much
sum(dat2$Sleep %in% badsleep)

## How much did I exercise?
ex <- dat2$Exercise
## take out days where I noted NOT exercising
ex[ex %in% c("no - too sore","no")] <- ""
unique(ex)
## fix the pushups -- I used to just note the number, not the activity
ex[ex=="40"] <- "40 pushups"
ex[ex=="80"] <- "80 pushups"
ex[ex=="100pushups"] <- "100 pushups"

## How many days did I do the following activities?
length(ex[ex!=""]) ## anything at all
length(ex[grep("surf",ex)])  ## surfing
length(ex[grep("swim",ex)])  ## swim
length(c(ex[grep("run",ex)],ex[grep("sprint",ex)]) )  ## run
length(ex[grep("pushup",ex)])  ## pushups
length(ex[grep("weight",ex)])  ## weights
length(ex[grep("climb",ex)])  ## climb
length(ex[grep("sail",ex)])  ## sail
length(ex[grep("skat",ex)])  ## skating

## how many pushups
ex[grep("pushup",ex)]
tmp <- gsub("[[:alpha:]]","",ex[grep("pushup",ex)])
tmp <- gsub("[[:punct:]]","",tmp)
tmp <- gsub(" ","",tmp)
sum(na.omit(as.numeric(tmp)))  ## the total number of pushups I did

## what did I work on?
library(stringr)
activities <- sapply(dat2$activities,function(x){strsplit(x,",")[[1]]})
activities <- str_trim(unlist(activities))
names(activities) <- NULL
activities
sort(unique(activities))

## days I mentioned specific activities
act <- dat2$activities
length(act[grep("reward",act)])  ## Rewards paper
length(c(act[grep("VeF",act)],act[grep("vef",act)]))  ## VeF
length(c(act[grep("Syria",act)],act[grep("syria",act)]))  ## Syria
length(c(act[grep("Muslim int",act)],act[grep("muslim int",act)]))  ## Muslim Integration
length(act[grep("sermons",act)])  ## sermons
length(c(act[grep("sovereignty",act)],act[grep("sovereignty",act)]))  ## sovereignty
length(c(act[grep("SMR",act)],act[grep("case selection",act)],act[grep("smr",act)]))  ## case selection
length(c(act[grep("pscores",act)],act[grep("psparadox",act)]))  ## psparadox


######################################################
## If you want to look at work over time, for example

unique(dat$X[duplicated(dat$X)])
## subset to just the days
dat3 <- dat[dat$day %in% c("S","M","T","W","Th","F"),]
## make them dates
as.Date(dat3$X,"%m/%d/%Y")
dat3$date <- as.Date(dat3$X,"%m/%d/%Y")
dat3 <- dat3[order(dat3$date),]
dim(dat3)
## fix a few vars that aren't numeric
dat3$teaching <- as.numeric(dat3$teaching)
dat3$service <- as.numeric(dat3$service)
## replace the NAs
dat3[is.na(dat3)] <- 0


##  plot work over time
daytotal <- apply(dat3[,c("research","arabic","book","teaching","service","other","freethink")],1,sum)
plot(dat3$date, daytotal,type="l")


##
dat3[1:10,]

## make indicators for each week
ss <- seq(1,round(nrow(dat3)/7),1)
weekid <- sort(rep(ss,7))
length(weekid)
dat3$day
dat3$week <- weekid

## make a table of daily averages
holder <- matrix(NA,71,7)
for(i in 1:max(dat3$week)){
  holder[i,] <- apply(dat3[dat3$week==i,c("research","arabic","book","teaching","service","other","freethink")],1,sum)
}
holder

par(mfrow=c(2,2))
## plot the averages by day
plot(1:7,apply(holder,2,mean),axes=F,type="n",ylim=c(0,max(holder)),main="Work hours per week day",ylab="hours",xlab="day of the week")
axis(2);axis(1,at=1:7,labels=c("S","M","T","W","Th","F","S"))
for(i in 1:nrow(holder)){
  lines(1:7,holder[i,],col="#00000015")
}
lines(1:7,apply(holder,2,mean),lwd=2)


## redo the plot, just for research
## make a table of daily averages
holder.research <- matrix(NA,71,7)
for(i in 1:max(dat3$week)){
  holder.research [i,] <- apply(dat3[dat3$week==i,c("research","book")],1,sum)
}
holder.research 
plot(1:7,apply(holder.research ,2,mean),axes=F,type="n",ylim=c(0,max(holder)),main="Research hours per week day",ylab="hours",xlab="day of the week")
axis(2);axis(1,at=1:7,labels=c("S","M","T","W","Th","F","S"))
for(i in 1:nrow(holder.research )){
  lines(1:7,holder.research [i,],col="#ff000015")
}
lines(1:7,apply(holder.research ,2,mean),lwd=2,col="red")


## redo the plot, just for teaching
## make a table of daily averages
holder.teaching <- matrix(NA,71,7)
for(i in 1:max(dat3$week)){
  holder.teaching [i,] <- apply(dat3[dat3$week==i,c("teaching"),drop=F],1,sum)
}
holder.teaching 
plot(1:7,apply(holder.teaching ,2,mean),axes=F,type="n",ylim=c(0,max(holder.teaching)),main="Teaching hours per week day",ylab="hours",xlab="day of the week")
axis(2);axis(1,at=1:7,labels=c("S","M","T","W","Th","F","S"))
for(i in 1:nrow(holder.teaching )){
  lines(1:7,holder.teaching [i,],col="#00ff0015")
}
lines(1:7,apply(holder.teaching ,2,mean),lwd=2,col="green")

## redo the plot, just for teaching
## make a table of daily averages
holder.service <- matrix(NA,71,7)
for(i in 1:max(dat3$week)){
  holder.service [i,] <- apply(dat3[dat3$week==i,c("other","service"),drop=F],1,sum)
}
holder.service 
plot(1:7,apply(holder.service ,2,mean),axes=F,type="n",ylim=c(0,max(holder.service)),main="Other/Service hours per week day",ylab="hours",xlab="day of the week")
axis(2);axis(1,at=1:7,labels=c("S","M","T","W","Th","F","S"))
for(i in 1:nrow(holder.service )){
  lines(1:7,holder.service [i,],col="#0000FF15")
}
lines(1:7,apply(holder.service ,2,mean),lwd=2,col="blue")


weektotalhours <- apply(holder,1,sum)

weektotalhours[2:length(weektotalhours)]

## do a linear model with last week's hours predicting this week's
summary(lm(weektotalhours[2:length(weektotalhours)] ~ weektotalhours[1:(length(weektotalhours)-1)]))
## add more lags

laggedWeekHourDat <- data.frame(cbind(weektotalhours[9:length(weektotalhours)], 
      weektotalhours[8:(length(weektotalhours)-1)],
      weektotalhours[7:(length(weektotalhours)-2)],
      weektotalhours[6:(length(weektotalhours)-3)],
      weektotalhours[5:(length(weektotalhours)-4)],
      weektotalhours[4:(length(weektotalhours)-5)],
      weektotalhours[3:(length(weektotalhours)-6)],
      weektotalhours[2:(length(weektotalhours)-7)],
      weektotalhours[1:(length(weektotalhours)-8)]))
colnames(laggedWeekHourDat) <- c("outcome",paste("lag",1:8,sep=""))
head(laggedWeekHourDat)

## regression of current week's hours on the last 8 week's hours
summary(lm(outcome~lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8, laggedWeekHourDat))
## interesting -- no evidence that big weeks tire me out.  Seems that low hour weeks are random
## but big weeks lead are associated with big weeks following them.



