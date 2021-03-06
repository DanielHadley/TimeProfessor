## read in the data (change to your own directory of course)

setwd("/Users/dphnrome/Documents/Git/TimeProfessor/")

dat <- read.csv("./timecard_10jan2015.csv", as.is=T)

dat <- read.csv("C:/Users/dhadley/Documents/GitHub/TimeProfessor/timecard_10jan2015.csv",as.is=T)
head(dat)

library(dplyr)
library(ggplot2)
library(scales) # for changing from scientific notation
library(tidyr)
library(stringr)
library(lubridate)


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



#### Settings to make charts prettier ####

lime_green = "#2ecc71"
soft_blue = "#3498db"
pinkish_red = "#e74c3c"
purple = "#9b59b6"
teele = "#1abc9c"
nice_blue = "#2980b9"


my.theme <- 
  theme(#plot.background = element_rect(fill="white"), # Remove background
    panel.grid.major = element_blank(), # Remove gridlines
    # panel.grid.minor = element_blank(), # Remove more gridlines
    # panel.border = element_blank(), # Remove border
    # panel.background = element_blank(), # Remove more background
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text=element_text(size=6), # Enlarge axis text font
    axis.title=element_text(size=8), # Enlarge axis title font
    plot.title=element_text(size=12) # Enlarge, left-align title
    ,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )



#### Munge ####

## subset to just the days
d <- dat[dat$day %in% c("S","M","T","W","Th","F"),]
## make them dates
as.Date(d$X,"%m/%d/%Y")
d$date <- as.Date(d$X,"%m/%d/%Y")
d <- d[order(d$date),]
## keep just 2014
d <- d[d$date >= as.Date("2014-01-01") & d$date <= as.Date("2014-12-31"),] 
dim(d)
## fix a few vars that aren't numeric
d$teaching <- as.numeric(d$teaching)
d$service <- as.numeric(d$service)
## replace the NAs
d[is.na(d)] <- 0


# make tbl
d <- tbl_df(d)

# Add variables
d <- d %>%
  mutate(workTotal = research + arabic + book + teaching + service + other + freethink,
         weekNumber = week(date),
         Research = research + book + arabic,
         Service = service + other)

# no sleep variable
badSleep <- c("late","3.50am","no","not much","up early","5 hrs","4.5 hrs",
              "allnighter","late, 2am","up late","2:30:00","6hrs","4hrs","late!")

pattern <- paste(badSleep, collapse = "|") # create regex pattern
d$badSleep <- grepl(pattern, d$Sleep)

d$id <- 1
d$time <- seq(from=1, to =365)

d <- d %>%
  mutate(event = cumsum(c(FALSE, diff(badSleep)))) %>%
  mutate(tmpG = cumsum(c(FALSE, as.logical(diff(event))))) %>%
  group_by(id) %>%
  mutate(tmp_a = c(0, diff(time)) * !event,
         tmp_b = c(diff(time), 0) * !event) %>%
  group_by(tmpG) %>%
  mutate(daysAfterAllNi = cumsum(tmp_a),
         daysBeforeAllNi = rev(cumsum(rev(tmp_b)))) %>%
  ungroup() %>%
  select(-c(tmp_a, tmp_b, tmpG))


####  Visualize ####

weekly <- d %>%
  group_by(weekNumber) %>%
  summarize(weeklyWork=sum(workTotal), weekEnding = max(date))

ggplot(weekly, aes(x=weekEnding, y=weeklyWork)) + 
  geom_line(colour=pinkish_red, size = 1) + 
  my.theme + ggtitle("Weekly Work Hours, 2014") + xlab("Month") +
  ylab("Hours Worked Per Week") + 
  scale_y_continuous(labels = comma) +
  annotate("text", x=as.Date("2014-08-16"), y=24, label="Camping", size=1.5) +
  annotate("text", x=as.Date("2014-03-29"), y=81, label="Conference", size=1.5) +
  annotate("text", x=as.Date("2014-04-20"), y=36, label="Easter, Surfing", size=1.5) +
  annotate("text", x=as.Date("2014-11-20"), y=82, label="Conference", size=1.5) +

ggsave("./plots/weekly.png", dpi=250, width=5, height=3)



weekly <- d %>%
  group_by(weekNumber) %>%
  summarize(Research = sum(Research), Service = sum(Service), Teaching = sum(teaching), weekEnding = max(date)) %>%
  gather(variable, value, -weekEnding) %>%
  filter(variable != "weekNumber")


ggplot(weekly, aes(x=weekEnding, y=value, group=variable)) + 
  geom_line(size = .5, color=pinkish_red) + 
  my.theme + ggtitle("Weekly Work Hours, 2014") + xlab("Month") +
  ylab("Hours Worked Per Week") + 
  scale_y_continuous(labels = comma) + 
  facet_wrap( ~ variable, nrow=3, ncol=1)
  
  ggsave("./plots/weeklyThree.png", dpi=250, width=5, height=5)



### Heatmap!
# We will facet by year ~ month, and each subgraph will
# show week-of-month versus weekday
# the year is simple
d$year<-year(d$date)
# the month too 
d$month<-month(d$date)
# but turn months into ordered facors to control the appearance/ordering in the presentation
d$monthf<-factor(d$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
# the day of week is again easily found
d$weekday = wday(d$date)
# again turn into factors to control appearance/abbreviation and ordering
# I use the reverse function rev here to order the week top down in the graph
# you can cut it out to reverse week order
d$weekdayf<-factor(d$weekday,levels=rev(1:7),labels=rev(c("Sun", "Mon","Tues","Wed","Thurs","Fri","Sat")),ordered=TRUE)
# the monthweek part is a bit trickier 
# first a factor which cuts the da into month chunks
d$yearmonth<-paste(d$year, d$month)
d$yearmonthf<-factor(d$yearmonth)
# then find the "week of year" for each day
d$week <- week(d$date)
# and now for each monthblock we normalize the week to start at 1 
d$monthweek = ceiling(day(d$date) / 7)

# Now for the plot
ggplot(d, aes(monthweek, weekdayf, fill = workTotal)) + 
  geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradient(low="yellow", high="red") +
  ggtitle("Daily Work Hours, 2014") + xlab("Week of The Month") +
  ylab(NULL) + labs(fill="Hours Worked") + 
  theme(axis.ticks = element_blank(), # Remove axis ticks
        panel.grid.major = element_blank(), # Remove gridlines
        panel.grid.minor = element_blank(), # Remove more gridlines
        # panel.border = element_blank(), # Remove border
        panel.background = element_blank(), # Remove more background
        axis.text=element_text(size=6), # Enlarge axis text font
        axis.title=element_text(size=8), # Enlarge axis title font
        plot.title=element_text(size=12)) # Enlarge, left-align title


ggsave("./plots/weeklyHeat.png", dpi=250, width=6, height=3)




