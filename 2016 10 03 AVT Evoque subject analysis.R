## Analysis for Evoque data
## Purpose: analyze data from Land Rover Evoque data for the AVT meeting
## Author: Joonbum Lee (joonbum@mit.edu)



# 1. Load ----

# load library and function
library(reshape2)
library(magrittr)
library(ggplot2)
library(dplyr)
library(plyr)
source("~/Dropbox/Works/R/Fuction/multiplot.R")


# load data from Dropbox
data.sub1 <- read.csv("~/Dropbox (MIT)/jb_data/13837.csv", header=TRUE)
data.sub2 <- read.csv("~/Dropbox (MIT)/jb_data/13838.csv", header=TRUE)
data.sub3 <- read.csv("~/Dropbox (MIT)/jb_data/14134.csv", header=TRUE)
data.sub4 <- read.csv("~/Dropbox (MIT)/jb_data/14157.csv", header=TRUE)
data.sub5 <- read.csv("~/Dropbox (MIT)/jb_data/14239.csv", header=TRUE)

data.aeb <- read.csv("~/Dropbox (MIT)/jb_data/aeb_timeline.csv", header=TRUE)






# 2. Clean ----

# add subject id
data.sub1$sub_id <- 13837
data.sub2$sub_id <- 13838
data.sub3$sub_id <- 14134
data.sub4$sub_id <- 14157
data.sub5$sub_id <- 14239


# combine all
data.all <- rbind(data.sub1, data.sub2, data.sub3, data.sub4, data.sub5)


# summary table
data.sum <- ddply(data.all, .(sub_id), summarise,
                  num_trip = length(trip_id),
                  num_fcw = sum(fcw_count),
                  num_park_assist = sum(park_assist_count),
                  num_acc = sum(acc_count),
                  mile_acc = sum(acc_odo),
                  num_bsm_left = sum(bsm_left),
                  num_bsm_right = sum(bsm_right))


# add total mileage
data.sum$total_mile[1] <- 806
data.sum$total_mile[2] <- 814
data.sum$total_mile[3] <- 4184
data.sum$total_mile[4] <- 2334
data.sum$total_mile[5] <- 2062


# calculate additional variables
data.sum$freq_fcw <- data.sum$num_fcw/data.sum$total_mile
data.sum$freq_fcw2 <- data.sum$num_fcw/data.sum$num_trip
data.sum$freq_park <- data.sum$num_park_assist/data.sum$num_trip
data.sum$freq_acc <- data.sum$num_acc/data.sum$num_trip
data.sum$rate_acc_mile <- data.sum$mile_acc/data.sum$total_mile
data.sum$freq_bsm_left <- data.sum$num_bsm_left/data.sum$total_mile
data.sum$freq_bsm_right <- data.sum$num_bsm_right/data.sum$total_mile




# 3. Plot----
plot.trip<-ggplot(data.sum, aes(x=factor(sub_id), y=num_trip))+geom_point(size=3)+xlab("Subject_ID")+ylab("Number of trips")+theme_bw(16)
plot.mile<-ggplot(data.sum, aes(x=factor(sub_id), y=total_mile))+geom_point(size=3)+xlab("Subject_ID")+ylab("Total mileage")+theme_bw(16)
multiplot(plot.trip, plot.mile)

plot.acc.freq<-ggplot(data.sum, aes(x=factor(sub_id), y=freq_acc))+geom_point(size=3)+xlab("Subject_ID")+ylab("Frequency of ACC use\n/Number of trips")+theme_bw(16)
plot.acc.mile<-ggplot(data.sum, aes(x=factor(sub_id), y=rate_acc_mile))+geom_point(size=3)+xlab("Subject_ID")+ylab("ACC mileage\n/Total mileage")+theme_bw(16)
multiplot(plot.acc.freq, plot.acc.mile)

# raw
plot.acc.freq<-ggplot(data.sum, aes(x=factor(sub_id), y=num_acc))+geom_point(size=3)+xlab("Subject_ID")+ylab("Frequency of ACC use")+theme_bw(16)
plot.acc.mile<-ggplot(data.sum, aes(x=factor(sub_id), y=mile_acc))+geom_point(size=3)+xlab("Subject_ID")+ylab("ACC mileage")+theme_bw(16)
multiplot(plot.acc.freq, plot.acc.mile)

# convert to long format
data.acc <- data.sum[c("sub_id","mile_acc", "total_mile")]
long.acc <- melt(data.acc, id.vars = c("sub_id"))

ggplot(long.acc, aes(x=factor(sub_id), y= value))+geom_bar(stat="identity",aes(fill=variable))+xlab("Subject_ID")+ylab("Mileage")+
  scale_fill_discrete(name="", labels=c("ACC mileage", "Total mileage"))+theme_bw(16)


plot.fcw<-ggplot(data.sum, aes(x=factor(sub_id), y=freq_fcw))+geom_point(size=3)+xlab("Subject_ID")+ylab("Frequency of FCW\n/Total mileage")+theme_bw(16)
plot.park<-ggplot(data.sum, aes(x=factor(sub_id), y=freq_park))+geom_point(size=3)+xlab("Subject_ID")+ylab("Frequency of park assist\n/Number of trips")+theme_bw(16)
multiplot(plot.fcw, plot.park)

plot.fcw1<-ggplot(data.sum, aes(x=factor(sub_id), y=num_fcw))+geom_point(size=3)+xlab("Subject_ID")+ylab("Frequency of FCW")+theme_bw(16)
plot.fcw2<-ggplot(data.sum, aes(x=factor(sub_id), y=freq_fcw))+geom_point(size=3)+xlab("Subject_ID")+ylab("Frequency of FCW\n/ Total mileage")+theme_bw(16)
plot.fcw3<-ggplot(data.sum, aes(x=factor(sub_id), y=freq_fcw2))+geom_point(size=3)+xlab("Subject_ID")+ylab("Frequency of FCW\n/ Number of trips")+theme_bw(16)
multiplot(plot.fcw1, plot.fcw2, plot.fcw3)

plot.park1<-ggplot(data.sum, aes(x=factor(sub_id), y=num_park_assist))+geom_point(size=3)+xlab("Subject_ID")+ylab("Frequency of park assist")+theme_bw(16)
plot.park2<-ggplot(data.sum, aes(x=factor(sub_id), y=freq_park))+geom_point(size=3)+xlab("Subject_ID")+ylab("Frequency of park assist\n/ Number of trips")+theme_bw(16)
multiplot(plot.park1, plot.park2)


# AEB
ggplot(data.aeb, aes(x=Trip.ID, y=Time.to.first.break.seconds.))+geom_point(size=3)+theme_bw(16)+ylab("Time to first break (sec)")+xlab("Trip ID")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data.aeb, aes(x=Trip.ID, y=Total.AEB..seconds.))+geom_point(size=3, alpha=I(.5))+theme_bw(16)+ylab("AEB duration (sec)")+xlab("Trip ID")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


write.csv(data.sum, "~/Desktop/evoque_summary.csv", row.names = FALSE)
