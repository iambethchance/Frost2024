
library(plyr)
library(tidyverse)
load(url("http://www.rossmanchance.com/iscam3/ISCAM.RData"))

alldata <- read.csv("C:/Users/bchance/Dropbox/ISI Assessment Data Summer 2021/All Years Final v3 Public.csv", stringsAsFactors = T)
alldata21 <- read.csv("C:/Users/bchance/Dropbox/ISI Assessment Data Summer 2021/All Years Final Public - with Vars.csv")


data <- alldata21
data <- alldata

data <- read.csv("../../All Years Final Public - with Vars.csv", stringsAsFactors = T)

data <- data %>%  filter(is.na(opt.out.pre) == F & is.na(opt.out.post) == F)

data <- data[data$c.rr.pre >= .8,]

data <- data[data$c.rr.post >= .8,]

nrow(data) #11733
data <- data[data$ach.gain.24 > -1.1,]

data <- data %>%
  filter((math.prereq != "Calculus") | is.na(math.prereq))

nrow(data) #11300
data$math.prereq <- droplevels(data$math.prereq)

data$math.prereq <- factor(data$math.prereq, levels = c("Precalculus", "College Algebra", "High School Algebra", "None", "Other"))

data$textbook.classification <- droplevels(data$textbook.classification)

data$textbook.classification <- factor(data$textbook.classification, levels = c("ISI", "ISI1st", "NotSBI", "NotSBI2", "Other", "OtherSBI"))
nrow(data) #11300

data <- data %>%
  filter(carnegie.classification != "High School" | is.na(carnegie.classification))
nrow(data) #10514

### NOTE, when removing HS student both Carnegie and student.type need to be refactored ###
### There are no more "High School Students" in student.type ###
data$carnegie.classification <- droplevels(data$carnegie.classification)

data$student.type <- droplevels(data$student.type)

### Re-orders the factors ###
data$carnegie.classification <- factor(data$carnegie.classification, levels = c("Community College", "Baccalaureate College", "Master's", "Doctoral Universities"))

data$student.type <- factor(data$student.type, levels = c("LowerGE", "LowerReq", "UpperGE", "UpperReq"))
nrow(data)


write.csv(data, "SERJData.csv", row.names = FALSE)

data$textbook.classification2 <- recode(data$textbook.classification, ISI = "ISI", ISI1st = "ISI", NotSBI = "NotSBI", NotSBI2 = "NotSBI", Other = "Other", OtherSBI = "OtherSBI")

