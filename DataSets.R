# loading in data sets for comparison
SERJData <- read.csv("FinalData/paper_SERJData.csv", stringsAsFactors = TRUE)
appData <- read.csv("FinalData/AllYearsFinalSubset.csv", stringsAsFactors = TRUE)
startData <- read.csv("FinalData/All Years Final Public - with Vars.csv", stringsAsFactors = TRUE)
# newSERJ <- read.csv("SERJData.csv", stringsAsFactors = TRUE)

count(allYrs$textbook.classification.old)
count(allYrs$textbook.classification)

table(allYrs$textbook.classification, allYrs$math.prereq)

year3 <- read.csv("FinalData/3 Years Final Public.csv", stringsAsFactors = TRUE)
allYrs <- read.csv("FinalData/All Years Final Public - with Vars.csv", stringsAsFactors = TRUE)

dim(year3)
dim(allYrs)
allYrs <- allYrs |>
  select(textbook.used.old, textbook.classification.old, textbook.used, textbook.classification)

table(allYrs$textbook.classification, allYrs$textbook.classification.old)

allYrs <- allYrs |>
  select(-c(textbook.used.old, textbook.classification.old)) |>
  rename(textbook.used = textbook.used.old,
         textbook.classification = textbook.classification.old)

# the textbook.used and textbook.classification columns in year3 match the columns in allYrs (not old)

setdiff(names(year3), names(allYrs))
setdiff(names(allYrs), names(year3))

diff1 <- setdiff(year3, allYrs)
diff2 <- setdiff(allYrs, year3)
nrow(diff1)
nrow(diff2)

# Settings for appData:
  # all years
  # students who took both pre- and post-tests
  # 0.8 < response rate < 1
  # -1 < achievable gain < 1
  # all textbooks
  # not including calc pre-req courses
  # all schools but high school
  # all class sizes (0-450)
  # all questions
  # not imputated
  # section-level variables not created

SERJData <- SERJData |>
  filter(instructor == "Instructor3")

newSERJ <- newSERJ |>
  filter(instructor == "Instructor3")

# checking dimentions of datasets
ncol(appData)
nrow(appData)
ncol(SERJData)
nrow(SERJData)
# appData: 297 cols, 10511 rows
# SERJData: 297 cols, 10514 rows

# comparing column names
setdiff(names(appData), names(SERJData))
setdiff(names(SERJData), names(appData))
# appData and SERJData have the same column names

# # removing extra columns from SERJData
# SERJData <- SERJData |>
#   select(-any_of(setdiff(names(SERJData), names(appData))))

# matching data types across datasets
SERJData <- SERJData |>
  mutate(race.origin = as.integer(race.origin),
         morecourses = (morecourses == "Yes"))

diff1 <- setdiff(SERJData, appData)
diff2 <- setdiff(appData, SERJData)
nrow(diff1)
nrow(diff2)

# comparing rows across datasets
diff1 <- setdiff(SERJData, appData)
diff2 <- setdiff(appData, SERJData)
nrow(diff1)
nrow(diff2)
# SERJData contains 6502 rows that are not in appData
# appData contains 6499 rows that are not in SERJData
View
# looking at a few row of each for comparison
View(bind_rows(diff1[1,], diff2[1,]))
# For many rows, the only difference is race.origin (e.g. 7 vs. 1)

# removing race.origin
diff1 <- diff1 |>
  select(-race.origin)
diff2 <- diff2 |>
  select(-race.origin)

# re-comparing rows across datasets
new_diff1 <- setdiff(diff1, diff2)
new_diff2 <- setdiff(diff2, diff1)
nrow(new_diff1)
nrow(new_diff2)
# diff1 (SERJData) contains 4735 rows that are not in diff2 (appData)
# diff2 (appData) contains 4732 rows that are not in diff1 (SERJData)
  # 1767 rows only had a difference of race.origin

# looking at a few row of each for comparison
View(bind_rows(new_diff1[1,], new_diff2[1,]))
# For many rows, the only difference is years.teaching.experience (e.g. 1 vs. 2)

# removing years.teaching.experience
new_diff1 <- new_diff1 |>
  select(-years.teaching.experience)
new_diff2 <- new_diff2 |>
  select(-years.teaching.experience)

# re-comparing rows across datasets
diff1 <- setdiff(new_diff1, new_diff2)
diff2 <- setdiff(new_diff2, new_diff1)
nrow(diff1)
nrow(diff2)
# new_diff1 (SERJData) contains 4379 rows that are not in new_diff2 (appData)
# new_diff2 (appData) contains 4376 rows that are not in new_diff1 (SERJData)
  # 356 rows only had a difference of years.teaching.experience

# looking at a few row of each for comparison
View(bind_rows(diff1[1,], diff2[1,]))
# For many rows, the only difference is position.classification

# removing position.classification
diff1 <- diff1 |>
  select(-position.classification)
diff2 <- diff2 |>
  select(-position.classification)

# re-comparing rows across datasets
new_diff1 <- setdiff(diff1, diff2)
new_diff2 <- setdiff(diff2, diff1)
nrow(new_diff1)
nrow(new_diff2)
# diff1 (SERJData) contains 4350 rows that are not in diff2 (appData)
# diff2 (appData) contains 4347 rows that are not in diff1 (SERJData)
  # 19 rows only had a difference of position.classification

# looking at a few row of each for comparison
View(bind_rows(new_diff1[1,], new_diff2[1,]))
# For many rows, the only difference is advanced.stats.degree.type

# removing years.teaching.experience
new_diff1 <- new_diff1 |>
  select(-advanced.stats.degree.type)
new_diff2 <- new_diff2 |>
  select(-advanced.stats.degree.type)

# re-comparing rows across datasets
diff1 <- setdiff(new_diff1, new_diff2)
diff2 <- setdiff(new_diff2, new_diff1)
nrow(diff1)
nrow(diff2)
# new_diff1 (SERJData) contains 3161 rows that are not in new_diff2 (appData)
# new_diff2 (appData) contains 4158 rows that are not in new_diff1 (SERJData)
  # 1189 rows only had a difference of years.teaching.experience

# looking at a few row of each for comparison
View(bind_rows(diff1[1,], diff2[1,]))
# For many rows, the only difference is years.teaching.experience.binned

# removing  years.teaching.experience.binned
diff1 <- diff1 |>
  select(- years.teaching.experience.binned)
diff2 <- diff2 |>
  select(- years.teaching.experience.binned)

# re-comparing rows across datasets
new_diff1 <- setdiff(diff1, diff2)
new_diff2 <- setdiff(diff2, diff1)
nrow(new_diff1)
nrow(new_diff2)
# diff1 (SERJData) contains 3066 rows that are not in diff2 (appData)
# diff2 (appData) contains 3063 rows that are not in diff1 (SERJData)
  # 95 rows only had a difference of years.teaching.experience.binned

# looking at a few row of each for comparison
View(bind_rows(new_diff1[1,], new_diff2[1,]))
# For many rows, the only difference is years.teaching.intro.stats

# removing years.teaching.intro.stats
new_diff1 <- new_diff1 |>
  select(-years.teaching.intro.stats)
new_diff2 <- new_diff2 |>
  select(-years.teaching.intro.stats)

# re-comparing rows across datasets
diff1 <- setdiff(new_diff1, new_diff2)
diff2 <- setdiff(new_diff2, new_diff1)
nrow(diff1)
nrow(diff2)
# new_diff1 (SERJData) contains 1239 rows that are not in new_diff2 (appData)
# new_diff2 (appData) contains 1237 rows that are not in new_diff1 (SERJData)
  # 1827 rows only had a difference of years.teaching.intro.stats

# looking at a few row of each for comparison
View(bind_rows(diff1[1,], diff2[1,]))
# For many rows, the only difference is morecourses

# removing morecourses
diff1 <- diff1 |>
  select(-morecourses)
diff2 <- diff2 |>
  select(-morecourses)

# re-comparing rows across datasets
new_diff1 <- setdiff(diff1, diff2)
new_diff2 <- setdiff(diff2, diff1)
nrow(new_diff1)
nrow(new_diff2)
# diff1 (SERJData) contains 713 rows that are not in diff2 (appData)
# diff2 (appData) contains 710 rows that are not in diff1 (SERJData)
  # 1114 rows only had a difference of morecourses

# looking at a few row of each for comparison
View(bind_rows(new_diff1[1,], new_diff2[1,]))
# For many rows, the only difference is years.teaching.intro.stats.binned

# removing years.teaching.intro.stats.binned
new_diff1 <- new_diff1 |>
  select(-years.teaching.intro.stats.binned)
new_diff2 <- new_diff2 |>
  select(-years.teaching.intro.stats.binned)

# re-comparing rows across datasets
diff1 <- setdiff(new_diff1, new_diff2)
diff2 <- setdiff(new_diff2, new_diff1)
nrow(diff1)
nrow(diff2)
# new_diff1 (SERJData) contains 477 rows that are not in new_diff2 (appData)
# new_diff2 (appData) contains 474 rows that are not in new_diff1 (SERJData)
  # 236 rows only had a difference of years.teaching.intro.stats.binned

# looking at a few row of each for comparison
View(bind_rows(diff1[1,], diff2[1,]))
# For many rows, the only difference is isi.workshop

# removing isi.workshop
diff1 <- diff1 |>
  select(-isi.workshop)
diff2 <- diff2 |>
  select(-isi.workshop)

# re-comparing rows across datasets
new_diff1 <- setdiff(diff1, diff2)
new_diff2 <- setdiff(diff2, diff1)
nrow(new_diff1)
nrow(new_diff2)
# diff1 (SERJData) contains 312 rows that are not in diff2 (appData)
# diff2 (appData) contains 309 rows that are not in diff1 (SERJData)
  # 165 rows only had a difference of isi.workshop

# looking at a few row of each for comparison
View(bind_rows(new_diff1[1,], new_diff2[1,]))
# For many rows, the only difference is years.teaching.intro.stats.binned

# removing years.teaching.intro.stats.binned
new_diff1 <- new_diff1 |>
  select(-years.teaching.intro.stats.binned)
new_diff2 <- new_diff2 |>
  select(-years.teaching.intro.stats.binned)

# re-comparing rows across datasets
diff1 <- setdiff(new_diff1, new_diff2)
diff2 <- setdiff(new_diff2, new_diff1)
nrow(diff1)
nrow(diff2)
# new_diff1 (SERJData) contains 477 rows that are not in new_diff2 (appData)
# new_diff2 (appData) contains 474 rows that are not in new_diff1 (SERJData)
# 236 rows only had a difference of years.teaching.intro.stats.binned



