# Use 64bit RGUI. 

#                --------------------------- 
#################       UF Foundation       #################
#                --------------------------- 


#################       Setup       #################

### Gets rid of scientific notation
options(scipen=999)

### Libraries
library(tidyverse)
library(data.table) # version 1.13.4
library(dplyr)
library(lubridate)
library(fastDummies)
library(gridExtra)
library(caret)
library(ISLR)
library(pROC)
library(plotROC)
library(ROCit)
library(precrec)
library(rattle)
library(DMwR) # Smote
library(ROSE) # Sampling
library(AppliedPredictiveModeling)
library(e1071)
library(cranlogs)
library(stringr)
library(forcats)
library(tidyquant)
library(forecast)


### Set Memory To Maximum Speed
memory.limit(size=34000)

### Set WD
setwd("J:/Data Services/Practicum Student Folder/Data")

#################       Loading Data       #################

# Imports as data table. 
# The fread command is being used here becuase read_excel had memory issues.
myData = fread("JOINED_EXPORT_UPDATE_20210409.csv", header = TRUE)

# The above returns Warning: package 'bit64' may be needed to prevent some integer columns from printing as nonsense.
# The integers print just fine when I run the head() command, so I am not taking their advice for the time being.

backup = myData

# fread returns a data table, so we convert it to a data frame.
myData <- as.data.frame(myData)

# Let's get some preliminary info on our data.
#dim(myData) ## 291 variables,  520,458 observations
#head(myData)
#str(myData, list.len = ncol(myData))
#colnames(myData)

###### IMPORTING CLICK DATA #######

# This reads in any weekly Advancement Files. Just drop them in the
# relevant folder and it'll be included in the data frame.
clickDataTable <- 
  list.files(pattern = "Advancement_University_of_Florida_weekly.*.csv") %>%
  map_df(~fread(.))

# We only need ID_NUMBER and TOTAL_VISITS columns.
clickDataTable = clickDataTable[,c(2,10)]

colnames(clickDataTable) = c("ID_NUMBER", "TOTAL_VISITS") # For later merge

dim(clickDataTable) # 38500 observations
clickDistinct <- unique(clickDataTable$ID_NUMBER) 
length(clickDistinct) # but only 21200 unique IDs in the list.

# Summing total visits by ID.
distinctClickData <- clickDataTable %>%
  group_by(ID_NUMBER) %>%
  transmute(TOTAL_VISITS_SUMMED = sum(TOTAL_VISITS))

# This gets rid of duplicate ID rows. The previous code summed them,
# but didn't remove the multiple instances of ID NUMBER.
distinctClickData = distinctClickData[!duplicated(distinctClickData$ID_NUMBER), ]

# The below two sums should match. 
sum(clickDataTable$TOTAL_VISITS)
sum(distinctClickData$TOTAL_VISITS_SUMMED)

# Tidying up data; we're done here.
clickDataTable = distinctClickData
colnames(clickDataTable)[2] = "TOTAL_VISITS"

# This click data will be merged with myData later.

#################       Data Wrangling       #################


##### COVERT DATE COLUMNS TO APPROPRIATE TYPE  #####

# Confirming we have real dates here.
sum(myData$TOUCH_DATE_MR1 != "") # 302,000 date observations NOT empty
sum(myData$TOUCH_DATE_MR1 == "") # 218,000 date observations empty

# Create vectors for the column numbers of 
# related dates -- touches, gifts, and miscellaneous.
touchDateColumnNums = c(197, 199, 201, 203, 205, 207, 209, 211, 213, 215)
giftDateColumnNums = c(157,161,165,169,173,177,181,185,189, 193)
miscDateColumnNums = c(243, 245, 256, 263, 270) #First date +MFOS, last date +MFOS, largest date

dateColumnNumbers = c(touchDateColumnNums, giftDateColumnNums, 
  miscDateColumnNums)

# Converts all date columns to type Date.
myData[,dateColumnNumbers] <- lapply(myData[,dateColumnNumbers], 
  as.Date, format = "%m/%d/%Y")

# Check if successful. 
sapply(myData[,dateColumnNumbers], is.Date) # should all be TRUE

#str(myData[,dateColumnNumbers])


##### CONVERT CHAR COLUMNS TO FACTOR  #####

# List all columns that are type character.
str(myData[, sapply(myData, class) == 'character'])

# Convert to factor.
myData[, sapply(myData, class) == 'character'] <- lapply(myData[, sapply(myData, class) == 'character'], 
  as.factor)

# Check if successful.
str(myData[, sapply(myData, class) == 'factor']) # should return columns from above
str(myData[, sapply(myData, class) == 'character']) # should return 0 variable data frame


#####  DROPPING UNIMPORTANT COLUMNS PRE-MODELING  #####

# Creating dummy variables at first crashed because some of our factors 
# have thousands of levels, despite little relevance to the model.

# Getting rid of some of the unnecessary ones.
drops <- c("BAR_INDEX", "FIRST_DEPT_DESC", "FIRST_FUND", 
  "FIRST_FUND_NAME", "LAST_DEPT_DESC", "LAST_FUND", "LAST_FUND_NAME", 
  "LARGEST_DEPT_DESC", "LARGEST_FUND", "LARGEST_FUND_NAME", "ESTHHI",
  "COUNTY" )
myData = myData[, !(names(myData) %in% drops )]

drops2 <- c("FIRST_UNIT", "LAST_UNIT", "LARGEST_UNIT", 
  "PRIMARY_UNIT_OF_GIVING", "INDUSTRY", "PRIM_UNIT", "PREF_SCHOOL_CODE",
  "STATE_CODE")
myData = myData[, !(names(myData) %in% drops2 )]

# I'll also drop columns where the vast (95+%) of values are null.
# These columns are also not very useful.
# They were purchased data from a 3rd organization. 
moreToDrop = c("OPG_ART", "OPG_EDU", "OPG_ENVIR", "OPG_ANIMAL",
  "OPG_HEALTH", "OPG_MNTL_HLTH", "OPG_VOL_HLTH_ASSOC",
  "OPG_MED_RES", "OPG_LEGAL", "OPG_EMPL", "OPG_FOOD_AG", 
  "OPG_HOUSING", "OPG_PUB_SAFE", "OPG_REC_SPRTS", "OPG_YOUTH_DEV", 
  "OPG_HUM_SERV", "OPG_INTERNTNL", "OPG_CIVIL_RHGTS", "OPG_COM_IMPR",
  "OPG_PHILAN", "OPG_SCI_TECH", "OPG_SOC_SCI", "OPG_PUB_BEN", "OPG_RELIGION",
  "OPG_MUT_MEMBER", "OPG_UNKN", "TG_TENPLUS_YRS", "TG_ANN", "TG_CHARTER", 
  "TG_CAMPAIGN", "TG_CUMM", "TG_EVENT", "TG_ENDOW", "TG_EVENT_SPON", "TG_HON",
  "TG_INKIND", "TG_MATCH", "TG_MEM", "TG_MONTHLY", "TG_NAMED", 
  "TG_PLANNEDGT", "TG_RESTRICTED", "TG_SCHOLAR", "TG_UNKNOWN", 
  "TG_VEHICLE", "TG_TWO_FOUR_YRS", "TG_FIVE_NINE_YRS")
myData = myData[, !(names(myData) %in% moreToDrop )]

str(myData, list.len = ncol(myData))
colnames(myData)

# Removing more variables outlined as unimportant 
# in the Variable Dictionary excel file.
unimportantPerExcel = c("PERSON_OR_ORG", "RECORD_STATUS_CODE",
  "BUSINT", "BUS_MILES", "GGAAF", "GGAGC", "GGAMG", "GGANL",
  "GGAPG", "GTR_CLUB", "HIGH_PTR", "JNTMEM", "MAIDENNAME", 
  "MANAGR", "MAXYRCON", "MEDALUM", "MEDICAL", "MRKGUIDE", "NICKNAME",
  "NUMCOMM", "OTHER_AFF", "OTHRSPRT", "PENSION", "PRIMUF", "PRODUCT",
  "RCCODE", "REUNION", "RSCODE", "RUCODE", "SP_ID", "SURVEY", 
  "TOTSEC", "TOTYRS", "TRAVEL", "UNITPOOL", "VOLUNT", 
  "GIVING_STATUS_CODE", "SOFT_GIVING_STATUS_CODE")
myData = myData[, !(names(myData) %in% unimportantPerExcel )]

# Removing variables with zero variance.
zeroVarianceVars = c("DNB", "FAMWEAL", "HH_ID_NUMBER", "DR_PERFECT_YEARS")
myData = myData[, !(names(myData) %in% zeroVarianceVars )]

#####  CREATE DUMMY VARIABLES AND REMOVE FACTORS FROM MYDATA #####
### FOR LATER REJOINING AFTER CENTERING AND SCALING ###

# This will make dummies out of all the factor columns.
# It also returns all original columns in myData.
myDataDummied <- dummy_cols(myData, select_columns =
  as.vector(colnames(myData[, sapply(myData, class) == "factor"])),
  remove_first_dummy = TRUE )
str(myDataDummied, list.len = ncol(myDataDummied))

# I want to isolate the dummied variables for passing on later,
# so I'm removing all the non-dummy columns besides ID_NUMBER.
myDataDummied = myDataDummied[,-c(2:183)]

# In preparation for centering and scaling the data, 
# I'm going to remove the 45+ columns 
# that have been dummied and only center and scale the rest. This will
# hopefully help that preProces function not crash my computer.

myDataWithoutFactors = myData[, sapply(myData, class) !='factor']
myData = myDataWithoutFactors


#####  CREATE CONTACT SUCCESS VARIABLE  #####

numDaysToRespond = 31
# This code will mark a touch as successfull is the donor
# made a gift within the # of days specified above.
myData$touchOneSuccessful = myData$TOUCH_DATE_MR1 + days(numDaysToRespond) < myData$GIFT_DATE_MR1
myData$touchTwoSuccessful = myData$TOUCH_DATE_MR2 + days(numDaysToRespond) < myData$GIFT_DATE_MR2
myData$touchThreeSuccessful = myData$TOUCH_DATE_MR3 + days(numDaysToRespond) < myData$GIFT_DATE_MR3
myData$touchFourSuccessful = myData$TOUCH_DATE_MR4 + days(numDaysToRespond) < myData$GIFT_DATE_MR4
myData$touchFiveSuccessful = myData$TOUCH_DATE_MR5 + days(numDaysToRespond) < myData$GIFT_DATE_MR5
myData$touchSixSuccessful = myData$TOUCH_DATE_MR6 + days(numDaysToRespond) < myData$GIFT_DATE_MR6
myData$touchSevenSuccessful = myData$TOUCH_DATE_MR7 + days(numDaysToRespond) < myData$GIFT_DATE_MR7
myData$touchEightSuccessful = myData$TOUCH_DATE_MR8 + days(numDaysToRespond) < myData$GIFT_DATE_MR8
myData$touchNineSuccessful = myData$TOUCH_DATE_MR9 + days(numDaysToRespond) < myData$GIFT_DATE_MR9
myData$touchTenSuccessful = myData$TOUCH_DATE_MR10 + days(numDaysToRespond) < myData$GIFT_DATE_MR10

# Sums the number of above columns that contain TRUE.
colnames(myData)
myData$donationsWithinMonth = rowSums( myData[,137:146])

# Now that we've summed the successful contacts, we can remove
# the columns touchOneSuccessful through touchTenSuccessful
myData = myData[,-c(137:146)]

# For visualization of who responds to contact.
regularDonors = as.data.frame(table(myData$donationsWithinMonth))
regularDonors = regularDonors[-1,]

# Bar plots number of donors by how many times they've donated
# within a month.
ggplot(regularDonors, aes(x=as.factor(Var1), y=Freq)) +
  geom_bar(stat='identity') +
  labs(y="Number of Donors", x="# of Times They Donated Within a Month")

ggsave("donorsByNumberOfDonationsWithinMonth.png") # Saves above graph.


#####  CREATE INTERVAL VARIABLES  #####

# I'd like to convert all the remaining date columns
# from the type DATE to an int reflecting the weeks since
# the event occured. 
str(myData[,sapply(myData, class) == "Date"])

# Create a list of all the date columns I want to select as well as ID_NUMBER.
dateColumnNames = names(myData[,sapply(myData, class) == "Date"])
dateColumnNames = append(dateColumnNames, "ID_NUMBER")

dateColumns = myData[,dateColumnNames]

convertToWeeksSince <- function(x) {
  return(floor(as.double(difftime(today(), x, unit = "weeks"))))
}

# This will run every date through the function above
# and return the rounded # of weeks from today to the event.
transformedDates = data.frame(dateColumns[26], lapply(dateColumns[1:25], convertToWeeksSince))

# Create new column names for these transformed date columns.
newColumnNames = c("ID_NUMBER", "weeksSinceGiftDate1", "weeksSinceGiftDate2",
  "weeksSinceGiftDate3", "weeksSinceGiftDate4", "weeksSinceGiftDate5",
  "weeksSinceGiftDate6", "weeksSinceGiftDate7", "weeksSinceGiftDate8",  
  "weeksSinceGiftDate9", "weeksSinceGiftDate10", "weeksSinceTouch1", "weeksSinceTouch2", 
  "weeksSinceTouch3", "weeksSinceTouch4", "weeksSinceTouch5", "weeksSinceTouch6",
  "weeksSinceTouch7", "weeksSinceTouch8", "weeksSinceTouch9", "weeksSinceTouch10",
  "weeksSinceFirstMFOSGift", "weeksSinceLastMFOSGift", "weeksSinceFirstGift", 
  "weeksSinceLastGift", "weeksSinceLargestGift")
colnames(transformedDates) = newColumnNames

# Remove the old date columns from myData in anticipation of a merge.
myData = myData[, sapply(myData, class) != "Date"]

# Left join transformed dates with myData
myData <- merge(x=myData,y=transformedDates,by="ID_NUMBER",all.x=FALSE)

colnames(myData)
str(myData, list.len = ncol(myData)) 

# All columns are now int or num. Success!

##### REMOVING NA VALUES WITH TIME INTERVAL YEAR BUCKETS #####

colnames(dateColumns)

# This is poor code, but it marks a 1 if a donor
# donated in a particular year and 0 otherwise.
# It does this for 2021 through 2015.
dateColumns$donated2021 <- ifelse(year(dateColumns$GIFT_DATE_MR1) == 2021, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR2) == 2021, 1, 
  ifelse(year(dateColumns$GIFT_DATE_MR3) == 2021, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR4) == 2021, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR5) == 2021, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR6) == 2021, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR7) == 2021, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR8) == 2021, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR9) == 2021, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR10) == 2021, 1, 0))))))))))

table(dateColumns$donated2021)

dateColumns$donated2020 <- ifelse(year(dateColumns$GIFT_DATE_MR1) == 2020, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR2) == 2020, 1, 
  ifelse(year(dateColumns$GIFT_DATE_MR3) == 2020, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR4) == 2020, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR5) == 2020, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR6) == 2020, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR7) == 2020, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR8) == 2020, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR9) == 2020, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR10) == 2020, 1, 0))))))))))

table(dateColumns$donated2020)

dateColumns$donated2019 <- ifelse(year(dateColumns$GIFT_DATE_MR1) == 2019, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR2) == 2019, 1, 
  ifelse(year(dateColumns$GIFT_DATE_MR3) == 2019, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR4) == 2019, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR5) == 2019, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR6) == 2019, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR7) == 2019, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR8) == 2019, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR9) == 2019, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR10) == 2019, 1, 0))))))))))

table(dateColumns$donated2019)

dateColumns$donated2018 <- ifelse(year(dateColumns$GIFT_DATE_MR1) == 2018, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR2) == 2018, 1, 
  ifelse(year(dateColumns$GIFT_DATE_MR3) == 2018, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR4) == 2018, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR5) == 2018, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR6) == 2018, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR7) == 2018, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR8) == 2018, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR9) == 2018, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR10) == 2018, 1, 0))))))))))

table(dateColumns$donated2018)

dateColumns$donated2017 <- ifelse(year(dateColumns$GIFT_DATE_MR1) == 2017, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR2) == 2017, 1, 
  ifelse(year(dateColumns$GIFT_DATE_MR3) == 2017, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR4) == 2017, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR5) == 2017, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR6) == 2017, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR7) == 2017, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR8) == 2017, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR9) == 2017, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR10) == 2017, 1, 0))))))))))

table(dateColumns$donated2017)

dateColumns$donated2016 <- ifelse(year(dateColumns$GIFT_DATE_MR1) == 2016, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR2) == 2016, 1, 
  ifelse(year(dateColumns$GIFT_DATE_MR3) == 2016, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR4) == 2016, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR5) == 2016, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR6) == 2016, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR7) == 2016, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR8) == 2016, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR9) == 2016, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR10) == 2016, 1, 0))))))))))

table(dateColumns$donated2016)

dateColumns$donated2015 <- ifelse(year(dateColumns$GIFT_DATE_MR1) == 2015, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR2) == 2015, 1, 
  ifelse(year(dateColumns$GIFT_DATE_MR3) == 2015, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR4) == 2015, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR5) == 2015, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR6) == 2015, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR7) == 2015, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR8) == 2015, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR9) == 2015, 1,
  ifelse(year(dateColumns$GIFT_DATE_MR10) == 2015, 1, 0))))))))))

table(dateColumns$donated2015)


# Merging these new columns back with myData
oldDateColumns = dateColumns
dateColumns = dateColumns[,c(26:33)]
myData <- merge(x=myData,y=dateColumns,by="ID_NUMBER",all.x=TRUE)

colnames(myData)

#####  AUTOCORRELATION TO DETERMINE APPROPRIATE LAG  #####

set.seed(365)

# This creates a tibble for each date that had a donation, and the number of donations on that date.
dateTibble = oldDateColumns %>%
  gather("key", "value", GIFT_DATE_MR1, GIFT_DATE_MR2, GIFT_DATE_MR3, GIFT_DATE_MR4,
  GIFT_DATE_MR5, GIFT_DATE_MR6, GIFT_DATE_MR7, GIFT_DATE_MR8, GIFT_DATE_MR9, GIFT_DATE_MR10) %>%
  group_by(value) %>%
  summarize(n=n())

# Convert to data frame and changes column names.
datesCounts_df = data.frame(dateTibble)
colnames(datesCounts_df) = c("donationDate", "numDonations")

# Creating a data frame of all days (we want to include days with no donation). 
allDatesSince2000 = data.frame(seq(as.Date("2000/1/1"), today(), "days"))
colnames(allDatesSince2000) = c("donationDate")

# Left joining donation date data frame to the one with all dates, that way we can lag by day.
donationsPerDaySince2000 <- merge(x=allDatesSince2000,y=datesCounts_df,by="donationDate",all.x=TRUE)
dim(donationsPerDaySince2000)

# Setting number of donations to 0 for days with NAs currently.
donationsPerDaySince2000$numDonations = replace_na(donationsPerDaySince2000$numDonations, 0)

head(donationsPerDaySince2000)

#### AUTOCORRELATIONS for data FROM 2000-onwards

## BY DAY
# You actually don't see much yearly seasonality as I would have expected.
yearAutoCorByDay.2000 <- acf(donationsPerDaySince2000$numDonations, lag.max = 370)

# There is a trivial weekly trend.
monthAutoCorByDay.2000 <- acf(donationsPerDaySince2000$numDonations, lag.max = 31)

## BY WEEK

# Sum the daily counts to create numdonations by week.
xtsData <- as.xts(donationsPerDaySince2000$numDonations, order.by = as.Date(donationsPerDaySince2000$donationDate))
weekly <- apply.weekly(xtsData, sum)
weekly_df <- data.frame(weekly)

# Weeks currently stored in rownames. We'll change that now.
weekly_df <- cbind(rownames(weekly_df), data.frame(weekly_df, row.names=NULL))
colnames(weekly_df) <- c("week", "numDonations")

# Once again we don't see any seasonality. Next I will try using data from only 2015 onwards.
yearAutoCorWeekly.2000 <- acf(weekly_df$numDonations, lag.max = 55)

## BY MONTH

# Sum the daily counts to create numdonations by month.
xtsDataM <- as.xts(donationsPerDaySince2000$numDonations, order.by = as.Date(donationsPerDaySince2000$donationDate))
monthly <- apply.monthly(xtsDataM, sum)
monthly_df <- data.frame(monthly)

# Months currently stored in rownames. We'll change that now.
monthly_df <- cbind(rownames(monthly_df), data.frame(monthly_df, row.names=NULL))
colnames(monthly_df) <- c("month", "numDonations")

# You can see yearly correlation persists for many lags at the monthly level. ## THIS IS THE BEST
## GRAPH MAKING THE CASE FOR A YEAR LONG LAG. It's most predictive for longest, and on the largest data set.
yearAutoCorMonthly.2000 <- acf(monthly_df$numDonations, lag.max = 140)

#### AUTOCORRELATION FROM 2015 ONWARD

# Filtering day, week, and month data frames to include only 2015-onward.
# More recent dates are GREATER than older dates. 
donationsPerDaySince2015 = donationsPerDaySince2000[ donationsPerDaySince2000$donationDate > as.Date("2014-12-31"),]
weeklySince2015_df = weekly_df[ as.Date(weekly_df$week) > as.Date("2014-12-31"),]
monthlySince2015_df = monthly_df[ as.Date(monthly_df$month) > as.Date("2014-12-31"),]

## BY DAY

# Still only a mild weekly trend in data.
yearAutoCorDaily.2015 <- acf(donationsPerDaySince2015$numDonations, lag.max = 370)
monthAutoCorDaily.2015 <- acf(donationsPerDaySince2015$numDonations, lag.max = 31)

## BY WEEK 

# You do see pretty high autocorrelation at the year mark in the post-2015 data. Let's try post-2019. 
yearAutoCorWeekly.2015 <- acf(weeklySince2015_df$numDonations, lag.max = 55)

## BY MONTH

# Here the year autocorrelation is weaker and decays fast.
yearAutoCorMonthly.2015 <- acf(monthlySince2015_df$numDonations, lag.max = 55)


#### AUTOCORRELATION FROM 2019 ONWARD

weeklySince2019_df = weekly_df[ as.Date(weekly_df$week) > as.Date("2018-12-31"),]
monthlySince2019_df = monthly_df[ as.Date(monthly_df$month) > as.Date("2018-12-31"),]

## BY WEEKS

# By weeks, the yearly autocorrelation is clearly the highest, suggesting we should use that.
# Because this autocorrelation decays into "noise" and hits negative values within 2 lags, that suggests
# this is a stationary variable.
yearAutoCorWeekly.2019 <- acf(weeklySince2019_df$numDonations, lag.max = 55)

## BY MONTHS

# You don't see a yearly autocorrelation in months since 2019, but you see it in most other graphs, so
# I don't think it changes my final conclusion.
yearAutoCorMonthly.2019 <- acf(monthlySince2019_df$numDonations, lag.max = 24)


######## MUST ALSO REMOVE NAS FROM ALL SUCCESS VARIABLES (SET NA TO FALSE)

# Create three variables representing weeks since lastMFOSgift.
myData$madeDonationLastMonth = ifelse(
  myData$weeksSinceLastMFOSGift < 5,
  1,0)
myData$madeDonationLastMonth = replace_na(myData$madeDonationLastMonth, 0)

# Let's also make two more for within the past six months and year.
myData$madeDonationLastSixMonths = ifelse(
  myData$weeksSinceLastMFOSGift < 27,
  1,0)
myData$madeDonationLastSixMonths = replace_na(myData$madeDonationLastSixMonths, 0)

myData$madeDonationLastYear = ifelse(
  myData$weeksSinceLastMFOSGift < 53,
  1,0)
myData$madeDonationLastYear = replace_na(myData$madeDonationLastYear , 0)

##### REPLACING NAs

# A lot of columns have NAs that can be accurately represented
# by a real value. I replace those NAs here.
myData$madeDonationLastYear = replace_na(myData$madeDonationLastYear, FALSE)
myData$donated2021 = replace_na(myData$donated2021, 0)
myData$donated2020 = replace_na(myData$donated2020, 0)
myData$donated2019 = replace_na(myData$donated2019, 0)
myData$donated2018 = replace_na(myData$donated2018, 0)
myData$donated2017 = replace_na(myData$donated2017, 0)
myData$donated2016 = replace_na(myData$donated2016, 0)
myData$donated2015 = replace_na(myData$donated2015, 0)

myData$donationsWithinMonth = replace_na(myData$donationsWithinMonth, 0)
myData$TOTAL_YEARS = replace_na(myData$TOTAL_YEARS, 0)
myData$GIFT_COUNT = replace_na(myData$GIFT_COUNT, 0)

myData$FIRST_AMOUNT = replace_na(myData$FIRST_AMOUNT, 0)
myData$LAST_AMOUNT = replace_na(myData$LAST_AMOUNT, 0)
myData$LONGEST_CONSECUTIVE_STREAK = replace_na(myData$LONGEST_CONSECUTIVE_STREAK, 0)
myData$MFOS_FIRST_AMOUNT = replace_na(myData$MFOS_FIRST_AMOUNT, 0)
myData$MFOS_GIFT_COUNT = replace_na(myData$MFOS_GIFT_COUNT, 0)
myData$MFOS_TOTAL_COMMIT = replace_na(myData$MFOS_TOTAL_COMMIT, 0)
myData$MFOS_LAST_AMOUNT = replace_na(myData$MFOS_LAST_AMOUNT, 0)
myData$GIFT_MR1 = replace_na(myData$GIFT_MR1, 0)
myData$GIFT_MR2 = replace_na(myData$GIFT_MR2, 0)
myData$GIFT_MR3 = replace_na(myData$GIFT_MR3, 0)
myData$GIFT_MR4 = replace_na(myData$GIFT_MR4, 0)
myData$GIFT_MR5 = replace_na(myData$GIFT_MR5, 0)
myData$GIFT_MR6 = replace_na(myData$GIFT_MR6, 0)
myData$GIFT_MR7 = replace_na(myData$GIFT_MR7, 0)
myData$GIFT_MR8 = replace_na(myData$GIFT_MR8, 0)
myData$GIFT_MR9 = replace_na(myData$GIFT_MR9, 0)
myData$GIFT_MR10 = replace_na(myData$GIFT_MR10, 0)
myData$LARGEST_AMOUNT = replace_na(myData$LARGEST_AMOUNT, 0)
myData$MFOS_TOTAL_YEARS = replace_na(myData$MFOS_TOTAL_YEARS, 0)
myData$TOTALUF = replace_na(myData$TOTALUF, 0)
myData$REFUSALS = replace_na(myData$REFUSALS, 0)
myData$TOTRE = replace_na(myData$TOTRE, 0)
myData$CURCONSEC = replace_na(myData$CURCONSEC, 0)
myData$ADJ_AM1STGT = replace_na(myData$ADJ_AM1STGT, 0)  

# This code was used to find columns for discarding that were mostly empty.
#na_count = sapply(myData, function(y) sum(length(which(is.na(y)))))
#na_count_backup = na_count
#na_count = data.frame(na_count)
#na_count %>% filter ( na_count == 0)

#ggplot(myData, aes(x=GIFT_MR1)) + geom_histogram() + xlim(0,1000)

# The dummied variables have no NAs as confirmed below.
na_count = sapply(myDataDummied, function(y) sum(length(which(is.na(y)))))
na_count_backup = na_count
na_count = data.frame(na_count)
na_count %>% filter ( na_count == 0)

#####  CREATING AVG DONATION FREQUENCY  #####

# Calculate how frequently they donate by dividing
# the time between the first and last gift by the number of gifts.
myData$donationFrequencyInWeeks = (myData$weeksSinceFirstMFOSGift - 
  myData$weeksSinceLastMFOSGift) / myData$MFOS_GIFT_COUNT

# View frequency by year
table(floor(myData$donationFrequencyInWeeks / 4))


#####  CREATING LAGGED DEPENDENT VARIABLE BUILDING BLOCK  #####

# Marks YES if their last MFOS gift date + average weekly interval is within 4 weeks of today. 
myData$laggedAverageIntervalNow = ifelse( myData$weeksSinceLastMFOSGift - myData$donationFrequencyInWeeks < 5, "Yes", "No")
myData$laggedAverageIntervalNow = replace_na(myData$laggedAverageIntervalNow, "No")

table(myData$laggedAverageIntervalNow) ## 351 yes as of Feb 27


myData$laggedYearDonationNow = ifelse( myData$weeksSinceLastMFOSGift - 52 < 5, "Yes", "No")
myData$laggedYearDonationNow = replace_na(myData$laggedYearDonationNow, "No")

table(myData$laggedYearDonationNow ) ## 1041 yes as of March 28

#####  CREATING THE COMBINED DEPENDENT VARIABLE  #####

# If either the yearly autocorrelation or an individual's average interval suggests 
# a donation this month, we'll mark them YES as likelyToDonateThisMonth.
myData$likelyToDonateThisMonth = ifelse( myData$laggedYearDonationNow == "Yes" | 
  myData$laggedAverageIntervalNow == "Yes", "Yes", "No")

table(myData$likelyToDonateThisMonth) # 1200 YES after adjusting
# for MFOS. 17,300 YES prior to this. 

myData$likelyToDonateThisMonth = factor(myData$likelyToDonateThisMonth, levels=c("Yes", "No"))

## Removing the predecessors.
## I don't want to feed the model the building blocks of this dependent variable.
myData = myData %>% select(-laggedYearDonationNow, -laggedAverageIntervalNow)

### The idea is that false positives would be people you should still contact,
### because UFF wants a wide net and because many people have a "NO" in
### the dependent column because of some lack of information that caused
### them to fail the above ifelse statements. If the model says that
### based on the independent variables those people DO have that they're
### likely to donate, we could see that as exposing the aforementioned
### incomplete-data people. Hope this makes sense.

########   IDENTIFYING FIRST TIME DONORS      ########

# We'll create a variable that represents whether the donor has never 
# made a gift before, for MFOS specifically and then UF generally.

myData$donatedToMFOS = ifelse( myData$MFOS_GIFT_COUNT == 0, 0, 1)

myData$donatedToUF = ifelse ( myData$GIFT_COUNT == 0, 0, 1)

########   REMOVING DONATION BASED DUMMY VARIABLES   #######

# Because we are adding the dummy variables back for the 
# purpose of identifying new donors who are likely to
# donate, we are not going to use any dummy variables
# that depend on / are only useful if the person has
# already donated.

# myDataDummied currently has 577 variables. Let's reduce that.

# These cols represent allocation of first/largest gift.
myDataDummied = myDataDummied[,-c(19:117)]

# Cols for Gift_Type 1-10
giftTypeCols = c(c(147:165), c(168:184), c(187:203), c(206:220), c(223:239),
  c(242:256), c(259:276), c(279:293), c(296:311), c(314:328))

myDataDummied = myDataDummied[, -giftTypeCols ]

# Down to 314. I will remove more for lack of impact, 
# but this is fine for now.

# Removing MR{x}_FOS_IND_{N/Y} variables. They showed really high correlation
# with a bunch of other variables.
myDataDummied = myDataDummied[, -c(147:166)]

# We'll also remove the touch type columns, as these don't really
# describe the non-donors. They just describe how we 
# approached them.
myDataDummied = myDataDummied[, -c(147:290)]

# Just checked -- none of the dummy variables have any NAs.
# This checks out intuitively. 



########   MODEL TRAINING AND FINAL SUBSET    ########  

# Training control setup
trn_ctrl <- trainControl(summaryFunction = twoClassSummary,
  savePredictions = TRUE,
  sampling = "smote",
  method = "repeatedcv",
  number = 2, # Reduced from 10 to 2 in order to make models run.
  repeats = 2,
  classProbs = TRUE,
  allowParallel = FALSE)


# We're not going to feed the model any columns with NAs.
# Here we identify those.
na_count = sapply(myData, function(y) sum(length(which(is.na(y)))))
na_count_backup = na_count
na_count = data.frame(na_count)
na_count %>% filter ( na_count == 0)

# Subsetting for columns with 0 NAs.
dataForModel = myData[, colSums(is.na(myData)) == 0]

##### ADDING CLICKDATATABLE BACK IN    #######
dataForModel <- merge(x=dataForModel, y=clickDataTable, by="ID_NUMBER",all.x=TRUE)

colnames(dataForModel)
sum(!is.na(dataForModel$TOTAL_VISITS)) #~10k IDs now have click data 

dataForModel$TOTAL_VISITS = replace_na(dataForModel$TOTAL_VISITS, 0)

#####  PREPROCESSING  ######

# This line of code crashed my shit before.
preProcValues <- preProcess(dataForModel, method = c("center", "scale"), na.remove=TRUE)
myDataTransformed <- predict(preProcValues, dataForModel)

# We need the ID_Number column not to be centered and scaled.
# Because we're going to merge the dummy vars back in.
myDataTransformed$ID_NUMBER = dataForModel$ID_NUMBER

#myDataBackup = myData

dataForModelScaled = myDataTransformed

##### ADDING DUMMIES BACK IN    #######
dataForModelScaled <- merge(x=dataForModelScaled, y=myDataDummied, by="ID_NUMBER",all.x=TRUE)



########   REMOVING CORRELATED & UNIMPORTANT (PER GLM+RF) VARS    ########  

dataForCorrelation = dataForModelScaled

# If two variables have too much correlation, I compare their p-value
# and remove the less important one.
removedForCor = c("GIFT_MR1", "LONGEST_CONSECUTIVE_STREAK",
  "GIFT_MR9", "ANY_EMP", "FIRST_AMOUNT", "TOTALUF", "MFOS_FIRST_AMOUNT",
  "donated2020")

#removedForNotInt = c("likelyToDonateThisMonth","noDonationYet")

# If the p-value is miserable or the random forest model said it was
# unimportant, it's removed below.
removedForUnimportance = c("VARSPORT", "CHILDALUM", "CREDCARD",
  "GATRTAG", "ETHNALUM", "EVENT", "EMAIL", "UF_EMP", "UFF_BRD",
  "SEC_HOME", "FFF", "LIFEMEM", "madeDonationLastMonth",
  "madeDonationLastYear")

dummyRemovedInsig = c("HQ_YN_N", "DR_PERFECT_YN", "AGE_CAT_25 AND UNDER", "CLASS_CAT_1940 and Earlier",
  "HIGHEST_HH_RATING_A - $100M +", "REGION_San Antonio", "REGION_Detroit",
  "CLASS_CAT_2011-present", "GENDER_N", "HIGH_DEG_R", "MARSTAT_Married at Death",
  "REGION_Nevada", "REGION_Arizona (except Phoenix)", "SPRECTYP_UF and UFF Employees", 
  "HIGH_DEG_ ", "MARSTAT_Deceased Spouse", "PRIZM_Upscale", "MARSTAT_Other",
  "AGGR_ENT_CODE_UF and UFF Employees", "HIGH_DEG_A", "AGE_CAT_31-35",
  "AGE_CAT_36-40", "AGE_CAT_41-45", "AGE_CAT_OVER 85", "CLASS_CAT_1941-1950",
  "CLASS_CAT_1951-1960", "MARSTAT_Formerly Married", "PRIZM_Poor", "REGION_Austin",
  "REGION_Denver Metro", "REGION_Greater Southern California", "REGION_Louisiana",
  "REGION_Michigan (except Detroit)", "REGION_Monroe", 
  "REGION_Ohio (except Cincinnati)", "REGION_Oregon", "REGION_San Diego",
  "REGION_Seattle", "REGION_Upstate New York", "HIGHEST_HH_RATING_O - No Rating",
  "HIGHEST_HH_RATING_C - $50M - $74,999,999", "HIGHEST_HH_RATING_D - $25M - $49,999,999",
  "HIGHEST_HH_RATING_E - $10M - $24,999,999", "HIGHEST_HH_RATING_F - $5M - $9,999,999",
  "HIGHEST_HH_RATING_G - $1M - $4,999,999", "REGION_Minnesota (except Minn,/St. Paul)",
  "CAROUTRC", "PRIZM_Midscale", "RECCONTACT_1 to 2 years", "RECCONTACT_10 or more years",
  "RECCONTACT_2 to 5 years", "RECCONTACT_5 to 10 years", "REGION_Boston", 
  "REGION_Houston", "REGION_Massachusetts (except Boston)", "REGION_Phoenix",
  "REGION_Missouri", "REGION_Los Angeles", "REGION_NC Highlands",
  "REGION_Philadelphia", "REGION_Pittsburgh", "REGION_San Francisco Bay Metro",
  "HIGH_DEG_C", "MARSTAT_Separated", "MARSTAT_Marital Status Unknown",
  "HIGHEST_HH_RATING_B - $75M - $99,999,999", "SPRECTYP_Students", 
  "HIGH_DEG_I", "REGION_Cincinnati", "HIGH_DEG_F", "REGION_Minneapolis/St. Paul",
  "REGION_Kentucky", "REGION_Washington (except Seattle)", "REGION_Chicago",
  "REGION_NC Research Triangle", "REGION_Charlotte", "REGION_Okeechobee",
  "GENDER_U", "REGION_Naples", "REGION_North Central", "REGION_Trenton",
  "REGION_Dallas", "REGION_Pensacola", "REGION_Ft. Myers", "MARSTAT_Partner (Significant Other)",
  "REGION_Ft. Pierce", "REGION_Lake City", "REGION_Hernando/Citrus", "REGION_Brevard",
  "REGION_Daytona", "MARSTAT_Widowed", "REGION_Atlanta Metro", "REGION_Sarasota/Manatee",
  "HIGH_DEG_D", "REGION_Greater D.C.", "REGION_Tallahassee", "RECONTACT_7 to 11 months",
  "AGE_CAT_81-85", "REGION_St. Pete/Clearwater", "REGION_New York City Metropolitan",
  "SPRECTYP_Family", "REGION_unassigned", "AGE_CAT_61-65", "AGE_CAT_66-70",
  "AGE_CAT_56-60", "AGE_CAT_51-55", "AGE_CAT_46-50"
  )

# Toggle this one if you'd like to run a model identifying first time donors.
# It will force the model to rely only on gift-independent variables.
donationDependent = c("ADJ_AM1STGT", "CURCONSEC", "VELOCITY", "RFM_SCORE",
  "GIFT_MR2", "GIFT_MR3", "GIFT_MR4", "GIFT_MR5", "GIFT_MR6", "GIFT_MR7",
  "GIFT_MR8", "GIFT_MR10", "MFOS_TOTAL_COMMIT", "MFOS_GIFT_COUNT",
  "MFOS_LAST_AMOUNT", "MFOS_TOTAL_YEARS", "GIFT_COUNT", "LAST_AMOUNT", 
  "LARGEST_AMOUNT", "TOTAL_YEARS", "donationsWithinMonth", "donated2021",
  "donated2019", "donated2018", "donated2017", "donated2016", "donated2015",
  "madeDonationLastSixMonths", "MFOS_TOTAL_COMMIT_RANGE_ ", "TOTAL_COMMIT_RANGE_ " )

# Toggle this to overwrite donationDependent if you're running
# the ALL DONORS model.
#donationDependent = c()

# These were the least predictive variables on our final random forest model
# run. (Regarding our first time donor model).
finalRFremoved = c("REGION_Polk", "HIGHEST_HH_RATING_H - $500,000 - $999,999",
  "REGION_Panama City", "REGION_Ocala", "MARSTAT_Divorced", "RECCONTACT_7 to 11 months",
  "AGE_CAT_76-80", "REGION_Miami", "REGION_Ft. Lauderdale", "REGION_Orlando",
  "REGION_Palm Beach", "PRIZM_LowerMid", "MARSTAT_Single", "HIGH_DEG_P",
  "AGE_CAT_26-30", "EXECJOB", "PRIZM_UpperMid", "CLASS_CAT_1971-1980",
  "CLASS_CAT_2001-2010", "RECCONTACT_Last 6 months", "HIGH_DEG_M",
  "CLASS_CAT_1991-2000", "CLASS_CAT_1981-1990", "REGION_Jacksonville",
  "HIGHEST_HH_RATING_K - $50,000 - $99,999", "AGE_CAT_71-75", "DR_PERFECT_YN_N")

removed = c(removedForCor, removedForUnimportance, dummyRemovedInsig,
  donationDependent, finalRFremoved)

dataForCorrelation = dataForCorrelation[, !(names(dataForCorrelation) %in% removed )]

## Calculate correlations among variables and print the correlations above .7.
## All correlations above .7 were removed.
res <- cor(dataForCorrelation[,-c(16)])  # use c(UPDATE THIS) if you didn't remove donation-dependent vars
res_df <- as.data.frame(as.table(res))
#colnames(res_df)
res_df[ res_df$Freq > .5 & res_df$Freq < 1.00,]


## This code ran a simple model that I used to compare two correlated vars p-values
# and choose one for removal. I also consulted our first iteration random forest model's
# variable importance graph.
dataForModelOne = dataForModelScaled[, !(names(dataForModelScaled) %in% removed)]
modelOne <- glm(likelyToDonateThisMonth ~.-ID_NUMBER -donatedToMFOS, family = binomial(link="logit"), data = dataForModelOne ,  maxit = 100)
summary(modelOne)
with(summary(modelOne), 1 - deviance / null.deviance) # prints R, currently .45

# Finally we'll remove the identified columns from our data for the models.
dataForModelScaled = dataForModelScaled[, !(names(dataForModelScaled) %in% removed)]

#####  SPLITTING TEST AND TRAIN DATA  #####

set.seed(365)
lastMonth_idx <- createDataPartition(dataForModelScaled$likelyToDonateThisMonth,
 p = 0.7, list = FALSE)
lastMonth_trn <- dataForModelScaled[lastMonth_idx,]
lastMonth_tst <- dataForModelScaled[-lastMonth_idx,]

table(lastMonth_trn$likelyToDonateThisMonth)
table(lastMonth_tst$likelyToDonateThisMonth)
# Proportions look good. Let's continue.

# Commenting out LogReg and DTree for now.
########   LOGISTIC REGRESSION MODEL    ########  

# GLM model on the likelyToDonateThisMonth training data. 
## Took 15 minutes to run.
logreg_model <- train(likelyToDonateThisMonth~.-ID_NUMBER -donatedToMFOS, data=lastMonth_trn,
  method = "glm",
  family = "binomial",
  metric = "ROC",
  trControl = trn_ctrl )

logreg_model
summary(logreg_model)
# All the p values are significant.
#View(logreg_model$pred)

lvs <- c("Yes", "No")
truth <- factor(logreg_model$pred$obs)
pred <- factor(logreg_model$pred$pred)
xtab <- table(pred, truth)
confusionMatrix(xtab)

# Applying the trained model to the test data lastMonth_tst
pred_logreg <- predict.train(object=logreg_model, 
  newdata = lastMonth_tst, type = "raw")
prob_logreg <- predict.train(object=logreg_model, 
  newdata = lastMonth_tst, type = "prob"

# Evaluate results
confusionMatrix(pred_logreg, lastMonth_tst$likelyToDonateThisMonth)

# Save the logistic model results in new data frame.
lastMonth_tst_logreg <- lastMonth_tst
lastMonth_tst_logreg$pred <- pred_logreg
lastMonth_tst_logreg$prob <- prob_logreg


########   DECISION TREE MODEL    ########  

# Now we'll do a decision tree model. 
# Took 15 minutes to run.
dtree_model <- train(likelyToDonateThisMonth~.-ID_NUMBER, data=lastMonth_trn,
  method="rpart",
  metric="ROC",
  trControl = trn_ctrl)

dtree_model
summary(dtree_model$finalModel) 


# Predict the test data using the model 
pred_dtree <- predict.train(object = dtree_model, newdata = lastMonth_tst, type = "raw")
prob_dtree <- predict.train(object = dtree_model, newdata = lastMonth_tst, type = "prob")
# Evaluate the model performance on the test data
confusionMatrix(pred_dtree, lastMonth_tst$likelyToDonateThisMonth) 

# Save the decision tree model results in a new data frame
lastMonth_tst_dtree <- lastMonth_tst
lastMonth_tst_dtree$pred <- pred_dtree
lastMonth_tst_dtree$prob <- prob_dtree

fancyRpartPlot(dtree_model$finalModel)



########   RANDOM FOREST MODEL    ########  

mtry <- 1:3
tunegrid <- expand.grid(.mtry=mtry)

# The RF model was still not running (crashed computer)
# so I am going to feed it a subset of half of the training data.
lastMonth_trn_backup = lastMonth_trn
further_idx <- createDataPartition(lastMonth_trn$likelyToDonateThisMonth,
 p = 0.5, list = FALSE)
lastMonth_trn <- lastMonth_trn[further_idx,] 

# Now running RF model with half the observations.
# It ran successfully with half in 15 minutes.
rf_model <- train(likelyToDonateThisMonth~.-ID_NUMBER -donatedToMFOS -PERSCONT -OTHRCONT, data = lastMonth_trn,
                      method="rf",
                      metric="ROC",
                      tuneGrid = tunegrid,
                      trControl = trn_ctrl)

# We want to restore the training data to its full size
# in case we run further models.
lastMonth_trn = lastMonth_trn_backup

rf_model
summary(rf_model$results)


# predict the test data using the model
pred_rf <- predict.train(object = rf_model, newdata = lastMonth_tst, type = "raw")
prob_rf <- predict.train(object = rf_model, newdata = lastMonth_tst, type = "prob")
# and let's evaluate the model performance on the test data
confusionMatrix(pred_rf, lastMonth_tst$likelyToDonateThisMonth)

# Evaluate the model performance on FIRST TIME donors only.
# Attach predictions to a new firstTimers data frame.
firstTimers = lastMonth_tst
firstTimers$rf_pred = pred_rf
# Subset firstTimers to include only first-timers.

# The reason this says "< 1" and not "== 0" is that this column was 
# centered and scaled, so the former 0s are now -.24 or thereabouts.
firstTimers = firstTimers[firstTimers$donatedToMFOS < 1,]
confusionMatrix(firstTimers$rf_pred, firstTimers$likelyToDonateThisMonth)


# lets find the important variables with VIF
rfVarImp <- varImp(rf_model$finalModel)
# rfVarImp <- arrange(rfVarImp, desc(Overall))

# Graphs each variable's importance.
rfVarImp %>%
  mutate(Variable = factor(rownames(rfVarImp))) %>%
  ggplot(aes(x = reorder(Variable,desc(Overall)), y = Overall))+
    geom_bar(stat = "identity", fill = "#f36f21")+
    ylab("Importance of Variables in the rf model")+
    xlab("Overall importance VIF")+
    coord_flip()+
    scale_x_discrete(limits=rev)
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# This just flips the previous graph for a different look.
rfVarImp %>%
  mutate(Variable = factor(rownames(rfVarImp))) %>%
  ggplot(aes(x = reorder(Variable,(Overall)), y = Overall))+
    geom_bar(stat = "identity", fill = "#f36f21")+
    ylab("Variable Importance")+
    xlab("Variable Name")+
    #coord_flip()+
    scale_x_discrete(limits=rev)+
    theme(axis.text = element_text(angle = 90),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white")
   )


table(rfVarImp$Overall)

#####   3/28/21 VISUALIZATIONS     ######

# This info is for infographic purposes. I'm passing on %s to teammates.
theSelected = firstTimers[firstTimers$rf_pred == "Yes",]

theSelectedBackup = theSelected
firstTimersBackup = firstTimers

# I want the data that is not centered and scaled. I'm filtering the
# pre-scaled data by the ID numbers of the selected.
theSelected = dataForModel[dataForModel$ID_NUMBER %in% theSelected$ID_NUMBER, ]
firstTimers = dataForModel[dataForModel$ID_NUMBER %in% firstTimers$ID_NUMBER, ]

# Now we just have to remerge the dummy vars, which were added after centering
# and scaling and thus aren't in dataForModel yet.
theSelected <- merge(x=theSelected, y=myDataDummied, by="ID_NUMBER",all.x=FALSE)
firstTimers <- merge(x=firstTimers, y=myDataDummied, by="ID_NUMBER",all.x=FALSE)

table(theSelected$CURMEM)
table(firstTimers$CURMEM)

table(theSelected$donatedToUF)
table(firstTimers$donatedToUF)

table(theSelected$`SPRECTYP_Other Alumni`)
table(firstTimers$`SPRECTYP_Other Alumni`)

table(theSelected$RETIRED)
table(firstTimers$RETIRED)

table(theSelected$LINKEDREL)
table(firstTimers$LINKEDREL)

mean(theSelected$LINKEDREL)
mean(firstTimers$LINKEDREL)

table(theSelected$MARSTAT_Married)
table(firstTimers$MARSTAT_Married)

mean(theSelected$NUMCHILD)
mean(firstTimers$NUMCHILD)

mean(theSelected$TOTRE)
mean(firstTimers$TOTRE)

table(theSelected$FRATSOR)
table(firstTimers$FRATSOR)

table(theSelected$GENDER_M)
table(firstTimers$GENDER_M)

table(theSelected$TOTAL_VISITS > 0)
table(firstTimers$TOTAL_VISITS > 0)

mean(theSelected$TOTAL_VISITS)
mean(firstTimers$TOTAL_VISITS)


table(theSelected$CLASS_CAT_1961-1970)
table(firstTimers$CLASS_CAT_1961-1970)

table(theSelected$ACT_EMP)
table(firstTimers$ACT_EMP)


#####  BOXPLOTS  #####

plot01 <- ggplot(data = lastMonth_trn) + 
  geom_boxplot(aes(x = likelyToDonateThisMonth, y = RFM_SCORE), outlier.shape=NA) + 
  labs(title = "RFM_SCORE") +
  ylim(0,6)
plot02 <- ggplot(data = lastMonth_trn) + 
  geom_boxplot(aes(x = likelyToDonateThisMonth , y = GIFT_COUNT), outlier.shape=NA) + 
  labs(title = "Total Gift Count") + 
  ylim(0,4)
plot03 <- ggplot(data = lastMonth_trn) + 
  geom_boxplot(aes(x = likelyToDonateThisMonth , y = CURCONSEC), outlier.shape=NA) + 
  labs(title = "CurConSec") +
  ylim(0,3)
plot04 <- ggplot(data = lastMonth_trn) + 
  geom_boxplot(aes(x = likelyToDonateThisMonth , y = GIFT_MR3), outlier.shape=NA) + 
  labs(title = "Third Gift Amount") +
  ylim(0,.035)
grid.arrange(plot01, plot02, plot03, plot04, ncol = 2)



##### K NEAREST NEIGHBORS #####

library(class)

k_targetCat = lastMonth_trn[,15]
k_train = lastMonth_trn[,-c(9,10, 16, 1)] # col 15 is likelyToDonateThisMonth
k_test = lastMonth_tst[,-c(9,10, 16, 1)]


#myk <- knn(k_train, k_test, cl=k_targetCat, k=5)

#tab <- table(myk,k_targetCat)

#svm(likelyToDonateThisMonth ~ ., data=k_train)


#####  OTHER VISUALIZATIONS  ######

totReGraph <- lastMonth_trn[c(1:2500),] %>% ggplot(aes(x=RFM_SCORE,y=TOTRE))+ 
     geom_jitter(size=2, alpha=0.8, color="salmon3") + 
     labs(title="Real Estate Value plotted with RFM Score", 
     x = "RFM Score (Recency, Frequency, Money)", y = "Total Real Estate Value") + 
     theme(axis.title = element_text()) +
     geom_smooth(method='lm', formula=y~x) +
     ylim(0,5)

totReGraph 

ggsave("finalVisualizationZZZ.png")


graph2 <- lastMonth_trn[c(1:2500),] %>% ggplot(aes(x=GIFT_COUNT,y=LARGEST_AMOUNT))+ 
     geom_jitter(size=2, alpha=0.8, color="darkseagreen4") + 
     labs(title="Largest Gift Amount plotted with Total Gift Count", 
     x = "Total Gift Count", y = "Largest Amount") + 
     theme(axis.title = element_text()) +
     geom_smooth(method='lm', formula=y~x) +
     ylim(0,10) + xlim(0,10)

ggsave("finalVisualizationBBB.png")








