setwd("C:/Users/kenj/Google Drive/Research/Annual Report/02 - Feb")
library(tidyverse)
library(scales)
library(RColorBrewer)
library(psych)

#Load in the data and make the needed adjustments.
waldo <- read.csv("leadwaldo.csv", header = T, stringsAsFactors = T)

waldo$Created.Date <- as.Date(waldo$Created.Date, "%m/%d/%Y")
waldo$Contacted.Date <- as.Date(waldo$Contacted.Date, "%m/%d/%Y")
waldo$Scheduled.Date <- as.Date(waldo$Scheduled.Date, "%m/%d/%Y")
waldo$Showed.Date <- as.Date(waldo$Showed.Date, "%m/%d/%Y")
waldo$Enrollment.Date <- as.Date(waldo$Enrollment.Date, "%m/%d/%Y")

#Segment the leads according to the month of their created date.
jan <- filter(waldo, Created.Date < "2018-02-01")
feb <- filter(waldo, Created.Date > "2018-01-31")


count(jan, How.did.you.Hear.about.us.)
count(feb, How.did.you.Hear.about.us.)
#Looks like referred leads are down 100, Internet is down 100, and Radio is down 1500.

#Let's see if there is any difference between the two types of campuses
levels(waldo$Market)

IDL <- filter(waldo, Market == "Austin, TX - LIVE ONLINE" | Market == "Austin" | 
                Market == "Charleston, SC - LIVE ONLINE"  | Market == "Clarksville" |
                Market == "Clarksville, TN - LIVE ONLINE" | Market == "Columbus, GA - LIVE ONLINE" |
              Market == "Columbus_GA" | Market == "DC"  | Market == "DC/Baltimore - LIVE ONLINE" |
              Market == "Fayetteville" | Market == "Fayetteville - LIVE ONLINE" | Market == "New York Metro - LIVE ONLINE" |
                Market == "NY" | Market == "Other Market - LIVE ONLINE")

CAMPUS <- filter(waldo, Market == "Arlington" | Market == "Charlotte" | Market == "Columbus"  | Market == "Dallas" |
             Market == "Houston" | Market == "Indianapolis" | Market == "Raleigh" | Market == "Raleigh, NC")

IDL_jan <- filter(IDL, Created.Date < "2018-01-29" & Created.Date > "2018-01-03")
IDL_feb <- filter(IDL, Created.Date > "2018-01-31" & Created.Date < "2018-02-26")
CAMPUS_jan <- filter(CAMPUS, Created.Date < "2018-01-29" & Created.Date > "2018-01-03")
CAMPUS_feb <- filter(CAMPUS, Created.Date > "2018-01-31" & Created.Date < "2018-02-26")

#How are we doing on Dials to each type of campus?
summary(CAMPUS_jan$Dials)
describe(CAMPUS_jan$Dials)
summary(CAMPUS_feb$Dials)
describe(CAMPUS_feb$Dials)

summary(IDL_jan$Dials)
describe(IDL_jan$Dials)
summary(IDL_feb$Dials)
describe(IDL_feb$Dials)

#Let's look at campus a little closer. We appear to be having an issue there.
#Need to create a new variable so I can graph this correctly.
CAMPUS$Month <- "Other"
CAMPUS$Month[CAMPUS$Created.Date < "2018-01-29" & CAMPUS$Created.Date > "2018-01-03"] <- "Jan"
CAMPUS$Month[CAMPUS$Created.Date > "2018-01-31" & CAMPUS$Created.Date < "2018-02-26"] <- "Feb"


IDL$Month <- "Other"
IDL$Month[IDL$Created.Date < "2018-01-29" & IDL$Created.Date > "2018-01-03"] <- "Jan"
IDL$Month[IDL$Created.Date > "2018-01-31" & IDL$Created.Date < "2018-02-26"] <- "Feb"

CAMPUS %>%
  filter(Month == "Feb" | Month == "Jan") %>%
  ggplot(aes(x = Month, y = Dials))+
  geom_bar(aes(fill = Market),
               stat = "identity")


#I want to look at a few individual markets
CAMPUS %>%
  filter(Month == "Feb" | Month == "Jan") %>%
  filter(Market == "Charlotte" | Market == "Columbus" | Market == "Dallas" | Market == "Houston") %>%
  ggplot(aes(x = Month, y = Dials))+
  geom_bar(aes(fill = Market),
           stat = "identity")

#Let's find the proportion of dials/lead across the markets. For this, I need a variable that is a just leads
CAMPUS$leadnum <- 1
IDL$leadnum <- 1

CAMPUS %>%
  filter(Month == "Jan") %>%
  group_by(Market) %>%
  summarise(dials = sum(Dials, na.rm=T)/sum(leadnum))

CAMPUS %>%
  filter(Month == "Feb") %>%
  group_by(Market) %>%
  summarise(dials = sum(Dials, na.rm=T)/sum(leadnum))


IDL %>%
  filter(Month == "Jan") %>%
  group_by(Market) %>%
  summarise(dials = sum(Dials, na.rm=T)/sum(leadnum))

IDL %>%
  filter(Month == "Feb") %>%
  group_by(Market) %>%
  summarise(dials = sum(Dials, na.rm=T)/sum(leadnum))


#What percentage of leads did each campus have?

CAMPUS %>%
  filter(Month == "Jan") %>%
  group_by(Market) %>%
  summarise(leads = sum(leadnum, na.rm=T)/2271)

CAMPUS %>%
  filter(Month == "Feb") %>%
  group_by(Market) %>%
  summarise(leads = sum(leadnum, na.rm=T)/2124)


#What about our lead sources? Did those change?
CAMPUS %>%
  filter(Month == "Jan") %>%
  group_by(How.did.you.Hear.about.us.) %>%
  summarise(leads = sum(leadnum), dials = sum(Dials, na.rm=T)/sum(leadnum))

CAMPUS %>%
  filter(Month == "Feb") %>%
  group_by(How.did.you.Hear.about.us.) %>%
  summarise(leads = sum(leadnum), dials = sum(Dials, na.rm=T)/sum(leadnum))

#No changes really. I wonder what is going on with TV. That's the only category that didn't drop in dials or leads.

CAMPUS %>%
  filter(Month == "Jan") %>%
  filter(How.did.you.Hear.about.us.== "TV Ad") %>%
  group_by(How.did.you.Hear.about.us...Details) %>%
  summarise(leads = sum(leadnum), dials = sum(Dials, na.rm=T)/sum(leadnum))

  CAMPUS %>%
    filter(Month == "Feb") %>%
    filter(How.did.you.Hear.about.us.== "TV Ad") %>%
    group_by(How.did.you.Hear.about.us...Details) %>%
    summarise(leads = sum(leadnum), dials = sum(Dials, na.rm=T)/sum(leadnum))
  
  
#Let's look deeper into sources. Let's start with Radio since that is the biggest area for improvement.
radio_feb <-  CAMPUS %>%
    filter(Month == "Feb") %>%
    filter(How.did.you.Hear.about.us.== "Radio") %>%
    group_by(How.did.you.Hear.about.us...Details) %>%
    summarise(leads = sum(leadnum), dials = sum(Dials, na.rm=T)/sum(leadnum))

radio_jan <-  CAMPUS %>%
  filter(Month == "Jan") %>%
  filter(How.did.you.Hear.about.us.== "Radio") %>%
  group_by(How.did.you.Hear.about.us...Details) %>%
  summarise(leads = sum(leadnum), dials = sum(Dials, na.rm=T)/sum(leadnum))


CAMPUS %>%
  filter(Month == "Feb" | Month == "Jan") %>%
  filter(How.did.you.Hear.about.us.== "Radio") %>%
  ggplot()+
  geom_bar(mapping = aes(x=How.did.you.Hear.about.us...Details, fill = Month),
           position = "dodge",
           stat = "count",
           na.rm = T)+
  coord_flip()

CAMPUS %>%
  filter(Month == "Feb" | Month == "Jan") %>%
  filter(How.did.you.Hear.about.us.== "Radio") %>%
  filter(Admissions.Status == "Closed - Enrolled") %>%
  ggplot()+
  geom_bar(mapping = aes(x=How.did.you.Hear.about.us...Details, fill = Month),
           position = "dodge",
           stat = "count",
           width = 0.5,
           na.rm = T)+
  coord_flip()

CAMPUS %>%
  filter(How.did.you.Hear.about.us. == "Radio") %>%
  group_by(How.did.you.Hear.about.us...Details) %>%
  summarise(Janleads = sum(leadnum[Month == "Jan"]),
            Janratio = 100*sum(leadnum[Admissions.Status == "Closed - Enrolled" & Month == "Jan"])/sum(leadnum[Month == "Jan"]),
            Febleads = sum(leadnum[Month == "Feb"]),
            Febratio = 100*sum(leadnum[Admissions.Status == "Closed - Enrolled" & Month == "Feb"])/sum(leadnum[Month == "Feb"])) %>%
  print(n=40)

#We should export this to excel.

RalphExport <- CAMPUS %>%
  filter(How.did.you.Hear.about.us. == "Radio") %>%
  group_by(How.did.you.Hear.about.us...Details) %>%
  summarise(Janleads = sum(leadnum[Month == "Jan"]),
            Janratio = 100*sum(leadnum[Admissions.Status == "Closed - Enrolled" & Month == "Jan"])/sum(leadnum[Month == "Jan"]),
            Febleads = sum(leadnum[Month == "Feb"]),
            Febratio = 100*sum(leadnum[Admissions.Status == "Closed - Enrolled" & Month == "Feb"])/sum(leadnum[Month == "Feb"]))

write.csv(RalphExport, "RalphExport.csv")
