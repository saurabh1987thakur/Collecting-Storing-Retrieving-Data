
######################################## Data Cleaning ########################################## 

Cleaning  <- function(){

data <- read.csv(file="crime_incidents_2013.csv",header=TRUE,sep=",")  
  
# 'Unk' values has been changed into NA in PSA column 
l <- length(data$PSA)
for (i in 1:l)
{ 
  if (data$PSA[i] == 'Unk')
    data$PSA[i] <- NA
}


# Blanks in column Voting Precinct has been changed into NA

le <- length(data$VOTING_PRECINCT)
for (i in 1:l)
{ 
  if (data$VOTING_PRECINCT[i] == "" )
    data$VOTING_PRECINCT[i] <- NA
}

# Blanks in column Block Group has been changed into NA

len <- length(data$BLOCK_GROUP)
for (i in 1:l)
{ 
  if (data$BLOCK_GROUP[i] == "" )
    data$BLOCK_GROUP[i] <- NA
}

# Blanks in column Start Date and End Date has been changed into NA

leng <- length(data$START_DATE)
for (i in 1:l)
{ 
  if (data$START_DATE[i] == "" )
    data$START_DATE[i] <- NA
  if (data$END_DATE[i] == "" )
    data$END_DATE[i] <- NA 
}

# Blanks in column Census Tract has been changed into NA

data$CENSUS_TRACT[is.na(data$CENSUS_TRACT)] <- 0

# Blanks in column Neighborhoodcluster has been changed into 0

data$NEIGHBORHOODCLUSTER[is.na(data$NEIGHBORHOODCLUSTER)] <- 0

# Create new cleaned data file(CSV) 

write.csv(data, file = "new_crime_incidents_2013.csv")

}

Cleaning()   # Function to clean the data and create new data file




############################# Crime Incidents-2013 Database Deisgn ###############################


library(RSQLite)
database <- dbConnect(SQLite(), dbname="CrimeIncidents.sqlite")


# Create Table 'Incident'

data <- read.csv("new_crime_incidents_2013.csv")[,c(2,3,4,5,6,7)]

write.csv(data, file = "Incident.csv")
dbWriteTable(conn = database, name = "Incident",value = "Incident.csv", row.names = FALSE, header = TRUE)


# Create Table 'Address'

data2 <- read.csv("new_crime_incidents_2013.csv")[,c(8,9,10,11,12,13,14,15,2)]
AddressID <- 1:35896
data1 <- cbind(AddressID,data2)

write.csv(data1, file = "Address.csv")
dbWriteTable(conn = database, name = "Address",value = "Address.csv", row.names = FALSE, header = TRUE)


# Create Table 'Census'

data4 <- read.csv("new_crime_incidents_2013.csv")[,c(18,19,20,21,2)]
CensusID <- 1:35896
data5 <- cbind(CensusID,data4)

write.csv(data5, file = "Census.csv")
dbWriteTable(conn = database, name = "Census",value = "Census.csv", row.names = FALSE, header = TRUE)



# Create Table 'Business Improvement'

data6 <- read.csv("new_crime_incidents_2013.csv")[,c(16,17,2)]
ImprovementID <- 1:35896
data7 <- cbind(ImprovementID,data6)

write.csv(data7, file = "Business_Improvement.csv")
dbWriteTable(conn = database, name = "Business_Improvement",value = "Business_Improvement.csv", row.names = FALSE, header = TRUE)



# Create Table 'ImprovementDistrict'


dbSendQuery(conn = database,  "CREATE TABLE ImprovementDistrict (BusinessImprovementDistrict 
            TEXT)")

dbSendQuery(conn = database,
            "INSERT INTO ImprovementDistrict
         VALUES ('ADAMS MORGAN')")
dbSendQuery(conn = database,
            "INSERT INTO ImprovementDistrict
         VALUES ('ANACOSTIA')")
dbSendQuery(conn = database,
            "INSERT INTO ImprovementDistrict
         VALUES ('CAPITOL HILL')")
dbSendQuery(conn = database,
            "INSERT INTO ImprovementDistrict
         VALUES ('CAPITOL RIVERFRONT')")
dbSendQuery(conn = database,
            "INSERT INTO ImprovementDistrict
         VALUES ('DOWNTOWN')")
dbSendQuery(conn = database,
            "INSERT INTO ImprovementDistrict
         VALUES ('GEORGETOWN')")
dbSendQuery(conn = database,
            "INSERT INTO ImprovementDistrict
         VALUES ('GOLDEN TRIANGLE')")
dbSendQuery(conn = database,
            "INSERT INTO ImprovementDistrict
            VALUES ('MOUNT VERNON TRIANGLE CID')")
dbSendQuery(conn = database,
            "INSERT INTO ImprovementDistrict
            VALUES ('NOMA')")


# Create Table 'DistrictNumber'


dbSendQuery(conn = database,  "CREATE TABLE DistrictNumber (District 
            TEXT)")

dbSendQuery(conn = database,
            "INSERT INTO DistrictNumber
            VALUES ('FIRST')")
dbSendQuery(conn = database,
            "INSERT INTO DistrictNumber
            VALUES ('SECOND')")
dbSendQuery(conn = database,
            "INSERT INTO DistrictNumber
            VALUES ('THIRD')")
dbSendQuery(conn = database,
            "INSERT INTO DistrictNumber
            VALUES ('FOURTH')")
dbSendQuery(conn = database,
            "INSERT INTO DistrictNumber
            VALUES ('FIFTH')")
dbSendQuery(conn = database,
            "INSERT INTO DistrictNumber
            VALUES ('SIXTH')")
dbSendQuery(conn = database,
            "INSERT INTO DistrictNumber
            VALUES ('SEVENTH')")



# Create Table 'WardNumber'


dbSendQuery(conn = database,  "CREATE TABLE WardNumber (Ward 
            INT)")

dbSendQuery(conn = database,
            "INSERT INTO WardNumber
            VALUES (1)")
dbSendQuery(conn = database,
            "INSERT INTO WardNumber
            VALUES (2)")
dbSendQuery(conn = database,
            "INSERT INTO WardNumber
            VALUES (3)")
dbSendQuery(conn = database,
            "INSERT INTO WardNumber
            VALUES (4)")
dbSendQuery(conn = database,
            "INSERT INTO WardNumber
            VALUES (5)")
dbSendQuery(conn = database,
            "INSERT INTO WardNumber
            VALUES (6)")
dbSendQuery(conn = database,
            "INSERT INTO WardNumber
            VALUES (7)")
dbSendQuery(conn = database,
            "INSERT INTO WardNumber
            VALUES (8)")



# Create Table 'PSANumber'

PSA <- c(101,102,103,104,105,106,107,108,201,202,203,204,205,206,207,208,301,302,303,304,305,306,307,308,401,402,403,404,405,406,407,408,409,501,502,503,504,505,506,507,601,602,603,604,605,606,607,608,701,702,703,704,705,706,707,708)
      
d1 <- data.frame(PSA)
write.csv(d1, file = "PSANumber.csv")
dbWriteTable(conn = database, name = "PSANumber",value = "PSANumber.csv", row.names = FALSE, header = TRUE)


# Create Table 'NeighborhoodClusterNumber'

NeighborhoodCluster <- 1:39

d2 <- data.frame(NeighborhoodCluster)
write.csv(d2, file = "NeighborhoodCluster.csv")
dbWriteTable(conn = database, name = "NeighborhoodClusterNumber",value = "NeighborhoodCluster.csv", row.names = FALSE, header = TRUE)




############################## Data Analysis - Running some queries ############################## 


# Query 1: Number of incidents occured during each shift

dbGetQuery(conn = database, " SELECT Shift,COUNT(*) AS NUMBER FROM Incident GROUP BY Shift")

# Conclusion:
# Maximum incidents ocuured during evening shift
# Minimum incidents occured during midnight shift




# Query 2: Number of incidents occured for each offense

dbGetQuery(conn = database, " SELECT OFFENSE,COUNT(*) AS NUMBER FROM Incident GROUP BY OFFENSE ORDER BY NUMBER DESC")

# Conclusion:
# Highest number of incidents occured for Theft/Other offense
# Lowest number of incidents occured for Arson offense




# Query 3: Number of incidents occured by each method

dbGetQuery(conn = database, " SELECT METHOD,COUNT(*) AS NUMBER FROM Incident GROUP BY METHOD")

# Conclusion:
# There were 2161 and 1247 number of incidents where GUN and KNIFE were used for committing a 
# crime respectively 




# Query 4: Number of incidents occured in each district

dbGetQuery(conn = database, " SELECT DISTRICT,COUNT(*) AS NUMBER FROM Address GROUP BY DISTRICT ORDER BY NUMBER DESC")

# Conclusion:
# Safest district is seventh as it has the minimum number of incidents happened
# Most unsafe district is third as it has the maximum number of incidents happened




# Query 5: Number of incidents occured in each ward

dbGetQuery(conn = database, " SELECT WARD,COUNT(*) AS NUMBER FROM Address GROUP BY WARD ORDER BY NUMBER DESC")

# Conclusion:
# Safest ward is ward no.3 as it has the minimum number of incidents happened
# Most unsafe ward is ward no.2 as it has the maximum number of incidents happened




# Query 6: Number of incidents occured in each ward for 'THIRD' district

dbGetQuery(conn = database, " SELECT WARD,COUNT(*) AS NUMBER FROM Address WHERE DISTRICT = '\"THIRD\"' GROUP BY WARD")

# Conclusion:
# Safest ward is ward no.6 in third district as it has the minimum number of incidents happened
# Most unsafe ward is ward no.1 in third district as it has the maximum number of incidents happened




# Query 7: Kind of offense happened maximum during each shift 

dbGetQuery(conn = database, " SELECT OFFENSE,COUNT(*) AS NUMBER FROM Incident WHERE SHIFT = '\"DAY\"' GROUP BY OFFENSE ORDER BY NUMBER DESC")

dbGetQuery(conn = database, " SELECT OFFENSE,COUNT(*) AS NUMBER FROM Incident WHERE SHIFT = '\"EVENING\"' GROUP BY OFFENSE ORDER BY NUMBER DESC")

dbGetQuery(conn = database, " SELECT OFFENSE,COUNT(*) AS NUMBER FROM Incident WHERE SHIFT = '\"MIDNIGHT\"' GROUP BY OFFENSE ORDER BY NUMBER DESC")

# Conclusion:
# During day shift, maximum number of incidents occured for Theft F/Auto offense
# During day evening, maximum number of incidents occured for Theft/Other offense
# During day midnight, maximum number of incidents occured for Theft F/Auto offense




# Query 8: Incidents reported in each month

library(lubridate)

a <- as.Date(data$REPORTDATETIME,format="%m/%d/%y")
len <- length(data$REPORTDATETIME)
m1 <- 0;m2 <- 0;m3 <- 0;m4 <- 0;m5 <- 0;m6 <- 0;m7 <- 0;m8 <- 0;m9 <- 0;m10<- 0;m11 <-0;m12 <-0;

for (i in 1:len)
{
  if (month(a[i]) == 1)
  { m1 <- m1+1
  }
  else if (month(a[i]) == 2)
  { m2 <- m2+1
  }
  else if (month(a[i]) == 3)
  { m3 <- m3+1
  } 
  else if (month(a[i]) == 4)
  { m4 <- m4+1
  }
  else if (month(a[i]) == 5)
  { m5 <- m5+1
  }
  else if (month(a[i]) == 6)
  { m6 <- m6+1
  }
  else if (month(a[i]) == 7)
  { m7 <- m7+1
  }
  else if (month(a[i]) == 8)
  { m8 <- m8+1
  }
  else if (month(a[i]) == 9)
  { m9 <- m9+1
  }
  else if (month(a[i]) == 10)
  { m10 <- m10+1
  }
  else if (month(a[i]) == 11)
  { m11 <- m11+1
  }
  else if (month(a[i]) == 12)
  { m12 <- m12+1
  }
}

z1 <- c("January","February","March","April","May","June","July","August","September","October","November","December")
z <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)

mon <- data.frame(MONTH=z1[1:12],NUMBER=z[1:12])

x <- mon$MONTH
y <- mon$NUMBER

plot(y,type="h",col="red",lwd=10,main="Month Vs No. of Incidents ",xlab="Month",ylab="Number")

# Conclusion:
# September and February month has recorded maximum and minimum number of crime incidents 
# respectively.         
# Top three months which recorded maximum number of crime incidents are- August,September and 
# December.  




# Query 9: Shift when Knife and Gun were used maximum number of times 

dbGetQuery(conn = database, " SELECT SHIFT,COUNT(*) AS NUMBER FROM Incident WHERE METHOD = '\"GUN\"' GROUP BY SHIFT ORDER BY NUMBER DESC")

dbGetQuery(conn = database, " SELECT SHIFT,COUNT(*) AS NUMBER FROM Incident WHERE METHOD = '\"KNIFE\"' GROUP BY SHIFT ORDER BY NUMBER DESC")

# Conclusion:
# Knife was used during evening shift for maximum number of times
# Gun was used during midnight shift for maximum number of times




# Query 10: Most safe and unsafe day of the week

a <- as.Date(data$REPORTDATETIME,format="%m/%d/%y")
len <- length(data$REPORTDATETIME)
m1 <- 0;m2 <- 0;m3 <- 0;m4 <- 0;m5 <- 0;m6 <- 0;m7 <- 0;

for (i in 1:len)
{
  if (wday(a[i],label=TRUE) == 'Mon')
  { m1 <- m1+1
  }
  else if (wday(a[i],label=TRUE) == "Tues")
  { m2 <- m2+1
  }
  else if (wday(a[i],label=TRUE) == "Wed")
  { m3 <- m3+1
  } 
  else if (wday(a[i],label=TRUE) == "Thurs")
  { m4 <- m4+1
  }
  else if (wday(a[i],label=TRUE) == "Fri")
  { m5 <- m5+1
  }
  else if (wday(a[i],label=TRUE) == "Sat")
  { m6 <- m6+1
  }
  else if (wday(a[i],label=TRUE) == "Sun")
  { m7 <- m7+1
  }
}


z1 <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
z <- c(m1,m2,m3,m4,m5,m6,m7)

mon <- data.frame(DAY=z1[1:7],NUMBER=z[1:7])

x <- mon$DAY
y <- mon$NUMBER

plot(y,type="h",col="red",lwd=10,main="Day Vs No. of Incidents ",xlab="Day",ylab="Number")

# Conclusion:
# The highest number of incidents occured on Monday
# The least number of incidents occured on Wednesday



# Query 11: Most safe and unsafe Voting Precinct in terms of crime incidents

dbGetQuery(conn = database, " SELECT VOTING_PRECINCT,COUNT(*) AS NUMBER FROM Census GROUP BY VOTING_PRECINCT ORDER BY NUMBER DESC")

# Conclusion:
# Most safe voting precinct is Precinct-136
# Most unsafe voting precinct is Precinct-129




# Query 12: Number of incidents in all Business Improvement Districts 

dbGetQuery(conn = database, " SELECT BUSINESSIMPROVEMENTDISTRICT,COUNT(*) AS NUMBER FROM BUSINESS_IMPROVEMENT GROUP BY BUSINESSIMPROVEMENTDISTRICT ORDER BY NUMBER DESC")

# Conclusion:
# The highest number of incidents occured in DOWNTOWN district
# The lowest number of incidents occured in MOUNT VERNON TRIANGE CID district




# Query 13: District-wise incidents during evening shift

dbGetQuery(conn = database, " SELECT DISTRICT,COUNT(*) AS NUMBER FROM ADDRESS INNER JOIN INCIDENT USING(CCN) WHERE SHIFT= '\"EVENING\"' GROUP BY DISTRICT ORDER BY NUMBER DESC ")

# Conclusion:
# The most safe district during evening is Seventh district 
# The most unsafe district during evening is First district


