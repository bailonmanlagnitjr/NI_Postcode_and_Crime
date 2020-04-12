
# set working directory every after a restart of computer
# setwd("C:/Users/bailo/OneDrive/Documents/NI_Postcode_and_Crime")

# A. ) Amalagamate all the crime files into AllNICRimeData
# read all files in a loop and concatenate using rbind
files <- list.files(pattern = "\\-northern-ireland-street.csv")

AllNICrimeData <-  read.csv(files[1])

#reading each file within the range and append them to create one file
for (f in files[-1]){
  temp_df <- read.csv(f)                              # read the file
  AllNICrimeData <- rbind(AllNICrimeData, temp_df)    # append the current file
}

# writing the appended file  
write.csv(AllNICrimeData,"AllNICRimeData.csv",row.names = F,quote = F)

# get the structure
str(AllNICrimeData)

# count the number of rows
nrow(AllNICrimeData)

# B.) Remove the collowing columns: CrimeID, Reported by, Falls within, LSOA code, LSOA name, 
#     last outcome and context
# show structure before 
str(AllNICrimeData)

# remove columns using subset command
mod_AllNICrimeData <- subset(AllNICrimeData, select=-c(Crime.ID, Reported.by, Falls.within, LSOA.code,
      LSOA.name, Last.outcome.category, Context))

# check structure after removing some columns
str(mod_AllNICrimeData)

# C.) Shorten Crime.type description in AllNICRimeData
# display few rows of the AllNICrimeData
head(AllNICrimeData)

# get structure before changing the type
str(AllNICrimeData)

# change the type of Ccolumn Crime.type to Character
AllNICrimeData$Crime.type <- as.character(AllNICrimeData$Crime.type)

# get structure after changing the type
str(AllNICrimeData)

# update the crime type
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Anti-social behaviour"] <- "ASBO"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Bicycle theft"] <- "BITH"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Burglary"] <- "BURG"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Criminal damage and arson"] <- "CDAR"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "DRUGS"] <- "DRUG"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Other theft"] <- "OTTH"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Public order"] <- "PUBO"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Robbery"] <- "ROBY"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Shoplifting"] <- "SHOP"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Theft from the person"] <- "THPR"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Vehicle crime"] <- "VECR"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Violence and sexual offences"] <- "VISO"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Other crime"] <- "OTCR"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Possession of weapons"] <- "POSW"

# display few  rows
head(AllNICrimeData)

# D.) create CrimeFreq table
# create simple frequency table
CrimeFreq <- with(AllNICrimeData, table(Crime.type))
CrimeFreq

# by proportions
prop.table(CrimeFreq) # proportions

# by percentages
prop.table(CrimeFreq)*100 # percentages

# Plot
plot(CrimeFreq,main="Crime Frequency", ylab = "Frequency", xlab = "Crime type", col = "blue",
     las=3,cex.axis=0.7,ylim=c(1,200000))
#plot(prop.table(CrimeFreq),main="Crime Frequency", ylab = "Frequency", xlab = "Region", col = "blue")
#plot(prop.table(CrimeFreq)*100,main="NI Crime Frequency", ylab = "% in Overall Crime", xlab = "Crime type", col = "green",
#     las=3,cex.axis=0.7,ylim=c(1,40))

# E.) MOdify Location attribute
# use gsub to replace phrase
AllNICrimeData$Location <- gsub("On or near ","", AllNICrimeData$Location)

# replace spaces with NA
AllNICrimeData$Location[AllNICrimeData$Location == " " | AllNICrimeData$Location == ""] <- NA

# display few lines after replace
head(AllNICrimeData)

# F.) Choose 5000 random sample rows from AllNICrimedata then create find_a_town function to match the Location on the crime data and 
# primary thorfare in CleanNIPostcodeData to retireve the Town information. 
# set seed to 100
set.seed(100)

# get random 5000 rows with valid Location data
random_crime_sample <- AllNICrimeData[!is.na(AllNICrimeData$Location),][sample(nrow(AllNICrimeData[!is.na(AllNICrimeData$Location),]), 5000), ]

# count the rows 
nrow(random_crime_sample)

# display few row
head(random_crime_sample)

# display strcuture
str(random_crime_sample)

# load CleanNIPostcode
NIPostcode_data <- read.csv("CleanNIPostcodeData.csv")

# using subset() get the relevant columns only
loc_NIPostcodeData <- subset(NIPostcode_data, select=c(Primary_Thorfare, Town))

# display few rows
head(loc_NIPostcodeData)

# sort the loc_NIPostcodeData by primary thorfare colomn
Order_NIPostcode_Data <- loc_NIPostcodeData[order(loc_NIPostcodeData$Primary_Thorfare, decreasing=FALSE),]

# remove duplicates
dedup_NIPostcode_Data <- Order_NIPostcode_Data[!duplicated(Order_NIPostcode_Data$Primary_Thorfare),]

# display few rows
head(dedup_NIPostcode_Data)

# create the find_a_town() function
find_a_town <- function(NIPostData, Location )
{
  Town <- with(NIPostData, Town[match(tolower(Location), tolower(NIPostData$Primary_Thorfare))])
  return(Town)
}

# add the Town column in the random_crime_sample
random_crime_sample$Town <- find_a_town(dedup_NIPostcode_Data, random_crime_sample$Location)

# display few rows
head(random_crime_sample)

# G. Add_town_data function
# load the VillageList.csv
VillageList_data <- read.csv("VillageList.csv")

# display few rows
head(VillageList_data)

# display the structure
str(VillageList_data)

# sort the data by the city/town/village
Ord_VillageList_data <- VillageList_data[order(VillageList_data$ï..CITY.TOWN.VILLAGE, decreasing=FALSE),]

# change to character
Ord_VillageList_data$ï..CITY.TOWN.VILLAGE <- as.character(Ord_VillageList_data$ï..CITY.TOWN.VILLAGE)

# step to synchronize the name of Derry to Londonderry - so that the matching later will be correct
Ord_VillageList_data$ï..CITY.TOWN.VILLAGE[Ord_VillageList_data$ï..CITY.TOWN.VILLAGE == "Derry"]  <- "Londonderry"

# change to factor
Ord_VillageList_data$ï..CITY.TOWN.VILLAGE <- as.factor(Ord_VillageList_data$ï..CITY.TOWN.VILLAGE)

# get the count of rows
nrow(Ord_VillageList_data)

# remove duplicates by city/town/village - as we want to get only the unique rows
dedup_VillageList_data <- Ord_VillageList_data[!duplicated(Ord_VillageList_data$ï..CITY.TOWN.VILLAGE),]
nrow(dedup_VillageList_data)

# make the data in city/town/village as upper case in order to match property
dedup_VillageList_data$ï..CITY.TOWN.VILLAGE <- toupper(dedup_VillageList_data$ï..CITY.TOWN.VILLAGE) 

# display few rows to check
head(dedup_VillageList_data)

# sort the random crime dataset by Town
Ord_random_crime_sample <- random_crime_sample[order(random_crime_sample$Town, decreasing=FALSE),]

# display a few rows
head(Ord_random_crime_sample)

# check the number of rows just to make sure
nrow(Ord_random_crime_sample)

# create the add_town_function to merge the random crime data and the village list.
# this is a left outer join - if it does not find a match in the village list, still it will be written in the output.
add_town_data <- function(RandomCrimeData, VillageList )
{
  Merged_data <- merge(x = RandomCrimeData, y = VillageList, by.x="Town", by.y="ï..CITY.TOWN.VILLAGE", all.x = TRUE)
  return(Merged_data)
}

# call the add_town_data function 
New_random_crime_sample <- add_town_data(Ord_random_crime_sample, dedup_VillageList_data)

# display a few rows
head(New_random_crime_sample)

# check the number of rows just to make sure
nrow(New_random_crime_sample)


# H.) Update random crime sample dataset to select only few column
# get the structure
str(New_random_crime_sample)

# select only the specific column
Rev_random_crime_sample <- subset(New_random_crime_sample, 
    select=c(Month, Longitude, Latitude, Location, Crime.type, Town, POPULATION))

# get the structure of the new dataset
str(Rev_random_crime_sample)

# display some rows
head(Rev_random_crime_sample)

# writing the appended file  
write.csv(Rev_random_crime_sample,"random_crime_sample.csv",row.names = F,quote = T)

# I. ) Display side by side using par()

# extract rows for Belfast
Belfast_Crime_Data <- subset(Rev_random_crime_sample, Town == "BELFAST" )
head(Belfast_Crime_Data)
nrow(Belfast_Crime_Data)

Londonderry_Crime_Data <- subset(Rev_random_crime_sample, Town == "LONDONDERRY" )
head(Londonderry_Crime_Data)
nrow(Londonderry_Crime_Data)

# sort the random crime dataset by Town
#Belfast_Derry_Crime_Data <- Belfast_Derry_Crime_Data[order(Belfast_Derry_Crime_Data$Town,Belfast_Derry_Crime_Data$Crime.type),]
#head(Belfast_Derry_Crime_Data)

CrimeFreq_Belfast <- with(Belfast_Crime_Data, table(Crime.type))
CrimeFreq_Belfast

#plot(CrimeFreq_Belfast,main="Belfast Crime Frequency", ylab = "Frequency", xlab = "Crime type", col = "blue")

CrimeFreq_Londonderry <- with(Londonderry_Crime_Data, table(Crime.type))
CrimeFreq_Londonderry

par(mfrow=c(1,2))
plot(CrimeFreq_Belfast, main="Crime Frequency - Belfast", ylab = "Frequency", xlab = "Crime type",
    las=3,cex.axis=0.7,col = "blue", ylim=c(1,500))
plot(CrimeFreq_Londonderry, main="Crime Frequency - Derry",ylab = "Frequency", xlab = "Crime type",
    las=3,cex.axis=0.7,col = "red", ylim=c(1,500))

# by proportions
prop.table(CrimeFreq_Belfast) # proportions

# by percentages
prop.table(CrimeFreq_Belfast)*100 # percentages

# by proportions
prop.table(CrimeFreq_Londonderry) # proportions

# by percentages
prop.table(CrimeFreq_Londonderry)*100 # percentages

