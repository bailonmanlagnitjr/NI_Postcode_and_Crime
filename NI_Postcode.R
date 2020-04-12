# load NI post code dataset - set header = FALSE since there is none
ni_post_data <- read.csv("NIPostcodes.csv", header = FALSE)

# count the number of rows
nrow(ni_post_data)

# structure of the data
str(ni_post_data)

# show first 10 of all data
head(ni_post_data, 10)

# add column names
col_names = c("Organisation_Name", "Sub_building_Name", "Building_Name", "Number", "Primary_Thorfare",
  "Alt_Thorfare", "Secondary_Thorfare", "Locality", "Townland", "Town", "County", "Postcode",
  "X_coordinates", "Y_coordinates", "Primary_Key")
colnames(ni_post_data) <- col_names
head(ni_post_data)

# replace missing values with with NA
ni_post_data$Organisation_Name[ni_post_data$Organisation_Name == ""] <- NA
ni_post_data$Sub_building_Name[ni_post_data$Sub_building_Name == ""] <- NA
ni_post_data$Building_Name[ni_post_data$Building_Name == ""] <- NA
ni_post_data$Number[ni_post_data$Number == ""] <- NA
ni_post_data$Primary_Thorfare[ni_post_data$Primary_Thorfare == ""] <- NA
ni_post_data$Alt_Thorfare[ni_post_data$Alt_Thorfare == ""] <- NA
ni_post_data$Secondary_Thorfare[ni_post_data$Secondary_Thorfare == ""] <- NA
ni_post_data$Locality[ni_post_data$Locality == ""] <- NA
ni_post_data$Townland[ni_post_data$Townland == ""] <- NA
ni_post_data$Town[ni_post_data$Town == ""] <- NA
ni_post_data$County[ni_post_data$County == ""] <- NA
ni_post_data$Postcode[ni_post_data$Postcode == ""] <- NA
ni_post_data$X_coordinates[ni_post_data$X_coordinates == ""] <- NA
ni_post_data$Y_coordinates[ni_post_data$Y_coordinates == ""] <- NA
head(ni_post_data)

# count the total missing values in column Postcode
sum(is.na(ni_post_data$Postcode))

# remove the rows with missing Postcode
clean_ni_post_data <- ni_post_data[!is.na(ni_post_data$Postcode),]

# count again after removing rows with missing Postcode
sum(is.na(clean_ni_post_data$Postcode))

# count the total missing values in each column
sum(is.na(clean_ni_post_data$Organisation_Name))
sum(is.na(clean_ni_post_data$Sub_building_Name))
sum(is.na(clean_ni_post_data$Building_Name))
sum(is.na(clean_ni_post_data$Number))
sum(is.na(clean_ni_post_data$Primary_Thorfare))
sum(is.na(clean_ni_post_data$Alt_Thorfare))
sum(is.na(clean_ni_post_data$Secondary_Thorfare))
sum(is.na(clean_ni_post_data$Locality))
sum(is.na(clean_ni_post_data$Townland))
sum(is.na(clean_ni_post_data$Town))
sum(is.na(clean_ni_post_data$County))
sum(is.na(clean_ni_post_data$Postcode))
sum(is.na(clean_ni_post_data$X_coordinates))
sum(is.na(clean_ni_post_data$Y_coordinates))

# display some rows to show the column
head(clean_ni_post_data)

# move primary key column at the start of the dataset
clean_ni_post_data <- clean_ni_post_data[,c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]

# display again after moving the Primary key column at the start
head(clean_ni_post_data)

# install.packages("stringr")
library("stringr")
Limavady <- str_view(ni_post_data$Town, "LIMAVADY")
Limavady


ni_post_data$Organisation_Name <- as.character(Organisation_Name)
str(ni_post_data)

# search for LIMAVADY in Locality, Townland, Town 
Limavady_data <- clean_ni_post_data[grepl("LIMAVADY", ni_post_data$Locality) | 
                             grepl("LIMAVADY", ni_post_data$Townland) |
                              grepl("LIMAVADY", ni_post_data$Town), ]
# count the rows with Limavady
nrow(Limavady_data)

# diplay some Limavady data
head(Limavady_data)

# save the search into LIMAVADY.CSV 
write.csv(Limavady_data,'Limavady.csv')


# save the modified NIPostcode dataset into CleanNIPostcodeData
write.csv(clean_ni_post_data,'CleanNIPostcodeData.csv')
