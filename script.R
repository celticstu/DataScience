##### TASK 1
#Import the path name of each csv file to csv_file_list
csv_file_list <- list.files(path = "c:/NI Crime Data", pattern = "*.csv")
csv_file_list
class(csv_file_list)

#Function that reads in all csv files into one data frame and returns the result.
combine.results <- function(file_list) {
    #Initialise all_crime_data variable to NULL
    all_crime_data <- NULL
    for (csv_file in file_list) {
        #Read each of the csv files in turn and skip the first line as it contains a header line
        crime_data <- read.csv(header = TRUE, paste("c:/NI Crime Data/", csv_file, sep = ""), stringsAsFactors = FALSE)
        #Only select attributes required
        data_of_interest <- crime_data
        # append to the all_crime_data data frame
        all_crime_data <- rbind(all_crime_data, data_of_interest)
    }
    return(all_crime_data)
}

#Call the function and return the result to a data frame
AllNICrimeData <- combine.results(csv_file_list)
write.csv(AllNICrimeData, "AllNICrimeData.csv")
#show the contents of AllNICrimeData
head(AllNICrimeData)
#show structure of AllNICrimeData
str(AllNICrimeData)
#show the number of columns and rows
dim(AllNICrimeData)

##### TASK 2
# Remove CrimeID, Reported.by, Falls.within, LSOA.code, LSOA.name

head(AllNICrimeData)
attach(AllNICrimeData)
# Using the sybset function, overwrite the AllNICrimeData with it's existing data
# but by using select = -c, the selected variable names (columns) will be excluded from the new data frame
AllNICrimeData <- subset(AllNICrimeData, select = -c(Crime.ID, Reported.by, Falls.within, LSOA.code, LSOA.name))
head(AllNICrimeData)
#show the structure of AllNICrimeData
str(AllNICrimeData)

##### TASK 3
#show the different types of crimes without duplicates
unique(AllNICrimeData$Crime.type)

#using the factor function, set the levels of Crime.type
AllNICrimeData$Crime.type <- factor(AllNICrimeData$Crime.type, levels = c("Anti-social behaviour", "Bicycle theft", "Burglary", "Criminal damage and arson", "Drugs", "Other theft",
"Possession of weapons", "Public order", "Robbery", "Shoplifting", "Theft from the person", "Vehicle crime", "Violence and sexual offences", "Other crime"))

# Check that the correct level had been set
levels(AllNICrimeData$Crime.type)
head(AllNICrimeData)
# View structure of AllNICrimeData 
str(AllNICrimeData)


##### TASK 4
library(DescTools) # loaded to used the gsub function

str(AllNICrimeData) # View structure of the Location column

# Using the gsub function, search for all occurances of the term "On or near" in the Location column whilst ignoring the case sensitivity
AllNICrimeData$Location <- gsub("On or near", "", AllNICrimeData$Location, ignore.case = T)

head(AllNICrimeData)
str(AllNICrimeData) # view structure after change



##### TASK 5
##### NOTE:
##### I WAS UNABLE TO GET THIS TASK WOKRING PROPERLY 

# import the CleanNIPostcode dataset
CleanNIPostcodeData <- read.csv("c:/CleanNIPostcodeData.csv")
# Create a function called find_a_postcode that takes in the variables Location and Primary_Thorefare
find_a_postcode <- function(Location, Primary_Thorfare) {
    # Initialise ptcodes variable
    ptcodes <- c()
    # using the identical function, check to see if Loaction and Primary_Thorefare are the same, with both variable changes to upper case
    if (mapply(identical, toupper(Location), toupper(Primary_Thorfare))) {
        # create a data frame called ptcodes that contain the relevant postcode along with it's street name
        ptcodes <- c(CleanNIPostcodeData$Postcode, Location)
    }
    # using the which.max function, find the most common postcode
    postcode <- names(which.max(table(ptcodes)))
    return(postcode)
}
#apply the find_a_postcode function to AllNICrimeData$Location, CleanNIPostcodeData$Postcode
mapply(find_a_postcode, AllNICrimeData$Location, CleanNIPostcodeData$Postcode)

#str(AllNICrimeData)
#install.packages("proj4")
#library(proj4) # Used to convert x and y coordinates to latitude and longitude
#proj4string <- "+proj=utm +zone=19 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
# Source data
#xy <- data.frame(x = CleanNIPostcodeData$x.coordinates, y = CleanNIPostcodeData$y.coordinates)

# Transformed data
#pj <- project(xy, proj4string, inverse = TRUE)
#latlon <- data.frame(lat = pj$y, lon = pj$x)
#head(latlon)
#CleanNIPostcodeData$x.coordinates <- latlon$lat
#CleanNIPostcodeData$y.coordinates <- latlon$lon
#head(CleanNIPostcodeData)
#head(AllNICrimeData)
#find_a_postcode <- function(location)
 #   {
#  if (AllNICrimeData$Latitude == CleanNIPostcodeData$x.coordinates) {
#     if (AllNICrimeData$Longitude == CleanNIPostcodeData$y.coordinates) {
    #        NI_postcode <- CleanNIPostcodeData$Postcode
     #   }
    #}
    #return(postcode)         
#}
#find_a_postcode("")



##### TASK 6

##### TASK 7

##### TASK 8

##### TASK 9
#Create a new data frmae called FinalNICrimeData 
FinalNICrimeData <- AllNICrimeData
write.csv(AllNICrimeData, file = "FinalNICrimeData.csv")

##### TASK 10
# Create a new data frame called strabane from a subset of FinalNICrimeData that only consists 
# of the variable Strabane under Town and BT82 under Postcode 
strabane <- subset(FinalNICrimeData, Town == "Strabane" & Postcode == "BT82")
# Using the head function, view the first 10 rows of FinalNICrimeData 
head(strabane, 10)