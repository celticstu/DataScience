##### TASK 1

csv_file_list <- list.files(path = "c:/NI Crime Data", pattern = "*.csv")
csv_file_list
class(csv_file_list)

#Function that reads in all csv files into one data frame and returns the result.
combine.results <- function(file_list) {
    #Initialise all_lotto_data variable
    #Note that it hasn't been assigned to a specific type yet
    all_crime_data <- NULL
    for (csv_file in file_list) {
        #Read each of the csv files in turn and skip the first line as it
        #contains a header line
        crime_data <- read.csv(header = TRUE, paste("c:/NI Crime Data/", csv_file, sep = ""), stringsAsFactors = FALSE)
        #Only select attributes we're interested in
        #We don't need the first attribute
        data_of_interest <- crime_data
        # append to the allCrimeData data frame
        all_crime_data <- rbind(all_crime_data, data_of_interest)
    }
    return(all_crime_data)
}

#Call the function and return the result to a data frame
my_crime_data <- combine.results(csv_file_list)
#show the contents of my_lotto_data
head(my_crime_data)
dim(my_crime_data)
#Save the contents of my_lotto_data to a csv file called "ld.csv"
#write.csv(my_crime_data, file = "AllNICrimeData.csv", quote = FALSE, na = "", row.names = FALSE)

##### TASK 2
# Remove CrimeID, Reported.by, Falls.within, LSOA.code, LSOA.name

head(my_crime_data)
attach(my_crime_data)
my_crime_data <- subset(my_crime_data, select = -c(Crime.ID, Reported.by, Falls.within, LSOA.code, LSOA.name))
head(my_crime_data)

##### TASK 3
unique(my_crime_data$Crime.type)

my_crime_data$Crime.type <- factor(my_crime_data$Crime.type, levels = c("Anti-social behaviour", "Bicycle theft", "Burglary", "Criminal damage and arson", "Drugs", "Other theft",
"Possession of weapons", "Public order", "Robbery", "Shoplifting", "Theft from the person", "Vehicle crime", "Violence and sexual offences", "Other crime"))

levels(my_crime_data$Crime.type)

str(my_crime_data)


##### TASK 4
library(DescTools)

sum(my_crime_data$Location %like% "On or near")

my_crime_data$Location <- gsub("On or near", "", my_crime_data$Location, ignore.case = T)

sum(my_crime_data$Location %like% "%on or near%")

##### TASK 5
CleanNIPostcodeData <- read.csv("c:/CleanNIPostcodeData.csv")
str(CleanNIPostcodeData)

str(my_crime_data)
install.packages("rdgal")
find_a_postcode <- function(my_crime_data$location)

    {
    if (as.character(CleanNIPostcodeData$Primary_Thorfare) == as.character(my_crime_data$Location))
        {
        my_crime_data("Postcode") <- CleanNIPostcodeData$Postcode
        }
    return(postcode)         
}
lapply(my_crime_data, find_a_postcode)
##### TASK 6


##### TASK 7
head(my_crime_data$Location, 50)

my_crime_data$Location <- gsub("On or near", "\\1", my_crime_data$Location)

##### TASK 8


##### TASK 9
FinalNICrimeData <- my_crime_data

##### TASK 10
strabane <- my_crime_data$Location %like% "%Strabane%"
head(strabane)