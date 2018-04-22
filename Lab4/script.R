library(rvest)

# Specifying url of the Guardian newspaper website to be scrapped
url <- 'https://www.theguardian.com/politics/ng-interactive/2018/jan/26/guardian-icm-brexit-poll-full-results'
web_page <- read_html(url)

# View url
head(web_page)

str(web_page)

#NOTE: The code commentd out below was an attempt to scrape the data relating to the question
# What impact do you think Brexit will have on the British economy? 
#from the Guardian website, but I wasn't able to seperate the values out correctly

#summary_html <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(n)')
#head(summary_html)
#length(summary_html)
#summary_data <- html_text(summary_html)
#summary_data

#The code below, scrapes each part of the question section by section

#1 Summary
# Target the Summary graph labels section of the graph which will scrape the word Total from the website
summary_html <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(4)')
# View summary_html
summary_html
# Check the length of summary_html
length(summary_html)
# Convert from html to text
summary_data <- html_text(summary_html)
# Check summary_data
summary_data
# Remove leading and trailing \n
summary_data <- gsub("\n", "", summary_data)

#2 Target the Summary positive figures section of the graph
summary_positive <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(4) > div:nth-child(n) > div > div.bar__inner.positive')
# View summary_positive 
summary_positive
# Check the length of summary_positive
length(summary_positive)
# Convert from html to text 
summary_positive_data <- html_text(summary_positive)
# View summary_positive 
summary_positive_data
# Since the data being scraped from the site doesn't technically exist, the name of the labels are what will be used
# as a guidance for manually recoding the values, as seen below
summary_positive_data <- as.numeric(c(36), summary_positive_data)
# View summary_positive 
summary_positive_data

#3 Target the Summary netural figures section of the graph
summary_neutral <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(4) > div:nth-child(n) > div > div.bar__inner.neutral')
#View summary_netural 
head(summary_neutral)
#Check length of summary_netural
length(summary_neutral)
# Convert html to text
summary_neutral_data <- html_text(summary_neutral)
#View summary_netural_data
summary_neutral_data
#Recode the values of summary_neutral_data with the hel of the labels as guidance
summary_neutral_data <- as.numeric(c(15), summary_neutral_data)
#View summary_netural_data
summary_neutral_data

#4 Target the Summary negative figures section of the graph
summary_negative <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(4) > div:nth-child(n) > div > div.bar__inner.negative')
#View summary_negative 
head(summary_negative)
#Check length of summary_negative
length(summary_negative)
#Convert html to text
summary_negative_data <- html_text(summary_negative)
#View summary_negative_data
head(summary_negative_data)
#Recode summary_negative_data to that of the labels
summary_negative_data <- as.numeric(c(49), summary_negative_data)
#View summary_negative_data
summary_negative_data

#5 Combine summary_data, summary_positive_data, summary_neutral_data, summary_negative_data into seperate columns
# which create a matrix called summary
summary <- cbind(summary_data, summary_positive_data, summary_neutral_data, summary_negative_data)
# View summary
summary
#Check class of summary
class(summary)
#Convert summary into a data frame
summary <- as.data.frame(summary)
#Check class again
class(summary)
#View summary
summary
library(data.table)
#Using the setnames function, change columns names to more appropriate names in the summary data frame
setnames(summary, old = c("summary_data", "summary_positive_data", "summary_neutral_data", "summary_negative_data"), new = c("Summary", "Positive", "Netural", "Negative"))
#Check structure of summary data frame
str(summary)
#View the final summary data frame
summary

#6 Gender
# Target the Gender labels section of the graph
gender_html <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(6) > div:nth-child(n) > h3')
#View gender_html
gender_html
# Convert html to text
gender_data <- html_text(gender_html)
gender_data

#7 Target the Gender positive labels section of the graph
gender_positive <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) >
     div:nth-child(6) > div:nth-child(n) > div > div.bar__inner.positive')
#View gender_positive
gender_positive
# Convert html to text
gender_positive_data <- html_text(gender_positive)
#View gender_positive_data
gender_positive_data
#Recode missing values with that matching the labels
gender_positive_data <- as.numeric(c(39, 33), gender_positive_data)
gender_positive_data

#8 Target the Gender netural labels section of the graph
gender_neutral <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(6) > div:nth-child(n) > div > div.bar__inner.neutral')
head(gender_neutral)
# Convert html to text
gender_neutral_data <- html_text(gender_neutral)
gender_neutral_data
#Recode missing values with that matching the labels
gender_neutral_data <- as.numeric(c(14, 15), gender_neutral_data)
gender_neutral_data

#9 Target the Gender negative labels section of the graph
gender_negative <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(6) > div:nth-child(n) > div > div.bar__inner.negative')
gender_negative
# Convert html to text
gender_negative_data <- html_text(gender_negative)
gender_negative_data
#Recode missing values with that matching the labels
gender_negative_data <- as.numeric(c(47, 51), gender_negative_data)
gender_negative_data

#10
#Create a matrix called gender consisting of gender_data, gender_positive_data, gender_neutral_data, gender_negative_data
gender <- cbind(gender_data, gender_positive_data, gender_neutral_data, gender_negative_data)
gender
class(gender)
#Convert to data frame
gender <- as.data.frame(gender)
class(gender)
gender
# Change column names from gender_data, gender_positive_data, gender_neutral_data and gender_negative_data
# to Gender, Positive, Netural and Negative
setnames(gender, old = c("gender_data", "gender_positive_data", "gender_neutral_data", "gender_negative_data"), new = c("Gender", "Positive", "Netural", "Negative"))
str(gender)

# Age
#11 Target the Age labels section of the graph
age_html <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(8) > div:nth-child(n) > h3')
age_html
length(age_html)
# Convert html to text
age_data <- html_text(age_html)
age_data

#12 Target the Age positive labels section of the graph
age_positive <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > 
    div:nth-child(8) > div:nth-child(n) > div > div.bar__inner.positive')
age_positive
# Convert html to text
age_positive_data <- html_text(age_positive)
age_positive_data
#Recode missing values with that matching the labels
age_positive_data <- as.numeric(c(24, 28, 34, 51, 55), age_positive_data)
age_positive_data

#13 Target the Age netural labels section of the graph
age_neutral <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(8) > div:nth-child(n) > div > div.bar__inner.neutral')
age_neutral
# Convert html to text
age_neutral_data <- html_text(age_neutral)
age_neutral_data
#Recode missing values with that matching the labels
age_neutral_data <- as.numeric(c(13, 16, 16, 12, 15), age_neutral_data)
age_neutral_data

#14 Target the Age negative labels section of the graph
age_negative <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(8) > div:nth-child(n) > div > div.bar__inner.negative')
age_negative
# Convert html to text
age_negative_data <- html_text(age_negative)
age_negative_data
#Recode missing values with that matching the labels
age_negative_data <- as.numeric(c(63, 55, 50, 38, 30), age_negative_data)
age_negative_data

#15
#Create a matrix called age consisting of age_data, age_positive_data, age_neutral_data, age_negative_data
age <- cbind(age_data, age_positive_data, age_neutral_data, age_negative_data)
age
class(age)
#Convert to data frame
age <- as.data.frame(age)
class(age)
age
setnames(age, old = c("age_data", "age_positive_data", "age_neutral_data", "age_negative_data"), new = c("Age", "Positive", "Netural", "Negative"))
str(age)

# Ethnicity
#16 Target the Ethnicity labels section of the graph
ethnicity_html <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > 
    div:nth-child(10) > div:nth-child(n) > h3')
head(ethnicity_html)
length(ethnicity_html)
# Convert html to text
ethnicity_data <- html_text(ethnicity_html)
ethnicity_data

#17 Target the Ethnicity positive labels section of the graph
ethnicity_positive <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) >
     div:nth-child(10) > div:nth-child(n) > div > div.bar__inner.positive')
ethnicity_positive
length(ethnicity_positive)
# Convert html to text
ethnicity_positive_data <- html_text(ethnicity_positive)
ethnicity_positive_data
head(ethnicity_positive)
#Recode missing values with that matching the labels
ethnicity_positive_data <- as.numeric(c(37, 28), ethnicity_positive_data)
ethnicity_positive_data

#18 Target the Ethnicity netural labels section of the graph
ethnicity_neutral <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(10) > div:nth-child(n) > div > div.bar__inner.neutral')
head(ethnicity_neutral)
length(ethnicity_neutral)
# Convert html to text
ethnicity_neutral_data <- html_text(ethnicity_neutral)
ethnicity_neutral_data
#Recode missing values with that matching the labels
ethnicity_neutral_data <- as.numeric(c(15, 12), ethnicity_neutral_data)
class(ethnicity_neutral_data)
ethnicity_neutral_data

#19 Target the Ethnicity negative labels section of the graph
ethnicity_negative <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(10) > div:nth-child(n) > div > div.bar__inner.negative')
head(ethnicity_negative)
length(ethnicity_negative)
# Convert html to text
ethnicity_negative_data <- html_text(ethnicity_negative)
ethnicity_negative_data
#Recode missing values with that matching the labels
ethnicity_negative_data <- as.numeric(c(48, 60), ethnicity_negative_data)
class(ethnicity_negative_data)
ethnicity_negative_data

#20 Create a matrix called ethnicity consisting of ethnicity_data, ethnicity_positive_data, ethnicity_neutral_data, ethnicity_negative_data
ethnicity <- cbind(ethnicity_data, ethnicity_positive_data, ethnicity_neutral_data, ethnicity_negative_data)
ethnicity
class(ethnicity)
#Convert to data frame
ethnicity <- as.data.frame(ethnicity)
class(ethnicity)
ethnicity
# Change column names from ethnicity_data, ethnicity_positive_data, ethnicity_neutral_data and ethnicity_negative_data
# to Ethnicity, Positive, Netural and Negative
setnames(ethnicity, old = c("ethnicity_data", "ethnicity_positive_data", "ethnicity_neutral_data", "ethnicity_negative_data"), new = c("Ethnicity", "Positive", "Netural", "Negative"))
str(ethnicity)

# Working Status
#21 Target the Working Class labels section of the graph
working_status_html <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(12) > div:nth-child(n) > h3')
head(working_status_html)
length(working_status_html)
# Convert html to text
working_status_data <- html_text(working_status_html)
working_status_data

#22 Target the Working class positive labels section of the graph
working_status_positive <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > 
    figure > div:nth-child(5) > div:nth-child(12) > div:nth-child(n) > div > div.bar__inner.positive')
working_status_positive
length(working_status_positive)
# Convert html to text
working_status_positive_data <- html_text(working_status_positive)
working_status_positive_data
head(working_status_positive)
#Recode missing values with that matching the labels
working_status_positive_data <- as.numeric(c(33, 29, 34, 51, 23), working_status_positive_data)
working_status_positive_data

#23 Target the Working class netural labels section of the graph
working_status_neutral <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(12) > div:nth-child(n) > div > div.bar__inner.neutral')
head(working_status_neutral)
length(working_status_neutral)
# Convert html to text
working_status_neutral_data <- html_text(working_status_neutral)
working_status_neutral_data
#Recode missing values with that matching the labels
working_status_neutral_data <- as.numeric(c(16, 14, 21, 12, 6), working_status_neutral_data)
class(working_status_neutral_data)
working_status_neutral_data

#24 Target the Working class negative labels section of the graph
working_status_negative <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(12) > div:nth-child(n) > div > div.bar__inner.negative')
head(working_status_negative)
length(working_status_negative)
# Convert html to text
working_status_negative_data <- html_text(working_status_negative)
working_status_negative_data
#Recode missing values with that matching the labels
working_status_negative_data <- as.numeric(c(52, 57, 45, 37, 71), working_status_negative_data)
class(working_status_negative_data)
working_status_negative_data

#25
#Create a matrix called working_class consisting of working_status_data, working_status_positive_data, 
# working_status_neutral_data and working_status_negative_data by using the cbind function - creating columns
working_class <- cbind(working_status_data, working_status_positive_data, working_status_neutral_data, working_status_negative_data)
working_class
class(working_class)
#Convert to data frame
working_class <- as.data.frame(working_class)
class(working_class)
working_class
# Change column names from working_status_data, working_status_positive_data, working_status_neutral_data and working_status_negative_data
# to Working_Class, Positive, Netural and Negative
setnames(working_class, old = c("working_status_data", "working_status_positive_data", "working_status_neutral_data", "working_status_negative_data"),
    new = c("Working_Class", "Positive", "Netural", "Negative"))
str(working_class)

# Social
#26 Target the Social class labels section of the graph
social_class_html <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(14) > div:nth-child(n) > h3')
head(social_class_html)
length(social_class_html)
# Convert html to text
social_class_data <- html_text(social_class_html)
social_class_data

#27 Target the Social class positive labels section of the graph
social_class_positive <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure >
     div:nth-child(5) > div:nth-child(14) > div:nth-child(n) > div > div.bar__inner.positive')
social_class_positive
length(social_class_positive)
# Convert html to text
social_class_positive_data <- html_text(social_class_positive)
social_class_positive_data
head(social_class_positive)
#Recode missing values with that matching the labels
social_class_positive_data <- as.numeric(c(33, 33, 38, 42), social_class_positive_data)
social_class_positive_data

#28 Target the Social class netural labels section of the graph
social_class_neutral <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(14) > div:nth-child(n) > div > div.bar__inner.neutral')
head(social_class_neutral)
length(social_class_neutral)
# Convert html to text
social_class_neutral_data <- html_text(social_class_neutral)
social_class_neutral_data
#Recode missing values with that matching the labels
social_class_neutral_data <- as.numeric(c(10, 15, 19, 18), social_class_neutral_data)
class(social_class_neutral_data)
social_class_neutral_data

#29 Target the Social class negative labels section of the graph
social_class_negative <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(14) > div:nth-child(n) > div > div.bar__inner.negative')
head(social_class_negative)
length(social_class_negative)
# Convert html to text
social_class_negative_data <- html_text(social_class_negative)
social_class_negative_data
#Recode missing values with that matching the labels
social_class_negative_data <- as.numeric(c(57, 52, 43, 40), social_class_negative_data)
class(social_class_negative_data)
social_class_negative_data

#30
#Create a matrix called s_class consisting of social_class_data, social_class_positive_data, social_class_neutral_data and social_class_negative_data
# by using the cbind function - creating columns
s_class <- cbind(social_class_data, social_class_positive_data, social_class_neutral_data, social_class_negative_data)
s_class
class(s_class)
#Convert to data frame
s_class <- as.data.frame(s_class)
class(s_class)
s_class
setnames(s_class, old = c("social_class_data", "social_class_positive_data", "social_class_neutral_data", "social_class_negative_data"),
    new = c("Social_Class", "Positive", "Netural", "Negative"))
str(s_class)


# Region
#31 Target the Region labels section of the graph
region <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(16) > div:nth-child(n) > h3')
head(region)
length(region)
# Convert html to text
region_data <- html_text(region)
region_data

#32 Target the Region positive labels section of the graph
region_positive <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > 
    div:nth-child(16) > div:nth-child(n) > div > div.bar__inner.positive')
region_positive
length(region_positive)
# Convert html to text
region_positive_data <- html_text(region_positive)
region_positive_data
#Recode missing values with that matching the labels
region_positive_data <- as.numeric(c(25, 34, 33, 40, 36), region_positive_data)
region_positive_data

#33 Target the Region netural labels section of the graph
region_neutral <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(16) > div:nth-child(n) > div > div.bar__inner.neutral')
head(region_neutral)
length(region_neutral)
# Convert html to text
region_neutral_data <- html_text(region_neutral)
region_neutral_data
#Recode missing values with that matching the labels
region_neutral_data <- as.numeric(c(13, 18, 15, 15, 15), region_neutral_data)
class(region_neutral_data)
region_neutral_data

#34 Target the Region negative labels section of the graph
region_negative <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(16) > div:nth-child(n) > div > div.bar__inner.negative')
head(region_negative)
length(region_negative)
# Convert html to text
region_negative_data <- html_text(region_negative)
region_negative_data
#Recode missing values with that matching the labels
region_negative_data <- as.numeric(c(62, 48, 52, 45, 49), region_negative_data)
class(region_negative_data)
region_negative_data

#35
#Create a matrix called regions consisting of region_data, region_positive_data, region_neutral_data, region_negative_data
# by using the cbind function - creating columns
regions <- cbind(region_data, region_positive_data, region_neutral_data, region_negative_data)
regions
class(regions)
#Convert to data frame
regions <- as.data.frame(regions)
class(regions)
regions
setnames(regions, old = c("region_data", "region_positive_data", "region_neutral_data", "region_negative_data"), new = c("Regions", "Positive", "Netural", "Negative"))
str(regions)

#Voting Intentions
#36 Target the Voting Intentions labels section of the graph
vote_int_html <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(7) > div:nth-child(18) > div:nth-child(n) > h3')
vote_int_html
tail(vote_int_html)
length(vote_int_html)
# Convert html to text
vote_int_data <- html_text(vote_int_html)
vote_int_data <- as.character(vote_int_data)
class(vote_int_data)
vote_int_data

#37 Target the Voting intentions positive labels section of the graph
vote_int_positive <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > 
    div:nth-child(5) > div:nth-child(18) > div:nth-child(n) > div > div.bar__inner.positive')
vote_int_positive
length(vote_int_positive)
# Convert html to text
vote_int_positive_data <- html_name(vote_int_positive)
vote_int_positive_data
#Recode missing values with that matching the labels
vote_int_positive_data <- as.numeric(c(28, 40, 34, 41, 38), vote_int_positive_data)
class(vote_int_positive_data)

#38 Target the Voting intentions netural labels section of the graph
vote_int_neutral <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(18) > div:nth-child(n) > div > div.bar__inner.neutral')
head(vote_int_neutral)
length(vote_int_neutral)
# Convert html to text
vote_int_neutral_data <- html_text(vote_int_neutral)
vote_int_neutral_data
#Recode missing values with that matching the labels
vote_int_neutral_data <- as.numeric(c(16, 12, 9, 15, 11), vote_int_neutral_data)
class(vote_int_neutral_data)
vote_int_neutral_data

#39 Target the Voting Intentions negative labels section of the graph
vote_int_negative <- html_nodes(web_page, '#article > div > div:nth-child(2) > div > figure:nth-child(8) > figure > div:nth-child(5) > div:nth-child(18) > div:nth-child(n) > div > div.bar__inner.negative')
head(vote_int_negative)
length(vote_int_negative)
# Convert html to text
vote_int_negative_data <- html_text(vote_int_negative)
vote_int_negative_data
#Recode missing values with that matching the labels
vote_int_negative_data <- as.numeric(c(27, 66, 77, 10, 71), vote_int_negative_data)
class(vote_int_negative_data)
vote_int_negative_data

#40 
#Create a matrix called voting_intention consisting of vote_int_data, vote_int_positive_data, vote_int_neutral_data and vote_int_negative_data
# by using the cbind function - creating columns
voting_intention <- cbind(vote_int_data, vote_int_positive_data, vote_int_neutral_data, vote_int_negative_data)
voting_intention
class(voting_intention)
#Convert to data frame
voting_intention <- as.data.frame(voting_intention)
class(voting_intention)
voting_intention
setnames(voting_intention, old = c("vote_int_data", "vote_int_positive_data", "vote_int_neutral_data", "vote_int_negative_data"),
     new = c("Party_Name", "Positve", "Neutral", "Negative"))
str(voting_intention)



#41 Scraping Brexit headlines from RTE website
url1 <- 'https://www.rte.ie/news/brexit/'
web_page <- read_html(url1)

# look at the contents of web_page
head(web_page)
str(web_page)

# Pull the top story headline
main_data_html <- html_nodes(web_page, '#panel-1 > div.lead-story-container.pillar-news > div.row > div > article > div > div.article-meta.column > h3 > a')
# View main_data_html
main_data_html
# Convert main_data_html to text
main_data <- html_text(main_data_html)
# View contents of main_data
main_data
# Remove trailing and leading white spaces 
main_data <- gsub("\n", "", main_data)
# Final content of main_data
str(main_data)

# Pull sub-headlines 
sub_data_html <- html_nodes(web_page, '#panel-1 > div.row.top-articles > div:nth-child(n) > article > a > div.article-meta.equalise')
# View first 6 rows of sub_data_html
head(sub_data_html)
# Check lngth of sub_data_html
length(sub_data_html)
# Convert sub_data_html to text
sub_data <- html_text(sub_data_html)
sub_data
# Remove trailing and leading white spaces 
sub_data <- gsub("[\n]", "", sub_data)
sub_data
#Dealing with date:
# Remove and occurencaes of Apr 
sub_data <- gsub("Apr", "", sub_data)
# Remove any digits
sub_data <- gsub("[[:digit:]]", "", sub_data)
# Remove remaing spaces 
sub_data <- trimws(sub_data)
str(sub_data)

# Pull remaing headlines relating to Brexit
other_data_html <- html_nodes(web_page, '#panel-1 > div.more-stream > div > div:nth-child(n) > article:nth-child(n)')
other_data_html
length(other_data_html)
# Conver to text
other_data <- html_text(other_data_html)
other_data
other_data <- gsub("\n", "", other_data) # Remove trailing and leading white spaces 
other_data
other_data <- gsub("[[:punct:]]", "", other_data) # Remove and punctuation such as single quotes
other_data
other_data <- gsub("Apr", "", other_data) # Remove and occurencaes of Apr 
other_data <- gsub("[[:digit:]]", "", other_data) # Remove any digits
other_data <- trimws(other_data) # Remove remaing spaces
other_data
str(other_data)
# Combine the vectors main_data, sub_data, other_data into a data frame called brexit_news
brexit_news <- as.data.frame(c(main_data, sub_data, other_data))
# Create a column names for the news stories
names(brexit_news) <- c("Headlines")
str(brexit_news)