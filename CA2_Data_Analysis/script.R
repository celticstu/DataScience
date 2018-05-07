library(pwr)
### QUESTION 4: TRADE

# Import the Value_of_Merchandise_Trade.csv dataset to q4 data frame, whilst removing the header 
q4 <- read.csv("Value_of_Merchandise_Trade.csv", header = FALSE)

# Add new cloumn names to the q4 data frame 
colnames(q4) <- c("Country", "Import_Export", "Jan10", "Feb10", "Mar10", "Arp10", "May10", "Jun10", "Ju10", "Aug10", "Sep10", "Oct10", "Nov10", "Dec10",
    "Jan11", "Feb11", "Mar11", "Arp11", "May11", "Jun11", "Ju11", "Aug11", "Sep11", "Oct11", "Nov11", "Dec11",
    "Jan12", "Feb12", "Mar12", "Arp12", "May12", "Jun12", "Ju12", "Aug12", "Sep12", "Oct12", "Nov12", "Dec12",
    "Jan13", "Feb13", "Mar13", "Arp13", "May13", "Jun13", "Ju13", "Aug13", "Sep13", "Oct13", "Nov13", "Dec13",
    "Jan14", "Feb14", "Mar14", "Arp14", "May14", "Jun14", "Ju14", "Aug14", "Sep14", "Oct14", "Nov14", "Dec14",
    "Jan15", "Feb15", "Mar15", "Arp15", "May15", "Jun15", "Ju15", "Aug15", "Sep15", "Oct15", "Nov15", "Dec15",
    "Jan16", "Feb16", "Mar16", "Arp16", "May16", "Jun16", "Ju16", "Aug16", "Sep16", "Oct16", "Nov16", "Dec16",
    "Jan17", "Feb17", "Mar17", "Arp17", "May17", "Jun17", "Ju17", "Aug17", "Sep17", "Oct17", "Nov17", "Dec17",
    "Jan18", "Feb18")

# Check structure of q4
str(q4)
# View the first six rows of q4 
head(q4)
# View a sample of q4
sample(q4)

# Using the pwr.chisq.test function, find the number of sample required
# whilst indicating the effect size as .3, the degree of freedom as .1,
# the confidence level and .05 and also the power as 0.8
# Note degree of fredom come from number of classes minus 1
# Sampling
q4_test <- pwr.chisq.test(w = .3, N = NULL, df = .2, sig.level = .05, power = 0.8)
q4_test
# Visualise q4_test
plot(q4_test)

# Hypothesis
q4_hyp_test <- pwr.anova.test(k = 100, n= 54, f = .25, sig.level = .05)
q4_hyp_test

# Question 5: Trade cont.
q5 <- read.csv("Exports_and_Imports_of_Services.csv")
head(q5, 10)
str(q5)
# Add tempoary column headings
colnames(q5) <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q")

str(q5)

exports <- q5[grep("Imports", q5$c, invert = TRUE),]
head(exports, 10)
colnames(exports) <- c("Service","Country", "Exports", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
exports <- subset(exports, select = -c(3))
str(exports)
head(exports)

# Sampling
q5_exports <- pwr.chisq.test(w = .5, N = NULL, df = .3, sig.level = .05, power = 0.8)
q5_exports
plot(q5_exports)

# Hypothesis
exports_hyp_test <- pwr.anova.test(k = 16, n = 22, f = .25, sig.level = .05)
exports_hyp_test


# Find the mean of each year and round it to zero
# avg <- round(sapply(exports[3:16], mean),0)
# new_avg <- sweep(avg,2, exports[3:16], "-")

imports <- q5[grep("Exports", q5$c, invert = TRUE),]
colnames(imports) <- c("Service", "Country", "Imports", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
imports <- subset(imports, select = -c(3))
str(imports)
head(imports)
q5_imports <- pwr.chisq.test(w = .5, N = NULL, df = .1, sig.level = .05, power = 0.8)
q5_imports
plot(q5_imports)

# Hypothesis
imports_hyp_test <- pwr.anova.test(k = 16, n = 16, f = .25, sig.level = .05)
imports_hyp_test

# Question 6: Trade cont.
q6 <- read.csv("Value_of_Merchandise_Trade_by_Commodity_Group.csv", header = FALSE)
head(q6, 20)
str(industries)

# Rename the columns
colnames(q6) <- c("Industry", "Currency", "1973", "1974", "1975", "1976", "1977", "1978", "1979",
"1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", 
"1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", 
"2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
"2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")

# Remove the Currency column
q6$Currency <- NULL
head(q6)

str(q6)

q6_test <- pwr.chisq.test(w = .3, N = NULL, df = .1, sig.level = .03, power = 0.95)
q6_test
plot(q6_test)
library(ggplot2)
plot(q6_test)

q6_hyp_test <- pwr.anova.test(k = 46, n = 93, f = .25, sig.level = .05)
q6_hyp_test


# Question 7:
q7 <- read.csv("Passenger_Movement_by_Irish_Airport.csv", header = FALSE)
head(q7)
sum(is.na(q7))
colnames(q7) <- c("Irish_Airport", "Direction", "GB_NI_Airpot","2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12",
"2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12",
"2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12",
"2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12", "2018M01")

str(q7)
head(q7)
q7_test <- pwr.chisq.test(w = .3, N = NULL, df = .3, sig.level = .03, power = 0.95)
q7_test
plot(q7_test)

q7_hyp_test <- pwr.anova.test(k = 52, n = 122, f = .25, sig.level = .05)
q7_hyp_test

library(dplyr)
#c <- read.csv("Outward_Passenger_Movement_by_Irish_Airport.csv")

head(d)
# Question 8:
q8 <- read.csv("Overseas_Trips_incl_Expenditure_to_Ireland.csv", header = FALSE)
print(trips$V2)
colnames(q8) <- c("Country","Length_of_Stay","2012Q1", "2012Q2", "2012Q3", "2012Q4",
"2013Q1", "2013Q2", "2013Q3", "2013Q4",
"2014Q1", "2014Q2", "2014Q3", "2014Q4",
"2015Q1", "2015Q2", "2015Q3", "2015Q4",
"2016Q1", "2016Q2", "2016Q3", "2016Q4",
"2017Q1", "2017Q2","2017Q3", "2017Q4")

str(q8)
head(q8)
q8_test <- pwr.chisq.test(w = .3, N = NULL, df = .1, sig.level = .03, power = 0.90)
q8_test
plot(q8_test)

q8_hyp_test <- pwr.anova.test(k = 26, n = 73, f = .25, sig.level = .05)
q8_hyp_test

# Question 9: Employment
q9 <- read.csv("Persons_Aged_15_Years_and_Over.csv", header = FALSE)
# Viw structure of q9
str(q9)

head(q9)
#View a random sample of q9 
sample(q9)
q9$Country
# Add new colum headings
colnames(q9) <- c("Status","Country","2006Q3", "2006Q4",
"2007Q1", "2007Q2", "2007Q3", "2007Q4",
"2008Q1", "2008Q2", "2008Q3", "2008Q4",
"2009Q1", "2009Q2", "2009Q3", "2009Q4",
"2010Q1", "2010Q2", "2010Q3", "2010Q4",
"2011Q1", "2011Q2", "2011Q3", "2011Q4",
"2012Q1", "2012Q2", "2012Q3", "2012Q4",
"2013Q1", "2013Q2", "2013Q3", "2013Q4",
"2014Q1", "2014Q2", "2014Q3", "2014Q4",
"2015Q1", "2015Q2", "2015Q3", "2015Q4",
"2016Q1", "2016Q2", "2016Q3", "2016Q4",
"2017Q1", "2017Q2")


str(q9)
# Create a new data frame called uk that contains all data where Status value os UK
uk <- subset(q9, Country == "UK")
# Remove Country column
head(uk)
uk$Country <- NULL
library(dplyr)
str(uk)
head(uk)
uk_test <- pwr.chisq.test(w = .3, N = NULL, df = .1, sig.level = .05, power = 0.90)
uk_test
plot(uk_test)

q9_hyp_test <- pwr.anova.test(k = 45, n = 62, f = .25, sig.level = .05)
q8_hyp_test