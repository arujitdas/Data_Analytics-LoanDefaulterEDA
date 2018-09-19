############################# Load required libraries ##########################################
library(tidyr)
library(stringr)
library(ggplot2)
library(stats)
library(lubridate)

# Load the dataset
loan.details <- read.csv("loan.csv",stringsAsFactors = F)

############################### Cleaning the dataset, if any ####################################

# Check the duplicates, if any, for the id field
sum(duplicated(loan.details$id))
# Check the duplicates, if any, for the member field
sum(duplicated(loan.details$member_id))
#Analysis : There are no duplicate/repeated loan id or loan member id 


############################### Missing Values and their treatment ##############################

# Basic attributes of the dataset

paste("There are",nrow(loan.details)," observations of", ncol(loan.details), "variables")
paste("There are",sum(is.na(loan.details))," N/A Values and", 
      sum(length(loan.details == "")), "empty values" )
# Analysis : 
#1.There are 39717 observations of 111 Variables
#2. There are 2208180  N/A Values and 4408587 empty values

############################### Custom Functions ####################################################

# Write two functions separately which will look for each column of the dataset and find out if  
# all the values in column is null or empty. If yes, we can remove the complete column as it 
# will not have any significance in our analysis

# Function to retrieve all the columns with all N/A values
AllNA.test <-  function (x) {
  w <- sapply(x, function(x)all(is.na(x)))
  if (any(w)) {
    # Columns with all NA values are listed here
    #paste("All NA in columns", paste(which(w), collapse=", "))
    return(which(w))
  }
}

# Function to retrieve all the columns with all Empty values
AllEmpty.test <- function(x) {
  w <- sapply(x, function(x)all(x==""))
  if (any(w)) {
    # Columns with all Empty values are listed here
    paste("All Empty in columns", paste(which(w), collapse=", "))
    return(which(w))
  }
}

# Function to retrieve all the columns with only one unique value. If there is any such column then
# this column is not significant for any analysis
SingleUniqueValue.test <- function(x) {
  w <- sapply(x, function(x)length(unique(x)) == 1)
  if (any(w)) {
    return(which(w))
  }
}

############## Removing complete empty or N/A or one unique values columns ##########################

#Delete Columns With Complete NA Values
x <- AllNA.test(loan.details)
loan.details <- loan.details[, - c(x)]

#Delete Columns With Complete Empty Values
y <- AllEmpty.test(loan.details)
if(!is.null(y)) loan.details <-loan.details[, - c(y)]

#Delete Columns With only one unique Value
z <- SingleUniqueValue.test(loan.details)
loan.details <- loan.details[, - c(z)]

# URL column can be removed as it doesn't have any significance
loan.details <- loan.details[, -which(colnames(loan.details) == "url")]

# Remove the columns which are having the values of only 0 or N/A's
loan.details <- loan.details[, -which(colnames(loan.details) == "collections_12_mths_ex_med")]
loan.details <- loan.details[, -which(colnames(loan.details) == "chargeoff_within_12_mths")]
loan.details <- loan.details[, -which(colnames(loan.details) == "tax_liens")]

loan.details$next_pymnt_d <- paste( "01-", loan.details$next_pymnt_d,sep="")
loan.details$next_pymnt_d <- as.Date(loan.details$next_pymnt_d, format = "%d-%b-%y")
# Remove the column which has 97% of N/A
paste("There are",sum(is.na(loan.details$next_pymnt_d))/nrow(loan.details) *100," % N/A's in next_pymnt_d")
loan.details <- loan.details[, -which(colnames(loan.details) == "next_pymnt_d")] 


############## Treating some columns by splitting them into multiple columns 
# or retaining specific values to make more sense and for analysis ############

# removing the 'months' string from the term column
loan.details$term <- gsub("[^0-9]", "",loan.details$term) 
# removing the % from the 'int_rate' column
loan.details$int_rate <- gsub("[^0-9,.]", "",loan.details$int_rate) 
# removing the % from the 'revol_util' column
loan.details$revol_util <- gsub("[^0-9,.]", "",loan.details$revol_util) 

################# Outlier Treatment for Annual Income #####################

stats<- boxplot.stats(loan.details$annual_inc)$stats
minValue <- stats[1]
FirstQuartile <- stats[2]
MedianValue <- stats[3]
ThirdQuartile <- stats[4]
maxValue <- stats[5]
lowerOutlier <- FirstQuartile - (1.5*(ThirdQuartile-FirstQuartile))
upperOutlier <- ThirdQuartile + (1.5*(ThirdQuartile-FirstQuartile))
loan.details$annual_inc_outlier_removed <- ifelse(loan.details$annual_inc < lowerOutlier |
                                                    loan.details$annual_inc > upperOutlier,NA
                                                  ,loan.details$annual_inc)



############################# Fix Data Type #######################################

#Convert few character columns to factor for univariate analysis
FactorColNames <- c("term","grade","sub_grade","emp_length","home_ownership", "verification_status", "loan_status","purpose","zip_code")
loan.details[FactorColNames] <- lapply(loan.details[FactorColNames] , factor)

# Convert int_rate and revol_util from character to numeric
loan.details$int_rate <- as.numeric(loan.details$int_rate)
loan.details$revol_util <- as.numeric(loan.details$revol_util)

###################### Implementing Business Rules ####################################

############# Derived Metrics - Business Driven metrics ###############################

# Employment length in years. Possible values are between 0 and 10 where 0 means less than one year 
# and 10 means ten or more years. 
loan.details$emp_length <- gsub("[^0-9,<]", "",loan.details$emp_length) 
loan.details$emp_length <- gsub("<1", "0",loan.details$emp_length) 
# replace blank values by NA
loan.details$emp_length[which(loan.details$emp_length == "")] <- NA
# convert the column back to factor
loan.details$emp_length <- as.factor(loan.details$emp_length)

##############  Debt To Income and rented home business rule #################################
################  Reducing the amount of loan ######################
# Any of your debts that report to credit bureaus would count in your debt to income ratio, 
# along with your new mortgage payment. Debts to consider would include student loans, credit cards, 
# auto, and any other installment payments. Payment towards your rent would not be considered 
# during the DTI ratio
# Business Rule assumption : In general any DTI less than or equal to 36% is considered as ideal. 
# Any DTI above 36% is not ideal and candidate for loan amount reduction
# In our case:
# 1. Any DTI less than or equal to 18% is ideal irrespective of the home ownership status
# 2. Any DTI greater than 18% and with rented house is risk to loan as the
# installment might impact. Hence in this case, we can reduce the loan amount requested for
# 3. Any DTI greater than 18% and less than or equal to 36% but not with rented house is also ideal
# 4. Any DTI greater than 36% is risk to the loan

loan.details$reduceLoanAmount = ifelse(loan.details$dti <= 18,"No" , 
                                    ifelse(loan.details$dti >36 ,"Yes",
                                           ifelse(loan.details$home_ownership == "RENT","Yes",
                                                  "No")))

############## business rule for risky applicant #################################
################  Provide loan with higher rate of interest ######################
# Insight : if in the credit history, it is found that the person opting for the loan
# had several credit enquiry then this implies that this person might have several other
# loans and may be trying to reduce his/her current loan APR by opting new loan
# In this case, provide loan to these applicants but with a higher interest rate to
# compensate the loss of investor, if any
# We will consider that if number of credit enquiry in last 6 month is less than or equal
# to 3, it is idle case otherwise provide loan with increase in rate of interest
loan.details$IncreaseInterestRate <- ifelse(loan.details$inq_last_6mths <=3,"No", "Yes")


############# Derived Metrics - Type Driven metrics ###############################

# A type driven metrics is derived that if the loan description is given in less
# than 50 characters, we assume that less details are provided
loan.details$descriptionLength <- ifelse(nchar(loan.details$desc) < 200 | is.na(loan.details$desc), 
                                  "Less Details Provided", "Good Details Provided")

############## Convert the date columns in appropriate date format for analysis ############
loan.details$issue_d <- paste( "01-", loan.details$issue_d,sep="")
loan.details$issue_d <- as.Date(loan.details$issue_d, format = "%d-%b-%y")

# earliest_cr_line doesn't have 4 digits year. By default R will convert it to current century
# Hence a proper formatting is required
loan.details$earliest_cr_line <- paste( "01-", loan.details$earliest_cr_line,sep="")
loan.details$earliest_cr_line <- ifelse(as.numeric(str_sub(loan.details$earliest_cr_line, -2)) > 
                                          as.numeric(str_sub(year(Sys.Date()),-2)),
                                        sub("-([0-9]+)$", "-19\\1", loan.details$earliest_cr_line),
                                        sub("-([0-9]+)$", "-20\\1", loan.details$earliest_cr_line))
loan.details$earliest_cr_line <- as.Date(loan.details$earliest_cr_line, format = "%d-%b-%Y")

loan.details$last_pymnt_d <- paste( "01-", loan.details$last_pymnt_d,sep="")
loan.details$last_pymnt_d <- as.Date(loan.details$last_pymnt_d, format = "%d-%b-%y")

loan.details$last_credit_pull_d <- paste( "01-", loan.details$last_credit_pull_d,sep="")
loan.details$last_credit_pull_d <- as.Date(loan.details$last_credit_pull_d, format = "%d-%b-%y")

# Extract year and month from the issue date
loan.details$issue_Year <- format(loan.details$issue_d, "%Y")
loan.details$issue_Month <- format(loan.details$issue_d, "%m")

#If needed, Remove the redundant column issue_d since it is split into Year and month
#loan.details <- loan.details[, -which(colnames(loan.details) == "issue_d")]

############# Derived Metrics - Data Driven metrics #############################

# Split the investor funded amount into 7 categories in the range of 5000. This will help in analyzing
# the loan amount for which defaulter is most
loan.details$funded_amount = ifelse(loan.details$funded_amnt <= 5000,"0-5000" , 
                                    ifelse(loan.details$funded_amnt <= 10000,"5001-10000",
                                           ifelse(loan.details$funded_amnt <= 15000,"10001 - 15000",
                                                  ifelse(loan.details$funded_amnt <= 20000,"15001 - 20000",
                                                         ifelse(loan.details$funded_amnt <= 25000,"20001 - 25000",
                                                                ifelse(loan.details$funded_amnt <= 30000,"25001 - 30000",
                                                                       "30001-35000"))))))

# Split the loan amount into 7 categories in the range of 5000. This will help in analyzing
# the loan amount for which defaulter is most
loan.details$loan_amount = ifelse(loan.details$loan_amnt <= 5000,"0-5000" , 
                                  ifelse(loan.details$loan_amnt <= 10000,"5001-10000",
                                         ifelse(loan.details$loan_amnt <= 15000,"10001 - 15000",
                                                ifelse(loan.details$loan_amnt <= 20000,"15001 - 20000",
                                                       ifelse(loan.details$loan_amnt <= 25000,"20001 - 25000",
                                                              ifelse(loan.details$loan_amnt <= 30000,"25001 - 30000",
                                                                     "30001-35000"))))))

# let us create a variable which will store the ratio of funded amount against the loan applied for
loan.details$approved_loan_ratio <- (loan.details$funded_amnt/loan.details$loan_amnt)*100

# Split the annual income into 6 categories in the range of 25000. This will help in analyzing
# the annual income category for which defaulter is most
loan.details$annual_income <- ifelse(loan.details$annual_inc_outlier_removed <= 25000,"0-25000" , 
                                     ifelse(loan.details$annual_inc_outlier_removed <= 50000,"25001-50000",
                                            ifelse(loan.details$annual_inc_outlier_removed <= 75000,"50001 - 75000",
                                                   ifelse(loan.details$annual_inc_outlier_removed <= 100000,"75001 - 100000",
                                                          ifelse(loan.details$annual_inc_outlier_removed <= 125000,"100001 - 125000",
                                                                  "1250001-150000")))))
# Split the DTI into 6 categories in the range of 0-5 This will help in analyzing
# the DTI for which number of loans are more and even where defaulters are most
loan.details$DTICategory <- ifelse(loan.details$dti <= 5,"0-5" , 
                                     ifelse(loan.details$dti <= 10,"5.1-10",
                                            ifelse(loan.details$dti <= 15,"10.1 - 15",
                                                   ifelse(loan.details$dti <= 20,"15.1 - 20",
                                                          ifelse(loan.details$dti <= 25,"20.1 - 25",
                                                                 "25.1-30")))))

#########################################################################################
# Make the dataset consistent with respect to NA's and decimal places

# Replace the empty values with NA; leave other values. This will help in analysis
# After this all the empty values will be removed from dataset and will have only NA's
loan.details$emp_title[which(loan.details$emp_title == "")] <- NA
loan.details$desc[which(loan.details$desc == "")] <- NA
loan.details$title[which(loan.details$title == "")] <- NA

# Converting the numeric columns to two digits for consistency and better analysis
loan.details$funded_amnt_inv <- round(as.numeric(loan.details$funded_amnt_inv), 2)
loan.details$installment <- round(as.numeric(loan.details$installment), 2)
loan.details$int_rate <- round(as.numeric(loan.details$int_rate), 2)
loan.details$total_pymnt <- round(as.numeric(loan.details$total_pymnt), 2)
loan.details$total_rec_late_fee <- round(as.numeric(loan.details$total_rec_late_fee), 2)
loan.details$total_pymnt_inv <- round(as.numeric(loan.details$total_pymnt_inv), 2)
loan.details$total_rec_int <- round(as.numeric(loan.details$total_rec_int), 2)
loan.details$collection_recovery_fee <- round(as.numeric(loan.details$collection_recovery_fee), 2)
loan.details$last_pymnt_amnt <- round(as.numeric(loan.details$last_pymnt_amnt), 2)

######## Univariate analysis on continuous  and Categorical variables for insights #########

# plot a graph for annual income- It seems most Loans are from the annual income between 25000-75000
ggplot(loan.details, aes(x = as.factor(annual_income))) + 
  geom_bar()+theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Annual Income vs Loan Frequency") +  xlab("Annual Income") + ylab(" Loan Frequency")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )

# plot a graph for DTI- It seems most Loans are from the DTI between 10-20
ggplot(loan.details, aes(x = as.factor(DTICategory))) + 
  geom_bar()+theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("DTI vs Loan Frequency") +  xlab("DTI") + ylab(" Loan Frequency")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )

# plot a graph for Purpose- It seems most Loans are for purpose - debt_consolidation
ggplot(loan.details, aes(x = as.factor(purpose))) + 
  geom_bar()+theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Purpose vs Loan Frequency") +  xlab("Purpose") + ylab(" Loan Frequency")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )

# plot a graph for state address- It seems most Loans are from CA state
ggplot(loan.details, aes(x = as.factor(addr_state))) + 
  geom_bar()+theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("State vs Loan Frequency") +  xlab("State") + ylab(" Loan Frequency")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )


ggplot(loan.details, aes(x=loan.details$loan_amnt)) + 
geom_histogram(binwidth = 100, fill = "red") +  ggtitle("Loan Amount Frequency")+
xlab("Loan Amount")+ylab("Frequency")

# Mean 11219.44 and Median 10000 for loan_amnt are almost same
mean(loan.details$loan_amnt)
median(loan.details$loan_amnt)

ggplot(loan.details, aes(x=loan.details$funded_amnt_inv))+
geom_histogram(binwidth = 100, fill = "red") +  ggtitle("Investor Funded Amount Frequency")+
xlab("Investor Funded Amount")+ylab("Frequency")
# Mean 10397.45 and Median 8975 for funded_amnt_inv are significantly different
# it is advisable to consider median here to remove any outliers
mean(loan.details$funded_amnt_inv)
median(loan.details$funded_amnt_inv)

########### It seems that the amount that is requested  by the borrower
# is almost fulfilled by the investors. This can be clearly seen from the two graphs

ggplot(loan.details, aes(x=loan.details$term))+
  geom_bar(fill="red") +  ggtitle("Term Vs. Frequency")+
  xlab("Term in months")+ylab("Frequency")
# 10,000 people opt for 60 month term and around 30,000 people opt for 36 month

ggplot(loan.details, aes(x=loan.details$grade))+
  geom_bar(fill="red") +  ggtitle("Loan Grade Vs. Frequency")+
  xlab("Loan Grade")+ylab("Frequency")
# Grade frequency in this order B,A,C,D,E,F,G


ggplot(loan.details, aes(x=loan.details$emp_length))+
  geom_bar(fill="red") +  ggtitle("Employee Length Vs. Frequency")+
  xlab("Employee Length")+ylab("Frequency")
# people with 10 or more exp. requests for loan more or people with
# experience of less than 1. For committments or for initial set up respectively

ggplot(loan.details, aes(x=loan.details$home_ownership))+
  geom_bar(fill="red") +  ggtitle("Home Ownership Vs. Frequency")+
  xlab("Home Ownership")+ylab("Frequency")
# people who have mortgage their home or people staying in rent requests for more loan
# mortgage - need for more money, rented - still need to fulfil other requirement
# Own home owner - they are very less 

ggplot(loan.details, aes(x=loan.details$verification_status))+
  geom_bar(fill="red") +
  ggtitle("Verification Status Vs. Frequency")+
  xlab("Verification Status")+ylab("Frequency") 
# Not verified cases are more in numbers where loans are applied or approved

ggplot(loan.details, aes(x=loan.details$loan_status))+
  geom_bar(fill="red") +  ggtitle("Loan Status Vs. Frequency")+
  xlab("Loan Status")+ylab("Frequency")
# Most of the loans 35000 are fully paid and around 5000 are charged off
# we will analyze to find attributes of people who fall in this category


# plot a graph between funded amount and number of charged off
# It seems most charged off are from the funded amount between 0-10000
ggplot(loan.details, aes(x = as.factor(funded_amount), fill= loan_status)) + 
  geom_bar()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Funded amount vs charged off") +  xlab("Funded amount") + ylab("Number of charged off")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )

# plot a graph between loan amount and number of charged off
# It seems most charged off are from the loan amount between 0-10000
ggplot(loan.details, aes(x = as.factor(loan_amount), fill= loan_status)) + 
  geom_bar()+theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Loan amount vs charged off") +  xlab("Loan amount") + ylab("Number of charged off")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )


################# Segmented Univariate Analysis to get insights ##############################

ggplot(data = subset(loan.details,!is.na(loan_amount)), aes(purpose,loan.details$loan_amnt)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Loan Purpose vs Loan Amount") +  xlab("Loan Purpose") + ylab("Loan Amount")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# Small Business and Debt consolidation have the highest loan amounts across categories;  


ggplot(data = subset(loan.details,!is.na(loan_amount)), aes(term,loan.details$loan_amnt)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Loan Term vs Loan Amount") +  xlab("Loan Term") + ylab("Loan Amount")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# Surprisingly the 60 month term has a mean value much higher than that of the 36 month term loan. 


ggplot(loan.details, aes(loan_status,funded_amnt_inv)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Loan Status vs Loan Amount") +  xlab("Loan Status") + ylab("Loan amount")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# Out of all the charged off loans, 50% of loan amount is less than or approx. equal to 10,000

ggplot(loan.details, aes(loan_status,int_rate)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Loan Status vs Loan Interest Rate") +  xlab("Loan Status") + ylab("Loan Interest Rate")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# Out of all the charged off loans, 50% of loan interest rate is less than or approx. equal to 13.75

ggplot(loan.details, aes(loan_status,installment)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Loan Status vs Loan Installment") +  xlab("Loan Status") + ylab("Loan Installment")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# Out of all the charged off loans, 50% of loan installment is less than or approx. equal to 300

ggplot(data = subset(loan.details,!is.na(annual_inc_outlier_removed)), aes(loan_status,annual_inc_outlier_removed)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Loan Status vs Annual Income") +  xlab("Loan Status") + ylab("Annual Income")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# Out of all the charged off loans, 50% of annual income is approx. equal to 50000


############ Bivariate analysis on continuous variables ##################

ggplot(loan.details, aes(x = loan_amnt, y = funded_amnt_inv)) + geom_point() + geom_smooth(method = "gam") +
theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Loan Amount vs Funded Amount") +  xlab("Loan Amount") + ylab("Funded Amount")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
cor(loan.details$loan_amnt,loan.details$funded_amnt_inv)
# .94 - This implies that the loan amount applied for is highly coorelated with amount 
# being issued. More or less the amount of loan requested will be given

ggplot(loan.details, aes(x = loan_amnt, y = installment)) + geom_point() + geom_smooth(method = "gam") +
theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Loan Amount vs Installment") +  xlab("Loan Amount") + ylab("Installment")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
cor(loan.details$loan_amnt,loan.details$installment)
# .93 - This implies that the loan amount applied for is highly coorelated with installment amount 
# More the loan amount, more the installment amount

ggplot(loan.details, aes(x = dti, y = int_rate)) + geom_point() + geom_smooth(method = "gam") +
theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Debt to Income Vs Interest Rate") +  xlab("Debt to Income") + ylab("Interest Rate")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
cor(loan.details$dti,loan.details$int_rate) 
# 0.1 It seems that change in dti value has not effect in interest rate

ggplot(data = subset(loan.details,!is.na(annual_inc_outlier_removed)), aes(x = dti, y = annual_inc_outlier_removed)) + geom_point() + geom_smooth(method = "gam") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("DTI vs Annual Income") +  xlab("DTI") + ylab("Annual Income")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )

Income <- subset(loan.details,!is.na(annual_inc_outlier_removed))
cor(Income$dti,Income$annual_inc_outlier_removed)
# -0.07 It seems that change in dti value has not effect in annual income

############ Bivariate analysis on categorical variables ##################

ggplot(data= loan.details, aes(x=loan.details$addr_state, fill = loan_status))+ 
  geom_bar(position = "dodge")+ theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("State vs. Loan Status") +  xlab("State") + ylab("Frequency of Loan")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# people staying in CA state are mostly defaulter


# Let us plot some bar graph for loan status against different variables to find the possible defaulter parameter
ggplot(data= loan.details, aes(x=loan.details$emp_length, fill = loan_status))+ 
  geom_bar(position = "dodge")+ theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Employee Experience vs. Loan Status") +  xlab("Employee Experience (in Years)") + ylab("Frequency of Loan")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# people with experience of 10 or more years are mostly defaulter

ggplot(data= loan.details, aes(x=loan.details$verification_status, fill = loan_status))+ 
  geom_bar(position = "dodge")+ theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Verification Status vs. Loan Status") +  xlab("Verification Status") + ylab("Frequency of Loan")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# people for whom verification is not done are mostly defaulter

ggplot(data= loan.details, aes(x=loan.details$term, fill = loan_status))+ 
  geom_bar(position = "dodge")+ theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Term vs. Loan Status") +  xlab("Term (in months)") + ylab("Frequency of Loan")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# people who opted or provided with 36 month loan term are mostly defaulter

ggplot(data= loan.details, aes(x=loan.details$grade, fill = loan_status))+ 
  geom_bar(position = "dodge")+ theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Grade vs. Loan Status") +  xlab("Grade") + ylab("Frequency of Loan")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# people who have provided with loan under grade under B & C are mostly defaulter

ggplot(data= loan.details, aes(x=loan.details$descriptionLength, fill = loan_status))+ 
  geom_bar(position = "dodge")+ theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Loan avail intention vs. Loan Status") +  xlab("Loan avail intention") + ylab("Frequency of Loan")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
 # people who have not provided enough details for the intention of loan are mostly defaulter

ggplot(data= loan.details, aes(x=loan.details$issue_Year, fill = loan_status))+ 
  geom_bar(position = "dodge")+ theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Loan Issue Year vs. Loan Status") +  xlab("Loan Issue Year") + ylab("Frequency of Loan")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# Insight : Mostly the number of defaulters are increasing year by year

################## Some more analysis to find more insights using Bivariate analysis ##########################

###  1. Impact of charged off when verification status and Experience of borrower are considered

# subset the data to consider only the charged off record
Total.chargedoff <- subset(loan.details, loan_status == "Charged Off")
# Make two sets - one for verified loan subset and one for non-verifed loan subset
Verified.ChargedOff <- subset(Total.chargedoff, verification_status == "Verified")
NotVerified.ChargedOff <- subset(Total.chargedoff, verification_status == "Not Verified")

# Aggregate the data based on the experience
BorrowerExperience.Verified <- aggregate(funded_amnt~emp_length,Verified.ChargedOff,length)
BorrowerExperience.NotVerified <- aggregate(funded_amnt~emp_length,NotVerified.ChargedOff,length)

# It is found that Percentage of charged off for non verified loan(38%) is more than verified loan (36%)
NonVerifiedPercentage <- nrow(NotVerified.ChargedOff)/nrow(Total.chargedoff) * 100
VerifiedPercentage <- nrow(Verified.ChargedOff)/nrow(Total.chargedoff) * 100

# Let us see how borrower's experience level affects this - Top two categories are 0 and 10 level experience

# 30% of borrowers are defaulters even though they are verified compare to
# 20% of borrowers who are defaulters when  they are Not verified
VerifiedAndExp10 <- BorrowerExperience.Verified[BorrowerExperience.Verified$emp_length==10,2]/ nrow(Verified.ChargedOff) * 100
NonVerifiedAndExp10 <- BorrowerExperience.NotVerified[BorrowerExperience.NotVerified$emp_length==10,2]/ nrow(NotVerified.ChargedOff) * 100
# It feels like non verified borrowers will mostly be the defaulters but it is determined that verfied customers are
# also defaulters if the experience level is more than 10

###  2. Impact of charged off when verification status and Loan term of borrower are considered

# subset the data to consider only the charged off record
Total.chargedoff <- subset(loan.details, loan_status == "Charged Off")
# Make two sets - one for verified loan subset and one for non-verifed loan subset
Verified.ChargedOff <- subset(Total.chargedoff, verification_status == "Verified")
NotVerified.ChargedOff <- subset(Total.chargedoff, verification_status == "Not Verified")

# Aggregate the data based on the term
Term.Verified <- aggregate(funded_amnt~term,Verified.ChargedOff,length)
Term.NotVerified <- aggregate(funded_amnt~term,NotVerified.ChargedOff,length)

# It is found that Percentage of charged off for non verified loan(38%) is more than verified loan (36%)
NonVerifiedPercentage <- nrow(NotVerified.ChargedOff)/nrow(Total.chargedoff) * 100
VerifiedPercentage <- nrow(Verified.ChargedOff)/nrow(Total.chargedoff) * 100

# Let us see how term affects this - two terms are 36 and 60

# 58% of borrowers are defaulters even though they are verified and opted for 60 month term compare to
# 23% of borrowers who are defaulters when  they are Not verified and opted for 60 month term
VerifiedAnd60Term <- Term.Verified[Term.Verified$term=="60",2]/ nrow(Verified.ChargedOff) * 100
NonVerifiedAnd60Term <- Term.NotVerified[Term.NotVerified$term=="60",2]/ nrow(NotVerified.ChargedOff) * 100

# It seems that mostly borrowers with 36 months will be the defaulter but it is also determined that
# verified borrowers with 60 month term are also defaulters

###  3. Impact of charged off when term and loan grade are considered

# subset the data to consider only the charged off record
Total.chargedoff <- subset(loan.details, loan_status == "Charged Off")
# Make two sets - one for 36 month term and one for 60 month term
month36.ChargedOff <- subset(Total.chargedoff, term == "36")
month60.Chargedoff <- subset(Total.chargedoff, term == "60")

# Let us see how term affects this - two terms are 36 and 60
Term60 <- nrow(month60.Chargedoff)/ nrow(Total.chargedoff) * 100
Term36 <- nrow(month36.ChargedOff)/ nrow(Total.chargedoff) * 100
# 43% and 57% of defaulters are having a loan term of 60 and 36 month respectively

# Aggregate the data based on the grade
Term36.grade <- aggregate(funded_amnt~grade,month36.ChargedOff,length)
Term60.grade <- aggregate(funded_amnt~grade,month60.Chargedoff,length)
# It seems defaulters are mostly from grade B and with 36 month term - 985 borrowers
# and grade E with 60 month term - 539 borrowers

###  4. Impact of charged off when experience and home ownership are considered

# subset the data to consider only the charged off record
Total.chargedoff <- subset(loan.details, loan_status == "Charged Off")

# Aggregate the data to see the split of experience level
# it is found that borrower with experience of 10 years are more prone to defaulter than anyone else
exp.chargedoff <- aggregate(funded_amnt~emp_length,Total.chargedoff,length)

# Lets find the percentage of 10years (24%) and 0(11%) years defaulters - the biggest default categories
Exp10 <- exp.chargedoff[exp.chargedoff$emp_length=="10",2]/ nrow(Total.chargedoff) * 100
Exp0 <- exp.chargedoff[exp.chargedoff$emp_length=="0",2]/ nrow(Total.chargedoff) * 100

# Lets find the impact of home ownership in this case
ownerhip.rent <- subset(Total.chargedoff, home_ownership == "RENT")
#aggregate data to see how experience level plays here 
exp.ownership <- aggregate(funded_amnt~emp_length,ownerhip.rent,length)
# 36%
RentWithExp10 <- (exp.ownership[exp.ownership$emp_length == 10,2]/exp.chargedoff[exp.chargedoff$emp_length=="10",2])*100
# 64%
RentWithExp0 <- (exp.ownership[exp.ownership$emp_length == 0,2]/exp.chargedoff[exp.chargedoff$emp_length=="0",2])*100
# It is true that experience level of 10 or more borrowers are risk to loan, however if they have a rented house then
# borrowers with experience level of 0 are more risk to the loan


