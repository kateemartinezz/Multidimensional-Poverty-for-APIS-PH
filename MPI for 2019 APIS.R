library(readxl)
APIS_PUF_2019_Person <- read_excel("~/THESIS/APIS MICRODATA/APIS 2019 EXCEL FILES/APIS PUF 2019 Person.xlsx")
View(APIS_PUF_2019_Person)

library(data.table)     

install.packages("dplyr")
library(dplyr)

install.packages("tidyverse")
library(tidyverse)

# Grouping consecutive numbers in C101_LNO column
runs2 <- rle(APIS_PUF_2019_Person$C01_LNO)
APIS_PUF_2019_Person$C101 <- rep(seq_along(runs2$values), times = runs2$lengths)
APIS_PUF_2019_Person$C1011 <- cumsum(APIS_PUF_2019_Person$C01_LNO == 1)

#aggregate new table with households
count_households <- APIS_PUF_2019_Person %>%
  select(REG, HHID, C13_HGC, C1011)
View(count_households)

#aggregate data for C1011
C1011_aggregated_data <- APIS_PUF_2019_Person %>% 
  group_by(REG,HHID,C1011) %>% 
  summarize("Number of Households' Size" =n())
print(C1011_aggregated_data)


#assign codes for primary education
completed_educodes <- c("0", "1000", "10011", "10018", "24011", "24015", "34011")
completed_educodes <- as.integer(completed_educodes)

#when TRUE = no member of the household did not finish at least 6 years
#when FALSE = all members of the household finished at least 6 years
check_condition_C1011 <- APIS_PUF_2019_Person %>%
  group_by(REG,HHID,C1011) %>%
  summarize(HasNotCompleted6Years = all((C13_HGC %in% completed_educodes)))

# Filter households where no member has completed 6 years of education
households_no_completion_by_C1011 <- check_condition_C1011 %>%
  filter(HasNotCompleted6Years)

# Combine the columns Region, Household ID, and the Condition for no members that completed primary education
combined_HH_NotCompleted6Years_C1011 <- cbind(C1011_aggregated_data, check_condition_C1011$HasNotCompleted6Years)

# Rename the second column of a dataframe
colnames(combined_HH_NotCompleted6Years_C1011)[5] <- "No Member of the Household has Completed 6 Years of Primary Schooling"

# Replace values in a column for Households
combined_HH_NotCompleted6Years_C1011$`No Member of the Household has Completed 6 Years of Primary Schooling` <- ifelse(
  combined_HH_NotCompleted6Years_C1011$`No Member of the Household has Completed 6 Years of Primary Schooling` 
  == TRUE, 1, 0)



############## SECOND EXCEL FILE ######################
APIS_PUF_2019_Household <- read_excel("~/THESIS/APIS MICRODATA/APIS 2019 EXCEL FILES/APIS PUF 2019 Household.xlsx")
View(APIS_PUF_2019_Household)

#### aggregate those families that have children but not attending school #########

#### FOR 6:11
Family_6_11_noeduc <- APIS_PUF_2019_Household %>%
  select(REG, FSIZE, CHLD_6_11, HHMELEM_6_11)

Family_6_11_noeduc$NotAttending6_11 <- with(Family_6_11_noeduc, CHLD_6_11 == 1 & HHMELEM_6_11 == 0)
Family_6_11_noeduc$NotAttending6_11 <- ifelse(with(Family_6_11_noeduc, CHLD_6_11 == 1 & HHMELEM_6_11 == 0), 1, 0)

### FOR 12:15
Family_12_15_noeduc <- APIS_PUF_2019_Household %>%
  select(REG, FSIZE, CHLD_12_15, HHMHS_12_15)

Family_12_15_noeduc$NotAttending12_15 <- with(Family_12_15_noeduc, CHLD_12_15 == 1 & HHMHS_12_15 == 0)
Family_12_15_noeduc$NotAttending12_15 <- ifelse(with(Family_12_15_noeduc, CHLD_12_15 == 1 & HHMHS_12_15 == 0), 1, 0)

####Merge columns ####
FINAL_merged <- cbind(combined_HH_NotCompleted6Years_C1011,
                      Family_6_11_noeduc$NotAttending6_11,
                      Family_12_15_noeduc$NotAttending12_15)

#FINAL_merged <- FINAL_merged %>%
  #select(-'Number.of.Households..Size', -'No.Member.of.the.Household.has.Completed.6.Years.of.Primary.Schooling')

# Rename the FINAL_merged
colnames(FINAL_merged)[6] <- "A school-age child (up to grade 10, i.e. between age 6-11) is not
attending school"

colnames(FINAL_merged)[7] <- "A School-age child (up to grade 10, i.e. between age 12-15) is not
Attending School"


#################################### Using another excel file for each Household ###################################################
APIS_PUF_2019_RTM_Other_Relevant_Information_and_Drug_Awareness_and_Prevention <- read_excel("~/THESIS/APIS MICRODATA/APIS 2019 EXCEL FILES/APIS PUF 2019 RTM - Other Relevant Information and Drug Awareness and Prevention.xlsx")
View(APIS_PUF_2019_RTM_Other_Relevant_Information_and_Drug_Awareness_and_Prevention)

Below_foodpov_thresh1 <- APIS_PUF_2019_RTM_Other_Relevant_Information_and_Drug_Awareness_and_Prevention %>% 
  select(L4)
Below_foodpov_thresh2 <- APIS_PUF_2019_Household %>% 
  select(REG, FSIZE)

Below_foodpov_thresh2 <- Below_foodpov_thresh2 %>%
  mutate(REG = recode(REG,
                      "1" = "Region I - Ilocos Region",
                      "2" = "Region II - Cagayan Valley",
                      "3" = "Region III - Central Luzon",
                      "4" = "Region IV-A Calabarzon",
                      "5" = "Region V - Bicol",
                      "6" = "Region VI - Western Visayas",
                      "7" = "Region VII - Central Visayas",
                      "8" = "Region VIII - Eastern Visayas",
                      "9" = "Region IX - Zamboanga Peninsula",
                      "10" = "Region X - Northern Mindanao",
                      "11" = "Region XI - Davao",
                      "12" = "Region XII - SOCCKSARGEN",
                      "13" = "National Capital Region",
                      "14" = "Cordillera Administrative Region",
                      "15" = "Bangsamoro Autonomous Region in Muslim Mindanao",
                      "16" = "Region XIII - Caraga", 
                      "17" = "Region IV-B MIMAROPA"))

Foodpovthreshold <- tribble (
  ~REG, ~Food_Poverty_Threshold,
  "Region I - Ilocos Region", 355.05,
  "Region II - Cagayan Valley", 331.98,
  "Region III - Central Luzon", 365.08,
  "Region IV-A Calabarzon", 371.10,
  "Region V - Bicol", 325.55,
  "Region VI - Western Visayas", 321.47,
  "Region VII - Central Visayas", 348.23,
  "Region VIII - Eastern Visayas", 326.88,
  "Region IX - Zamboanga Peninsula",343.86,
  "Region X - Northern Mindanao", 333.59,
  "Region XI - Davao",333.17,
  "Region XII - SOCCKSARGEN",320.70,
  "National Capital Region", 385.58,
  "Cordillera Administrative Region", 333.07,
  "Bangsamoro Autonomous Region in Muslim Mindanao",358.54,
  "Region XIII - Caraga",330.17,
  "Region IV-B MIMAROPA", 308.91,
)

Below_foodpov_thresh2 <- Below_foodpov_thresh2 %>%
  left_join(Foodpovthreshold, by = "REG")


Below_foodpov_thresh2 <- Below_foodpov_thresh2 %>%
  mutate(Fsize_PovThreshold = FSIZE * Food_Poverty_Threshold)

FINAL_below_foodpov <- cbind(Below_foodpov_thresh2, Below_foodpov_thresh1)

## aggregate those who are below the food poverty threshold 

FINAL_below_foodpov <- FINAL_below_foodpov %>%
  mutate(Deprived = if_else(L4 < Fsize_PovThreshold, 1, 0))



####Merge columns ####
FINAL_merged2 <- cbind(FINAL_merged,
                       FINAL_below_foodpov$Deprived)

# Rename the FINAL_merged
colnames(FINAL_merged2)[8] <- "If per capita food consumption < 4/5 of food poverty"



#aggregate new table with the fourth indicator
Add_Fourth_ind <- APIS_PUF_2019_Person %>%
  select(REG, HHID,C16_ILL)
View(Add_Fourth_ind)

#aggregated data for those who are sick in the household
fourthgrouped_data <- Add_Fourth_ind %>%
  group_by(REG, HHID) %>%
  summarize(total_members = n(),
            sick_members = sum(C16_ILL %in% c(1)))

#calculating the percentage
fourthgrouped_data2 <- fourthgrouped_data %>%
  mutate(illness_percentage = sick_members / total_members)

#cut off for more than 50 percent of Households
FINAL_fourthgrouped <- fourthgrouped_data2 %>%
  mutate(MoreThan50PercentIllness = ifelse(illness_percentage > 0.5, 1, 0))
print(FINAL_fourthgrouped)


#to merge columns we need the same number of rows
new_row2 <- data.frame(REG = NA,
                       HHID = NA,
                       total_members = NA,
                       sick_members = NA,
                       illness_percentage = NA,
                       MoreThan50PercentIllness = NA)
#combined the rows with rbind
FINAL_fourthgrouped <- rbind(FINAL_fourthgrouped, new_row2)



#bind the new data frame
FINAL_merged3 <- cbind(FINAL_merged2, FINAL_fourthgrouped$MoreThan50PercentIllness)

# Rename the FINAL_merged3
colnames(FINAL_merged3)[9] <- "If more than 50% of household members report illness or injury over the
past month"



####################################### ANOTHER EXCEL #################################################
library(readxl)
APIS_PUF_2019_RTK_Water_and_Sanitation <- read_excel("~/THESIS/APIS MICRODATA/APIS 2019 EXCEL FILES/APIS PUF 2019 RTK - Water and Sanitation.xlsx")
View(APIS_PUF_2019_RTK_Water_and_Sanitation)

FINAL_merged4 <- cbind(FINAL_merged3, APIS_PUF_2019_RTK_Water_and_Sanitation$WSQ14, APIS_PUF_2019_RTK_Water_and_Sanitation$KQ1)

# Rename the FINAL_merged
colnames(FINAL_merged4)[10] <- "If household does not use own flush or closed pit toilet"

colnames(FINAL_merged4)[11] <- "If household's main source of water is not piped water or protected well"

##replacing the values## for column 11 and 12
FINAL_merged4$`If household does not use own flush or closed pit toilet` <- ifelse(FINAL_merged4$`If household does not use own flush or closed pit toilet` == 3, 1, 0)

FINAL_merged4$`If household's main source of water is not piped water or protected well` <- ifelse(FINAL_merged4$`If household's main source of water is not piped water or protected well` %in% c(5, 7, 8, 9), 1, 0)


################### HOUSING INDICATOR #######################################
library(readxl)
APIS_PUF_2019_RTJ_Housing <- read_excel("~/THESIS/APIS MICRODATA/APIS 2019 EXCEL FILES/APIS PUF 2019 RTJ - Housing.xlsx")
View(APIS_PUF_2019_RTJ_Housing)

FINAL_merged5 <- cbind(FINAL_merged4, APIS_PUF_2019_RTJ_Housing$JQ6, 
                       APIS_PUF_2019_RTJ_Housing$JQ2, 
                       APIS_PUF_2019_RTJ_Housing$JQ3)

#aggregate new table FOR COM ASSETS
com_asset <- APIS_PUF_2019_RTJ_Housing %>%
  select(HHID,JQ7K,JQ7P,JQ7N,JQ7I)

# Create a new column with default value as FALSE
com_asset$NoCommunicationAsset <- FALSE

# Check if any of the communication assets columns (I8M, I8U, I8R, I8K) is equal to 2 (No)
#com_asset$NoCommunicationAsset <- apply(com_asset[, c("I8M", "I8U", "I8R", "I8K")], 1, function(x) any(x == 2))

com_asset$NoCommunicationAsset <- apply(com_asset[, c("JQ7K","JQ7P","JQ7N","JQ7I")], 1, function(x) all(x != 1))


# Convert TRUE/FALSE to Yes/No
com_asset$NoCommunicationAsset <- ifelse(com_asset$NoCommunicationAsset, "Yes", "No")

################################################################################################################################
#aggregate new table FOR MOB ASSETS
mob_asset <- APIS_PUF_2019_RTJ_Housing %>%
  select(HHID,JQ7A,JQ7B,JQ7C)

# Create a new column with default value as FALSE
mob_asset$NoMobilityAsset <- FALSE

# Check if any of the communication assets columns (I8M, I8U, I8R, I8K) is equal to 2 (No)
#com_asset$NoCommunicationAsset <- apply(com_asset[, c("I8M", "I8U", "I8R", "I8K")], 1, function(x) any(x == 2))

mob_asset$NoMobilityAsset <- apply(mob_asset[, c("JQ7A","JQ7B","JQ7C")], 1, function(x) all(x != 1))


# Convert TRUE/FALSE to Yes/No
mob_asset$NoMobilityAsset <- ifelse(mob_asset$NoMobilityAsset, "Yes", "No")

####################################################################################################

#aggregate new table FOR LIV ASSETS
liv_asset <- APIS_PUF_2019_RTJ_Housing %>%
  select(HHID,JQ7H,JQ7R)


# Create a new column with default value as FALSE
liv_asset$NolivAsset <- FALSE

# Check if any of the communication assets columns (I8M, I8U, I8R, I8K) is equal to 2 (No)
#com_asset$NoCommunicationAsset <- apply(com_asset[, c("I8M", "I8U", "I8R", "I8K")], 1, function(x) any(x == 2))

liv_asset$NolivAsset <- apply(liv_asset[, c("JQ7H","JQ7R")], 1, function(x) all(x != 1))


# Convert TRUE/FALSE to Yes/No
liv_asset$NolivAsset <- ifelse(liv_asset$NolivAsset, "Yes", "No")


################removing columns for housing ################

com_asset <- com_asset %>% 
  select(HHID,NoCommunicationAsset)

mob_asset <- mob_asset %>% 
  select(HHID,NoMobilityAsset)

liv_asset <- liv_asset %>% 
  select(HHID,NolivAsset)

ASSETS <- cbind(com_asset,mob_asset$NoMobilityAsset, liv_asset$NolivAsset)

### YES = WALA NO = MERON ###
# Create a new column with default value as 0
ASSETS$DeprivedHH <- 0

#head(ASSETS)
# Check if all the asset columns contain "Yes"
ASSETS$DeprivedHH <- apply(ASSETS[, c("NoCommunicationAsset", "mob_asset$NoMobilityAsset", "liv_asset$NolivAsset")], 1, function(x) ifelse(all(x == "Yes"), 1, 0))

FINAL_merged5 <- cbind(FINAL_merged5, ASSETS$DeprivedHH)


#if else condition cut-offs to represent the deprived in column 5 & 6
FINAL_merged5[, 12] <- ifelse(FINAL_merged5[, 12] == 1, 0, 1)

#FINAL_merged5[, 13] <- ifelse(FINAL_merged5[, 13] %in% c(3, 6, 7), 1, 0)
FINAL_merged5$`If roof wall is not made of predominantly strong materials` <- ifelse(FINAL_merged5$`If roof wall is not made of predominantly strong materials` %in% c(3, 6, 7), 1, 0)


#FINAL_merged5[, 14] <- ifelse(FINAL_merged5[, 14] %in% c(3, 6, 7), 1, 0)
FINAL_merged5$`If outer wall is not made of predominantly strong materials` <- ifelse(FINAL_merged5$`If outer wall is not made of predominantly strong materials` %in% c(3, 6, 7), 1, 0)


#rename columns
colnames(FINAL_merged5)[12] <- "If there is no electricity in the house"

colnames(FINAL_merged5)[13] <- "If roof wall is not made of predominantly strong materials"

colnames(FINAL_merged5)[14] <- "If outer wall is not made of predominantly strong materials"

colnames(FINAL_merged5)[15] <- "If household does not have at least one communication asset (amongst
phone, tv, radio, or PC) AND at least one mobility asset (amongst car,
motorcycle or motorboat) OR at least one livelihood asset (amongst
agricultural land, livestock, refrigerator/freezer)"


########################### LAST INDICATOR ########################################
library(readxl)
APIS_PUF_2019_Person <- read_excel("~/THESIS/APIS MICRODATA/APIS 2019 EXCEL FILES/APIS PUF 2019 Person.xlsx")

Looking_for_Work <- APIS_PUF_2019_Person %>% 
  select(REG, HHID, C05_AGF, C09_CUR_ATTEND, C11_YNOT_ATTND)

#aggregated data for those who are sick in the household
Looking_for_Work$WorkingAgeStudents_NotinSchool_Employed <- with(Looking_for_Work, C05_AGF >= 18 & C09_CUR_ATTEND == 4 & C11_YNOT_ATTND == 7)


Looking_for_Work <- Looking_for_Work %>% 
  select(REG, HHID, WorkingAgeStudents_NotinSchool_Employed)

### replacing values of NAs to FALSE
Looking_for_Work$WorkingAgeStudents_NotinSchool_Employed <- ifelse(is.na(Looking_for_Work$WorkingAgeStudents_NotinSchool_Employed), FALSE, Looking_for_Work$WorkingAgeStudents_NotinSchool_Employed)

Looking2 <- Looking_for_Work %>%
  group_by(REG, HHID) %>%
  summarise(
    total_members = n(),
    workingagenotstudents_members = sum(WorkingAgeStudents_NotinSchool_Employed == TRUE, na.rm = TRUE),
    less_than_50_percent = (workingagenotstudents_members / total_members) 
  )

Looking2$Verify <- ifelse(Looking2$less_than_50_percent == 0, FALSE, Looking2$less_than_50_percent < (0.5 * 39316))

Looking2$Verify <- as.integer(Looking2$Verify)


num_rows <- 6

# Create a data frame with NA values
new_rows <- data.frame(REG = rep(NA, 1), 
                       HHID = rep(NA, 1), 
                     total_members = rep(NA, 1), 
                     workingagenotstudents_members = rep(NA, 1), 
                     less_than_50_percent = rep(NA, 1),
                     Verify = rep(NA, 1))

# Add the new rows to the end of the existing data frame
Looking2 <- rbind(Looking2, new_rows)


################### COMBINE THE FINAL INDICATORS ########################################################################
FINAL_MPI_Indicators <- cbind(FINAL_merged5, APIS_PUF_2019_Household$WRK_18, APIS_PUF_2019_Household$WRK_5_17)

#rename columns
colnames(FINAL_MPI_Indicators)[16] <- "If less than 50% of working age members who are not students
worked over the past 6 months"


####### replacing values #######
FINAL_MPI_Indicators[, 5] <- ifelse(FINAL_MPI_Indicators[, 5] == 1, 0.167, 0)

FINAL_MPI_Indicators[, 6] <- ifelse(FINAL_MPI_Indicators[, 6] == 1, 0.083, 0)

FINAL_MPI_Indicators[, 7] <- ifelse(FINAL_MPI_Indicators[, 7] == 1, 0.083, 0)

FINAL_MPI_Indicators[, 8] <- ifelse(FINAL_MPI_Indicators[, 8] == 1, 0.167, 0)

FINAL_MPI_Indicators[, 9] <- ifelse(FINAL_MPI_Indicators[, 9] == 1, 0.167, 0)

FINAL_MPI_Indicators[, 10] <- ifelse(FINAL_MPI_Indicators[, 10] == 1, 0.056, 0)

FINAL_MPI_Indicators[, 11] <- ifelse(FINAL_MPI_Indicators[, 11] == 1, 0.056, 0)

FINAL_MPI_Indicators[, 12] <- ifelse(FINAL_MPI_Indicators[, 12] == 1, 0.056, 0)

FINAL_MPI_Indicators[, 13] <- ifelse(FINAL_MPI_Indicators[, 13] == 1, 0.028, 0)

FINAL_MPI_Indicators[, 14] <- ifelse(FINAL_MPI_Indicators[, 14] == 1, 0.028, 0)

FINAL_MPI_Indicators[, 15] <- ifelse(FINAL_MPI_Indicators[, 15] == 1, 0.056, 0)

FINAL_MPI_Indicators[, 16] <- ifelse(FINAL_MPI_Indicators[, 16] == 1, 0.028, 0)

FINAL_MPI_Indicators[, 17] <- ifelse(FINAL_MPI_Indicators[, 17] == 1, 0.028, 0)

##################################################################################################

################## REPLACING VALUES ###########################################################################

# Define the column numbers for which you want to calculate the sum
columns <- c(5:17)


# Calculate the sum for the selected columns                    MERON NA.RM = TRUE 
FINAL_MPI_Indicators$MPIsum_columns <- rowSums(FINAL_MPI_Indicators[columns], na.rm = TRUE)



# Calculate the percentage values
FINAL_MPI_Indicators$MPIsum_columns <- (FINAL_MPI_Indicators$MPIsum_columns) * 100



####CALCULATING THE POVERTY THRESHOLD OF 33.33% ##################
# Assuming the data frame is named 'data' and the column is named 'column_name'
poverty_threshold <- 33.33  # Threshold for deprivation

# Identify households below the threshold
deprived_households <- FINAL_MPI_Indicators[FINAL_MPI_Indicators$MPIsum_columns >= poverty_threshold, ]

# Count the total number of deprived households
total_deprived_households <- nrow(deprived_households)

# Print the deprived households and the total count
print(deprived_households)

### NUMBER OF TOTAL DEPRIVED HOUSEHOLDS OR HOUSEHOLDS THAT ARE MULTIDIMENSIONALLY POOR ####
print(total_deprived_households)
# 13,187
#764

### NUMBER OF HOUSEHOLDS IN TOTAL FROM THE DATA FRAME ###
nrow(FINAL_MPI_Indicators)
# [1] 41,839

#TOTAL_HH <- 41838
#### CALCULATE THE HEADCOUNT RATIO (H) #####
#Headcount_Ratio <- print(total_deprived_households/TOTAL_HH)

Headcount_Ratio <- print(total_deprived_households/nrow(FINAL_MPI_Indicators))
# 0.07205434 (2017)
# 0.01943229 (2019)






#### CALCULATE THE INTENSITY OF POVERTY  #####
# Get the sum of the deprivation scores
#MPIsum_columns <- sum(deprived_households$MPIsum_columns, na.rm = TRUE)


#immultiply is the deprived vectors to the family size 
deprived_households$MPIsumproduct <- deprived_households$`Number of Households' Size` * deprived_households$MPIsum_columns

MPIsumproduct <- sum(deprived_households$MPIsumproduct, na.rm = TRUE)

Fsize <- sum(deprived_households$`Number of Households' Size`, na.rm = TRUE)

# Print the sum
print(MPIsumproduct)
#1543557 (2017)
#52752.7 (2019)


# Calculate the average
Intensity <- print(MPIsumproduct/Fsize)
# the intensity of the poor is 38.7955 (2017)
# the intensity of the poor is 35.5237 (2019)

### FINAL MPI RESULTS ###
MPI_ESTIMATE <- print(Headcount_Ratio*Intensity)
# 3.124487




########### CONTRIBUTION OF EACH DIMENSION ####################################################################################

library(dplyr)
head(deprived_households)
# Assuming your data frame is called 'df'
EDUCATION_1 <- deprived_households %>% 
  select(`Number of Households' Size`, `No Member of the Household has Completed 6 Years of Primary Schooling`)

EDUCATION_1 <- EDUCATION_1 %>%
  filter(`No Member of the Household has Completed 6 Years of Primary Schooling` != 0)

EDUCATION_1$EDUproduct <- EDUCATION_1$`Number of Households' Size` * EDUCATION_1$`No Member of the Household has Completed 6 Years of Primary Schooling`*100


##############################################################################################################################
EDUCATION_2 <- deprived_households %>% 
  select(`Number of Households' Size`, "A school-age child (up to grade 10, i.e. between age 6-11) is not\nattending school")

EDUCATION_2 <- EDUCATION_2 %>%
  filter(`A school-age child (up to grade 10, i.e. between age 6-11) is not\nattending school` != 0)

EDUCATION_2$EDUproduct <- EDUCATION_2$`Number of Households' Size` * EDUCATION_2$"A school-age child (up to grade 10, i.e. between age 6-11) is not\nattending school"*100


EDUCATION_3 <- deprived_households %>% 
  select(`Number of Households' Size`, `A School-age child (up to grade 10, i.e. between age 12-15) is not\nAttending School`)

EDUCATION_3 <- EDUCATION_3 %>%
  filter(`A School-age child (up to grade 10, i.e. between age 12-15) is not\nAttending School` != 0)

EDUCATION_3$EDUproduct <- EDUCATION_3$`Number of Households' Size` * EDUCATION_3$`A School-age child (up to grade 10, i.e. between age 12-15) is not\nAttending School`*100


# Calculate the sum of values in a specific column
sum_of_EDU1 <- sum(EDUCATION_1$EDUproduct) #3.974607
sum_of_EDU2 <- sum(EDUCATION_2$EDUproduct) #0.6802096
sum_of_EDU3 <- sum(EDUCATION_3$EDUproduct) #1.511059
sumoffamilysize <-sum(APIS_PUF_2019_Household$FSIZE)

educationshare <- ((sum_of_EDU1 + sum_of_EDU2 + sum_of_EDU3)/(nrow(APIS_PUF_2019_Person)))/.03673572
print(educationshare)
# 13.06292


############################################################################################################################
head(deprived_households)
# Assuming your data frame is called 'df'
HEALTH_1 <- deprived_households %>% 
  select(`Number of Households' Size`, `If per capita food consumption < 4/5 of food poverty`)

HEALTH_1 <- HEALTH_1 %>%
  filter(`If per capita food consumption < 4/5 of food poverty` != 0)

HEALTH_1$HEALTHproduct <- HEALTH_1$`Number of Households' Size` * HEALTH_1$`If per capita food consumption < 4/5 of food poverty`*100

###########################################################################################################################
HEALTH_2 <- deprived_households %>% 
  select(`Number of Households' Size`, "If more than 50% of household members report illness or injury over the
past month")

HEALTH_2 <- HEALTH_2 %>%
  filter(`If more than 50% of household members report illness or injury over the\npast month` != 0)

HEALTH_2$HEALTHproduct <- HEALTH_2$`Number of Households' Size` * HEALTH_2$`If more than 50% of household members report illness or injury over the\npast month`*100

# Calculate the sum of values in a specific column
sum_of_H1 <- sum(HEALTH_1$HEALTHproduct) #12.36753
sum_of_H2 <- sum(HEALTH_2$HEALTHproduct) #9.483437
sumoffamilysize <-sum(APIS_PUF_HOUSEHOLD2$FSIZE)

healthshare <- ((sum_of_H1 + sum_of_H2)/(nrow(APIS_PUF_2019_Person)))/.03673572 #.02526681
print(healthshare)
# second attempt 54.71297


###################################################### LIVING STANDARDS ######################################################################
head(deprived_households)
# Assuming your data frame is called 'df'
LIV_1 <- deprived_households %>% 
  select(`Number of Households' Size`, `If household does not use own flush or closed pit toilet`)

LIV_1 <- LIV_1 %>%
  filter(`If household does not use own flush or closed pit toilet` != 0)

LIV_1$LIVproduct <- LIV_1$`Number of Households' Size` * LIV_1$`If household does not use own flush or closed pit toilet`*100



# Assuming your data frame is called 'df'
LIV_2 <- deprived_households %>% 
  select(`Number of Households' Size`, `If household's main source of water is not piped water or protected well`)

LIV_2 <- LIV_2 %>%
  filter(`If household's main source of water is not piped water or protected well` != 0)

LIV_2$LIVproduct <- LIV_2$`Number of Households' Size` * LIV_2$`If household's main source of water is not piped water or protected well`*100



# Assuming your data frame is called 'df'
LIV_3 <- deprived_households %>% 
  select(`Number of Households' Size`, `If there is no electricity in the house`)

LIV_3 <- LIV_3 %>%
  filter(`If there is no electricity in the house` != 0)

LIV_3$LIVproduct <- LIV_3$`Number of Households' Size` * LIV_3$`If there is no electricity in the house`*100


# Assuming your data frame is called 'df'
LIV_4 <- deprived_households %>% 
  select(`Number of Households' Size`, `If roof wall is not made of predominantly strong materials`)

LIV_4 <- LIV_4 %>%
  filter(`If roof wall is not made of predominantly strong materials` != 0)

LIV_4$LIVproduct <- LIV_4$`Number of Households' Size` * LIV_4$`If roof wall is not made of predominantly strong materials`*100



# Assuming your data frame is called 'df'
LIV_5 <- deprived_households %>% 
  select(`Number of Households' Size`, `If outer wall is not made of predominantly strong materials`)

LIV_5 <- LIV_5 %>%
  filter(`If outer wall is not made of predominantly strong materials` != 0)

LIV_5$LIVproduct <- LIV_5$`Number of Households' Size` * LIV_5$`If outer wall is not made of predominantly strong materials`*100


# Assuming your data frame is called 'df'
LIV_6 <- deprived_households %>% 
  select(`Number of Households' Size`, "If household does not have at least one communication asset (amongst
phone, tv, radio, or PC) AND at least one mobility asset (amongst car,
motorcycle or motorboat) OR at least one livelihood asset (amongst
agricultural land, livestock, refrigerator/freezer)")

LIV_6 <- LIV_6 %>%
  filter(across(2, ~. != 0))

LIV_6$LIVproduct <- LIV_6$`Number of Households' Size` * LIV_6$"If household does not have at least one communication asset (amongst
phone, tv, radio, or PC) AND at least one mobility asset (amongst car,
motorcycle or motorboat) OR at least one livelihood asset (amongst
agricultural land, livestock, refrigerator/freezer)"*100


# Assuming your data frame is called 'df'
LIV_7 <- deprived_households %>% 
  select(`Number of Households' Size`, "If less than 50% of working age members who are not students
worked over the past 6 months")

LIV_7 <- LIV_7 %>%
  filter(across(2, ~. != 0))

LIV_7$LIVproduct <- LIV_7$`Number of Households' Size` * LIV_7$"If less than 50% of working age members who are not students
worked over the past 6 months"*100

# Assuming your data frame is called 'df'
LIV_8 <- deprived_households %>% 
  select(`Number of Households' Size`, ...17)

LIV_8 <- LIV_8 %>%
  filter(across(2, ~. != 0))

LIV_8$LIVproduct <- LIV_8$`Number of Households' Size` * LIV_8$...17*100

################################################################################################################################
# Calculate the sum of values in a specific column
sum_of_L1 <- sum(LIV_1$LIVproduct) #0.446363
sum_of_L2 <- sum(LIV_2$LIVproduct) #1.001697
sum_of_L3 <- sum(LIV_3$LIVproduct) #1.338041
sum_of_L4 <- sum(LIV_4$LIVproduct) #0
sum_of_L5 <- sum(LIV_5$LIVproduct) #0
sum_of_L6 <- sum(LIV_6$LIVproduct) #0.5804815
sum_of_L7 <- sum(LIV_7$LIVproduct) #2.375888 
sum_of_L8 <- sum(LIV_8$LIVproduct) #0.4117856
sumoffamilysize <-sum(APIS_PUF_HOUSEHOLD2$FSIZE)

livingstandardshare <- ((sum_of_L1 + sum_of_L2 + sum_of_L3 + sum_of_L4 + sum_of_L5 + sum_of_L6 + sum_of_L7 + sum_of_L8)/(nrow(APIS_PUF_2019_Person)))/.03673572
print(livingstandardshare)
#6.154256




################################################# SHARE OF DEPRIVATION ##########################################################
# education 1 = 629
# education 2 = 91
#education 3 = 208

# Health 1 = 939
# Health 2 = 812

# Liv 1 = 105
# Liv 2 = 220
# Liv 3 = 333
# Liv 4 = 0
# Liv 5 = 0
# Liv 6 = 144
# Liv 7 = 1217
# Liv 8 = 174

################################################### RENAMING OF REGIONS ###################################################################################

# Assuming you have a data frame named 'df' with a column named 'region_id'

library(dplyr)

FINALDEP <- deprived_households %>%
  mutate(REG = recode(REG,
                      "1" = "Region I - Ilocos Region",
                      "2" = "Region II - Cagayan Valley",
                      "3" = "Region III - Central Luzon",
                      "4" = "Region IV-A Calabarzon",
                      "5" = "Region V - Bicol",
                      "6" = "Region VI - Western Visayas",
                      "7" = "Region VII - Central Visayas",
                      "8" = "Region VIII - Eastern Visayas",
                      "9" = "Region IX - Zamboanga Peninsula",
                      "10" = "Region X - Northern Mindanao",
                      "11" = "Region XI - Davao",
                      "12" = "Region XII - SOCCKSARGEN",
                      "13" = "National Capital Region",
                      "14" = "Cordillera Administrative Region",
                      "15" = "Bangsamoro Autonomous Region in Muslim Mindanao",
                      "16" = "Region XIII - Caraga", 
                      "17" = "Region IV-B MIMAROPA"))

REGION_I <- FINALDEP %>%
  filter(REG == "Region I - Ilocos Region")

#####################################################################################################################################
REGION_II <- FINALDEP %>%
  filter(REG == "Region II - Cagayan Valley")

REGION_III <- FINALDEP %>%
  filter(REG == "Region III - Central Luzon")

REGION_IV_A <- FINALDEP %>%
  filter(REG == "Region IV-A Calabarzon")

REGION_V <- FINALDEP %>%
  filter(REG == "Region V - Bicol")

REGION_VI <- FINALDEP %>%
  filter(REG == "Region VI - Western Visayas")

REGION_VII <- FINALDEP %>%
  filter(REG == "Region VII - Central Visayas")

REGION_VIII <- FINALDEP %>%
  filter(REG == "Region VIII - Eastern Visayas")

REGION_IX <- FINALDEP %>%
  filter(REG == "Region IX - Zamboanga Peninsula")

REGION_X <- FINALDEP %>%
  filter(REG == "Region X - Northern Mindanao")

REGION_XI <- FINALDEP %>%
  filter(REG == "Region XI - Davao")

REGION_XII <- FINALDEP %>%
  filter(REG == "Region XII - SOCCKSARGEN")

NCR <- FINALDEP %>%
  filter(REG == "National Capital Region")

CAR <- FINALDEP %>%
  filter(REG == "Cordillera Administrative Region")

BARMM <- FINALDEP %>%
  filter(REG == "Bangsamoro Autonomous Region in Muslim Mindanao")

CARAGA <- FINALDEP %>%
  filter(REG == "Region XIII - Caraga")

MIMAROPA <- FINALDEP %>%
  filter(REG == "Region IV-B MIMAROPA")








############################################################## ROBUST CHECK USING BETA 1&2 ############################################
UNION_MPI <- cbind(FINAL_merged5, APIS_PUF_2019_Household$WRK_18, APIS_PUF_2019_Household$WRK_5_17)


# Assuming your dataset is named 'data'

# Define the column numbers for each dimension
education_columns <- c(5, 6, 7)  # Replace with the actual column numbers for education
health_columns <- c(8, 9)  # Replace with the actual column numbers for health
living_standards_columns <- c(10, 11, 12, 13, 14, 15, 16, 17)  # Replace with the actual column numbers for living standards

# Create a new column to store the poverty measure using the union approach with beta = 1
UNION_MPI$PovertyMeasureBeta1 <- ifelse(rowSums(UNION_MPI[, education_columns], na.rm = TRUE) >= 3 |
                                          rowSums(UNION_MPI[, health_columns], na.rm = TRUE) >= 2 |
                                          rowSums(UNION_MPI[, living_standards_columns], na.rm = TRUE) >= 8,
                                        1, 0)

# Create a new column to store the poverty measure using the union approach with beta = 2
UNION_MPI$PovertyMeasureBeta2 <- ifelse(rowSums(UNION_MPI[, education_columns], na.rm = TRUE) >= 3 |
                                          rowSums(UNION_MPI[, health_columns], na.rm = TRUE) >= 2 |
                                          rowSums(UNION_MPI[, living_standards_columns], na.rm = TRUE) >= 8,
                                        2, 0)

# Print the resulting dataset with the calculated poverty measures
print(UNION_MPI)


### OR JUST PRINT THE MEAN
mean(UNION_MPI$PovertyMeasureBeta1)
#[1] 0.01360769 -> 0.013
mean(UNION_MPI$PovertyMeasureBeta2)
#[1] 0.02721538 -> 0.027






############### THIS IS USING INVERSE INCIDENCE WEIGHTS BUT SAME CUTOFF ########
UNION_MPI <- cbind(FINAL_merged5, APIS_PUF_2019_Household$WRK_18, APIS_PUF_2019_Household$WRK_5_17)

# how to get the Nested Inverse Incidence Weights
incidence <- colMeans(UNION_MPI[, 5:17], na.rm = TRUE)

print(incidence)

inverse_incidence <- 1 / (incidence + 0.001)
print(inverse_incidence)
inverse_incidence <- inverse_incidence / sum(inverse_incidence)

print(inverse_incidence)



####### replacing values #######
UNION_MPI[, 5] <- ifelse(UNION_MPI[, 5] == 1, 0.204, 0)

UNION_MPI[, 6] <- ifelse(UNION_MPI[, 6] == 1, 0.222, 0)

UNION_MPI[, 7] <- ifelse(UNION_MPI[, 7] == 1, 0.066, 0)

UNION_MPI[, 8] <- ifelse(UNION_MPI[, 8] == 1, 0.026, 0)

UNION_MPI[, 9] <- ifelse(UNION_MPI[, 9] == 1, 0.099, 0)

UNION_MPI[, 10] <- ifelse(UNION_MPI[, 10] == 1, 0.072, 0)

UNION_MPI[, 11] <- ifelse(UNION_MPI[, 11] == 1, 0.038, 0)

UNION_MPI[, 12] <- ifelse(UNION_MPI[, 12] == 1, 0.050, 0)

UNION_MPI[, 13] <- ifelse(UNION_MPI[, 13] == 1, 0.097, 0)

UNION_MPI[, 14] <- ifelse(UNION_MPI[, 14] == 1, 0.038, 0)

UNION_MPI[, 15] <- ifelse(UNION_MPI[, 15] == 1, 0.016, 0)

UNION_MPI[, 16] <- ifelse(UNION_MPI[, 16] == 1, 0.069, 0)

UNION_MPI[, 17] <- ifelse(UNION_MPI[, 17] == 1, 0.004, 0)


# Define the column numbers for which you want to calculate the sum
columns <- c(5:17)


# Calculate the sum for the selected columns
UNION_MPI$MPIsum_columns <- rowSums(UNION_MPI[columns], na.rm = TRUE)




# Calculate the percentage values
UNION_MPI$MPIsum_columns <- (UNION_MPI$MPIsum_columns) * 100

NODEPRIVATIONS <- UNION_MPI %>%
  filter(MPIsum_columns == 0)


####CALCULATING THE POVERTY THRESHOLD OF 33.33% ##################
# Assuming the data frame is named 'data' and the column is named 'column_name'
poverty_threshold <- 33.33  # Threshold for deprivation

# Identify households below the threshold
UNION_deprived_households <- UNION_MPI[UNION_MPI$MPIsum_columns >= poverty_threshold, ]

# Count the total number of deprived households
UNION_total_deprived_households <- nrow(UNION_deprived_households)

# Print the deprived households and the total count
print(UNION_deprived_households)

### NUMBER OF TOTAL DEPRIVED HOUSEHOLDS OR HOUSEHOLDS THAT ARE MULTIDIMENSIONALLY POOR ####
print(UNION_total_deprived_households)
# 2, 749

### NUMBER OF HOUSEHOLDS IN TOTAL FROM THE DATA FRAME ###
nrow(UNION_MPI)
# [1] 41,839



U_Headcount_Ratio <- print(UNION_total_deprived_households/nrow(UNION_MPI))





#### CALCULATE THE INTENSITY OF POVERTY  #####
# Get the sum of the deprivation scores
#MPIsum_columns <- sum(deprived_households$MPIsum_columns, na.rm = TRUE)


#immultiply is the deprived vectors to the family size 
UNION_deprived_households$MPIsumproduct <- UNION_deprived_households$`Number of Households' Size` * UNION_deprived_households$MPIsum_columns

U_MPIsumproduct <- sum(UNION_deprived_households$MPIsumproduct, na.rm = TRUE)

U_Fsize <- sum(UNION_deprived_households$`Number of Households' Size`, na.rm = TRUE)

# Print the sum
print(U_MPIsumproduct)



# Calculate the average
U_Intensity <- print(U_MPIsumproduct/U_Fsize)


### FINAL MPI RESULTS ###
U_MPI_ESTIMATE <- print(U_Headcount_Ratio*U_Intensity)


########### CONTRIBUTION OF EACH DIMENSION ####################################################################################
#rename columns
colnames(UNION_deprived_households)[16] <- "If less than 50% of working age members who are not students
worked over the past 6 months"

colnames(UNION_deprived_households)[17] <- "If less than 50% of working age members who are not students
worked over the past 6 months_2"



library(dplyr)
head(UNION_deprived_households)
# Assuming your data frame is called 'df'
EDUCATION_1 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, `No Member of the Household has Completed 6 Years of Primary Schooling`)

EDUCATION_1 <- EDUCATION_1 %>%
  filter(`No Member of the Household has Completed 6 Years of Primary Schooling` != 0)

EDUCATION_1$EDUproduct <- EDUCATION_1$`Number of Households' Size` * EDUCATION_1$`No Member of the Household has Completed 6 Years of Primary Schooling`*100


##############################################################################################################################
EDUCATION_2 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, "A school-age child (up to grade 10, i.e. between age 6-11) is not\nattending school")

EDUCATION_2 <- EDUCATION_2 %>%
  filter(`A school-age child (up to grade 10, i.e. between age 6-11) is not\nattending school` != 0)

EDUCATION_2$EDUproduct <- EDUCATION_2$`Number of Households' Size` * EDUCATION_2$"A school-age child (up to grade 10, i.e. between age 6-11) is not\nattending school"*100


EDUCATION_3 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, `A School-age child (up to grade 10, i.e. between age 12-15) is not\nAttending School`)

EDUCATION_3 <- EDUCATION_3 %>%
  filter(`A School-age child (up to grade 10, i.e. between age 12-15) is not\nAttending School` != 0)

EDUCATION_3$EDUproduct <- EDUCATION_3$`Number of Households' Size` * EDUCATION_3$`A School-age child (up to grade 10, i.e. between age 12-15) is not\nAttending School`*100




# Calculate the sum of values in a specific column
sum_of_EDU1 <- sum(EDUCATION_1$EDUproduct, na.rm = TRUE) #2.416312
sum_of_EDU2 <- sum(EDUCATION_2$EDUproduct, na.rm = TRUE) #1.549395
sum_of_EDU3 <- sum(EDUCATION_3$EDUproduct, na.rm = TRUE) #3.351714
sumoffamilysize <-sum(APIS_PUF_HOUSEHOLD2$FSIZE)

educationshare <- ((sum_of_EDU1 + sum_of_EDU2 + sum_of_EDU3)/nrow(APIS_PUF_2019_Person))/.007423813 #.02526681
print(educationshare)
# 40.66966

############################################################################################################################
# Assuming your data frame is called 'df'
HEALTH_1 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, `If per capita food consumption < 4/5 of food poverty`)

HEALTH_1 <- HEALTH_1 %>%
  filter(`If per capita food consumption < 4/5 of food poverty` != 0)

HEALTH_1$HEALTHproduct <- HEALTH_1$`Number of Households' Size` * HEALTH_1$`If per capita food consumption < 4/5 of food poverty`*100

###########################################################################################################################
HEALTH_2 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, "If more than 50% of household members report illness or injury over the
past month")

HEALTH_2 <- HEALTH_2 %>%
  filter(`If more than 50% of household members report illness or injury over the\npast month` != 0)

HEALTH_2$HEALTHproduct <- HEALTH_2$`Number of Households' Size` * HEALTH_2$`If more than 50% of household members report illness or injury over the\npast month`*100

###########################################################################################################################
# Calculate the sum of values in a specific column
sum_of_H1 <- sum(HEALTH_1$HEALTHproduct) #11.45958
sum_of_H2 <- sum(HEALTH_2$HEALTHproduct) #3.848759
sumoffamilysize <-sum(APIS_PUF_HOUSEHOLD2$FSIZE)

healthshare <- ((sum_of_H1 + sum_of_H2)/nrow(APIS_PUF_2019_Person))/.007423813 #.02526681
print(healthshare)
# 6.959005

###################################################### LIVING STANDARDS ######################################################################
head(deprived_households)
# Assuming your data frame is called 'df'
LIV_1 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, `If household does not use own flush or closed pit toilet`)

LIV_1 <- LIV_1 %>%
  filter(`If household does not use own flush or closed pit toilet` != 0)

LIV_1$LIVproduct <- LIV_1$`Number of Households' Size` * LIV_1$`If household does not use own flush or closed pit toilet`*100



# Assuming your data frame is called 'df'
LIV_2 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, `If household's main source of water is not piped water or protected well`)

LIV_2 <- LIV_2 %>%
  filter(`If household's main source of water is not piped water or protected well` != 0)

LIV_2$LIVproduct <- LIV_2$`Number of Households' Size` * LIV_2$`If household's main source of water is not piped water or protected well`*100



# Assuming your data frame is called 'df'
LIV_3 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, `If there is no electricity in the house`)

LIV_3 <- LIV_3 %>%
  filter(`If there is no electricity in the house` != 0)

LIV_3$LIVproduct <- LIV_3$`Number of Households' Size` * LIV_3$`If there is no electricity in the house`*100


# Assuming your data frame is called 'df'
LIV_4 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, `If roof wall is not made of predominantly strong materials`)

LIV_4 <- LIV_4 %>%
  filter(`If roof wall is not made of predominantly strong materials` != 0)

LIV_4$LIVproduct <- LIV_4$`Number of Households' Size` * LIV_4$`If roof wall is not made of predominantly strong materials`*100



# Assuming your data frame is called 'df'
LIV_5 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, `If outer wall is not made of predominantly strong materials`)

LIV_5 <- LIV_5 %>%
  filter(`If outer wall is not made of predominantly strong materials` != 0)

LIV_5$LIVproduct <- LIV_5$`Number of Households' Size` * LIV_5$`If outer wall is not made of predominantly strong materials`*100


# Assuming your data frame is called 'df'
LIV_6 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, "If household does not have at least one communication asset (amongst
phone, tv, radio, or PC) AND at least one mobility asset (amongst car,
motorcycle or motorboat) OR at least one livelihood asset (amongst
agricultural land, livestock, refrigerator/freezer)")

LIV_6 <- LIV_6 %>%
  filter(across(2, ~. != 0))

LIV_6$LIVproduct <- LIV_6$`Number of Households' Size` * LIV_6$"If household does not have at least one communication asset (amongst
phone, tv, radio, or PC) AND at least one mobility asset (amongst car,
motorcycle or motorboat) OR at least one livelihood asset (amongst
agricultural land, livestock, refrigerator/freezer)"*100


# Assuming your data frame is called 'df'
LIV_7 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, "If less than 50% of working age members who are not students
worked over the past 6 months")

LIV_7 <- LIV_7 %>%
  filter(across(2, ~. != 0))

LIV_7$LIVproduct <- LIV_7$`Number of Households' Size` * LIV_7$"If less than 50% of working age members who are not students
worked over the past 6 months"*100

# Assuming your data frame is called 'df'
LIV_8 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, "If less than 50% of working age members who are not students
worked over the past 6 months_2")

LIV_8 <- LIV_8 %>%
  filter(across(2, ~. != 0))

LIV_8$LIVproduct <- LIV_8$`Number of Households' Size` * LIV_8$"If less than 50% of working age members who are not students
worked over the past 6 months_2"*100


################################################################################################################################
# Calculate the sum of values in a specific column
sum_of_L1 <- sum(LIV_1$LIVproduct) #0.7217763
sum_of_L2 <- sum(LIV_2$LIVproduct) #1.418271
sum_of_L3 <- sum(LIV_3$LIVproduct) #1.698892
sum_of_L4 <- sum(LIV_4$LIVproduct) #0.5745137
sum_of_L5 <- sum(LIV_5$LIVproduct) #1.045375
sum_of_L6 <- sum(LIV_6$LIVproduct) #0.8026759
sum_of_L7 <- sum(LIV_7$LIVproduct) #2.165961 
sum_of_L8 <- sum(LIV_8$LIVproduct) #0.4563244
sumoffamilysize <-sum(APIS_PUF_HOUSEHOLD2$FSIZE)

livingstandardshare <- ((sum_of_L1 + sum_of_L2 + sum_of_L3 + sum_of_L4 + sum_of_L5 + sum_of_L6 + sum_of_L7 + sum_of_L8)/nrow(APIS_PUF_2019_Person))/.007423813
print(livingstandardshare)
#17.54115
