install.packages("readxl")
library(readxl)
APIS_PUF_2020_Member <- read_excel("~/THESIS/APIS MICRODATA/APIS 2020 EXCEL FILES/APIS PUF 2020 Member.xlsx")
View(APIS_PUF_2020_Member)

library(data.table)
install.packages("dplyr")
library(dplyr)

install.packages("tidyverse")
library(tidyverse)


runs2 <- rle(APIS_PUF_2020_Member$C101_LNO) 
APIS_PUF_2020_Member$C101 <- rep(seq_along(runs2$values), times = runs2$lengths)
APIS_PUF_2020_Member$C1011 <- cumsum(APIS_PUF_2020_Member$C101_LNO == 1)


#aggregate new table with households
count_households <- APIS_PUF_2020_Member %>%
  select(REG, HHID, C07_GRADE, C1011)
View(count_households)

#aggregate data for C1011 to check how many households there are
C1011_aggregated_data <- APIS_PUF_2020_Member %>% 
  group_by(REG,HHID,C1011) %>% 
  summarize("Number of Households' Size" =n())
print(C1011_aggregated_data)

#assign codes for primary education
completed_codes <- c("0","01000", "2000", "10011", "10012", "10013", "10014")
completed_codes <- as.integer(completed_codes)

#when TRUE = no member of the household did not finish at least 6 years
#when FALSE = all members of the household finished at least 6 years
check_condition_C1011 <- APIS_PUF_2020_Member %>%
  group_by(REG,HHID,C1011) %>%
  summarize(HasNotCompleted6Years = all((C07_GRADE %in% completed_codes)))

# Filter households where no member has completed 6 years of education
households_no_completion_by_C1011 <- check_condition_C1011 %>%
  filter(HasNotCompleted6Years)

# Combine the columns Region, Household ID, and the Condition for no members that completed primary education
combined_HH_NotCompleted6Years_C1011 <- cbind(C1011_aggregated_data, check_condition_C1011$HasNotCompleted6Years)

# Rename the second column of a dataframe
colnames(combined_HH_NotCompleted6Years_C1011)[5] <- "No Member of the Household has Completed 6 Years of Primary Schooling"

# Replace values in a column for Members
combined_HH_NotCompleted6Years_C1011$`No Member of the Household has Completed 6 Years of Primary Schooling` <- ifelse(
  combined_HH_NotCompleted6Years_C1011$`No Member of the Household has Completed 6 Years of Primary Schooling` 
  == TRUE, 1, 0)




#################################### Using another excel file for each Household ###################################################
library(readxl)
APIS_PUF_HOUSEHOLD2 <- read_excel("~/THESIS/APIS MICRODATA/APIS 2020 EXCEL FILES/APIS PUF HOUSEHOLD2.xlsx")
View(APIS_PUF_HOUSEHOLD2)

library(dplyr)
# rm(dataframe1,dataframe2)

#### aggregate those families that have children but not attending school #########

#### FOR 6:11
Family_6_11_noeduc <- APIS_PUF_HOUSEHOLD2 %>%
  select(REG, FSIZE, CHLD_6_11, HHMELEM_6_11)

Family_6_11_noeduc$NotAttending6_11 <- with(Family_6_11_noeduc, CHLD_6_11 == 1 & HHMELEM_6_11 == 0)
Family_6_11_noeduc$NotAttending6_11 <- ifelse(with(Family_6_11_noeduc, CHLD_6_11 == 1 & HHMELEM_6_11 == 0), 1, 0)

### FOR 12:15
Family_12_15_noeduc <- APIS_PUF_HOUSEHOLD2 %>%
  select(REG, FSIZE, CHLD_12_15, HHMHS_12_15)

Family_12_15_noeduc$NotAttending12_15 <- with(Family_12_15_noeduc, CHLD_12_15 == 1 & HHMHS_12_15 == 0)
Family_12_15_noeduc$NotAttending12_15 <- ifelse(with(Family_12_15_noeduc, CHLD_12_15 == 1 & HHMHS_12_15 == 0), 1, 0)

#to merge columns we need the same number of rows
new_row <- data.frame(REG = NA,
                      C1011 = NA,
                      `Number of Households' Size` = NA,
                      `No Member of the Household has Completed 6 Years of Primary Schooling` = NA)
#print(new_row)

#combined the rows with rbind
combined_HH_NotCompleted6Years_C1011 <- rbind(combined_HH_NotCompleted6Years_C1011, new_row)
#head(combined_HH_NotCompleted6Years_C1011)

#we removed and cleaned the data because there were additional rows that were unnecessary
combined_HH_NotCompleted6Years_C1011 <- combined_HH_NotCompleted6Years_C1011 %>%
select(-'Number.of.Households..Size', -'No.Member.of.the.Household.has.Completed.6.Years.of.Primary.Schooling')

# COMBINE THE HOUSEHOLD2 DF and THE Combined_HH_Not_Completed6Years_C1011
FINAL_merged <- cbind(combined_HH_NotCompleted6Years_C1011,
                      Family_6_11_noeduc$NotAttending6_11,
                      Family_12_15_noeduc$NotAttending12_15)

# Rename the FINAL_merged
colnames(FINAL_merged)[6] <- "A school-age child (up to grade 10, i.e. between age 6-11) is not
attending school"

colnames(FINAL_merged)[7] <- "A School-age child (up to grade 10, i.e. between age 12-15) is not
Attending School"

install.packages("tidyverse")
library(tidyverse)
#################################### Using another excel file for each Household ###################################################
library(readxl)
APIS_PUF_2020_OTHER_RELEVANT_INFO <- read_excel("~/THESIS/APIS MICRODATA/APIS 2020 EXCEL FILES/APIS PUF 2020 OTHER RELEVANT INFO.xlsx")
View(APIS_PUF_2020_OTHER_RELEVANT_INFO)

Below_foodpov_thresh1 <- APIS_PUF_2020_OTHER_RELEVANT_INFO %>% 
  select(K4A, K4B)

Below_foodpov_thresh2 <- APIS_PUF_HOUSEHOLD2 %>% 
  select(REG,FSIZE)
#rm(Below_foodpov_thresh2)

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
  "Region I - Ilocos Region", 377.11,
  "Region II - Cagayan Valley", 349.45,
  "Region III - Central Luzon", 392.26,
  "Region IV-A Calabarzon", 384.17,
  "Region V - Bicol", 342.91,
  "Region VI - Western Visayas", 335.42,
  "Region VII - Central Visayas", 369.54,
  "Region VIII - Eastern Visayas", 338.67,
  "Region IX - Zamboanga Peninsula",365.33,
  "Region X - Northern Mindanao",350.89,
  "Region XI - Davao",338.21,
  "Region XII - SOCCKSARGEN",331.32,
  "National Capital Region", 407.28,
  "Cordillera Administrative Region", 347.53,
  "Bangsamoro Autonomous Region in Muslim Mindanao",363.21,
  "Region XIII - Caraga",340.97,
  "Region IV-B MIMAROPA", 322.64,
)


Below_foodpov_thresh2 <- Below_foodpov_thresh2 %>%
  left_join(Foodpovthreshold, by = "REG")

Below_foodpov_thresh2 <- Below_foodpov_thresh2 %>%
  mutate(Fsize_PovThreshold = FSIZE * Food_Poverty_Threshold)


FINAL_below_foodpov <- cbind(Below_foodpov_thresh2, Below_foodpov_thresh1)


FINAL_below_foodpov <- FINAL_below_foodpov %>%
  mutate(Deprived = if_else(K4A < Fsize_PovThreshold & K4B < Fsize_PovThreshold, 1, 0))




####Merge columns ####
FINAL_merged2 <- cbind(FINAL_merged, 
                       FINAL_below_foodpov$Deprived)

# Rename the FINAL_merged
colnames(FINAL_merged2)[8] <- "If per capita food consumption < 4/5 of food poverty line"



#aggregate new table with the fourth indicator
Add_Fourth_ind <- APIS_PUF_2020_Member %>%
  select(REG, HHID, C07_GRADE,C47A_TIMES_ILL, C1011)
View(Add_Fourth_ind)

#remove columns to avoid confusion
Add_Fourth_ind <- Add_Fourth_ind %>%
  select(-C07_GRADE)

#aggregated data for those who are sick in the household
fourthgrouped_data <- Add_Fourth_ind %>%
  group_by(REG, HHID, C1011) %>%
  summarize(total_members = n(),
            sick_members = sum(C47A_TIMES_ILL == 1))
view(fourthgrouped_data)


#calculating the percentage
fourthgrouped_data <- fourthgrouped_data %>%
  mutate(illness_percentage = sick_members / total_members)

#cut off for more than 50 percent of Households
FINAL_fourthgrouped <- fourthgrouped_data %>%
  mutate(MoreThan50PercentIllness = ifelse(illness_percentage > 0.5, 1, 0))
print(FINAL_fourthgrouped)


#to merge columns we need the same number of rows
new_row2 <- data.frame(REG = NA,
                      C1011 = NA,
                      total_members = NA,
                      sick_members = NA,
                      illness_percentage = NA,
                      MoreThan50PercentIllness = NA)
#combined the rows with rbind
FINAL_fourthgrouped <- rbind(FINAL_fourthgrouped, new_row2)
head(combined_HH_NotCompleted6Years_C1011)


#bind the new data frame
FINAL_merged3 <- cbind(FINAL_merged2, FINAL_fourthgrouped$MoreThan50PercentIllness)

# Rename the FINAL_merged3
colnames(FINAL_merged3)[9] <- "If more than 50% of household members report illness or injury over the
past month"



#################################### Using another excel file for each Household ###################################################
library(readxl)
APIS_PUF_2020_RTJ_Water_Sanitation_and_Hygiene_2_ <- read_excel("~/THESIS/APIS MICRODATA/APIS 2020 EXCEL FILES/APIS PUF 2020 RTJ - Water Sanitation and Hygiene (2).xlsx")
View(APIS_PUF_2020_RTJ_Water_Sanitation_and_Hygiene_2_)

FINAL_merged4 <- cbind(FINAL_merged3, APIS_PUF_2020_RTJ_Water_Sanitation_and_Hygiene_2_$WS14, APIS_PUF_2020_RTJ_Water_Sanitation_and_Hygiene_2_$J1)

# Rename the FINAL_merged
colnames(FINAL_merged4)[10] <- "If household does not use own flush or closed pit toilet"

colnames(FINAL_merged4)[11] <- "If household's main source of water is not piped water or protected well"
head(FINAL_merged2)

##replacing the values## for column 11 and 12
FINAL_merged4$`If household does not use own flush or closed pit toilet` <- ifelse(FINAL_merged4$`If household does not use own flush or closed pit toilet` == 3, 1, 0)

FINAL_merged4$`If household's main source of water is not piped water or protected well` <- ifelse(FINAL_merged4$`If household's main source of water is not piped water or protected well` %in% c(5, 7, 8, 9), 1, 0)


########################### using the Household excel file #############################################
library(readxl)
APIS_PUF_2020_RTI_Housing <- read_excel("~/THESIS/APIS MICRODATA/APIS 2020 EXCEL FILES/APIS PUF 2020 RTI - Housing.xlsx")
View(APIS_PUF_2020_RTI_Housing)

FINAL_merged5 <- cbind(FINAL_merged4, APIS_PUF_2020_RTI_Housing$I7, APIS_PUF_2020_RTI_Housing$I2, APIS_PUF_2020_RTI_Housing$I3)

#aggregate new table FOR COM ASSETS
com_asset <- APIS_PUF_2020_RTI_Housing %>%
  select(HHID,I8M,I8U,I8R,I8K)

# Create a new column with default value as FALSE
com_asset$NoCommunicationAsset <- FALSE

# Check if any of the communication assets columns (I8M, I8U, I8R, I8K) is equal to 2 (No)
com_asset$NoCommunicationAsset <- apply(com_asset[, c("I8M", "I8U", "I8R", "I8K")], 1, function(x) all(x != 1))

# Convert TRUE/FALSE to Yes/No
com_asset$NoCommunicationAsset <- ifelse(com_asset$NoCommunicationAsset, "Yes", "No")

###########################################################################################################

#aggregate new table FOR MOB ASSETS
mob_asset <- APIS_PUF_2020_RTI_Housing %>%
  select(HHID,I8A, I8C, I8E)

# Create a new column with default value as FALSE
mob_asset$NoMobilityAsset <- FALSE

# Check if any of the communication assets columns (I8M, I8U, I8R, I8K) is equal to 2 (No)
#com_asset$NoCommunicationAsset <- apply(com_asset[, c("I8M", "I8U", "I8R", "I8K")], 1, function(x) any(x == 2))

mob_asset$NoMobilityAsset <- apply(mob_asset[, c("I8A", "I8C", "I8E")], 1, function(x) all(x != 1))


# Convert TRUE/FALSE to Yes/No
mob_asset$NoMobilityAsset <- ifelse(mob_asset$NoMobilityAsset, "Yes", "No")


##########################################################################################################

#aggregate new table FOR LIV ASSETS
liv_asset <- APIS_PUF_2020_RTI_Housing %>%
  select(HHID,I8J,I8V)


# Create a new column with default value as FALSE
liv_asset$NolivAsset <- FALSE

# Check if any of the communication assets columns (I8M, I8U, I8R, I8K) is equal to 2 (No)
#com_asset$NoCommunicationAsset <- apply(com_asset[, c("I8M", "I8U", "I8R", "I8K")], 1, function(x) any(x == 2))

liv_asset$NolivAsset <- apply(liv_asset[, c("I8J", "I8V")], 1, function(x) all(x != 1))


# Convert TRUE/FALSE to Yes/No
liv_asset$NolivAsset <- ifelse(liv_asset$NolivAsset, "Yes", "No")

#################removing columns for housing ################

com_asset <- com_asset %>% 
  select(HHID,NoCommunicationAsset)

mob_asset <- mob_asset %>% 
  select(HHID,NoMobilityAsset)

liv_asset <- liv_asset %>% 
  select(HHID,NolivAsset)

ASSETS <- cbind(com_asset,mob_asset$NoMobilityAsset, liv_asset$NolivAsset)

## YES = WALA NO = MERON ###
# Create a new column with default value as 0
ASSETS$DeprivedHH <- 0

# Check if all the asset columns contain "Yes"
ASSETS$DeprivedHH <- apply(ASSETS[, c("NoCommunicationAsset", "mob_asset$NoMobilityAsset", "liv_asset$NolivAsset")], 1, function(x) ifelse(all(x == "Yes"), 1, 0))

rm(FINAL_merged5)
FINAL_merged5 <- cbind(FINAL_merged4, 
                       APIS_PUF_2020_RTI_Housing$I7, 
                       APIS_PUF_2020_RTI_Housing$I2, 
                       APIS_PUF_2020_RTI_Housing$I3, 
                       ASSETS$DeprivedHH)
#rename columns
colnames(FINAL_merged5)[12] <- "If there is no electricity in the house"

colnames(FINAL_merged5)[13] <- "If roof wall is not made of predominantly strong materials"

colnames(FINAL_merged5)[14] <- "If outer wall is not made of predominantly strong materials"

colnames(FINAL_merged5)[15] <- "If household does not have at least one communication asset (amongst
phone, tv, radio, or PC) AND at least one mobility asset (amongst car,
motorcycle or motorboat) OR at least one livelihood asset (amongst
agricultural land, livestock, refrigerator/freezer)"

#if else condition cut-offs to represent the deprived in column 5 & 6
FINAL_merged5[, 12]<- ifelse(FINAL_merged5[, 12] == 1, 0, 1)
#FINAL_merged5[, 13] <- ifelse(FINAL_merged5[, 13] %in% c(5, 7), 1, 0)

FINAL_merged5$`If roof wall is not made of predominantly strong materials` <- ifelse(FINAL_merged5$`If roof wall is not made of predominantly strong materials` %in% c(5, 7), 1, 0)

FINAL_merged5$`If outer wall is not made of predominantly strong materials` <- ifelse(FINAL_merged5$`If outer wall is not made of predominantly strong materials` %in% c(5, 8, 9, 10), 1, 0)



################################### LAST INDICATOR LET'S GOOOOO TAPOS IYAK ########################################################################

#Looking_for_Work <- APIS_PUF_2020_Member %>% 
  #select(REG, HHID, C1011, C05_AGE, C08_CURSCH, C08B_NOT_ATTEND)

Looking_for_Work <- APIS_PUF_HOUSEHOLD2 %>% 
  select(REG, HHID, WRK_18, WRK_5_17)



                  




########################################### COMBINE FINAL INDICATORS ##########################################################################
FINAL_MPI_Indicators <- cbind(FINAL_merged5, APIS_PUF_HOUSEHOLD2$WRK_18, APIS_PUF_HOUSEHOLD2$WRK_5_17 )                                                                                                               

#rename columns
colnames(FINAL_MPI_Indicators)[16] <- "If less than 50% of working age members who are not students
worked over the past 6 months"

colnames(FINAL_MPI_Indicators)[17] <- "If less than 50% of working age members who are not students
worked over the past 6 months_2"

#rm(FINAL_MPI_Indicators)

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


# Calculate the sum for the selected columns
FINAL_MPI_Indicators$MPIsum_columns <- rowSums(FINAL_MPI_Indicators[columns], na.rm = TRUE)




# Calculate the percentage values
FINAL_MPI_Indicators$MPIsum_columns <- (FINAL_MPI_Indicators$MPIsum_columns) * 100

NODEPRIVATIONS <- FINAL_MPI_Indicators %>%
  filter(MPIsum_columns == 0)
#3028

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
# 2, 749

### NUMBER OF HOUSEHOLDS IN TOTAL FROM THE DATA FRAME ###
nrow(FINAL_MPI_Indicators)
# [1] 41,839

#TOTAL_HH <- 41838
#### CALCULATE THE HEADCOUNT RATIO (H) #####
#Headcount_Ratio <- print(total_deprived_households/TOTAL_HH)

Headcount_Ratio <- print(total_deprived_households/nrow(FINAL_MPI_Indicators))
# 0.07205434 (2017)
# 0.01943229 (2019)
# 0.01720882 (2020)




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
# 84619.4 (2020)


# Calculate the average
Intensity <- print(MPIsumproduct/Fsize)
# the intensity of the poor is 38.7955 (2017)
# the intensity of the poor is 35.5237 (2019)
# the intensity of the poor is 37.31014 (2020)

### FINAL MPI RESULTS ###
MPI_ESTIMATE <- print(Headcount_Ratio*Intensity)
# 2.763584 -> .02763584 (2017)
# 0.690307 -> .00690307 (2019)
# 0.6420637 -> .006420637 (2020)

head(FINAL_MPI_Indicators$MPIsum_columns)



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
sum_of_EDU1 <- sum(EDUCATION_1$EDUproduct, na.rm = TRUE) #2.416312
sum_of_EDU2 <- sum(EDUCATION_2$EDUproduct, na.rm = TRUE) #1.549395
sum_of_EDU3 <- sum(EDUCATION_3$EDUproduct, na.rm = TRUE) #3.351714
sumoffamilysize <-sum(APIS_PUF_HOUSEHOLD2$FSIZE)

educationshare <- ((sum_of_EDU3)/sumoffamilysize)/.02183694#.02526681
print(educationshare)
# 17.76611

############################################################################################################################
# Assuming your data frame is called 'df'
HEALTH_1 <- deprived_households %>% 
  select(`Number of Households' Size`, `If per capita food consumption < 4/5 of food poverty line`)

HEALTH_1 <- HEALTH_1 %>%
  filter(`If per capita food consumption < 4/5 of food poverty line` != 0)

HEALTH_1$HEALTHproduct <- HEALTH_1$`Number of Households' Size` * HEALTH_1$`If per capita food consumption < 4/5 of food poverty line`*100

###########################################################################################################################
HEALTH_2 <- deprived_households %>% 
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

healthshare <- ((sum_of_H1 + sum_of_H2)/sumoffamilysize)/.02183694 #.02526681
print(healthshare)
# second attempt 45.04305

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

livingstandardshare <- ((sum_of_L1 + sum_of_L2 + sum_of_L3 + sum_of_L4 + sum_of_L5 + sum_of_L6 + sum_of_L7 + sum_of_L8)/sumoffamilysize)/.02183694
print(livingstandardshare)
#24.93365




################################################# SHARE OF DEPRIVATION ##########################################################
# education 1 = 365
# education 2 = 173

# Health 1 = 695
# Health 2 = 374

# Liv 1 = 132
# Liv 2 = 252
# Liv 3 = 341
# Liv 4 = 224
# Liv 5 = 435
# Liv 6 = 184
# Liv 7 = 901
# Liv 8 = 166


################################################### RENAMING OF REGIONS ###################################################################################

# Assuming you have a data frame named 'df' with a column named 'region_id'

library(dplyr)
install.packages()

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


AGGREGATED_REGION <- FINAL_MPI_Indicators %>%
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

TOTALREGION_I <- AGGREGATED_REGION %>%
  filter(REG == "Region I - Ilocos Region")

#headcount ration for REGION 1
REG1_Headcount_Ratio <- print(nrow(REGION_I)/nrow(TOTALREGION_I))

#intensity for Region 1
REG1_MPIsumproduct <- sum(REGION_I$MPIsumproduct, na.rm = TRUE)

REG1_Fsize <- sum(REGION_I$`Number of Households' Size`, na.rm = TRUE)

REG1_Intensity <- print(REG1_MPIsumproduct/REG1_Fsize)


#MPI for Region 1
REG1_MPI_ESTIMATE <- print(REG1_Headcount_Ratio*REG1_Intensity)
#1.042142

#####################################################################################################################################
REGION_II <- FINALDEP %>%
  filter(REG == "Region II - Cagayan Valley")

TOTALREGION_II <- AGGREGATED_REGION %>%
  filter(REG == "Region II - Cagayan Valley")

#headcount ration for REGION 2
REG2_Headcount_Ratio <- print(nrow(REGION_II)/nrow(TOTALREGION_II))

#intensity for Region 2
REG2_MPIsumproduct <- sum(REGION_II$MPIsumproduct, na.rm = TRUE)

REG2_Fsize <- sum(REGION_II$`Number of Households' Size`, na.rm = TRUE)

REG2_Intensity <- print(REG2_MPIsumproduct/REG2_Fsize)


#MPI for Region 2
REG2_MPI_ESTIMATE <- print(REG2_Headcount_Ratio*REG2_Intensity)
#1.490459

#####################################################################################################################################

REGION_III <- FINALDEP %>%
  filter(REG == "Region III - Central Luzon")

TOTALREGION_III <- AGGREGATED_REGION %>%
  filter(REG == "Region III - Central Luzon")

#headcount ration for REGION 2
REG3_Headcount_Ratio <- print(nrow(REGION_III)/nrow(TOTALREGION_III))

#intensity for Region 2
REG3_MPIsumproduct <- sum(REGION_III$MPIsumproduct, na.rm = TRUE)

REG3_Fsize <- sum(REGION_III$`Number of Households' Size`, na.rm = TRUE)

REG3_Intensity <- print(REG3_MPIsumproduct/REG3_Fsize)


#MPI for Region 2
REG3_MPI_ESTIMATE <- print(REG3_Headcount_Ratio*REG3_Intensity)
#1.142412

#####################################################################################################################################


REGION_IV_A <- FINALDEP %>%
  filter(REG == "Region IV-A Calabarzon")

TOTALREGION_IV_A <- AGGREGATED_REGION %>%
  filter(REG == "Region IV-A Calabarzon")

REG4_Headcount_Ratio <- print(nrow(REGION_IV_A)/nrow(TOTALREGION_IV_A))

#intensity for Region 2
REG4_MPIsumproduct <- sum(REGION_IV_A$MPIsumproduct, na.rm = TRUE)

REG4_Fsize <- sum(REGION_IV_A$`Number of Households' Size`, na.rm = TRUE)

REG4_Intensity <- print(REG4_MPIsumproduct/REG4_Fsize)


#MPI for Region 2
REG4_MPI_ESTIMATE <- print(REG4_Headcount_Ratio*REG4_Intensity)
#1.24608

####################################################################################################################################
REGION_V <- FINALDEP %>%
  filter(REG == "Region V - Bicol")

TOTALREGION_V <- AGGREGATED_REGION %>%
  filter(REG == "Region V - Bicol")

REG5_Headcount_Ratio <- print(nrow(REGION_V)/nrow(TOTALREGION_V))

#intensity for Region 2
REG5_MPIsumproduct <- sum(REGION_V$MPIsumproduct, na.rm = TRUE)

REG5_Fsize <- sum(REGION_V$`Number of Households' Size`, na.rm = TRUE)

REG5_Intensity <- print(REG5_MPIsumproduct/REG5_Fsize)


#MPI for Region 2
REG5_MPI_ESTIMATE <- print(REG5_Headcount_Ratio*REG5_Intensity)
#3.25594


#################################################################################################################################

REGION_VI <- FINALDEP %>%
  filter(REG == "Region VI - Western Visayas")

TOTALREGION_VI <- AGGREGATED_REGION %>%
  filter(REG == "Region VI - Western Visayas")

REG6_Headcount_Ratio <- print(nrow(REGION_VI)/nrow(TOTALREGION_VI))

#intensity for Region 2
REG6_MPIsumproduct <- sum(REGION_VI$MPIsumproduct, na.rm = TRUE)

REG6_Fsize <- sum(REGION_VI$`Number of Households' Size`, na.rm = TRUE)

REG6_Intensity <- print(REG6_MPIsumproduct/REG6_Fsize)


#MPI for Region 2
REG6_MPI_ESTIMATE <- print(REG6_Headcount_Ratio*REG6_Intensity)
#2.348481


#############################################################################################################################

REGION_VII <- FINALDEP %>%
  filter(REG == "Region VII - Central Visayas")

TOTALREGION_VII <- AGGREGATED_REGION %>%
  filter(REG == "Region VII - Central Visayas")

REG7_Headcount_Ratio <- print(nrow(REGION_VII)/nrow(TOTALREGION_VII))

#intensity for Region 2
REG7_MPIsumproduct <- sum(REGION_VII$MPIsumproduct, na.rm = TRUE)

REG7_Fsize <- sum(REGION_VII$`Number of Households' Size`, na.rm = TRUE)

REG7_Intensity <- print(REG7_MPIsumproduct/REG7_Fsize)


#MPI for Region 2
REG7_MPI_ESTIMATE <- print(REG7_Headcount_Ratio*REG7_Intensity)
#2.029202
############################################################################################################################

REGION_VIII <- FINALDEP %>%
  filter(REG == "Region VIII - Eastern Visayas")

TOTALREGION_VIII <- AGGREGATED_REGION %>%
  filter(REG == "Region VIII - Eastern Visayas")

REG8_Headcount_Ratio <- print(nrow(REGION_VIII)/nrow(TOTALREGION_VIII))

#intensity for Region 2
REG8_MPIsumproduct <- sum(REGION_VIII$MPIsumproduct, na.rm = TRUE)

REG8_Fsize <- sum(REGION_VIII$`Number of Households' Size`, na.rm = TRUE)

REG8_Intensity <- print(REG8_MPIsumproduct/REG8_Fsize)


#MPI for Region 2
REG8_MPI_ESTIMATE <- print(REG8_Headcount_Ratio*REG8_Intensity)
#2.668661

###########################################################################################################################
REGION_IX <- FINALDEP %>%
  filter(REG == "Region IX - Zamboanga Peninsula")

TOTALREGION_IX <- AGGREGATED_REGION %>%
  filter(REG == "Region IX - Zamboanga Peninsula")

REG9_Headcount_Ratio <- print(nrow(REGION_IX)/nrow(TOTALREGION_IX))

#intensity for Region 2
REG9_MPIsumproduct <- sum(REGION_IX$MPIsumproduct, na.rm = TRUE)

REG9_Fsize <- sum(REGION_IX$`Number of Households' Size`, na.rm = TRUE)

REG9_Intensity <- print(REG9_MPIsumproduct/REG9_Fsize)


#MPI for Region 2
REG9_MPI_ESTIMATE <- print(REG9_Headcount_Ratio*REG9_Intensity)
#4.186576
##########################################################################################################################

REGION_X <- FINALDEP %>%
  filter(REG == "Region X - Northern Mindanao")

TOTALREGION_X <- AGGREGATED_REGION %>%
  filter(REG == "Region X - Northern Mindanao")

REG10_Headcount_Ratio <- print(nrow(REGION_X)/nrow(TOTALREGION_X))

#intensity for Region 2
REG10_MPIsumproduct <- sum(REGION_X$MPIsumproduct, na.rm = TRUE)

REG10_Fsize <- sum(REGION_X$`Number of Households' Size`, na.rm = TRUE)

REG10_Intensity <- print(REG10_MPIsumproduct/REG10_Fsize)


#MPI for Region 2
REG10_MPI_ESTIMATE <- print(REG10_Headcount_Ratio*REG10_Intensity)
#2.446222


#########################################################################################################################
REGION_XI <- FINALDEP %>%
  filter(REG == "Region XI - Davao")

TOTALREGION_XI <- AGGREGATED_REGION %>%
  filter(REG == "Region XI - Davao")

REG11_Headcount_Ratio <- print(nrow(REGION_XI)/nrow(TOTALREGION_XI))

#intensity for Region 2
REG11_MPIsumproduct <- sum(REGION_XI$MPIsumproduct, na.rm = TRUE)

REG11_Fsize <- sum(REGION_XI$`Number of Households' Size`, na.rm = TRUE)

REG11_Intensity <- print(REG11_MPIsumproduct/REG11_Fsize)


#MPI for Region 2
REG11_MPI_ESTIMATE <- print(REG11_Headcount_Ratio*REG11_Intensity)
#3.767365


########################################################################################################################
REGION_XII <- FINALDEP %>%
  filter(REG == "Region XII - SOCCKSARGEN")

TOTALREGION_XII <- AGGREGATED_REGION %>%
  filter(REG == "Region XII - SOCCKSARGEN")

REG12_Headcount_Ratio <- print(nrow(REGION_XII)/nrow(TOTALREGION_XII))

#intensity for Region 2
REG12_MPIsumproduct <- sum(REGION_XII$MPIsumproduct, na.rm = TRUE)

REG12_Fsize <- sum(REGION_XII$`Number of Households' Size`, na.rm = TRUE)

REG12_Intensity <- print(REG12_MPIsumproduct/REG12_Fsize)


#MPI for Region 2
REG12_MPI_ESTIMATE <- print(REG12_Headcount_Ratio*REG12_Intensity)
#3.492622

##########################################################################################################################
NCR <- FINALDEP %>%
  filter(REG == "National Capital Region")

TOTALNCR <- AGGREGATED_REGION %>%
  filter(REG == "National Capital Region")

NCR_Headcount_Ratio <- print(nrow(NCR)/nrow(TOTALNCR))

#intensity for Region 2
NCR_MPIsumproduct <- sum(NCR$MPIsumproduct, na.rm = TRUE)

NCR_Fsize <- sum(NCR$`Number of Households' Size`, na.rm = TRUE)

NCR_Intensity <- print(NCR_MPIsumproduct/NCR_Fsize)


#MPI for Region 2
NCR_MPI_ESTIMATE <- print(NCR_Headcount_Ratio*NCR_Intensity)
#0.2162321

#########################################################################################################################
CAR <- FINALDEP %>%
  filter(REG == "Cordillera Administrative Region")

TOTALCAR <- AGGREGATED_REGION %>%
  filter(REG == "Cordillera Administrative Region")

CAR_Headcount_Ratio <- print(nrow(CAR)/nrow(TOTALCAR))

#intensity for Region 2
CAR_MPIsumproduct <- sum(CAR$MPIsumproduct, na.rm = TRUE)

CAR_Fsize <- sum(CAR$`Number of Households' Size`, na.rm = TRUE)

CAR_Intensity <- print(CAR_MPIsumproduct/CAR_Fsize)


#MPI for Region 2
CAR_MPI_ESTIMATE <- print(CAR_Headcount_Ratio*CAR_Intensity)
#1.06545

########################################################################################################################
BARMM <- FINALDEP %>%
  filter(REG == "Bangsamoro Autonomous Region in Muslim Mindanao")

TOTALBARMM <- AGGREGATED_REGION %>%
  filter(REG == "Bangsamoro Autonomous Region in Muslim Mindanao")

BARMM_Headcount_Ratio <- print(nrow(BARMM)/nrow(TOTALBARMM))

#intensity for Region 2
BARMM_MPIsumproduct <- sum(BARMM$MPIsumproduct, na.rm = TRUE)

BARMM_Fsize <- sum(BARMM$`Number of Households' Size`, na.rm = TRUE)

BARMM_Intensity <- print(BARMM_MPIsumproduct/BARMM_Fsize)


#MPI for Region 2
BARMM_MPI_ESTIMATE <- print(BARMM_Headcount_Ratio*BARMM_Intensity)
#5.287804


#######################################################################################################################
CARAGA <- FINALDEP %>%
  filter(REG == "Region XIII - Caraga")

TOTALCARAGA <- AGGREGATED_REGION %>%
  filter(REG == "Region XIII - Caraga")

CARAGA_Headcount_Ratio <- print(nrow(CARAGA)/nrow(TOTALCARAGA))

#intensity for Region 2
CARAGA_MPIsumproduct <- sum(CARAGA$MPIsumproduct, na.rm = TRUE)

CARAGA_Fsize <- sum(CARAGA$`Number of Households' Size`, na.rm = TRUE)

CARAGA_Intensity <- print(CARAGA_MPIsumproduct/CARAGA_Fsize)


#MPI for Region 2
CARAGA_MPI_ESTIMATE <- print(CARAGA_Headcount_Ratio*CARAGA_Intensity)
#3.783647

#######################################################################################################################

MIMAROPA <- FINALDEP %>%
  filter(REG == "Region IV-B MIMAROPA")

TOTALMIMAROPA <- AGGREGATED_REGION %>%
  filter(REG == "Region IV-B MIMAROPA")


MIMAROPA_Headcount_Ratio <- print(nrow(MIMAROPA)/nrow(TOTALMIMAROPA))

#intensity for Region 2
MIMAROPA_MPIsumproduct <- sum(MIMAROPA$MPIsumproduct, na.rm = TRUE)

MIMAROPA_Fsize <- sum(MIMAROPA$`Number of Households' Size`, na.rm = TRUE)

MIMAROPA_Intensity <- print(MIMAROPA_MPIsumproduct/MIMAROPA_Fsize)


#MPI for Region 2
MIMAROPA_MPI_ESTIMATE <- print(MIMAROPA_Headcount_Ratio*MIMAROPA_Intensity)
#2.252592


###########################################################################################
#Region 1 = 10
#Region 2 = 40
#Region 3 = 48
#REgion 4-A =  23
#REgion 5 =  101
#Region 6 =  79
#Region 7 = 63
#Region 8 = 101
#REgion 9 = 83
#Region 10 = 86
#Region 11 = 117
#Region 12 = 117
#CARAGA = 96
#NCR = 9
#CAR = 29
#BARMM = 108
#CARAGA = 96
#MIMAROPA = 76











############################################################## ROBUST CHECK USING BETA 1&2 USING NESTED WEIGHTS ############################################
UNION_MPI <- cbind(FINAL_merged5, APIS_PUF_HOUSEHOLD2$WRK_18, APIS_PUF_HOUSEHOLD2$WRK_5_17 ) 



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

#mean(UNION_MPI$PovertyMeasureBeta2)
####################################################################################################################################

# Assuming your dataset is named 'data'

# Calculate the headcount ratio for each beta value
headcount_ratio_beta1 <- sum(UNION_MPI$PovertyMeasureBeta1) / nrow(UNION_MPI)
print(headcount_ratio_beta1)

headcount_ratio_beta2 <- sum(UNION_MPI$PovertyMeasureBeta2) / nrow(UNION_MPI)


# Calculate the intensity for each beta value
intensity_beta1 <- sum(UNION_MPI$PovertyMeasureBeta1) / sum(UNION_MPI$PovertyMeasureBeta1[UNION_MPI$PovertyMeasureBeta1 > 0])
print(intensity_beta1)

intensity_beta2 <- sum(UNION_MPI$PovertyMeasureBeta2) / sum(UNION_MPI$PovertyMeasureBeta2[UNION_MPI$PovertyMeasureBeta2 > 0])
print(intensity_beta2)

# Calculate the MPI estimate for each beta value
MPI_estimate_beta1 <- headcount_ratio_beta1 * intensity_beta1
print(MPI_estimate_beta1)

MPI_estimate_beta2 <- headcount_ratio_beta2 * intensity_beta2
print(MPI_estimate_beta2)


### OR JUST PRINT THE MEAN
mean(UNION_MPI$PovertyMeasureBeta1)
#[1] 0.00480413 -> 0.004
mean(UNION_MPI$PovertyMeasureBeta2)
#[1] 0.00960826 -> 0.010




####################################

################################### Get the INVERSE INCIDENCE WEIGHTS ###########################################################################################

# Step 3: Calculate incidence of deprivation for each binary indicator
#incidence <- sapply(deprived_households[columns], function(indicator) {
# Calculate the incidence of deprivation for each binary indicator
#mean(indicator, na.rm = TRUE)})











############### THIS IS USING INVERSE INCIDENCE WEIGHTS BUT SAME CUTOFF ########
UNION_MPI <- cbind(FINAL_merged5, APIS_PUF_HOUSEHOLD2$WRK_18, APIS_PUF_HOUSEHOLD2$WRK_5_17 ) 

# how to get the Nested Inverse Incidence Weights
incidence <- colMeans(UNION_MPI[, 5:17], na.rm = TRUE)

print(incidence)

inverse_incidence <- 1 / incidence

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
# 1,197

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

educationshare <- ((sum_of_EDU1 + sum_of_EDU2 + sum_of_EDU3)/sumoffamilysize)/.0116324#.02526681
print(educationshare)
# 45.1711

############################################################################################################################
# Assuming your data frame is called 'df'
HEALTH_1 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, `If per capita food consumption < 4/5 of food poverty line`)

HEALTH_1 <- HEALTH_1 %>%
  filter(`If per capita food consumption < 4/5 of food poverty line` != 0)

HEALTH_1$HEALTHproduct <- HEALTH_1$`Number of Households' Size` * HEALTH_1$`If per capita food consumption < 4/5 of food poverty line`*100

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

healthshare <- ((sum_of_H1 + sum_of_H2)/sumoffamilysize)/.0116324 #.02526681
print(healthshare)
# 4.372668

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

livingstandardshare <- ((sum_of_L1 + sum_of_L2 + sum_of_L3 + sum_of_L4 + sum_of_L5 + sum_of_L6 + sum_of_L7 + sum_of_L8)/sumoffamilysize)/.0116324
print(livingstandardshare)
#31.87075
