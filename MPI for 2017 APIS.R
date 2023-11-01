library(readxl)
APIS_PUF_2017_Person <- read_excel("~/THESIS/APIS MICRODATA/APIS 2018 EXCEL FILES/APIS PUF 2017 Person.xlsx")
View(APIS_PUF_2017_Person)

library(data.table)
install.packages("tidyverse")
library(tidyverse)

# Grouping consecutive numbers in C101_LNO column
runs2 <- rle(APIS_PUF_2017_Person$C01_LNO)
APIS_PUF_2017_Person$C101 <- rep(seq_along(runs2$values), times = runs2$lengths)
APIS_PUF_2017_Person$C1011 <- cumsum(APIS_PUF_2017_Person$C01_LNO == 1)

#aggregate new table with households
count_households <- APIS_PUF_2017_Person %>%
  select(REGO, ID, C12_HGC, C1011)
View(count_households)

#aggregate data for C1011
C1011_aggregated_data <- APIS_PUF_2017_Person %>% 
  group_by(REGO,ID,C1011) %>% 
  summarize("Number of Households' Size" =n())
print(C1011_aggregated_data)


#assign codes for primary education
completed_codes <- c("10", "10", "110", "120", "130", "140")
completed_codes <- as.integer(completed_codes)

#when TRUE = no member of the household did not finish at least 6 years
#when FALSE = all members of the household finished at least 6 years
check_condition_C1011 <- APIS_PUF_2017_Person %>%
  group_by(REGO,ID,C1011) %>%
  summarize(HasNotCompleted6Years = all((C12_HGC %in% completed_codes)))


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
APIS_PUF_2017_Household <- read_excel("~/THESIS/APIS MICRODATA/APIS 2018 EXCEL FILES/APIS PUF 2017 Household.xlsx")
View(APIS_PUF_2017_Household)

#### aggregate those families that have children but not attending school #########

#### FOR 6:11
Family_6_11_noeduc <- APIS_PUF_2017_Household %>%
select(REGO, FSIZE, PUFCHLD_6_11, PUFEDUC_6_11)

Family_6_11_noeduc$NotAttending6_11 <- with(Family_6_11_noeduc, PUFCHLD_6_11 == 1 & PUFEDUC_6_11 == 0)
Family_6_11_noeduc$NotAttending6_11 <- ifelse(with(Family_6_11_noeduc, PUFCHLD_6_11 == 1 & PUFEDUC_6_11 == 0), 1, 0)


### FOR 12:15
Family_12_15_noeduc <- APIS_PUF_2017_Household %>%
  select(REGO, FSIZE, PUFCHLD_12_15, PUFEDUC_12_15)

Family_12_15_noeduc$NotAttending12_15 <- with(Family_12_15_noeduc, PUFCHLD_12_15 == 1 & PUFEDUC_12_15 == 0)
Family_12_15_noeduc$NotAttending12_15 <- ifelse(with(Family_12_15_noeduc, PUFCHLD_12_15 == 1 & PUFEDUC_12_15 == 0), 1, 0)

####Merge columns ####
FINAL_merged <- cbind(combined_HH_NotCompleted6Years_C1011,
                      Family_6_11_noeduc$NotAttending6_11,
                      Family_12_15_noeduc$NotAttending12_15)

# if ever you're gonna take out one of the columns use this code
#FINAL_merged <- FINAL_merged %>%
  #select(-'Number.of.Households..Size', -'No.Member.of.the.Household.has.Completed.6.Years.of.Primary.Schooling')

# Rename the FINAL_merged
colnames(FINAL_merged)[6] <- "A school-age child (up to grade 10, i.e. between age 6-11) is not
attending school"

colnames(FINAL_merged)[7] <- "A School-age child (up to grade 10, i.e. between age 12-15) is not
Attending School"


########################################## ANOTHER INDICATOR #######################################################################
APIS_PUF_2017_Household <- read_excel("~/THESIS/APIS MICRODATA/APIS 2018 EXCEL FILES/APIS PUF 2017 Household.xlsx")
View(APIS_PUF_2017_Household)

## aggregate those who are below the poverty threshold with a family of 5
Below_foodpov_thresh1 <- APIS_PUF_2017_Household %>% 
  select(MON_FOOD)

Below_foodpov_thresh2 <- APIS_PUF_2017_Household %>% 
  select(REGO, FSIZE)


Below_foodpov_thresh2 <- Below_foodpov_thresh2 %>%
  mutate(REGO = recode(REGO,
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
  ~REGO, ~Food_Poverty_Threshold,
  "Region I - Ilocos Region", 1346.35,
  "Region II - Cagayan Valley", 1286.17,
  "Region III - Central Luzon", 1345.41,
  "Region IV-A Calabarzon", 1493.70,
  "Region V - Bicol", 1259.25,
  "Region VI - Western Visayas", 1271.21,
  "Region VII - Central Visayas", 1323.24,
  "Region VIII - Eastern Visayas", 1313.32,
  "Region IX - Zamboanga Peninsula",1302.98,
  "Region X - Northern Mindanao",1294.60,
  "Region XI - Davao", 1398.99,
  "Region XII - SOCCKSARGEN",1296.71,
  "National Capital Region", 1481.64,
  "Cordillera Administrative Region", 1316.96,
  "Bangsamoro Autonomous Region in Muslim Mindanao",1512.11,
  "Region XIII - Caraga", 1336.07,
  "Region IV-B MIMAROPA", 1218.66,
)

Below_foodpov_thresh2 <- Below_foodpov_thresh2 %>%
  left_join(Foodpovthreshold, by = "REGO")


Below_foodpov_thresh2 <- Below_foodpov_thresh2 %>%
  mutate(Fsize_PovThreshold = FSIZE * Food_Poverty_Threshold)

FINAL_below_foodpov <- cbind(Below_foodpov_thresh2, Below_foodpov_thresh1)

FINAL_below_foodpov <- FINAL_below_foodpov %>%
  mutate(Deprived = if_else(MON_FOOD < Fsize_PovThreshold, 1, 0))



####Merge columns ####
FINAL_merged2 <- cbind(FINAL_merged, FINAL_below_foodpov$Deprived)

# Rename the FINAL_merged
colnames(FINAL_merged2)[8] <- "If per capita food consumption < 4/5 of food poverty line monthly"



#bind the new data frame
FINAL_merged3 <- cbind(FINAL_merged2, APIS_PUF_2017_Household$PUFGQ4)

# Rename the FINAL_merged3
colnames(FINAL_merged3)[9] <- "If a family member has experience hunger"
FINAL_merged3[, 9] <- ifelse(FINAL_merged3[, 9] == 1, 1, 0)



######################### ANOTHER INDICATOR ###################################################
FINAL_merged4 <- cbind(FINAL_merged3, APIS_PUF_2017_Household$PUFWSQ14, APIS_PUF_2017_Household$PUFFQ1)


FINAL_merged4[, 10] <- ifelse(FINAL_merged4[, 10] == 3, 1, 0)

FINAL_merged4$...11 <- ifelse(FINAL_merged4$...11 %in% c(5, 7, 8, 9), 1, 0)


# Rename the FINAL_merged
colnames(FINAL_merged4)[10] <- "If household does not use own flush or closed pit toilet"

colnames(FINAL_merged4)[11] <- "If household's main source of water is not piped water or protected well"


################################## ANOTHER INDICATOR ########################################
FINAL_merged5 <- cbind(FINAL_merged4, APIS_PUF_2017_Household$PUFEQ5, APIS_PUF_2017_Household$PUFEQ2, APIS_PUF_2017_Household$PUFEQ3)


####COM ASSET######
com_asset <- APIS_PUF_2017_Household %>%
  select(ID,PUFEQ6I, PUFEQ6N, PUFEQ6O, PUFEQ6H)

# Create a new column with default value as FALSE
com_asset$NoCommunicationAsset <- FALSE

com_asset <- com_asset %>%
select(-NoCommunicationAsset)

com_asset$NoComAsset <- rowSums(com_asset[, c("PUFEQ6I", "PUFEQ6N", "PUFEQ6O", "PUFEQ6H")] == 0) == ncol(com_asset[, c("PUFEQ6I", "PUFEQ6N", "PUFEQ6O", "PUFEQ6H")])

com_asset$NoComAsset <- ifelse(com_asset$NoComAsset, 1, 0)


####MOB ASSET#####
mob_asset <- APIS_PUF_2017_Household %>%
  select(ID,PUFEQ6A, PUFEQ6B, PUFEQ6C)

# Create a new column with default value as FALSE
mob_asset$NoMobAsset <- FALSE

mob_asset$NoMobAsset <- rowSums(mob_asset[, c("PUFEQ6A", "PUFEQ6B", "PUFEQ6C")] == 0) == ncol(mob_asset[, c("PUFEQ6A", "PUFEQ6B", "PUFEQ6C")])

mob_asset$NoMobAsset <- ifelse(mob_asset$NoMobAsset, 1, 0)



####LIV ASSET####
liv_asset <- APIS_PUF_2017_Household %>%
  select(ID,PUFEQ6G)

# Create a new column with default value as FALSE
liv_asset$NoLivAsset <- FALSE

liv_asset$NoLivAsset <- rowSums(liv_asset[, c("PUFEQ6G")] == 0) == ncol(liv_asset[, c("PUFEQ6G")])

liv_asset$NoLivAsset <- ifelse(liv_asset$NoLivAsset, 1, 0)

#################################################################################################
com_asset <- com_asset %>% 
  select(ID,NoComAsset)

mob_asset <- mob_asset %>% 
  select(ID,NoMobAsset)

liv_asset <- liv_asset %>% 
  select(ID, NoLivAsset)

### MERGING THE THE ASSETS ALTOGETHER #################
ASSETS <- cbind(com_asset,mob_asset$NoMobAsset, liv_asset$NoLivAsset)

### YES = WALA NO = MERON ###
# Create a new column with default value as 0
ASSETS$DeprivedHH <- 0

#head(ASSETS)
# Check if all the asset columns contain "Yes"
ASSETS$DeprivedHH <- apply(ASSETS[, c("NoComAsset", "mob_asset$NoMobAsset", "liv_asset$NoLivAsset")], 1, function(x) ifelse(all(x == 0), 1, 0))

#remove the ID
ASSETS <- ASSETS %>% 
  select(DeprivedHH)

#MERGE THE ASSETS
FINAL_merged5 <- cbind(FINAL_merged5, ASSETS)


#if else condition cut-offs to represent the deprived in column 5 & 6
FINAL_merged5[, 12] <- ifelse(FINAL_merged5[, 12] == 1, 0, 1)

FINAL_merged5$...13 <- ifelse(FINAL_merged5$...13 %in% c(3, 5, 6, 7), 1, 0)

FINAL_merged5$...14 <- ifelse(FINAL_merged5$...14 %in% c(3,5,6,7), 1, 0)

#rename columns
colnames(FINAL_merged5)[12] <- "If there is no electricity in the house"

colnames(FINAL_merged5)[13] <- "If roof wall is not made of predominantly strong materials"

colnames(FINAL_merged5)[14] <- "If outer wall is not made of predominantly strong materials"

colnames(FINAL_merged5)[15] <- "If household does not have at least one communication asset (amongst
phone, tv, radio, or PC) AND at least one mobility asset (amongst car,
motorcycle or motorboat) OR at least one livelihood asset (amongst
agricultural land, livestock, refrigerator/freezer)"


###################################### LAST INDICATOR ON EMPLOYMENT ############################
Looking_for_Work <- APIS_PUF_2017_Person %>% 
  select(REGO, ID, C1011, C05_AGE, C08_CUR_ATTEND, C10_YNOT_ATTND)

#aggregated data for those who are sick in the household
Looking_for_Work$WorkingAgeStudents_NotinSchool_Employed <- with(Looking_for_Work, C05_AGE >= 18 & C08_CUR_ATTEND == 3 & C10_YNOT_ATTND == 5)


Looking_for_Work <- Looking_for_Work %>% 
  select(REGO, ID, C1011, WorkingAgeStudents_NotinSchool_Employed)

### replacing values of NAs to FALSE
Looking_for_Work$WorkingAgeStudents_NotinSchool_Employed <- ifelse(is.na(Looking_for_Work$WorkingAgeStudents_NotinSchool_Employed), FALSE, Looking_for_Work$WorkingAgeStudents_NotinSchool_Employed)


library(dplyr)

Looking2 <- Looking_for_Work %>%
  group_by(REGO, ID, C1011) %>%
  summarise(
    total_members = n(),
    workingagenotstudents_members = sum(WorkingAgeStudents_NotinSchool_Employed == TRUE, na.rm = TRUE),
    less_than_50_percent = (workingagenotstudents_members / total_members) 
  )

Looking2$Verify <- ifelse(Looking2$less_than_50_percent == 0, FALSE, Looking2$less_than_50_percent < (0.5 * 10159))

Looking2$Verify <- as.integer(Looking2$Verify)

#Looking2$working <- with(Looking2, workingagenotstudents_members >= 1)
#Looking2$working <- ifelse(with(Looking2, working >= 1), 1, 0)

FINAL_MPI_Indicators <- cbind(FINAL_merged5, Looking2$Verify)

#rename columns
colnames(FINAL_MPI_Indicators)[16] <- "If less than 50% of working age members who are not students
worked over the past 6 months"


FINAL_MPI_Indicators <- cbind(FINAL_merged5, APIS_PUF_2017_Household$PUFWRK_5_17, APIS_PUF_2017_Household$PUFWRK_18)




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

# Define the column numbers for which you want to calculate the sum
columns <- c(5:17)

# Calculate the sum for the selected columns
FINAL_MPI_Indicators$MPIsum_columns <- rowSums(FINAL_MPI_Indicators[columns], na.rm = TRUE)

# Calculate the percentage values
FINAL_MPI_Indicators$MPIsum_columns <- (FINAL_MPI_Indicators$MPIsum_columns) * 100


###CALCULATING THE POVERTY THRESHOLD OF 33.33% ##################
# Assuming the data frame is named 'data' and the column is named 'column_name'
poverty_threshold <- 33.33  # Threshold for deprivation

# Identify households below the threshold
deprived_households <- FINAL_MPI_Indicators[FINAL_MPI_Indicators$MPIsum_columns >= poverty_threshold, ]

# Count the total number of deprived households
total_deprived_households <- nrow(deprived_households)

### NUMBER OF TOTAL DEPRIVED HOUSEHOLDS OR HOUSEHOLDS THAT ARE MULTIDIMENSIONALLY POOR ####
print(total_deprived_households)

### NUMBER OF HOUSEHOLDS IN TOTAL FROM THE DATA FRAME ###
nrow(FINAL_MPI_Indicators)
# 10159

#### CALCULATE THE HEADCOUNT RATIO (H) #####

Headcount_Ratio <- print(total_deprived_households/nrow(FINAL_MPI_Indicators))
# 0.07205434

#### CALCULATE THE INTENSITY OF POVERTY  #####
# Get the sum of the deprivation scores

#immultiply is the deprived vectors to the family size 
deprived_households$MPIsumproduct <- deprived_households$`Number of Households' Size` * deprived_households$MPIsum_columns

MPIsumproduct <- sum(deprived_households$MPIsumproduct, na.rm = TRUE)

Fsize <- sum(deprived_households$`Number of Households' Size`, na.rm = TRUE)

# Print the sum
print(MPIsumproduct)
#32716.1


# Calculate the average
Intensity <- print(MPIsumproduct/Fsize)
# the intensity of the poor is 39.0698

### FINAL MPI RESULTS ###
MPI_ESTIMATE <- print(Headcount_Ratio*Intensity)
#3.802218



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
sum_of_EDU1 <- sum(EDUCATION_1$EDUproduct) #0.152704
sum_of_EDU2 <- sum(EDUCATION_2$EDUproduct) #0.7146762
sum_of_EDU3 <- sum(EDUCATION_3$EDUproduct) #2.706915
sumoffamilysize <-sum(APIS_PUF_2019_Household$FSIZE)

educationshare <- ((sum_of_EDU1 + sum_of_EDU2 + sum_of_EDU3)/(nrow(APIS_PUF_2017_Person)))/.0189257 #.03802218
print(educationshare)
# 15.88561


############################################################################################################################
head(deprived_households)
# Assuming your data frame is called 'df'
HEALTH_1 <- deprived_households %>% 
  select(`Number of Households' Size`, "If per capita food consumption < 4/5 of food poverty line monthly")

HEALTH_1 <- HEALTH_1 %>%
  filter(across(2, ~. != 0))

HEALTH_1$HEALTHproduct <- HEALTH_1$`Number of Households' Size` * HEALTH_1$"If per capita food consumption < 4/5 of food poverty line monthly"*100

###########################################################################################################################
HEALTH_2 <- deprived_households %>% 
  select(`Number of Households' Size`, "If a family member has experience hunger")

HEALTH_2 <- HEALTH_2 %>%
  filter(across(2, ~. != 0))

HEALTH_2$HEALTHproduct <- HEALTH_2$`Number of Households' Size` * HEALTH_2$"If a family member has experience hunger"*100

# Calculate the sum of values in a specific column
sum_of_H1 <- sum(HEALTH_1$HEALTHproduct, na.rm = TRUE) #13.2089
sum_of_H2 <- sum(HEALTH_2$HEALTHproduct, na.rm = TRUE) #6.986209
sumoffamilysize <-sum(APIS_PUF_HOUSEHOLD2$FSIZE)

healthshare <- ((sum_of_H1 + sum_of_H2)/(nrow(APIS_PUF_2017_Person)))/.0189257 #.03802218 #.02997311 
print(healthshare)
# second attempt 76.8652


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
sum_of_L1 <- sum(LIV_1$LIVproduct) #1.260795
sum_of_L2 <- sum(LIV_2$LIVproduct) #1.813417
sum_of_L3 <- sum(LIV_3$LIVproduct) #2.181831
sum_of_L4 <- sum(LIV_4$LIVproduct) #0.1903473
sum_of_L5 <- sum(LIV_5$LIVproduct) #0.6570054
sum_of_L6 <- sum(LIV_6$LIVproduct) #0.1923941
sum_of_L7 <- sum(LIV_7$LIVproduct) #0.4871255 
sum_of_L8 <- sum(LIV_8$LIVproduct) #2.253467
sumoffamilysize <-sum(APIS_PUF_HOUSEHOLD2$FSIZE)

livingstandardshare <- ((sum_of_L1 + sum_of_L2 + sum_of_L3 + sum_of_L4 + sum_of_L5 + sum_of_L6 + sum_of_L7 + sum_of_L8)/(nrow(APIS_PUF_2017_Person)))/.0189257 #.03802218
print(livingstandardshare)
#33.89832


################################################# SHARE OF DEPRIVATION ##########################################################
# education 1 = 9
# education 2 = 18
# education 3 = 68

# Health 1 = 169
# Health 2 = 105

# Liv 1 = 54
# Liv 2 = 75
# Liv 3 = 93
# Liv 4 = 18
# Liv 5 = 57
# Liv 6 = 8
# Liv 7 = 37
# Liv 8 = 174


################################################### RENAMING OF REGIONS ###################################################################################

# Assuming you have a data frame named 'df' with a column named 'region_id'
install.packages("dplyr")
library(dplyr)

FINALDEP <- deprived_households %>%
  mutate(REGO = recode(REGO,
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
  filter(REGO == "Region I - Ilocos Region")

#####################################################################################################################################
REGION_II <- FINALDEP %>%
  filter(REGO == "Region II - Cagayan Valley")

REGION_III <- FINALDEP %>%
  filter(REGO == "Region III - Central Luzon")

REGION_IV_A <- FINALDEP %>%
  filter(REGO == "Region IV-A Calabarzon")

REGION_V <- FINALDEP %>%
  filter(REGO == "Region V - Bicol")

REGION_VI <- FINALDEP %>%
  filter(REGO == "Region VI - Western Visayas")

REGION_VII <- FINALDEP %>%
  filter(REGO == "Region VII - Central Visayas")

REGION_VIII <- FINALDEP %>%
  filter(REGO == "Region VIII - Eastern Visayas")

REGION_IX <- FINALDEP %>%
  filter(REGO == "Region IX - Zamboanga Peninsula")

REGION_X <- FINALDEP %>%
  filter(REGO == "Region X - Northern Mindanao")

REGION_XI <- FINALDEP %>%
  filter(REGO == "Region XI - Davao")

REGION_XII <- FINALDEP %>%
  filter(REGO == "Region XII - SOCCKSARGEN")

NCR <- FINALDEP %>%
  filter(REGO == "National Capital Region")

CAR <- FINALDEP %>%
  filter(REGO == "Cordillera Administrative Region")

BARMM <- FINALDEP %>%
  filter(REGO == "Bangsamoro Autonomous Region in Muslim Mindanao")

CARAGA <- FINALDEP %>%
  filter(REGO == "Region XIII - Caraga")

MIMAROPA <- FINALDEP %>%
  filter(REGO == "Region IV-B MIMAROPA")

###########################################################################################
#Region 1 = 4
#Region 2 = 3
#Region 3 = 18
#REgion 4-A = 0
#REgion 5 = 7
#Region 6 = 22
#Region 7 = 1
#Region 8 = 4
#REgion 9 = 14
#Region 10 = 24
#Region 11 = 26
#Region 12 = 25
#CARAGA = 8
#NCR = 6
#CAR = 4
#BARMM = 16
#CARAGA = 8
#MIMAROPA = 64


# how to get the Nested Inverse Incidence Weights
incidence <- colMeans(UNION_MPI[, 5:17], na.rm = TRUE)

print(incidence)

inverse_incidence <- 1 / (incidence + 0.001)

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



############################################################## ROBUST CHECK USING BETA 1&2 ############################################
UNION_MPI <- cbind(FINAL_merged5, APIS_PUF_2017_Household$PUFWRK_5_17, APIS_PUF_2017_Household$PUFWRK_18)


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
#[1] 0.008957575 -> 0.008
mean(UNION_MPI$PovertyMeasureBeta2)
#[1] 0.01791515 -> 0.017

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

educationshare <- ((sum_of_EDU1 + sum_of_EDU2 + sum_of_EDU3)/nrow(APIS_PUF_2017_Person))/.001870487 #.02526681
print(educationshare)
# 57.75275

############################################################################################################################
# Assuming your data frame is called 'df'
HEALTH_1 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, "If per capita food consumption < 4/5 of food poverty line monthly")

HEALTH_1 <- HEALTH_1 %>%
  filter(across(2, ~. != 0))

HEALTH_1$HEALTHproduct <- HEALTH_1$`Number of Households' Size` * HEALTH_1$"If per capita food consumption < 4/5 of food poverty line monthly"*100


###########################################################################################################################
HEALTH_2 <- UNION_deprived_households %>% 
  select(`Number of Households' Size`, "If a family member has experience hunger")

HEALTH_2 <- HEALTH_2 %>%
  filter(across(2, ~. != 0))

HEALTH_2$HEALTHproduct <- HEALTH_2$`Number of Households' Size` * HEALTH_2$"If a family member has experience hunger"*100

###########################################################################################################################
# Calculate the sum of values in a specific column
sum_of_H1 <- sum(HEALTH_1$HEALTHproduct) #11.45958
sum_of_H2 <- sum(HEALTH_2$HEALTHproduct) #3.848759
sumoffamilysize <-sum(APIS_PUF_HOUSEHOLD2$FSIZE)

healthshare <- ((sum_of_H1 + sum_of_H2)/nrow(APIS_PUF_2017_Person))/.001870487 #.02526681
print(healthshare)
# 8.434944

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

livingstandardshare <- ((sum_of_L1 + sum_of_L2 + sum_of_L3 + sum_of_L4 + sum_of_L5 + sum_of_L6 + sum_of_L7 + sum_of_L8)/nrow(APIS_PUF_2017_Person))/.001870487
print(livingstandardshare)
#42.25897

