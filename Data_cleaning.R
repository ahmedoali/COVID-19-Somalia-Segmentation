
# Data cleaning -----------------------------------------------------------

# Clear environment
rm(list = ls())
gc()

# Read data in
library(readr)
COVID <- read_csv("~/SOM_2020_HFPS_v01_M_CSV/SHFPS_R1.csv")

# Missing data check
colSums(is.na(COVID))

# Add unique id to data
COVID$unique_id <- seq(1,nrow(COVID),1)
COVID <- COVID[,c(563,1:562)]

# Profiling data
library(tidyverse)
library(fastDummies)
profiling_data <- COVID %>%
  filter(itw_result == "complete") %>% 
  dplyr::select(unique_id,CSW,age,state,region,ruralurban,idp_nomad,edu_level,govt_resp_sat,b_wash_hands,b_avoid_handshake,b_avoid_groups,covid_feel,covid_threat,
                limit_rights,great_risk,money_misuse,flee_home,no_soap,edu_last_week,income_1:income_9,fies_no_food,
                fies_hungry,fies_no_eat,cope_shock_1:cope_shock_9)

profiling_data_dum <- dummy_cols(profiling_data[,-c(1:3,13:17)])
profiling_data_dum <- profiling_data_dum[,-c(1:33)]
profiling_data <- cbind(profiling_data[,c(1:3)],profiling_data_dum)

profiling_data[is.na(profiling_data)] <- 0

columns_to_remove <- grep("_NA",names(profiling_data))
profiling_data <- profiling_data[,-columns_to_remove]

columns_to_remove <- grep("_No",names(profiling_data))
profiling_data <- profiling_data[,-columns_to_remove]

profiling_data <- merge(profiling_data,COVID[,c(1,183:187)], by = "unique_id")


# Input data
input_data <- profiling_data[,c(1:3,29,31,52:55,79:83)]

input_data <- input_data %>%
  mutate(covid_feel_r = recode(covid_feel,`0` = 0,
                               "Very worried" = 1,
                               "Somewhat worried" = 2,
                               "Not too worried" = 3,
                               "Not worried at all" = 4),
         covid_threat_r = recode(covid_threat,`0` = 0,
                                 "A substantial threat" = 1,
                                 "A moderate threat" = 2,
                                 "Not much of a threat" = 3,
                                 "Not a threat at all" = 4),
         limit_rights_r = recode(limit_rights,`0` = 0,"Strongly disagree" = 1,
                                 "Disagree" = 2,
                                 "Neutral" = 3,
                                 "Agree" = 4,
                                 "Strongly agree" = 5),
         great_risk_r = recode(great_risk,`0` = 0,"Strongly disagree" = 1,
                               "Disagree" = 2,
                               "Neutral" = 3,
                               "Agree" = 4,
                               "Strongly agree" = 5),
         money_misuse_r = recode(money_misuse,`0` = 0,"Strongly disagree" = 1,
                                 "Disagree" = 2,
                                 "Neutral" = 3,
                                 "Agree" = 4,
                                 "Strongly agree" = 5))

input_data <- input_data %>%
  mutate(covid_feel_r_2tb = ifelse(covid_feel_r >= 3,1,0),
         covid_threat_r_2tb = ifelse(covid_threat_r>=3,1,0),
         limit_rights_r_2tb = ifelse(limit_rights_r>=4,1,0),
         great_risk_r_2tb = ifelse(great_risk_r >= 4,1,0),
         money_misuse_r_2tb = ifelse(money_misuse_r>=4,1,0),
         covid_feel_r_2bb = ifelse(covid_feel_r == 1 | covid_feel_r == 2 ,1,0),
         covid_threat_r_2bb = ifelse(covid_threat_r == 1 | covid_threat_r == 2,1,0),
         limit_rights_r_2bb = ifelse(limit_rights_r == 1 | limit_rights_r == 2,1,0),
         great_risk_r_2bb = ifelse(great_risk_r == 1 | great_risk_r == 2,1,0),
         money_misuse_r_2bb = ifelse(money_misuse_r == 1 | money_misuse_r == 2,1,0))


input_data <- input_data[,-c(10:19)]
input_data[is.na(input_data)] <- 0

# Add recodes into profiling data
profiling_data <- profiling_data[,-c(79:83)]
profiling_data <- merge(input_data[,c(1,10:19)],profiling_data, by = "unique_id")
profiling_data <- profiling_data[,c(1,12,13,2:11,14:88)]

# Input data - col std
input_data_colstd <- data.frame("unique_id"=input_data$unique_id
                                ,apply(input_data[,-c(1:2)],2
                                       ,function(x) (x - mean(x))/sd(x)))

# Input data - col row std
input_data_colrowstd <- data.frame("unique_id"=input_data$unique_id
                                ,t(apply(input_data_colstd[,-1],1
                                       ,function(x) (x - mean(x))/sd(x))))

# Input data - row col std
input_data_rowstd <- data.frame("unique_id"=input_data$unique_id
                                   ,t(apply(input_data[,-c(1:2)],1
                                            ,function(x) (x - mean(x))/sd(x))))
input_data_rowcolstd <- data.frame("unique_id"=input_data$unique_id
                                   ,apply(input_data_colstd[,-1],2
                                            ,function(x) (x - mean(x))/sd(x)))

# Randomise order
set.seed(123)
rows <- sample(nrow(input_data))
input_data <- input_data[rows,]
input_data_colstd <- input_data_colstd[rows,]
profiling_data <- profiling_data[rows,]

# Remove unneeded files
rm(list = c("profiling_data_dum","columns_to_remove"))
