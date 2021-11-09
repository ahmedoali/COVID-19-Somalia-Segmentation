
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
                fies_hungry,fies_no_eat,cope_shock_1:cope_shock_9,inst_cash,inst_inkind)

profiling_data_dum <- dummy_cols(profiling_data[,-c(1:3,13:17)])
profiling_data_dum <- profiling_data_dum[,-c(1:35)]
profiling_data <- cbind(profiling_data[,c(1:3)],profiling_data_dum)

profiling_data[is.na(profiling_data)] <- 0

columns_to_remove <- grep("_NA",names(profiling_data))
profiling_data <- profiling_data[,-columns_to_remove]

columns_to_remove <- grep("_No",names(profiling_data))
profiling_data <- profiling_data[,-columns_to_remove]

profiling_data <- merge(profiling_data,COVID[,c(1,183:187)], by = "unique_id")


# Input data
input_data <- profiling_data[,c(1:3,5:10,29,30,53:58,68:70,79:85)]

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


input_data <- input_data[,-c(23:27)]
input_data[is.na(input_data)] <- 0

# Add recodes into profiling data
scaled_data <- profiling_data[,c(81:85)]
profiling_data <- profiling_data[,-c(81:85)]
profiling_data <- merge(input_data[,c(1,23:37)],profiling_data, by = "unique_id")
profiling_data <- profiling_data[,c(1,17,18,2:16,19:95)]

# Remove scales and b2b var
input_data <- input_data[,-c(23:27,33:37)]

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


# Remove unneeded files
rm(list = c("profiling_data_dum","columns_to_remove"))
