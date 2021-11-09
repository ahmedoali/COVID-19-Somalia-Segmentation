
# Exploratory analysis ----------------------------------------------------


# Plots with raw data
library(tidyverse)

ggplot(data = COVID) + 
  geom_bar(mapping = aes(x = covid_feel))

ggplot(data = COVID) + 
  geom_bar(mapping = aes(x = covid_threat)

ggplot(data = COVID) + 
  geom_bar(mapping = aes(x = money_misuse))

ggplot(data = COVID) + 
  geom_bar(mapping = aes(x = great_risk))


ggplot(data = COVID) + 
  geom_bar(mapping = aes(x = limit_rights))


# Summary
summary(profiling_data[,-c(1:2)])

# Similarity analysis
# Jaccards
Jaccards <- function(df){
  dat <- df
  J_sim <- matrix(data = NA, nrow = ncol(dat), ncol = ncol(dat))
  for(i in 1:ncol(dat)){
    for(j in 1:ncol(dat)){
      J_sim[i,j] <- sum(ifelse(dat[,i] == 1 & dat[,j] == 1,1,0))/(sum(ifelse(dat[,i] == 1 & dat[,j] == 0,1,0)) + 
        sum(ifelse(dat[,i] == 0 & dat[,j] == 1,1,0)) + sum(ifelse(dat[,i] == 1 & dat[,j] == 1,1,0)))
    }
  }
  
  rownames(J_sim) <- colnames(dat)
  colnames(J_sim) <- colnames(dat)
  return(J_sim)
}

similarity <- Jaccards(profiling_data[,-c(1:3)])

sort(similarity[,which(colnames(similarity) == "govt_resp_sat_Yes")], decreasing = TRUE)

# Cosine sim
Cosine <- function(df){
  dat <- df
  C_sim <- matrix(data = NA, nrow = ncol(dat), ncol = ncol(dat))
  for(i in 1:ncol(dat)){
    for(j in 1:ncol(dat)){
      C_sim[i,j] <- (dat[,i] %*% dat[,j])/(sqrt(dat[,i] %*% dat[,i])*sqrt(dat[,j] %*% dat[,j]))
    }
  }
  
  rownames(C_sim) <- colnames(dat)
  colnames(C_sim) <- colnames(dat)
  return(C_sim)
}

similarity_C <- Cosine(profiling_data[,-c(1:3)])
sort(similarity_C[,which(colnames(similarity_C) == "govt_resp_sat_Yes")], decreasing = TRUE)

# Pearson corr
pearson <- cor(profiling_data[,-c(1:2)], method = "pearson")
sort(pearson[,which(colnames(pearson) == "govt_resp_sat_Yes")], decreasing = TRUE)
