# Analysis ----------------------------------------------------------------


# Clustering --------------------------------------------------------------

# Non standardised input data

# Kmeans
for(i in 3:10){
  assign(paste0("k_mean_",i),kmeans(input_data[,-c(1:2)]
                                    ,centers = i,nstart = 50))
}

profile_km <- round(t(aggregate(profiling_data[,-c(1:2)], list(k_mean_3$cluster)
                                , mean))[-1,],2)

k_mean_3$centers # Centers only differentiates by age

fit <- k_mean_3$betweenss/k_mean_3$totss # 82.7% 


# Mclust
library(mclust)

for(i in 3:10){
  assign(paste0("mclust_",i),Mclust(input_data[,-c(1:2)],i))
} 

profile_mc <- round(t(aggregate(profiling_data[,-c(1:2)], list(mclust_3$classification)
                                , mean))[-1,],2)

mclust_3$bic
mclust_3$loglik
range(mclust_3$uncertainty)

# HCL
distance <- dist(input_data[,-c(1:2)], method = "euclidean")

hcl <- hclust(distance, method = "ward.D2")

profile_hcl <- round(t(aggregate(profiling_data[,-c(1:2)], list(cutree(as.hclust(hcl), 1:10)[,3])
                  ,mean))[-1,],2)

# HCL kmeans
library(factoextra)
for(i in 3:10){
  assign(paste0("hk_mean_",i),hkmeans(input_data[,-c(1:2)],k=i ,hc.method = "ward.D2"))
}

profile_hk <- round(t(aggregate(profiling_data[,-c(1:2)], list(hk_mean_3$cluster)
                                , mean))[-1,],2)

fit_hk <- hk_mean_3$betweenss/hk_mean_3$totss # 82.6%

# Visualisation for best solution
library(reshape2)
means <- data.frame(profile_mc, stringsAsFactors = FALSE)[-c(1,19:36,40:59,67:75,79:86),] %>%
  rownames_to_column() %>%
  rename(variables = rowname) %>%
  melt(id.vars = "variables", variable.name = "Segments", value.name = "Mean")

rn <- rownames(data.frame(profile_mc, stringsAsFactors = FALSE)[-c(1,19:36,40:59,67:75,79:86),])

p <- means %>%
  ggplot(aes(variables, Mean, group = Segments, color = Segments)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  scale_x_discrete(limits = colnames(profiling_data[rn])) +
  labs(x = NULL, y = "Means") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")


library(plotly)

ggplotly(p, tooltip = c("variables", "Mean")) %>%
  layout(legend = list(orientation = "h", y = 1.2))
