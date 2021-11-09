
# Analysis ----------------------------------------------------------------


# Clustering --------------------------------------------------------------

# Assess profiles

# Kmeans
for(i in 3:10){
  assign(paste0("k_mean_",i),kmeans(input_data[,-c(1:3)]
                                    ,centers = i,iter.max = 1000,nstart = 50))
}

for(i in 3:10){
  assign(paste0("k_mean_cr_",i),kmeans(input_data_colrowstd[,-c(1:2)]
                                           ,centers = i,iter.max = 1000,nstart = 50))
}

for(i in 3:10){
  assign(paste0("k_mean_rc_",i),kmeans(input_data_rowcolstd[,-c(1:2)]
                                           ,centers = i,iter.max = 1000,nstart = 50))
}


profile_km <- round(t(aggregate(profiling_data[,-c(1:2)], list(k_mean_5$cluster)
                                , mean))[-1,],2)

profile_km_cr <- round(t(aggregate(profiling_data[,-c(1:2)], list(k_mean_cr_5$cluster)
                                , mean))[-1,],2)

profile_km_rc <- round(t(aggregate(profiling_data[,-c(1:2)], list(k_mean_rc_5$cluster)
                                , mean))[-1,],2)

profile_km
profile_km_cr
profile_km_rc

# Mclust
library(mclust)

for(i in 3:10){
  assign(paste0("mclust_",i),Mclust(input_data[,-c(1:3)],i))
}

for(i in 3:10){
  assign(paste0("mclust_cr_",i),Mclust(input_data_colrowstd[,-c(1:2)],i))
} 

for(i in 3:10){
  assign(paste0("mclust_rc_",i),Mclust(input_data_rowcolstd[,-c(1:2)],i))
} 

profile_mc <- round(t(aggregate(profiling_data[,-c(1:2)], list(mclust_5$classification)
                                , mean))[-1,],3)

profile_mc <- round(t(aggregate(profiling_data[,-c(1:2)], list(mclust_cr_5$classification)
                                , mean))[-1,],3)

profile_mc <- round(t(aggregate(profiling_data[,-c(1:2)], list(mclust_rc_5$classification)
                                , mean))[-1,],3)

profile_mc
profile_mc_cr
profile_mc_rc

# HCL
distance <- dist(input_data[,-c(1:3)], method = "euclidean")
distance_rc <- dist(input_data_rowcolstd[,-c(1:2)], method = "euclidean")
distance_cr <- dist(input_data_colrowstd[,-c(1:2)], method = "euclidean")

hcl <- hclust(distance, method = "ward.D2")
hcl_rc <- hclust(distance_rc, method = "ward.D2")
hcl_cr <- hclust(distance_cr, method = "ward.D2")

profile_hcl <- round(t(aggregate(profiling_data[,-c(1:2)], list(cutree(as.hclust(hcl), 1:10)[,5])
                                 ,mean))[-1,],2)

profile_hcl <- round(t(aggregate(profiling_data[,-c(1:2)], list(cutree(as.hclust(hcl_cr), 1:10)[,5])
                                 ,mean))[-1,],2)
profile_hcl <- round(t(aggregate(profiling_data[,-c(1:2)], list(cutree(as.hclust(hcl_rc), 1:10)[,5])
                                 ,mean))[-1,],2)

profile_hcl
profile_hcl_rc 
profile_hcl_cr

opar <- par(mar = c(4.1, 1, 1, 12))
plot(as.dendrogram(hcl), horiz = TRUE, type = "triangle",
     xlab = "Height")
par(opar)

