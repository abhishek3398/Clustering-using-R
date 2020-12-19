                              ##### HCLUSTERING (crime data) #####

crime_data <- read.csv(choose.files())
View(crime_data)

summary(crime_data) ## except 3rd column(assault), all independent variables are normally distributed ##
str(crime_data)

boxplot(crime_data[,-1]) ## rape variable has outliers ##
pairs(crime_data[-c(1)])
skewness(crime_data[,-1])
kurtosis(crime_data[,-1])

crime_norm<-scale(crime_data[,2:5])
View(crime_norm)
summary(crime_norm)

d <- dist(crime_norm, method = "euclidean") 
fit <- hclust(d, method="complete")
windows()
plot(fit, hang=-1)

rect.hclust(fit, k=7, border="red")
groups <- cutree(fit, k=7)
membership<-as.matrix(groups) 

final <- data.frame(crime_data, membership)
View(final)
write.csv(final, file="final.csv",row.names = F)

aggregate(crime_data[,-1],by=list(final$membership),mean)

                             ####### K-MEANS CLUSTERING (crime data) ########

fit2 <- kmeans(crime_norm, 7)
str(fit2)
final2 <- data.frame(fit2$cluster, crime_data)
View(final2)
write.csv(final2, file="crime_kclust.csv",row.names = F)

aggregate(crime_data[,-1], by=list(final2$fit2.cluster),mean) 

twss <- NULL
for (i in 4:10){
  twss <- c(twss,kmeans(crime_norm,i)$tot.withinss)
}
twss
plot(twss,type="b", xlab = "no of clusters", ylab = "within group sum of sqaures")
title(sub = "K-MEANS clustering scree plot")
