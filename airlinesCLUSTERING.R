                            ###### HCLUSTERING (east west airlines) #######

EastWestAirlines <- read_excel(choose.files())
View(EastWestAirlines)

EastWestAirlines <- EastWestAirlines[,-1]

summary(EastWestAirlines)
str(EastWestAirlines)

boxplot(EastWestAirlines) 
pairs(EastWestAirlines)
skewness(EastWestAirlines)
kurtosis(EastWestAirlines)

airlines_norm<-scale(EastWestAirlines)
View(airlines_norm)
summary(airlines_norm)

d <- dist(airlines_norm, method = "euclidean") 
fit <- hclust(d, method="complete")
windows()
plot(fit, hang=-1)

rect.hclust(fit, k=25, border="red")
groups <- cutree(fit, k=25)
membership<-as.matrix(groups) 

final <- data.frame(EastWestAirlines, membership)
View(final)
write.csv(final, file="airlines_hclust.csv",row.names = F)

aggregate(EastWestAirlines,by=list(final$membership),mean)

                          ####### K-MEANS CLUSTERING (east west airlines) #########

fit2 <- kmeans(airlines_norm, 25)
str(fit2)
final2 <- data.frame(fit2$cluster, EastWestAirlines)
View(final2)
write.csv(final2, file="airlines_kclust.csv",row.names = F)

aggregate(EastWestAirlines, by=list(final2$fit2.cluster),mean) 

twss <- NULL
for (i in 21:29){
  twss <- c(twss,kmeans(airlines_norm,i)$tot.withinss)
}
twss
plot(21:29,twss,type="b", xlab = "no of clusters", ylab = "within group sum of sqaures")
title(sub = "K-MEANS clustering scree plot")
