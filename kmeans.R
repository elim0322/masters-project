mydata = trim3[, -c(obsolete, which(names(trim3) == "attack_type"))]
mydata = res3$ind$coord
wss = numeric()
tot.var = numeric()
aic = numeric()
bic = numeric()
for (i in 1:10) {
    if (i == 1) wss[i] = (nrow(mydata)-1)*sum(apply(mydata,2,var))
    set.seed(123)
    tmp = kmeans(mydata, centers = i, iter.max = 1000, nstart = 25)
    wss[i] = tmp$tot.withinss
    tot.var[i] = (tmp$betweenss / tmp$totss) * 100
    aic[i] = tmp$tot.withinss + 2 * ncol(tmp$centers) * nrow(tmp$centers)
    bic[i] = tmp$tot.withinss + 2 * log(length(tmp$cluster)) * ncol(tmp$centers) * nrow(tmp$centers)
}
par(mfrow = c(2, 2))
plot(1:10, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")
plot(1:10, tot.var, type = "b", xlab = "Number of clusters", ylab = "Variance explained (%)")
plot(1:10, aic, type = "b", xlab = "Number of clusters", ylab = "AIC")
plot(1:10, bic, type = "b", xlab = "Number of clusters", ylab = "BIC")
#mtext("Detected sample", side = 3, line = -2, outer = TRUE)
mtext("PC scores", side = 3, line = -2, outer = TRUE)
par(mfrow = c(1, 1))

kmeans.results = kmeans(mydata, centers = 3, iter.max = 1000, nstart = 20)
summary(trim3[which(kmeans.results$cluster == 1), "attack_type"])[summary(trim3[which(kmeans.results$cluster == 1), "attack_type"])!=0]
summary(trim3[which(kmeans.results$cluster == 2), "attack_type"])[summary(trim3[which(kmeans.results$cluster == 2), "attack_type"])!=0]
summary(trim3[which(kmeans.results$cluster == 3), "attack_type"])[summary(trim3[which(kmeans.results$cluster == 3), "attack_type"])!=0]

kmeans.results1 = kmeans(res3$ind$coord, centers = 4, iter.max = 1000, nstart = 25)
summary(trim3[which(kmeans.results1$cluster == 1), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 1), "attack_type"])!=0]
summary(trim3[which(kmeans.results1$cluster == 2), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 2), "attack_type"])!=0]
summary(trim3[which(kmeans.results1$cluster == 3), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 3), "attack_type"])!=0]
summary(trim3[which(kmeans.results1$cluster == 4), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 4), "attack_type"])!=0]

kmeans.results1 = kmeans(res3$ind$coord, centers = 5, iter.max = 1000, nstart = 25)
summary(trim3[which(kmeans.results1$cluster == 1), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 1), "attack_type"])!=0]
summary(trim3[which(kmeans.results1$cluster == 2), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 2), "attack_type"])!=0]
summary(trim3[which(kmeans.results1$cluster == 3), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 3), "attack_type"])!=0]
summary(trim3[which(kmeans.results1$cluster == 4), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 4), "attack_type"])!=0]
summary(trim3[which(kmeans.results1$cluster == 5), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 5), "attack_type"])!=0]



xmeans.results = XMeans(mydata, c("-I", 1, "-L", 2, "-H", 50, "-use-kdtree","-K", "weka.core.neighboursearch.KDTree -P"))
xmeans.results = XMeans(as.data.frame(res3$ind$coord), c("-I", 1, "-L", 2, "-H", 50, "-use-kdtree","-K", "weka.core.neighboursearch.KDTree -P"))
summary(trim3[which(xmeans.results$class_ids == 0), "attack_type"])[summary(trim3[which(xmeans.results$class_ids == 0), "attack_type"])!=0]
summary(trim3[which(xmeans.results$class_ids == 1), "attack_type"])[summary(trim3[which(xmeans.results$class_ids == 1), "attack_type"])!=0]
summary(trim3[which(xmeans.results$class_ids == 2), "attack_type"])[summary(trim3[which(xmeans.results$class_ids == 2), "attack_type"])!=0]
summary(trim3[which(xmeans.results$class_ids == 3), "attack_type"])[summary(trim3[which(xmeans.results$class_ids == 3), "attack_type"])!=0]



