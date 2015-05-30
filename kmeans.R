## normalise data (feature scaling)
mydata.norm = apply(trim1[, -which(names(trim1) == "attack_type")], MARGIN = 2, FUN = function(x) (x - min(x))/diff(range(x)))

## standardise data
mydata.stan = scale(trim1[, -which(names(trim1) == "attack_type")])

## PC scores
mydata.PC = PCA(trim1[, -which(names(trim1) == "attack_type")], ncp = 35, graph = FALSE)$ind$coord

plot.kmeans = function(data, title) {
    wss = numeric()
    tot.var = numeric()
    aic = numeric()
    bic = numeric()
    for (i in 1:10) {
        if (i == 1) wss[i] = (nrow(data)-1)*sum(apply(data,2,var))
        #set.seed(123)
        tmp = kmeans(data, centers = i, iter.max = 1000, nstart = 25)
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
    mtext(title, side = 3, line = -2, outer = TRUE)
    par(mfrow = c(1, 1))
}
plot.kmeans(trim1[,names(trim1)!="attack_type"], title = "Raw data")
plot.kmeans(mydata.norm, title = "Normalised data")
plot.kmeans(mydata.stan, title = "Standardised data")
plot.kmeans(mydata.PC, title = "PC scores")



# =======================================
preproc = function(data, mode) {
    
    ## remove redundant features which consist of one unique value
    #data = data[, sapply(data, function(x) length(unique(x))) != 1]
    
    if (mode == "pca" ) {
        
        require(FactoMineR)
        data.proc = PCA(data[, names(data) != "attack_type"], ncp = 35, graph = FALSE)$ind$coord
        data.proc = as.data.frame(data.proc)
        data.proc$attack_type = data$attack_type
        colnames(data.proc) = names(data)
        
    } else if (mode == "normalise") {
        
        data.proc = apply(data[, names(data) != "attack_type"], MARGIN = 2, FUN = function(x){
            norm = (x - min(x))/diff(range(x)))
            if (any(is.nan(norm))) {
                norm[is.nan(norm)] = 0
            }
            norm
        }
        data.proc = as.data.frame(data.proc)
        data.proc$attack_type = data$attack_type
        colnames(data.proc) = names(data)
        
    } else if (mode == "standardise") {
        
        data.proc = scale(data[, names(data) != "attack_type"])
        data.proc = as.data.frame(data.proc)
        data.proc$attack_type = data$attack_type
        colnames(data.proc) = names(data)
        
    }
    
    return(data.proc)
    
}
kmeans1 = function(data, mode = NULL, centers, iter.max = 1000, nstart = 20, ...) {
    
    data = data[, sapply(data, function(x) length(unique(x))) != 1]
    
    ## pre-process data
    if (!is.null(mode)) {
        data.proc = preproc(data, mode)
    } else {
        data.proc = data
    }
    
    kmeans.res = kmeans(data.proc[names(data.proc)!="attack_type"], centers = centers, ...)
    kmeans.res$table  = list()
    kmeans.res$purity = numeric()
    
    for (i in 1:centers) {
        tmp.table             = table(data.proc[which(kmeans.res$cluster == i), "attack_type"])
        kmeans.res$table[[i]] = tmp.table[tmp.table != 0]
        kmeans.res$purity[i]  = max(tmp.table) / kmeans.res$size[i]
    }
    
    return(kmeans.res)
}
xmeans1 = function(data, mode = NULL) {
    
    data = data[, sapply(data, function(x) length(unique(x))) != 1]
    
    ## pre-process data
    if (!is.null(mode)) {
        data.proc = preproc(data, mode)
    } else {
        data.proc = data
    }
    
    ## http://weka.sourceforge.net/doc.packages/XMeans/weka/clusterers/XMeans.html
    xmeans.res = XMeans(data.proc[, names(data.proc)!="attack_type"], c("-H", 50, "-use-kdtree","-K", "weka.core.neighboursearch.KDTree -P"))
    xmeans.res$table   = list()
    xmeans.res$purity  = numeric()
    xmeans.res$size    = numeric()
    xmeans.res$feature = names(data.proc[,names(data.proc)!="attack_type"])
    
    clusters = sort(unique(xmeans.res$class_ids))
    
    for (i in 1:length(clusters)) {
        members               = which(xmeans.res$class_ids == clusters[i])
        tmp.table             = table(data.proc[members, "attack_type"])
        xmeans.res$table[[i]] = tmp.table[tmp.table != 0]
        xmeans.res$size[i]    = length(members)
        xmeans.res$purity[i]  = max(tmp.table) / xmeans.res$size[i]
    }
    
    return(xmeans.res)
}

kmeans1(trim1, centers = 3)[c("size", "purity", "table")]
kmeans1(trim1, mode = "normalise", centers = 4)[c("size", "purity", "table")]
kmeans1(trim1, mode = "standardise", centers = 5)[c("size", "purity", "table")]
kmeans1(trim1, mode = "pca", centers = 5)[c("size", "purity", "table")]

xmeans1(trim1)[c("size", "purity", "table")]
xmeans1(trim1, mode = "normalise")[c("size", "purity", "table")]
xmeans1(trim1, mode = "standardise")[c("size", "purity", "table")]
xmeans1(trim1, mode = "pca")[c("size", "purity", "table")]


train = dat[sample(which(dat$attack_type == "normal."), 5000), -category]
train.norm = apply(train[, names(train)!="attack_type"], MARGIN = 2, FUN = function(x) (x - min(x))/diff(range(x)))
train.norm[is.nan(train.norm)] = 0
train.colmeans = as.vector(colMeans(train.norm))
names(train.colmeans) = names(train[,names(train)!="attack_type"])

#apply(dat[sample(which(dat$attack_type == "normal."), 5000), -c(category, 42)], 2, mean)
#apply(dat[dat$attack_type=="normal.", -c(category, 42)], 2, mean)

xmeans.norm = xmeans1(trim1, mode = "normalise")
centers = gsub("^.+?n(0.+)}.+$", "\\1", capture.output(xmeans.norm$clusterer$getClusterCenters()))
centers = eval(parse(text=paste("c(", gsub("\\\\n", ",", centers), ")")))
centers = cbind(centers[1:31], centers[32: 62], centers[63:93], centers[94:124])
rownames(centers) = xmeans.norm$feature

means = cbind(centers, train.colmeans[which(names(train.colmeans) %in% rownames(centers))])
colnames(means) = c("cluster 1", "cluster 2", "cluster 3", "cluster 4", "train")

sum((means[, 1] - means[, 5])^2)
sum((means[, 2] - means[, 5])^2)
sum((means[, 3] - means[, 5])^2)
sum((means[, 4] - means[, 5])^2)

apply(means[,1:4], 2, function(x) sum( (x-means[,5])^2 ))







# kmeans.results = kmeans(mydata, centers = 3, iter.max = 1000, nstart = 20)
# summary(trim3[which(kmeans.results$cluster == 1), "attack_type"])[summary(trim3[which(kmeans.results$cluster == 1), "attack_type"])!=0]
# summary(trim3[which(kmeans.results$cluster == 2), "attack_type"])[summary(trim3[which(kmeans.results$cluster == 2), "attack_type"])!=0]
# summary(trim3[which(kmeans.results$cluster == 3), "attack_type"])[summary(trim3[which(kmeans.results$cluster == 3), "attack_type"])!=0]
# 
# kmeans.results1 = kmeans(res3$ind$coord, centers = 4, iter.max = 1000, nstart = 25)
# summary(trim3[which(kmeans.results1$cluster == 1), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 1), "attack_type"])!=0]
# summary(trim3[which(kmeans.results1$cluster == 2), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 2), "attack_type"])!=0]
# summary(trim3[which(kmeans.results1$cluster == 3), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 3), "attack_type"])!=0]
# summary(trim3[which(kmeans.results1$cluster == 4), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 4), "attack_type"])!=0]
# 
# kmeans.results1 = kmeans(res3$ind$coord, centers = 5, iter.max = 1000, nstart = 25)
# summary(trim3[which(kmeans.results1$cluster == 1), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 1), "attack_type"])!=0]
# summary(trim3[which(kmeans.results1$cluster == 2), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 2), "attack_type"])!=0]
# summary(trim3[which(kmeans.results1$cluster == 3), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 3), "attack_type"])!=0]
# summary(trim3[which(kmeans.results1$cluster == 4), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 4), "attack_type"])!=0]
# summary(trim3[which(kmeans.results1$cluster == 5), "attack_type"])[summary(trim3[which(kmeans.results1$cluster == 5), "attack_type"])!=0]
# 
# xmeans.results = XMeans(mydata, c("-I", 1, "-L", 2, "-H", 50, "-use-kdtree","-K", "weka.core.neighboursearch.KDTree -P"))
# xmeans.results = XMeans(as.data.frame(res3$ind$coord), c("-I", 1, "-L", 2, "-H", 50, "-use-kdtree","-K", "weka.core.neighboursearch.KDTree -P"))
# summary(trim3[which(xmeans.results$class_ids == 0), "attack_type"])[summary(trim3[which(xmeans.results$class_ids == 0), "attack_type"])!=0]
# summary(trim3[which(xmeans.results$class_ids == 1), "attack_type"])[summary(trim3[which(xmeans.results$class_ids == 1), "attack_type"])!=0]
# summary(trim3[which(xmeans.results$class_ids == 2), "attack_type"])[summary(trim3[which(xmeans.results$class_ids == 2), "attack_type"])!=0]
# summary(trim3[which(xmeans.results$class_ids == 3), "attack_type"])[summary(trim3[which(xmeans.results$class_ids == 3), "attack_type"])!=0]
# 
# XMeans(mydata, c("-H", 50))
# XMeans(mydata, c("-H", 50, "-use-kdtree","-K", "weka.core.neighboursearch.KDTree -P"))











