set.seed(3); normal = sample(which(dat1$attack_type == "normal."), size = 4500, replace = FALSE)
set.seed(3); attack = sample(which(dat1$attack_type != "normal."), size = 500,  replace = FALSE)
## testset excludes any categorical/binary features
testset = dat1[c(normal, attack), -c(2,3,4,7,12,21,22)]

library(FactoMineR)
pca.res = PCA(testset[, -35], graph = FALSE, ncp = 34)
plot(pca.res$eig$eigenvalue)

## number of PCs to keep
library(nFactors)
nPC = nMreg(pca.res$eig$eigenvalue)$nFactors["b"]
pc = pca.res$ind$coord[, 1:nPC]

library(RWeka)
WPM("load-package", "XMeans")
XMeans = make_Weka_clusterer("weka/clusterers/XMeans")
xmeans.res = XMeans(as.data.frame(pc))
cMax = names(which.max(table(xmeans.res$class_ids)))  # largest cluster
table(testset[xmeans.res$class_ids != cMax, "attack_type"])



require(RWeka)
require(FactoMineR)
require(nFactors)





set.seed(1); normal = sample(which(dat1$attack_type == "normal."), size = 4500, replace = FALSE)
set.seed(1); attack = sample(which(dat1$attack_type != "normal."), size = 500,  replace = FALSE)
## testset excludes any categorical/binary features
testset = dat1[c(normal, attack), -c(2,3,4,7,12,21,22)]
pca.based(testset)


pca.results = pca.experiment(dat1, p.attack = 0.1)


pca.experiment = function(data, n = 5000, i = 100, p.attack = 0.3) {
    
    ## normal, attack and k
    nSize = n * (1 - p.attack)
    aSize = n * p.attack
    
    ## containers
    n = numeric(i)
    a = numeric(i)
    
    ## iteration up to i
    for (j in 1:i) {
        ## force garbage collection
        invisible((gc(reset = TRUE)))
        
        ## print
        cat(paste("seed", j, "\n"))
        
        ## random sampling without replacement
        set.seed(j); normal = sample(which(data$attack_type == "normal."), size = nSize, replace = FALSE)
        set.seed(j); attack = sample(which(data$attack_type != "normal."), size = aSize, replace = FALSE)
        ## testset excludes any categorical/binary features
        testset = data[c(normal, attack), -c(2,3,4,7,12,21,22)]
        
        ## do PCA-based approach
        res  = pca.based(testset)
        n[j] = res$normal
        a[j] = res$attack
    }
    
    return(list(TPR = a/aSize, FPR = n/nSize))
    
}

pca.based = function(data) {
    
    ## do a PCA
    pca.res = PCA(data[, -35], graph = FALSE, ncp = ncol(data))
    ## compute Zoski and Jurs' b coefficient
    ## (non-graphical alternative to Cattell's scree test)
    nPC = suppressWarnings(nMreg(pca.res$eig$eigenvalue)$nFactors["b"])
    pc  = pca.res$ind$coord[, 1:nPC]
    
    ## X-means
    xmeans.res = XMeans(as.data.frame(pc))
    cMax = names(which.max(table(xmeans.res$class_ids)))  # largest cluster
    
    ## number of normal and attacks classified
    n = sum(data[xmeans.res$class_ids != cMax, "attack_type"] == "normal.")
    a = sum(data[xmeans.res$class_ids != cMax, "attack_type"] != "normal.")
    
    return(list(normal = n, attack = a))
    
}

