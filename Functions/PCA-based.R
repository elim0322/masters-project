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
    
    ## k-means
    TP = numeric()
    FP = numeric()
    for (i in 2:10) {
        a = SimpleKMeans(data[, -35], control = Weka_control(N = i, S = 20))
        b = table(testset[a$class_ids != names(which.max(table(a$class_ids))), "attack_type"])
        TP = c(TP, sum(b[names(b) != "normal."]))
        FP = c(FP, sum(b[names(b) == "normal."]))
    }
    
    TP.max = which.max(TP)
    
    ## number of normal and attacks classified
    n = TP[TP.max]
    a = TP[TP.max]
    
    return(list(normal = n, attack = a))
    
}

pca.results10a = pca.experiment(dat1, p.attack = 0.1)
pca.results20a = pca.experiment(dat1, p.attack = 0.2)
pca.results30a = pca.experiment(dat1, p.attack = 0.3)
pca.results40a = pca.experiment(dat1, p.attack = 0.4)


