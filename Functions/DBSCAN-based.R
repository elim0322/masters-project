


dbscan.experiment = function(data, n = 5000, i = 100, p.attack = 0.3) {
    
    ## normal, attack
    nSize = n * (1 - p.attack)
    aSize = n * p.attack
    
    TPR = numeric(i)
    FPR = numeric(i)
    
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
        
        dbscan.res0.1 = DBScan(testset[, -35], control =  Weka_control(E = 0.1))
        dbscan.res0.2 = DBScan(testset[, -35], control =  Weka_control(E = 0.2))
        dbscan.res0.3 = DBScan(testset[, -35], control =  Weka_control(E = 0.3))
        dbscan.res0.4 = DBScan(testset[, -35], control =  Weka_control(E = 0.4))
        dbscan.res0.5 = DBScan(testset[, -35], control =  Weka_control(E = 0.5))
        dbscan.res0.6 = DBScan(testset[, -35], control =  Weka_control(E = 0.6))
        dbscan.res0.7 = DBScan(testset[, -35], control =  Weka_control(E = 0.7))
        dbscan.res0.8 = DBScan(testset[, -35], control =  Weka_control(E = 0.8))
        dbscan.res0.9 = DBScan(testset[, -35], control =  Weka_control(E = 0.9))
        dbscan.res1.0 = DBScan(testset[, -35], control =  Weka_control(E = 1.0))
        
        dbscan.results = list(
            dbscan.res0.1, dbscan.res0.2, dbscan.res0.3, dbscan.res0.4, dbscan.res0.5,
            dbscan.res0.6, dbscan.res0.7, dbscan.res0.8, dbscan.res0.9, dbscan.res1.0
        )
        
        n     = lapply(dbscan.results, function(x) tab(testset, x))
        TPR.i = sapply(n, function(x) x$attack / aSize)
        FPR.i = sapply(n, function(x) x$normal / nSize)
        
        maxTPR = which.max(TPR.i)
        
        TPR[j] = TPR.i[maxTPR]
        FPR[j] = FPR.i[maxTPR]
    }
    
}

tab = function(data, dbscan.object) {
    maxC     = names(which.max(table(dbscan.object$class_ids)))
    tabC = table(data[dbscan.object$class_ids == maxC, "attack_type"])
    return(list(
        "normal" = sum(tabC[names(tabC) == "normal."]),
        "attack" = sum(tabC[names(tabC) != "normal."])
    ))
}


