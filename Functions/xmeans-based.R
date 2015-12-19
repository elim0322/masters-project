
xmeans.experiment = function(data, n = 5000, i = 100, p.attack = 0.3) {
    ## normal, attack
    nSize = n * (1 - p.attack)
    aSize = n * p.attack
    
    ## containers
    nrm = numeric(i)
    atk = numeric(i)
    
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
        
        ## x-means
        a = XMeans(as.data.frame(testset[, -35]), control = Weka_control(H = 20))
        b = table(testset[a$class_ids != names(which.max(table(a$class_ids))), "attack_type"])
        
        ## number of normal and attacks classified
        nrm[j] = sum(b[names(b) == "normal."])
        atk[j] = sum(b[names(b) != "normal."])
    }
    return(list(TPR = atk/aSize, FPR = nrm/nSize))
}

# xmeans.results10a = xmeans.experiment(dat1, p.attack = 0.1)
# xmeans.results20a = xmeans.experiment(dat1, p.attack = 0.2)
# xmeans.results30a = xmeans.experiment(dat1, p.attack = 0.3)
# xmeans.results40a = xmeans.experiment(dat1, p.attack = 0.4)
# save(xmeans.results10a, xmeans.results20a, xmeans.results30a, xmeans.results40a, file = "Data/System_results/xmeans.results.RData")

