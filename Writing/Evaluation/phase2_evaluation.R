phase2_exp = function(n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k.csv") {
    
    nSize  = 5000 * (1 - p.attack)
    aSize  = 5000 * p.attack
    k      = k * 5000
    score  = read.csv(file = file, header = FALSE)
    
    purity      = matrix(nrow = 100, ncol = 4)
    size        = matrix(nrow = 100, ncol = 4)
    ncluster    = numeric(100)
    d           = matrix(nrow = 100, ncol = 4)
    max_class   = matrix(nrow = 100, ncol = 4)
    detection   = numeric(100)
    false.alarm = numeric(100)
    
    for (i in 1:n) {
        invisible((gc(reset = TRUE)))
        
        cat(paste(i, "..."))
        
        set.seed(i); normal = sample(which(dat$attack_type == "normal."), size = nSize, replace = FALSE)
        set.seed(i); attack = sample(which(dat$attack_type != "normal."), size = aSize, replace = FALSE)
        testset  = dat[c(normal, attack), -c(2,3,4,7,12,21,22)]
        
        p1.result            = experiment(as.numeric(score[i, ]), testset)
        detected.df          = testset[p1.result$detected, ]
        xmeans.res           = xmeans1(detected.df, "normalise")
        centers.detected     = xmeans.res$center.matrix
        
        purity[i, ]    = xmeans.res$purity
        max_class[i, ] = unlist(lapply(xmeans.res$table, function(x) names(which.max(x))))
        size[i, ]      = xmeans.res$size
        ncluster[i]    = length(xmeans.res$purity)
        
        return(xmeans.res$table)
        
        normal.df      = preproc(testset[-c(p1.result$detected), ], "normalise")
        centers.normal = colMeans(normal.df[, names(normal.df)!="attack_type"])
        centers.normal = centers.normal[which(names(centers.normal) %in% rownames(centers.detected))]
        
        dist = apply(centers.detected, 2, function(x) sqrt(sum((x-centers.normal)^2)))
        d[i, ] = dist
        
        normal.clust = which.min(dist)
        normal.ind = which(xmeans.res$class_ids == normal.clust)
        detected.final = detected.df$attack_type[-normal.ind]
        
        detection[i]   = sum(detected.final != "normal.") / aSize
        false.alarm[i] = sum(detected.final == "normal.") / nSize
        
        cat(paste0(" done.", "\n"))
    }
    
    list(max_class = max_class, purity = purity, size = size, ncluster = ncluster, distances = d, detection.rate = detection, false.alarm.rate = false.alarm)
    
}

a = phase2_exp(n = 1)
# phase2_results = phase2_exp()
# save(phase2_results, file = "Writing/Evaluation/phase2_evaluation.RData")

