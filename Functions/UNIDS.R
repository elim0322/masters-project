
UNIDS = function(dat) {
    
    ## for generalised inverse/pseudoinverse of covariance matrix
    require(MASS)
    
    n = nrow(dat)
    
    ## smallest cluster-size (alpha * n)
    n_min = 0.05 * n
    
    ## 34 numeric dimensions in data
    ## so N = m(m-1)/2 = 561
    comb = combn(ncol(dat), 2)
    N    = ncol(comb)
    
    ## dissimilarity vector
    D = numeric(n)
    
    
    for (i in 1:N) {
        
        cat(i)
        cat("\n")
        
        X = dat[, comb[, i]]
        d = 0.1 * mean(dist(X))  # delta
        P = DBScan(X, control = Weka_control(E = d, M = n_min))
        
        if (any(is.na(P$class_ids))) {
            maxC  = names(which.max(table(P$class_ids)))
            if (!is.null(maxC)) {
                n_max = sum(P$class_ids == maxC, na.rm = TRUE)
                w     = n / (n - n_max)
                
                o_ind = which(is.na(P$class_ids))
                o     = as.matrix(X[o_ind, ])
                C_max = colMeans(X[which(P$class_ids == maxC), ])
                
                d_M = numeric(n)
                d_M[o_ind] = apply(o, 1, function(x) {
                    #sqrt( (x-C_max) %*% ginv(cov(rbind(x, C_max))) %*% t(t(x-C_max)) )
                    sqrt(mahalanobis(x, C_max, cov = ginv(cov(rbind(x, C_max))), inverted = TRUE))
                })
                
                D = D + w * d_M
            } else {
                next
            }
        }
        
    }
    
    return(D)
    
}

unids_ranking = UNIDS(testset[,-35])
sort(unids_ranking, decreasing = TRUE)[1:10]









