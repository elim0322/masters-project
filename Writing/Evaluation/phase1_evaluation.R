phase1_eval = function(n = 100, k = 0.3) {
    
    k   = k * 5000
    res = matrix(nrow = 5000, ncol = n)
    for (i in 1:n) {
        cat(paste(i, "..."))
        
        set.seed(i); normal = sample(which(dat$attack_type == "normal."), size = 3500, replace = FALSE)
        set.seed(i); attack = sample(which(dat$attack_type != "normal."), size = 1500, replace = FALSE)
        testset  = dat[c(normal, attack), -c(2,3,4,7,12,21,22)]
        res[, i] = LOF(testset, control = Weka_control(min = k, max = k, "num-slots" = 2))[,"LOF"]
        
        cat(paste0(" done.", "\n"))
    }
    
    res
    
}

# phase1_results = phase1_eval(n = 5)


#lapply(phase1_results, function(x) max(x))


