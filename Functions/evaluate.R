# --------------------------------------------------------------------
# Function to evaluate the detection and false alarm rates of Phase 1
# --------------------------------------------------------------------
p1.evaluation(dat1, p.attack = 0.4, scores = scores.40a10k)



p1.evaluation = function(data, n = 5000, i = 100, p.attack = 0.3, scores) {
    ## normal, attack and k
    nSize = n * (1 - p.attack)
    aSize = n * p.attack
    
    TPR = numeric()
    FPR = numeric()
    
    for (j in 1:i) {
        set.seed(j); normal = sample(which(data$attack_type == "normal."), size = nSize, replace = FALSE)
        set.seed(j); attack = sample(which(data$attack_type != "normal."), size = aSize, replace = FALSE)
        ## testset excludes any categorical/binary features
        testset = data[c(normal, attack), c(1, 42)]
        
        res = evaluate(scores[, j], test = testset)
        TPR = c(TPR, res$detection.rate)
        FPR = c(FPR, res$false.alarm)
    }
    return(list(mean(TPR), mean(FPR)))
}

evaluate = function(score, e = 0.1, test) {
    
    # --------------
    # ARGUMENTS
    #     score: LOF scores
    #     e:     "generousness"/position of threshold
    #     test:  original testset with labels
    
    ## compute threshold
    threshold = threshold(score, e)
    
    ## floor() to be generous
    dr = sum(test$attack_type[score >= threshold] != "normal.") / sum(test$attack_type != "normal.")
    fa = sum(test$attack_type[score >= threshold] == "normal.") / sum(test$attack_type == "normal.")
    
    ret = list(threshold = threshold, detection.rate = dr, false.alarm = fa, detected = which(score >= threshold))
    return(ret[-4])
    
}
