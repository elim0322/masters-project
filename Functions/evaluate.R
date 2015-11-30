# --------------------------------------------------------------------
# Function to evaluate the detection and false alarm rates of Phase 1
# --------------------------------------------------------------------

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
