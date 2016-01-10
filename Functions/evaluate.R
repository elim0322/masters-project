# --------------------------------------------------------------------
# Function to evaluate the detection and false alarm rates of Phase 1
# --------------------------------------------------------------------
source("Functions/threshold.R")
library(scales)

p1.evaluation = function(data, n = 5000, i = 100, p.attack = 0.3, scores) {
    ## normal, attack and k
    nSize = n * (1 - p.attack)
    aSize = n * p.attack
    ## containers for loop
    TPR = numeric()
    FPR = numeric()
    for (j in 1:i) {
        cat(j)
        cat(" ")
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
    ## return
    ret = list(threshold = threshold, detection.rate = dr, false.alarm = fa, detected = which(score >= threshold))
    return(ret[-4])
}

## run
# res1 = p1.evaluation(dat1, p.attack = 0.05, scores = dat1_05a_05k)
# res2 = p1.evaluation(dat1, p.attack = 0.1, scores = dat1_10a_05k)
# res3 = p1.evaluation(dat1, p.attack = 0.2, scores = dat1_20a_05k)
# res4 = p1.evaluation(dat1, p.attack = 0.3, scores = dat1_30a_05k)
# res5 = p1.evaluation(dat1, p.attack = 0.4, scores = dat1_40a_05k)
# res6 = p1.evaluation(dat1, p.attack = 0.5, scores = dat1_50a_05k)
# p1.TPR.dat1.05k = list(TPR = c(res1[[1]], res2[[1]], res3[[1]], res4[[1]], res5[[1]]), FPR = c(res1[[2]], res2[[2]], res3[[2]], res4[[2]], res5[[2]]))
# res1 = p1.evaluation(dat1, p.attack = 0.05, scores = dat1_05a_10k)
# res2 = p1.evaluation(dat1, p.attack = 0.1, scores = dat1_10a_10k)
# res3 = p1.evaluation(dat1, p.attack = 0.2, scores = dat1_20a_10k)
# res4 = p1.evaluation(dat1, p.attack = 0.3, scores = dat1_30a_10k)
# res5 = p1.evaluation(dat1, p.attack = 0.4, scores = dat1_40a_10k)
# res6 = p1.evaluation(dat1, p.attack = 0.5, scores = dat1_50a_10k)
# p1.TPR.dat1.10k = list(TPR = c(res1[[1]], res2[[1]], res3[[1]], res4[[1]], res5[[1]]), FPR = c(res1[[2]], res2[[2]], res3[[2]], res4[[2]], res5[[2]]))
# res1 = p1.evaluation(dat1, p.attack = 0.05, scores = dat1_05a_30k)
# res2 = p1.evaluation(dat1, p.attack = 0.1, scores = dat1_10a_30k)
# res3 = p1.evaluation(dat1, p.attack = 0.2, scores = dat1_20a_30k)
# res4 = p1.evaluation(dat1, p.attack = 0.3, scores = dat1_30a_30k)
# res5 = p1.evaluation(dat1, p.attack = 0.4, scores = dat1_40a_30k)
# res6 = p1.evaluation(dat1, p.attack = 0.5, scores = dat1_50a_30k)
# p1.TPR.dat1.30k = list(TPR = c(res1[[1]], res2[[1]], res3[[1]], res4[[1]], res5[[1]]), FPR = c(res1[[2]], res2[[2]], res3[[2]], res4[[2]], res5[[2]]))
# res1 = p1.evaluation(dat1, p.attack = 0.05, scores = dat1_05a_40k)
# res2 = p1.evaluation(dat1, p.attack = 0.1, scores = dat1_10a_40k)
# res3 = p1.evaluation(dat1, p.attack = 0.2, scores = dat1_20a_40k)
# res4 = p1.evaluation(dat1, p.attack = 0.3, scores = dat1_30a_40k)
# res5 = p1.evaluation(dat1, p.attack = 0.4, scores = dat1_40a_40k)
# res6 = p1.evaluation(dat1, p.attack = 0.5, scores = dat1_50a_40k)
# p1.TPR.dat1.40k = list(TPR = c(res1[[1]], res2[[1]], res3[[1]], res4[[1]], res5[[1]]), FPR = c(res1[[2]], res2[[2]], res3[[2]], res4[[2]], res5[[2]]))
# res1 = p1.evaluation(dat1, p.attack = 0.05, scores = dat1_05a_50k)
# res2 = p1.evaluation(dat1, p.attack = 0.1, scores = dat1_10a_50k)
# res3 = p1.evaluation(dat1, p.attack = 0.2, scores = dat1_20a_50k)
# res4 = p1.evaluation(dat1, p.attack = 0.3, scores = dat1_30a_50k)
# res5 = p1.evaluation(dat1, p.attack = 0.4, scores = dat1_40a_50k)
# res6 = p1.evaluation(dat1, p.attack = 0.5, scores = dat1_50a_50k)
# p1.TPR.dat1.50k = list(TPR = c(res1[[1]], res2[[1]], res3[[1]], res4[[1]], res5[[1]]), FPR = c(res1[[2]], res2[[2]], res3[[2]], res4[[2]], res5[[2]]))
# 
# res1 = p1.evaluation(dat2, p.attack = 0.05, scores = dat2_05a_05k)
# res2 = p1.evaluation(dat2, p.attack = 0.1, scores = dat2_10a_05k)
# res3 = p1.evaluation(dat2, p.attack = 0.2, scores = dat2_20a_05k)
# res4 = p1.evaluation(dat2, p.attack = 0.3, scores = dat2_30a_05k)
# res5 = p1.evaluation(dat2, p.attack = 0.4, scores = dat2_40a_05k)
# res6 = p1.evaluation(dat2, p.attack = 0.5, scores = dat2_50a_05k)
# p1.TPR.dat2.05k = list(TPR = c(res1[[1]], res2[[1]], res3[[1]], res4[[1]], res5[[1]]), FPR = c(res1[[2]], res2[[2]], res3[[2]], res4[[2]], res5[[2]]))
# res1 = p1.evaluation(dat2, p.attack = 0.05, scores = dat2_05a_10k)
# res2 = p1.evaluation(dat2, p.attack = 0.1, scores = dat2_10a_10k)
# res3 = p1.evaluation(dat2, p.attack = 0.2, scores = dat2_20a_10k)
# res4 = p1.evaluation(dat2, p.attack = 0.3, scores = dat2_30a_10k)
# res5 = p1.evaluation(dat2, p.attack = 0.4, scores = dat2_40a_10k)
# res6 = p1.evaluation(dat2, p.attack = 0.5, scores = dat2_50a_10k)
# p1.TPR.dat2.10k = list(TPR = c(res1[[1]], res2[[1]], res3[[1]], res4[[1]], res5[[1]]), FPR = c(res1[[2]], res2[[2]], res3[[2]], res4[[2]], res5[[2]]))
# res1 = p1.evaluation(dat2, p.attack = 0.05, scores = dat2_05a_30k)
# res2 = p1.evaluation(dat2, p.attack = 0.1, scores = dat2_10a_30k)
# res3 = p1.evaluation(dat2, p.attack = 0.2, scores = dat2_20a_30k)
# res4 = p1.evaluation(dat2, p.attack = 0.3, scores = dat2_30a_30k)
# res5 = p1.evaluation(dat2, p.attack = 0.4, scores = dat2_40a_30k)
# res6 = p1.evaluation(dat2, p.attack = 0.5, scores = dat2_50a_30k)
# p1.TPR.dat2.30k = list(TPR = c(res1[[1]], res2[[1]], res3[[1]], res4[[1]], res5[[1]]), FPR = c(res1[[2]], res2[[2]], res3[[2]], res4[[2]], res5[[2]]))
# res1 = p1.evaluation(dat2, p.attack = 0.05, scores = dat2_05a_40k)
# res2 = p1.evaluation(dat2, p.attack = 0.1, scores = dat2_10a_40k)
# res3 = p1.evaluation(dat2, p.attack = 0.2, scores = dat2_20a_40k)
# res4 = p1.evaluation(dat2, p.attack = 0.3, scores = dat2_30a_40k)
# res5 = p1.evaluation(dat2, p.attack = 0.4, scores = dat2_40a_40k)
# res6 = p1.evaluation(dat2, p.attack = 0.5, scores = dat2_50a_40k)
# p1.TPR.dat2.40k = list(TPR = c(res1[[1]], res2[[1]], res3[[1]], res4[[1]], res5[[1]]), FPR = c(res1[[2]], res2[[2]], res3[[2]], res4[[2]], res5[[2]]))
# res1 = p1.evaluation(dat2, p.attack = 0.05, scores = dat2_05a_50k)
# res2 = p1.evaluation(dat2, p.attack = 0.1, scores = dat2_10a_50k)
# res3 = p1.evaluation(dat2, p.attack = 0.2, scores = dat2_20a_50k)
# res4 = p1.evaluation(dat2, p.attack = 0.3, scores = dat2_30a_50k)
# res5 = p1.evaluation(dat2, p.attack = 0.4, scores = dat2_40a_50k)
# res6 = p1.evaluation(dat2, p.attack = 0.5, scores = dat2_50a_50k)
# p1.TPR.dat2.50k = list(TPR = c(res1[[1]], res2[[1]], res3[[1]], res4[[1]], res5[[1]]), FPR = c(res1[[2]], res2[[2]], res3[[2]], res4[[2]], res5[[2]]))




