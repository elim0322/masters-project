phase1_eval = function(n = 100, p.attack = 0.3, k = 0.3, file) {
    
    nSize = 5000 * (1 - p.attack)
    aSize = 5000 * p.attack
    k   = k * 5000
    # res = matrix(nrow = 5000, ncol = n)
    for (i in 1:n) {
        invisible((gc(reset = TRUE)))
        
        cat(paste(i, "..."))
        
        set.seed(i); normal = sample(which(dat2$attack_type == "normal."), size = nSize, replace = FALSE)
        set.seed(i); attack = sample(which(dat2$attack_type != "normal."), size = aSize, replace = FALSE)
        testset  = dat2[c(normal, attack), -c(2,3,4,7,12,21,22)]
        result   = t(as.matrix(LOF(testset, control = Weka_control(min = k, max = k, "num-slots" = 2))[,"LOF"]))
        
        write.table(result, file = file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
        
        cat(paste0(" done.", "\n"))
    }
    
}

# phase1_eval(p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k-test.csv")

# phase1_eval(p.attack = 0.4, k = 0.1, file = "Writing/Evaluation/phase1_scores-40a-10k.csv")
# phase1_eval(p.attack = 0.4, k = 0.2, file = "Writing/Evaluation/phase1_scores-40a-20k.csv")
# phase1_eval(p.attack = 0.4, k = 0.3, file = "Writing/Evaluation/phase1_scores-40a-30k.csv")
# phase1_eval(p.attack = 0.4, k = 0.4, file = "Writing/Evaluation/phase1_scores-40a-40k.csv")
# 
# phase1_eval(p.attack = 0.3, k = 0.1, file = "Writing/Evaluation/phase1_scores-30a-10k.csv")
# phase1_eval(p.attack = 0.3, k = 0.2, file = "Writing/Evaluation/phase1_scores-30a-20k.csv")
# phase1_eval(p.attack = 0.3, k = 0.3, file = "Writing/Evaluation/phase1_scores-30a-30k.csv")
# phase1_eval(p.attack = 0.3, k = 0.4, file = "Writing/Evaluation/phase1_scores-30a-40k.csv")
# 
# phase1_eval(p.attack = 0.2, k = 0.1, file = "Writing/Evaluation/phase1_scores-20a-10k.csv")
# phase1_eval(p.attack = 0.2, k = 0.2, file = "Writing/Evaluation/phase1_scores-20a-20k.csv")
# phase1_eval(p.attack = 0.2, k = 0.3, file = "Writing/Evaluation/phase1_scores-20a-30k.csv")
# phase1_eval(p.attack = 0.2, k = 0.4, file = "Writing/Evaluation/phase1_scores-20a-40k.csv")
# 
# phase1_eval(p.attack = 0.1, k = 0.1, file = "Writing/Evaluation/phase1_scores-10a-10k.csv")
# phase1_eval(p.attack = 0.1, k = 0.2, file = "Writing/Evaluation/phase1_scores-10a-20k.csv")
# phase1_eval(p.attack = 0.1, k = 0.3, file = "Writing/Evaluation/phase1_scores-10a-30k.csv")
# phase1_eval(p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k.csv")

phase1_exp = function(data, p.attack = 0.3, k = 0.3, file) {
    
    nSize  = 5000 * (1 - p.attack)
    aSize  = 5000 * p.attack
    k      = k * 5000
    score  = read.csv(file = file, header = FALSE)
    result = matrix(nrow = 100, ncol = 3)
    
    for (i in 1:100) {
        invisible((gc(reset = TRUE)))
        
        cat(paste(i, "..."))
        
        set.seed(i); normal = sample(which(data$attack_type == "normal."), size = nSize, replace = FALSE)
        set.seed(i); attack = sample(which(data$attack_type != "normal."), size = aSize, replace = FALSE)
        testset  = data[c(normal, attack), -c(2,3,4,7,12,21,22)]
        
        res = evaluate(as.numeric(score[i, ]), testset)
        
        result[i, 1] = res$threshold
        result[i, 2] = res$detection.rate
        result[i, 3] = res$false.alarm
        
        cat(paste0(" done.", "\n"))
    }
    
    list(threshold        = c(mean(result[, 1]), sd(result[, 1])),
         detection.rate   = c(mean(result[, 2]), sd(result[, 2])),
         false.alarm.rate = c(mean(result[, 3]), sd(result[, 3])))
    
}

a = phase1_exp(dat, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k.csv")
b = phase1_exp(dat2, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k-test.csv")
# a10_10k = phase1_exp(p.attack = 0.1, k = 0.1, file = "Writing/Evaluation/phase1_scores-10a-10k.csv")
# a10_20k = phase1_exp(p.attack = 0.1, k = 0.2, file = "Writing/Evaluation/phase1_scores-10a-20k.csv")
# a10_30k = phase1_exp(p.attack = 0.1, k = 0.3, file = "Writing/Evaluation/phase1_scores-10a-30k.csv")
# a10_40k = phase1_exp(p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k.csv")
# 
# a20_10k = phase1_exp(p.attack = 0.2, k = 0.1, file = "Writing/Evaluation/phase1_scores-20a-10k.csv")
# a20_20k = phase1_exp(p.attack = 0.2, k = 0.2, file = "Writing/Evaluation/phase1_scores-20a-20k.csv")
# a20_30k = phase1_exp(p.attack = 0.2, k = 0.3, file = "Writing/Evaluation/phase1_scores-20a-30k.csv")
# a20_40k = phase1_exp(p.attack = 0.2, k = 0.4, file = "Writing/Evaluation/phase1_scores-20a-40k.csv")
# 
# a30_10k = phase1_exp(p.attack = 0.3, k = 0.1, file = "Writing/Evaluation/phase1_scores-30a-10k.csv")
# a30_20k = phase1_exp(p.attack = 0.3, k = 0.2, file = "Writing/Evaluation/phase1_scores-30a-20k.csv")
# a30_30k = phase1_exp(p.attack = 0.3, k = 0.3, file = "Writing/Evaluation/phase1_scores-30a-30k.csv")
# a30_40k = phase1_exp(p.attack = 0.3, k = 0.4, file = "Writing/Evaluation/phase1_scores-30a-40k.csv")
# 
# a40_10k = phase1_exp(p.attack = 0.4, k = 0.1, file = "Writing/Evaluation/phase1_scores-40a-10k.csv")
# a40_20k = phase1_exp(p.attack = 0.4, k = 0.2, file = "Writing/Evaluation/phase1_scores-40a-20k.csv")
# a40_30k = phase1_exp(p.attack = 0.4, k = 0.3, file = "Writing/Evaluation/phase1_scores-40a-30k.csv")
# a40_40k = phase1_exp(p.attack = 0.4, k = 0.4, file = "Writing/Evaluation/phase1_scores-40a-40k.csv")
# 
# save(a10_10k, a10_20k, a10_30k, a10_40k,
#      a20_10k, a20_20k, a20_30k, a20_40k,
#      a30_10k, a30_20k, a30_30k, a30_40k,
#      a40_10k, a40_20k, a40_30k, a40_40k,
#      file = "Writing/Evaluation/phase1_evaluation.RData")


















