results = list()
for (i in 1:20) {
    normal   = sample(which(dat$attack_type == "normal."), size = 7000, replace = FALSE)
    attack   = sample(which(dat$attack_type != "normal."), size = 3000, replace = FALSE)
    category = c(2,3,4,7,12,21,22)#,42)
    test     = dat[c(normal, attack), -category]; gc()
    test_lof_30p = LOF(test[,-35], control = Weka_control(min = 3000, max = 3000, "num-slots" = 1))[,"LOF"]; gc()
    names(test_lof_30p) = test$attack_type
    results[[i]] = lof_exp(train.df, test_lof_30p); gc()
}
save(results, file="lof_results.RData")





lof_exp = function(train, test) {
    at1p = c(
        sum(names(test[test > quantile(sort(train[,1], decreasing = TRUE)[51:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,2], decreasing = TRUE)[51:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,3], decreasing = TRUE)[51:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,4], decreasing = TRUE)[51:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,5], decreasing = TRUE)[51:5000], probs = 0.9)]) != "normal.") / 3000
    )
    
    at2p = c(
        sum(names(test[test > quantile(sort(train[,1], decreasing = TRUE)[101:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,2], decreasing = TRUE)[101:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,3], decreasing = TRUE)[101:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,4], decreasing = TRUE)[101:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,5], decreasing = TRUE)[101:5000], probs = 0.9)]) != "normal.") / 3000
    )
    
    at4p = c(
        sum(names(test[test > quantile(sort(train[,1], decreasing = TRUE)[201:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,2], decreasing = TRUE)[201:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,3], decreasing = TRUE)[201:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,4], decreasing = TRUE)[201:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,5], decreasing = TRUE)[201:5000], probs = 0.9)]) != "normal.") / 3000
    )
    
    at6p = c(
        sum(names(test[test > quantile(sort(train[,1], decreasing = TRUE)[301:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,2], decreasing = TRUE)[301:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,3], decreasing = TRUE)[301:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,4], decreasing = TRUE)[301:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,5], decreasing = TRUE)[301:5000], probs = 0.9)]) != "normal.") / 3000
    )
    
    at8p = c(
        sum(names(test[test > quantile(sort(train[,1], decreasing = TRUE)[401:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,2], decreasing = TRUE)[401:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,3], decreasing = TRUE)[401:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,4], decreasing = TRUE)[401:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,5], decreasing = TRUE)[401:5000], probs = 0.9)]) != "normal.") / 3000
    )
    
    at10p = c(
        sum(names(test[test > quantile(sort(train[,1], decreasing = TRUE)[501:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,2], decreasing = TRUE)[501:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,3], decreasing = TRUE)[501:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,4], decreasing = TRUE)[501:5000], probs = 0.9)]) != "normal.") / 3000,
        sum(names(test[test > quantile(sort(train[,5], decreasing = TRUE)[501:5000], probs = 0.9)]) != "normal.") / 3000
    )
    
    means  = sapply(list(at1p, at2p, at4p, at6p, at8p, at10p), function(x) mean(x))
    stdevs = sapply(list(at1p, at2p, at4p, at6p, at8p, at10p), function(x) sd(x))
    
    return(rbind(round(means, digits = 4), round(stdevs, digits = 4)))
}

lof_experiment = function() {
    
    # ======
    # train
    # ======
    repeat {
        sample1 = sys.sample(sum(dat$attack_type == "normal."), 5000)
        sample2 = sys.sample(sum(dat$attack_type == "normal."), 5000)
        sample3 = sys.sample(sum(dat$attack_type == "normal."), 5000)
        sample4 = sys.sample(sum(dat$attack_type == "normal."), 5000)
        sample5 = sys.sample(sum(dat$attack_type == "normal."), 5000)
        if (all(!duplicated(
            c(sample1[1], sample2[1], sample3[1], sample4[1], sample5[1])))
        ) break
    }
    
    train1 = dat[which(dat$attack_type == "normal.")[sample1], -c(2,3,4,7,12,21,22,42)]
    train2 = dat[which(dat$attack_type == "normal.")[sample2], -c(2,3,4,7,12,21,22,42)]
    train3 = dat[which(dat$attack_type == "normal.")[sample3], -c(2,3,4,7,12,21,22,42)]
    train4 = dat[which(dat$attack_type == "normal.")[sample4], -c(2,3,4,7,12,21,22,42)]
    train5 = dat[which(dat$attack_type == "normal.")[sample5], -c(2,3,4,7,12,21,22,42)]
    
    ## using %k = 30
    cat(paste("start trainset...", "\n"))
    train1_lof = LOF(train1, control = Weka_control(min = 1500, max = 1500, "num-slots" = 1))[,"LOF"]
    train2_lof = LOF(train2, control = Weka_control(min = 1500, max = 1500, "num-slots" = 1))[,"LOF"]
    train3_lof = LOF(train3, control = Weka_control(min = 1500, max = 1500, "num-slots" = 1))[,"LOF"]
    train4_lof = LOF(train4, control = Weka_control(min = 1500, max = 1500, "num-slots" = 1))[,"LOF"]
    train5_lof = LOF(train5, control = Weka_control(min = 1500, max = 1500, "num-slots" = 1))[,"LOF"]
    cat(paste("done.", "\n"))
        
    # ======
    # test
    # ======
    result = matrix(nrow = 6, ncol = 20)
    for (i in 1:1) {
        cat(paste0("\n", "starting iteration[", i, "]", "\n"))
        
        normal   = sample(which(dat$attack_type == "normal."), size = 7000, replace = FALSE)
        attack   = sample(which(dat$attack_type != "normal."), size = 3000, replace = FALSE)
        category = c(2,3,4,7,12,21,22)#,42)
        test     = dat[c(normal, attack), -category]
        
        cat(paste("start testset...", "\n"))
        test_lof = LOF(test[,-35], control = Weka_control(min = 3000, max = 3000, "num-slots" = 1))[,"LOF"]
        cat(paste("done.", "\n"))
        
        names(test_lof) = test$attack_type
        
        ## varying the false alarm rate from 1% to 10%
        at1p = c(
            sum(names(test_lof[test_lof > quantile(sort(train1_lof, decreasing = TRUE)[51:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train2_lof, decreasing = TRUE)[51:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train3_lof, decreasing = TRUE)[51:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train4_lof, decreasing = TRUE)[51:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train5_lof, decreasing = TRUE)[51:5000], probs = 0.9)]) != "normal.") / 3000
        )
        
        at2p = c(
            sum(names(test_lof[test_lof > quantile(sort(train1_lof, decreasing = TRUE)[101:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train2_lof, decreasing = TRUE)[101:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train3_lof, decreasing = TRUE)[101:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train4_lof, decreasing = TRUE)[101:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train5_lof, decreasing = TRUE)[101:5000], probs = 0.9)]) != "normal.") / 3000
        )
        
        at4p = c(
            sum(names(test_lof[test_lof > quantile(sort(train1_lof, decreasing = TRUE)[201:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train2_lof, decreasing = TRUE)[201:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train3_lof, decreasing = TRUE)[201:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train4_lof, decreasing = TRUE)[201:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train5_lof, decreasing = TRUE)[201:5000], probs = 0.9)]) != "normal.") / 3000
        )
        
        at6p = c(
            sum(names(test_lof[test_lof > quantile(sort(train1_lof, decreasing = TRUE)[301:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train2_lof, decreasing = TRUE)[301:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train3_lof, decreasing = TRUE)[301:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train4_lof, decreasing = TRUE)[301:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train5_lof, decreasing = TRUE)[301:5000], probs = 0.9)]) != "normal.") / 3000
        )
        
        at8p = c(
            sum(names(test_lof[test_lof > quantile(sort(train1_lof, decreasing = TRUE)[401:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train2_lof, decreasing = TRUE)[401:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train3_lof, decreasing = TRUE)[401:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train4_lof, decreasing = TRUE)[401:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train5_lof, decreasing = TRUE)[401:5000], probs = 0.9)]) != "normal.") / 3000
        )
        
        at10p = c(
            sum(names(test_lof[test_lof > quantile(sort(train1_lof, decreasing = TRUE)[501:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train2_lof, decreasing = TRUE)[501:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train3_lof, decreasing = TRUE)[501:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train4_lof, decreasing = TRUE)[501:5000], probs = 0.9)]) != "normal.") / 3000,
            sum(names(test_lof[test_lof > quantile(sort(train5_lof, decreasing = TRUE)[501:5000], probs = 0.9)]) != "normal.") / 3000
        )
        
        result[1:6, i] = sapply(list(at1p, at2p, at4p, at6p, at8p, at10p), function(x) mean(x))
        result[1:6, (i+1)] = sapply(list(at1p, at2p, at4p, at6p, at8p, at10p), function(x) sd(x))
        
        cat(paste0("finished iteration[", i, "]"))
        
    }
    
    return(result)
    
}


