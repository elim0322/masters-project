source("Functions/threshold.R")
source("Functions/evaluate.R")

# ------------------------------------------------------------------
# Function to read files of LOF scores and evaluate Phase 1 process
# ------------------------------------------------------------------

runSystem = function(data, n = 5000, i = 100, p.attack = 0.3, p.k = 0.3, score) {
    
    # -----------------------------------------------
    # ARGUMENTS
    #     data:     dataset
    #     n:        sample size
    #     i:        number of seeds/runs/experiments
    #     p.attack: percentage/ratio of attacks
    #     p.k:      percentage k
    #     file:     save directory
    # -----------------------------------------------
    
    ## Function to process output to generate results
    generateResults = function(dbscan.obj, nSize, aSize) {
        # tab = sapply(0:max(dbscan.obj$class_ids, na.rm = TRUE), function(ind) {
        #     table(Mset[dbscan.obj$class_ids == ind, "attack_type"])
        # })
        # normalCluster  = which.max(apply(tab, 2, function(x) sum(x)))
        
        normalCluster = which.max(table(dbscan.obj$class_ids))
        clusters = 0:max(dbscan.obj$class_ids, na.rm = TRUE)
        
        wc = sapply(clusters, function(ind) {
            mean(rownames(Mset[dbscan.obj$class_ids == ind, ]) %in% rownames(Mset)[1:1000])
        })
        ws = sapply(clusters, function(ind) {
            mean(rownames(Mset)[1:1000] %in% rownames(Mset[dbscan.obj$class_ids == ind, ]))
        })
        detection.rate = 1/aSize * sum(sapply(clusters[-normalCluster], function(ind) {
            sum(Mset[dbscan.obj$class_ids == ind, "attack_type"] != "normal.", na.rm = TRUE)
        }))
        
        FP.rate = 1/nSize * sum(sapply(clusters[-normalCluster], function(ind) {
            sum(Mset[dbscan.obj$class_ids == ind, "attack_type"] == "normal.", na.rm = TRUE)
        }))
        
        
        # detection.rate = 1/aSize * sum(unlist(apply(tab[,-normalCluster], 2, function(x) x[names(x)!="normal."])), na.rm = TRUE)
        # FP.rate        = 1/nSize * sum(unlist(apply(tab[,-normalCluster], 2, function(x) x["normal."])),           na.rm = TRUE)
        
        return(list("P.wc" = wc[normalCluster], "P.ws" = ws[normalCluster], "D.rate" = detection.rate, "FP.rate" = FP.rate))
    }
    
    ## normal, attack and k
    nSize = n * (1 - p.attack)
    aSize = n * p.attack
    k     = n * p.k
    
    ## read files of scores and set up a matrix to store results
    # score = as.matrix(read.csv(file = file, header = FALSE))
    
    ## vectors to store phase 1 & 2 results
    p1.t        = numeric(i)
    p1.d        = numeric(i)
    p1.f        = numeric(i)
    p2.p.avg    = numeric(i)
    p2.p.sd     = numeric(i)
    p2.ncluster = numeric(i)
    p2.noise    = numeric(i)
    p2.pwc = matrix(nrow = i, ncol = 10)
    p2.pws = matrix(nrow = i, ncol = 10)
    sys.d  = matrix(nrow = i, ncol = 10)
    sys.f  = matrix(nrow = i, ncol = 10)
    
    # p2.pwc.avg = matrix(nrow = i, ncol = 10)
    # p2.pws.avg = matrix(nrow = i, ncol = 10)
    # sys.d.avg  = matrix(nrow = i, ncol = 10)
    # sys.f.avg  = matrix(nrow = i, ncol = 10)
    # p2.pwc.sd  = matrix(nrow = i, ncol = 10)
    # p2.pws.sd  = matrix(nrow = i, ncol = 10)
    # sys.d.sd   = matrix(nrow = i, ncol = 10)
    # sys.f.sd   = matrix(nrow = i, ncol = 10)
    
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
        
        #################
        ## PHASE 1 START
        #################
        ## Phase 1 results
        p1.res = evaluate(score[, j], test = testset)
        
        ## partition into possible anomalies (PA) and non-intrusions (NI)
        PAset = which(score[, j] >= p1.res$threshold)
        NIset = which(score[, j] <  p1.res$threshold)
        
        ## store Phase 1 results
        p1.t[j] = p1.res$threshold
        p1.d[j] = p1.res$detection.rate
        p1.f[j] = p1.res$false.alarm
        
        ## random sample from NI set
        NIset.random = sample(NIset, size = 1000, replace = FALSE) # need to implement sample size computation
        
        ## merge the random sample with PA set
        Mset = testset[c(NIset.random, PAset), ]
        
        
        #################
        ## PHASE 2 START
        #################
        ## run DBSCAN on different epsilon (without the "attack_type" feature)
        dbscan.res0.1 = DBScan(Mset[, -35], control =  Weka_control(E = 0.1))
        dbscan.res0.2 = DBScan(Mset[, -35], control =  Weka_control(E = 0.2))
        dbscan.res0.3 = DBScan(Mset[, -35], control =  Weka_control(E = 0.3))
        dbscan.res0.4 = DBScan(Mset[, -35], control =  Weka_control(E = 0.4))
        dbscan.res0.5 = DBScan(Mset[, -35], control =  Weka_control(E = 0.5))
        dbscan.res0.6 = DBScan(Mset[, -35], control =  Weka_control(E = 0.6))
        dbscan.res0.7 = DBScan(Mset[, -35], control =  Weka_control(E = 0.7))
        dbscan.res0.8 = DBScan(Mset[, -35], control =  Weka_control(E = 0.8))
        dbscan.res0.9 = DBScan(Mset[, -35], control =  Weka_control(E = 0.9))
        dbscan.res1.0 = DBScan(Mset[, -35], control =  Weka_control(E = 1.0))
        
        ## generate results
        res0.1 = generateResults(dbscan.res0.1, nSize, aSize)
        res0.2 = generateResults(dbscan.res0.2, nSize, aSize)
        res0.3 = generateResults(dbscan.res0.3, nSize, aSize)
        res0.4 = generateResults(dbscan.res0.4, nSize, aSize)
        res0.5 = generateResults(dbscan.res0.5, nSize, aSize)
        res0.6 = generateResults(dbscan.res0.6, nSize, aSize)
        res0.7 = generateResults(dbscan.res0.7, nSize, aSize)
        res0.8 = generateResults(dbscan.res0.8, nSize, aSize)
        res0.9 = generateResults(dbscan.res0.9, nSize, aSize)
        res1.0 = generateResults(dbscan.res1.0, nSize, aSize)
        res.list = list(res0.1, res0.2, res0.3, res0.4, res0.5, res0.6, res0.7, res0.8, res0.9, res1.0)
        
        ## store results
        p2.pwc[j, ] = sapply(res.list, function(x) x$P.wc)
        p2.pws[j, ] = sapply(res.list, function(x) x$P.ws)
        sys.d[j, ]  = sapply(res.list, function(x) x$D.rate)
        sys.f[j, ]  = sapply(res.list, function(x) x$FP.rate)
        
    }
    
        ## store results
#         p2.pwc.avg[j, ] = colMeans(pwc)
#         p2.pws.avg[j, ] = colMeans(pws)
#         sys.d.avg[j, ]  = colMeans(d)
#         sys.f.avg[j, ]  = colMeans(f)
#         
#         p2.pwc.sd[j, ] = apply(pwc, 2, function(x) sd(x))
#         p2.pws.sd[j, ] = apply(pws, 2, function(x) sd(x))
#         sys.d.sd[j, ]  = apply(d,   2, function(x) sd(x))
#         sys.f.sd[j, ]  = apply(f,   2, function(x) sd(x))
        
#         result = t(c(
#             colMeans(pwc), colMeans(pws), colMeans(d), colMeans(f), 
#             apply(pwc, 2, function(x) sd(x)), apply(pws, 2, function(x) sd(x)), apply(d, 2, function(x) sd(x)), apply(f, 2, function(x) sd(x))
#         ))
#         
#         ## write result into file (by row)
#         write.table(result, file = outfile, append = TRUE, row.names = FALSE, sep = ",",  col.names = FALSE)
#         
        cat("\n")
        
#         ##purity
#         purity = sapply(cluster.seq, function(k) max(table(Mset[dbscan.res$class_ids == k, "attack_type"])) / sum(dbscan.res$class_ids == k, na.rm = TRUE))
#         
#         ## store Phase 2 evaluation
#         p2.p.avg[j]    = mean(purity)
#         p2.p.sd[j]     = sd(purity)
#         p2.ncluster[j] = length(cluster.ids)
#         p2.noise[j]    = sum(is.na(dbscan.res$class_ids))
#         
#         ## eliminate the members of the normal cluster
#         detected.final = Mset$attack_type[-which(dbscan.res$class_ids == (normal.clust - 1))]
#         
#         ## store overall system results
#         sys.d[j] = sum(detected.final != "normal.") / aSize
#         sys.f[j] = sum(detected.final == "normal.") / nSize
    
    
    ret = list(
        p1.t, p1.d, p1.f, 
        p2.pwc, p2.pws, sys.d, sys.f
    )
    
    names(ret) = c(
        "P1.threshold", "P1.detection.rate", "P1.false.alarm.rate",
        "p2.proportion.within.cluster", "p2.proportion.within.sample", "system.detection.rate", "system.false.alarm.rate"
    )
    
    return(ret)
    
}

# ## run:
# load(file = "Data/LOF_scores/scores.RData")
# ## 5% attack
# system.results_dat1_05a05k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.05, p.k = 0.05, score = dat1_05a_05k)
# system.results_dat1_05a10k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.05, p.k = 0.1,  score = dat1_05a_10k)
# system.results_dat1_05a20k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.05, p.k = 0.2,  score = dat1_05a_20k)
# system.results_dat1_05a30k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.05, p.k = 0.3,  score = dat1_05a_30k)
# system.results_dat1_05a40k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.05, p.k = 0.4,  score = dat1_05a_40k)
# system.results_dat1_05a50k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.05, p.k = 0.5,  score = dat1_05a_50k)
# save(system.results_dat1_05a05k, system.results_dat1_05a10k, system.results_dat1_05a20k,
#      system.results_dat1_05a30k, system.results_dat1_05a40k, system.results_dat1_05a50k, 
#      file = "Data/System_results/dat1_05a.RData")
# ## 10% attack
# system.results_dat1_10a05k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.05, score = dat1_10a_05k)
# system.results_dat1_10a10k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.1,  score = dat1_10a_10k)
# system.results_dat1_10a20k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.2,  score = dat1_10a_20k)
# system.results_dat1_10a30k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.3,  score = dat1_10a_30k)
# system.results_dat1_10a40k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.4,  score = dat1_10a_40k)
# system.results_dat1_10a50k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.5,  score = dat1_10a_50k)
# save(system.results_dat1_10a05k, system.results_dat1_10a10k, system.results_dat1_10a20k,
#      system.results_dat1_10a30k, system.results_dat1_10a40k, system.results_dat1_10a50k,
#      file = "Data/System_results/dat1_10a.RData")
# ## 20% attack
# system.results_dat1_20a05k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.05, score = dat1_20a_05k)
# system.results_dat1_20a10k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.1,  score = dat1_20a_10k)
# system.results_dat1_20a20k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.2,  score = dat1_20a_20k)
# system.results_dat1_20a30k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.3,  score = dat1_20a_30k)
# system.results_dat1_20a40k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.4,  score = dat1_20a_40k)
# system.results_dat1_20a50k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.5,  score = dat1_20a_50k)
# save(system.results_dat1_20a05k, system.results_dat1_20a10k, system.results_dat1_20a20k,
#      system.results_dat1_20a30k, system.results_dat1_20a40k, system.results_dat1_20a50k,
#      file = "Data/System_results/dat1_20a.RData")
# ## 30% attack
# system.results_dat1_30a05k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.05, score = dat1_30a_05k)
# system.results_dat1_30a10k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.1,  score = dat1_30a_10k)
# system.results_dat1_30a20k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.2,  score = dat1_30a_20k)
# system.results_dat1_30a30k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.3,  score = dat1_30a_30k)
# system.results_dat1_30a40k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.4,  score = dat1_30a_40k)
# system.results_dat1_30a50k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.5,  score = dat1_30a_50k)
# save(system.results_dat1_30a05k, system.results_dat1_30a10k, system.results_dat1_30a20k,
#      system.results_dat1_30a30k, system.results_dat1_30a40k, system.results_dat1_30a50k,
#      file = "Data/System_results/dat1_30a.RData")
# ## 40% attack
# system.results_dat1_40a05k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.05, score = dat1_40a_05k)
# system.results_dat1_40a10k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.1,  score = dat1_40a_10k)
# system.results_dat1_40a20k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.2,  score = dat1_40a_20k)
# system.results_dat1_40a30k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.3,  score = dat1_40a_30k)
# system.results_dat1_40a40k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.4,  score = dat1_40a_40k)
# system.results_dat1_40a50k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.5,  score = dat1_40a_50k)
# save(system.results_dat1_40a05k, system.results_dat1_40a10k, system.results_dat1_40a20k,
#      system.results_dat1_40a30k, system.results_dat1_40a40k, system.results_dat1_40a50k,
#      file = "Data/System_results/dat1_40a.RData")
# ## 50% attack
# system.results_dat1_50a05k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.5, p.k = 0.05, score = dat1_50a_05k)
# system.results_dat1_50a10k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.5, p.k = 0.1,  score = dat1_50a_10k)
# system.results_dat1_50a20k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.5, p.k = 0.2,  score = dat1_50a_20k)
# system.results_dat1_50a30k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.5, p.k = 0.3,  score = dat1_50a_30k)
# system.results_dat1_50a40k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.5, p.k = 0.4,  score = dat1_50a_40k)
# system.results_dat1_50a50k = runSystem(dat1, n = 5000, i = 100, p.attack = 0.5, p.k = 0.5,  score = dat1_50a_50k)
# save(system.results_dat1_50a05k, system.results_dat1_50a10k, system.results_dat1_50a20k,
#      system.results_dat1_50a30k, system.results_dat1_50a40k, system.results_dat1_50a50k,
#      file = "Data/System_results/dat1_50a.RData")





## 5% attack
# system.results_dat2_05a05k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.05, p.k = 0.05, score = dat2_05a_05k)
# system.results_dat2_05a10k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.05, p.k = 0.1,  score = dat2_05a_10k)
# system.results_dat2_05a20k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.05, p.k = 0.2,  score = dat2_05a_20k)
# system.results_dat2_05a30k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.05, p.k = 0.3,  score = dat2_05a_30k)
# system.results_dat2_05a40k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.05, p.k = 0.4,  score = dat2_05a_40k)
# system.results_dat2_05a50k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.05, p.k = 0.5,  score = dat2_05a_50k)
# save(system.results_dat2_05a05k, system.results_dat2_05a10k, system.results_dat2_05a20k,
#      system.results_dat2_05a30k, system.results_dat2_05a40k, system.results_dat2_05a50k, 
#      file = "Data/System_results/dat2_05a.RData")
# ## 10% attack
# system.results_dat2_10a05k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.1, p.k = 0.05, score = dat2_10a_05k)
# system.results_dat2_10a10k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.1, p.k = 0.1,  score = dat2_10a_10k)
# system.results_dat2_10a20k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.1, p.k = 0.2,  score = dat2_10a_20k)
# system.results_dat2_10a30k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.1, p.k = 0.3,  score = dat2_10a_30k)
# system.results_dat2_10a40k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.1, p.k = 0.4,  score = dat2_10a_40k)
# system.results_dat2_10a50k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.1, p.k = 0.5,  score = dat2_10a_50k)
# save(system.results_dat2_10a05k, system.results_dat2_10a10k, system.results_dat2_10a20k,
#      system.results_dat2_10a30k, system.results_dat2_10a40k, system.results_dat2_10a50k,
#      file = "Data/System_results/dat2_10a.RData")
# ## 20% attack
# system.results_dat2_20a05k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.2, p.k = 0.05, score = dat2_20a_05k)
# system.results_dat2_20a10k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.2, p.k = 0.1,  score = dat2_20a_10k)
# system.results_dat2_20a20k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.2, p.k = 0.2,  score = dat2_20a_20k)
# system.results_dat2_20a30k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.2, p.k = 0.3,  score = dat2_20a_30k)
# system.results_dat2_20a40k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.2, p.k = 0.4,  score = dat2_20a_40k)
# system.results_dat2_20a50k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.2, p.k = 0.5,  score = dat2_20a_50k)
# save(system.results_dat2_20a05k, system.results_dat2_20a10k, system.results_dat2_20a20k,
#      system.results_dat2_20a30k, system.results_dat2_20a40k, system.results_dat2_20a50k,
#      file = "Data/System_results/dat2_20a.RData")
# ## 30% attack
# system.results_dat2_30a05k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.3, p.k = 0.05, score = dat2_30a_05k)
# system.results_dat2_30a10k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.3, p.k = 0.1,  score = dat2_30a_10k)
# system.results_dat2_30a20k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.3, p.k = 0.2,  score = dat2_30a_20k)
# system.results_dat2_30a30k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.3, p.k = 0.3,  score = dat2_30a_30k)
# system.results_dat2_30a40k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.3, p.k = 0.4,  score = dat2_30a_40k)
# system.results_dat2_30a50k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.3, p.k = 0.5,  score = dat2_30a_50k)
# save(system.results_dat2_30a05k, system.results_dat2_30a10k, system.results_dat2_30a20k,
#      system.results_dat2_30a30k, system.results_dat2_30a40k, system.results_dat2_30a50k,
#      file = "Data/System_results/dat2_30a.RData")
# ## 40% attack
# system.results_dat2_40a05k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.4, p.k = 0.05, score = dat2_40a_05k)
# system.results_dat2_40a10k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.4, p.k = 0.1,  score = dat2_40a_10k)
# system.results_dat2_40a20k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.4, p.k = 0.2,  score = dat2_40a_20k)
# system.results_dat2_40a30k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.4, p.k = 0.3,  score = dat2_40a_30k)
# system.results_dat2_40a40k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.4, p.k = 0.4,  score = dat2_40a_40k)
# system.results_dat2_40a50k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.4, p.k = 0.5,  score = dat2_40a_50k)
# save(system.results_dat2_40a05k, system.results_dat2_40a10k, system.results_dat2_40a20k,
#      system.results_dat2_40a30k, system.results_dat2_40a40k, system.results_dat2_40a50k,
#      file = "Data/System_results/dat2_40a.RData")
# ## 50% attack
# system.results_dat2_50a05k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.5, p.k = 0.05, score = dat2_50a_05k)
# system.results_dat2_50a10k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.5, p.k = 0.1,  score = dat2_50a_10k)
# system.results_dat2_50a20k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.5, p.k = 0.2,  score = dat2_50a_20k)
# system.results_dat2_50a30k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.5, p.k = 0.3,  score = dat2_50a_30k)
# system.results_dat2_50a40k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.5, p.k = 0.4,  score = dat2_50a_40k)
# system.results_dat2_50a50k = runSystem(dat2, n = 5000, i = 100, p.attack = 0.5, p.k = 0.5,  score = dat2_50a_50k)
# save(system.results_dat2_50a05k, system.results_dat2_50a10k, system.results_dat2_50a20k,
#      system.results_dat2_50a30k, system.results_dat2_50a40k, system.results_dat2_50a50k,
#      file = "Data/System_results/dat2_50a.RData")



system.results_dat2_40


















# a1 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.1, file = "Writing/Evaluation/phase1_scores-10a-10k.csv")
# a2 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.2, file = "Writing/Evaluation/phase1_scores-10a-20k.csv")
# a3 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.3, file = "Functions/data/dat1_10a_30k.csv")
# a4 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k.csv")
# 
# b1 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.1, file = "Writing/Evaluation/phase1_scores-20a-10k.csv")
# b2 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.2, file = "Writing/Evaluation/phase1_scores-20a-20k.csv")
# b3 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.3, file = "Writing/Evaluation/phase1_scores-20a-30k.csv")
# b4 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.4, file = "Writing/Evaluation/phase1_scores-20a-40k.csv")
# 
# c1 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.1, file = "Writing/Evaluation/phase1_scores-30a-10k.csv")
# c2 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.2, file = "Writing/Evaluation/phase1_scores-30a-20k.csv")
# c3 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.3, file = "Writing/Evaluation/phase1_scores-30a-30k.csv")
# c4 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.4, file = "Writing/Evaluation/phase1_scores-30a-40k.csv")
# 
# d1 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.1, file = "Writing/Evaluation/phase1_scores-40a-10k.csv")
# d2 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.2, file = "Writing/Evaluation/phase1_scores-40a-20k.csv")
# d3 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.3, file = "Writing/Evaluation/phase1_scores-40a-30k.csv")
# d4 = phase1Eval(dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.4, file = "Writing/Evaluation/phase1_scores-40a-40k.csv")








## density plot
# scores = as.matrix(read.csv("Functions/data/dat1_20a_20k.csv", header = FALSE))
# plot(density(scores[1, ]), col = alpha("black", 0.1))
# invisible(lapply(2:nrow(scores), function(i) lines(density(scores[i, ]), col = alpha("black", 0.1))))

# scores = as.matrix(read.csv("Writing/Evaluation/phase1_scores-10a-10k.csv"))
# plot(density(scores[2, ]))
# threshold(scores[2, ])







