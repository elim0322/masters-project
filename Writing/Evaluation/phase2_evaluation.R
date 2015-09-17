source("evaluation.R")
source("kmeans.R")

phase2_exp = function(data, method,
                      n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k.csv") {
    
    nSize  = 5000 * (1 - p.attack)
    aSize  = 5000 * p.attack
    k      = k * 5000
    score  = as.matrix(read.csv(file = file, header = FALSE))
    
    #purity      = matrix(nrow = n, ncol = 4)
    #size        = matrix(nrow = n, ncol = 4)
    #ncluster    = numeric(n)
    #d           = matrix(nrow = n, ncol = 4)
    #max_class   = matrix(nrow = n, ncol = 4)
    detection   = numeric(n)
    false.alarm = numeric(n)
    
    for (i in 1:n) {
        invisible((gc(reset = TRUE)))
        
        cat(paste(i, "..."))
        
        set.seed(i); normal = sample(which(data$attack_type == "normal."), size = nSize, replace = FALSE)
        set.seed(i); attack = sample(which(data$attack_type != "normal."), size = aSize, replace = FALSE)
        testset  = data[c(normal, attack), -c(2,3,4,7,12,21,22)]
        
        if (method == 1) {
            p1.threshold = threshold(score[i, ])
            PAset = which(score[i, ] >= p1.threshold)
            NIset = which(score[i, ] < p1.threshold)
            NIset.random = sample(NIset, size = 1000, replace = FALSE)
            detected.df = testset[c(PAset, NIset.random), ]
            dbscan.res = DBScan(detected.df)
            cluster.ids = unique(dbscan.res$class_ids)
            cluster.seq = seq(min(cluster.ids, na.rm = TRUE), max(cluster.ids, na.rm = TRUE))
            normal.clust = which(sapply(cluster.seq, function(j) nrow(detected.df[dbscan.res$class_ids == j, ])) >= 1000)
            
        } else {
            p1.res = experiment(score[i, ])
            detected.df = testset[which(score[i, ] >= p1.res$threshold), ]
            dbscan.res = DBScan(detected.df)
            cluster.ids = unique(dbscan.res$class_ids)
            cluster.seq = seq(min(cluster.ids, na.rm = TRUE), max(cluster.ids, na.rm = TRUE))
            normal.clust = which(sapply(cluster.seq, function(j) nrow(detected.df[dbscan.res$class_ids == j, ])) >= p1.res$n)
            
        }
        
        # p1.result            = experiment(as.numeric(score[i, ]), testset)
        # detected.df          = testset[p1.result$detected, ]
        {
        # require(FactoMineR)
        # pca.res = PCA(detected.df[, -35], graph = FALSE)
        # pca.res = as.data.frame(cbind(pca.res$ind$coord[, 1:2], detected.df$attack_type))
        # colnames(pca.res)[3] = "attack_type"
        # xmeans.res = xmeans1(pca.res, "normalise")
        
        
        # xmeans.res           = xmeans1(detected.df, "normalise")
        # centers.detected     = xmeans.res$center.matrix
        
        # purity[i, 1:length(xmeans.res$purity)] = xmeans.res$purity
        # classes = unlist(lapply(xmeans.res$table, function(x) names(which.max(x))))
        # max_class[i, 1:length(classes)] = classes
        # size[i, 1:length(xmeans.res$size)] = xmeans.res$size
        # ncluster[i]    = length(xmeans.res$purity)
        
        # normal.df      = preproc(testset[-c(p1.result$detected), ], "normalise")
        # centers.normal = colMeans(normal.df[, names(normal.df)!="attack_type"])
        # centers.normal = centers.normal[which(names(centers.normal) %in% rownames(centers.detected))]
        # centers.normal = colMeans(testset[-c(p1.result$detected), -35])
        }
        # pca.res    = PCA(detected.df[, -35], graph = FALSE, ncp = 2)
        
        detected.final = detected.df$attack_type[-which(dbscan.res$class_ids == (normal.clust - 1))]
        
        detection[i]   = sum(detected.final != "normal.") / aSize
        false.alarm[i] = sum(detected.final == "normal.") / nSize
#         dist = numeric(length(unique(dbscan.res$class_ids)) -1)
#         for (j in 0:(length(unique(dbscan.res$class_ids)) - 2)) {
#             #dist[j+1] = mean(apply(pca.res$ind$coord[which(dbscan.res$class_ids == j), ], 1, function(x) sum(x^2)))
#             dist[j+1] = sum(dbscan.res$class_ids == j, na.rm = TRUE)
#         }
        
        # dist = apply(centers.detected, 2, function(x) sqrt(sum((x-centers.normal)^2)))
        # d[i, 1:length(dist)] = dist
        
        # normal.clust = which.min(dist)
#         # normal.ind = which(xmeans.res$class_ids == normal.clust)
#         # normal.clust = which(dist >= p1.result$n)
#         normal.ind = which(dbscan.res$class_ids == (normal.clust - 1))
#         detected.final = detected.df$attack_type[-normal.ind]
#         
#         detection[i]   = sum(detected.final != "normal.") / aSize
#         false.alarm[i] = sum(detected.final == "normal.") / nSize
        
        cat(paste0(" done.", "\n"))
    }
    
    # list(max_class = max_class, purity = purity, size = size, ncluster = ncluster, distances = d, detection.rate = detection, false.alarm.rate = false.alarm)
    return(list(detection, false.alarm))
}

# a1 = phase2_exp(dat, method = 1, n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k-test.csv")
# a2 = phase2_exp(dat, method = 1, n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k-test.csv")
# a3 = phase2_exp(dat, method = 1, n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k-test.csv")
# b = phase2_exp(dat, method = 2, n = 1, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k-test.csv")

# c1 = phase2_exp(dat2, method = 1, n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k-test.csv")
# c2 = phase2_exp(dat2, method = 1, n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k-test.csv")
# c3 = phase2_exp(dat2, method = 1, n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k-test.csv")
# d = phase2_exp(dat2, method = 2, n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k-test.csv")

# c = phase2_exp(dat, n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k.csv")
#d = phase2_exp(dat, n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k.csv")
#e = phase2_exp(dat, n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k.csv")
#f = phase2_exp(dat2, n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k-test.csv")
# e = phase2_exp(dat2, n = 100, p.attack = 0.1, k = 0.4, file = "Writing/Evaluation/phase1_scores-10a-40k-test.csv")
# phase2_results = phase2_exp()
# save(phase2_results, file = "Writing/Evaluation/phase2_evaluation.RData")

