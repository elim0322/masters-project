source("evaluation.R")
source("kmeans.R")

detect = function(data, k = 0.3, n_normal = 3500, n_attack = 1500, seed = 1, method = "euclidean", trace = TRUE) {
    
    invisible(gc(reset = TRUE))
    
    t0 = Sys.time()
    
    # =========
    # Sampling
    # =========
    if (trace) cat(paste0("Initializing samples .......... "))
    set.seed(seed)
    normal = sample(which(dat$attack_type == "normal."), size = n_normal, replace = FALSE)
    
    set.seed(seed)
    attack = sample(which(dat$attack_type != "normal."), size = n_attack, replace = FALSE)
    
    category = c(2,3,4,7,12,21,22)#,42)
    testset  = data[c(normal, attack), -category]
    if (trace) cat(paste0("done", "\n"))
    
    # ============
    # Phase 1 LOF
    # ============
    k_min = k * (n_normal + n_attack)
    k_max = k * (n_normal + n_attack)
    
    if (trace) cat(paste0("Running LOF algorithm ......... "))
    lof = LOF(testset, control = Weka_control(min = k_min, max = k_max, "num-slots" = 2))[,"LOF"]; invisible(gc())
    names(lof) <- data[c(normal, attack), "attack_type"]
    if (trace) cat(paste0("done", "\n"))
    
    # ==================
    # Phase 1 detection
    # ==================
    if (trace) cat(paste0("Running phase 1 detection ..... "))
    detected.ind = experiment(lof, testset)$detected
    if (trace) cat(paste0("done", "\n"))
    
    # ================
    # Phase 2 X-means
    # ================
    detected.df = testset[detected.ind, ]
    
    xmeans.res = xmeans1(detected.df, "normalise")
    xmeans.res$class_ids = xmeans.res$class_ids + 1 # class_ids start from 0
    centers = gsub("^.+?n(0.+)}.+$", "\\1", capture.output(xmeans.res$clusterer$getClusterCenters()))
    centers = eval(parse(text = paste("c(", gsub("\\\\n", ",", centers), ")")))
    
    n  = length(centers)
    nc = xmeans.res$clusterer$numberOfClusters()
    x  = 1:n
    split.list  = split(x, ceiling(x / (n / nc)))
    centers.detected = sapply(split.list, function(x) centers[x])
    rownames(centers.detected) <- xmeans.res$feature
    
    # ==========================
    # Phase 2 center comparison
    # ==========================
    normal.df = preproc(testset[-detected.ind, ], "normalise")
    centers.normal = colMeans(normal.df[, names(normal.df)!="attack_type"])
    centers.normal = centers.normal[which(names(centers.normal) %in% rownames(centers.detected))]
    
    if (method == "euclidean") {
        d = apply(centers.detected, 2, function(x) sqrt(sum((x-centers.normal)^2)))
        normal.clust = which.min(d)
    } else if (method == "w.euclidean") {
        weights.normal   = sapply(normal.df[,which(names(normal.df) %in% rownames(centers.detected))], function(x) sd(x))
        w.centers.normal = weights.normal * centers.normal
        for (i in 1:nc) {
            which.var = which(names(detected.df) %in% rownames(centers.detected))
            weights.detected = sapply(detected.df[which(xmeans.res$class_ids == i), which.var], function(x) sd(x))
            centers.detected[, i] = weights.detected * centers.detected[, i]
        }
        d = apply(centers.detected, 2, function(x) sum((x - w.centers.normal)^2))
        normal.clust = which.min(d)
    } else if (method == "manhattan") {
        d = apply(centers.detected, 2, function(x) sum(abs(x-centers.normal)))
        normal.clust = which.min(d)
    } else if (method == "chebyshev") {
        d = apply(centers.detected, 2, function(x) max(abs(x-centers.normal)))
        normal.clust = which.min(d)
    } else if (method == "minkoski") {
        d = apply(centers.detected, 2, function(x) {
            p = length(centers.normal)
            (sum((abs(x-centers.normal))^p))^(1/p)
        })
        normal.clust = which.min(d)
    } else if (method == "mahalanobis") {
        d = apply(centers.detected, 2, function(x) 
            sqrt((t(x-centers.normal) * cov(x, centers.normal)) %*% t(t(x-centers.normal))))
        normal.clust = which.min(d)
    } else if (method == "density") {
        #return(list(detected.df, normal.df, xmeans.res$class_ids))
        return(list(detected.df[which(xmeans.res$class_ids == 1), 2], normal.df[, 2]))
        overlap(detected.df[which(xmeans.res$class_ids == 1), 2], normal.df[, 2])
#         pAvg = numeric()
#         for (i in 1:nc) {
#             tmp.df  = detected.df[which(xmeans.res$class_ids == i), ]
#             probs   = sapply(1:(which(names(tmp.df)=="attack_type") - 1), function(j) overlap(tmp.df[, j], normal.df[, j]))
#             pAvg[i] = mean(probs[probs <= 1])
#             cat(probs)
#             cat("\n")
#         }
return()
        normal.clust = which.max(pAvg)
    }
    
    normal.ind = which(xmeans.res$class_ids == normal.clust)
    
    #return(summary(normal.df$attack_type))
    #return(summary(detected.df$attack_type))
    detected.final = detected.df$attack_type[-normal.ind]
    
    #return(summary(detected.df$attack_type[-normal.ind]))
    
    ## which cluster actually consists of normal
    actual.clust = which.max(sapply(xmeans.res$table, function(x) x["normal."] / sum(x)))
    
    names(actual.clust) = NULL
    names(normal.clust) = NULL
    
    result = list()
    result$detection.rate       = sum(detected.final != "normal.") / n_attack
    result$false.alarm.rate     = sum(detected.final == "normal.") / n_normal
    result$correctly.identified = ifelse(actual.clust == normal.clust, TRUE, FALSE)
    result$correct.cluster      = actual.clust
    result$identified.cluster   = normal.clust
    result$purity               = xmeans.res$purity
    if (method == "density")    { result$prob = pAvg }
    else                        { result$dist = d    }
    result$time                 = Sys.time() - t0
    #return(list(centers.detected,centers.normal, result$correct.cluster))
    result
}
eval.detect = function(data, k = 0.3, n_normal = 3500, n_attack = 1500, seed = 1, method, n = 5) {
    
    result = list()
    for (i in 1:n) {
        cat(paste0("Running [[", i, "]] ..."))
        result[[i]] = detect(data = data, k = k, n_normal = n_normal, n_attack = n_attack, seed = i, method = method, trace = FALSE)
        cat(paste0(" done", "\n"))
    }
    
    dr    = paste0("c(", paste0("result[[", 1:n, "]]$detection.rate", collapse = ", "), ")")
    far   = paste0("c(", paste0("result[[", 1:n, "]]$false.alarm.rate", collapse = ", "), ")")
    clust = paste0("c(", paste0("result[[", 1:n, "]]$correctly.identified", collapse = ", "), ")")
    time  = paste0("c(", paste0("result[[", 1:n, "]]$time", collapse = ", "), ")")
    
    dr.avg  = round(mean(eval(parse(text = dr))), digits = 5)
    far.avg = round(mean(eval(parse(text = far))), digits = 5)
    
    dr.sd  = round(sd(eval(parse(text = dr))), digits = 5)
    far.sd = round(sd(eval(parse(text = far))), digits = 5)
    
    ret = list()
    ret$table = data.frame(matrix(c(dr.avg, paste0("(", dr.sd, ")"), far.avg, paste0("(", far.sd, ")")), ncol = 2), row.names = c("mean", "sd"))
    colnames(ret$table)     = c("detection.rate", "false.alarm.rate")
    ret$tot.correct.cluster = sum(eval(parse(text = clust)))
    ret$correct.cluster     = eval(parse(text = clust))
    ret$avg.time            = mean(eval(parse(text = time)))
    ret$tot.time            = sum(eval(parse(text = time)))
    
    ret
    
}

# k30.euc = eval.detect(dat, n = 100, method = "euclidean")
# k30.weuc = eval.detect(dat, n = 100, method = "w.euclidean")
# k30.man = eval.detect(dat, n = 100, method = "manhattan")
# k30.che = eval.detect(dat, n = 100, method = "chebyshev")
# k30.min = eval.detect(dat, n = 100, method = "minkoski")
# k30.mah = eval.detect(dat, n = 100, method = "mahalanobis")
# k30.den = eval.detect(dat, n = 100, method = "density")

#a <- detect(dat, seed = 40, method = "density")


# k40 = eval.detect(dat, k = 0.4, n = 100)
# k50 = eval.detect(dat, k = 0.5, n = 100)
# 
# a = detect(dat, seed = 10)
# apply(a[[1]], 2, function(x) sum( (x - a[[2]])^2 ))
# apply(a[[1]], 2, function(x) sum( abs(x - a[[2]]) ))
# apply(a[[1]], 2, function(x) sum(x*a[[2]]) / (sqrt(sum(x^2)) * sqrt(sum(a[[2]]^2))) )




# detect(dat, seed = 10)
# detect(dat, seed = 10, k = 0.4)
# detect(dat, seed = 10, k = 0.5)

# a = detect(dat, lof = lof_30p)
# which.min(detect(dat, lof = lof_30p))




# xmeans.norm = xmeans1(trim1, mode = "normalise")
# centers = gsub("^.+?n(0.+)}.+$", "\\1", capture.output(xmeans.norm$clusterer$getClusterCenters()))
# centers = eval(parse(text=paste("c(", gsub("\\\\n", ",", centers), ")")))
# centers = cbind(centers[1:31], centers[32: 62], centers[63:93], centers[94:124])
# rownames(centers) <- xmeans.norm$feature







