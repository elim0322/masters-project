source("evaluation.R")
source("kmeans.R")
source("overlap.R")

detect = function(data, k = 0.3, n_normal = 3500, n_attack = 1500, seed = 1, method = "euclidean", trace = TRUE) {
    
    invisible(gc(reset = TRUE))
    
#     if (!(require()))
#     options(java.parameters = "-Xmx4g")
#     if (!require(RWeka))
#     WPM("install-package", "localOutlierFactor")
#     WPM("install-package", "XMeans")
#     LOF = make_Weka_filter("weka/filters/unsupervised/attribute/LOF")
    
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
    t1 = Sys.time() - t0
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
    #normal2.df = testset[-detected.ind, which(names(centers.normal) %in% rownames(centers.detected))]
    normal2.df = testset[-detected.ind, ]
    
    if (method == "euclidean") {
        d = apply(centers.detected, 2, function(x) sqrt(sum((x-centers.normal)^2)))
        normal.clust = which.min(d)
    } else if (method == "w.euclidean") {
        #return(list(detected.df, centers.detected, normal.df, centers.normal, xmeans.res$class_ids))
        weights.normal   = sapply(normal.df[,which(names(normal.df) %in% rownames(centers.detected))], function(x) sd(x))
        w.centers.normal = ifelse(weights.normal == 0, 0, 1/weights.normal) * centers.normal
        for (i in 1:nc) {
            which.var = which(names(detected.df) %in% rownames(centers.detected))
            weights.detected = sapply(detected.df[which(xmeans.res$class_ids == i), which.var], function(x) sd(x))
            centers.detected[, i] = ifelse(weights.detected == 0, 0, 1/weights.detected) * centers.detected[, i]
        }
        d = apply(centers.detected, 2, function(x) sqrt(sum((x - w.centers.normal)^2)))
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
        #return(list(detected.df, normal2.df, xmeans.res$class_ids))
        pAvg = numeric()
        for (i in 1:nc) {
            tmp.df  = detected.df[which(xmeans.res$class_ids == i), ]
            probs   = sapply(1:(which(names(tmp.df)=="attack_type") - 1), function(j) overlap(tmp.df[, j], normal2.df[, j]))
            pAvg[i] = mean(probs[probs <= 1])
        }
        normal.clust = which.max(pAvg)
    } else if (method == "density.norm") {
        detected.df2 = preproc(detected.df, "normalise")
        pAvg = numeric()
        for (i in 1:nc) {
            tmp.df  = detected.df2[which(xmeans.res$class_ids == i), ]
            probs   = sapply(1:(which(names(tmp.df)=="attack_type") - 1), function(j) overlap(tmp.df[, j], normal.df[, j]))
            pAvg[i] = mean(probs[probs <= 1])
        }
        normal.clust = which.max(pAvg)
    }
    
    normal.ind = which(xmeans.res$class_ids == normal.clust)
    
    #return(summary(normal.df$attack_type))
    #return(summary(detected.df$attack_type))
    detected.final = detected.df$attack_type[-normal.ind]
    
    t3 = Sys.time() - t0
    t2 = t3 - t1
    
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
    if (method == "density" | method == "density.norm") {
        result$prob = pAvg 
    } else {
        result$dist = d
    }
    result$time                 = t3
    result$p1.time              = t1
    result$p2.time              = t2
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
    p1.time  = paste0("c(", paste0("result[[", 1:n, "]]$p1.time", collapse = ", "), ")")
    p2.time  = paste0("c(", paste0("result[[", 1:n, "]]$p2.time", collapse = ", "), ")")
    
    dr.avg  = round(mean(eval(parse(text = dr))), digits = 5)
    far.avg = round(mean(eval(parse(text = far))), digits = 5)
    
    dr.sd  = round(sd(eval(parse(text = dr))), digits = 5)
    far.sd = round(sd(eval(parse(text = far))), digits = 5)
    
    avg.time            = round(mean(eval(parse(text = time))), 5)
    sd.time             = round(sd(eval(parse(text = time))), 5)
    p1.avg.time         = round(mean(eval(parse(text = p1.time))), 5)
    p1.sd.time          = round(sd(eval(parse(text = p1.time))), 5)
    p2.avg.time         = round(mean(eval(parse(text = p2.time))), 5)
    p2.sd.time          = round(sd(eval(parse(text = p2.time))), 5)
    
    ret = list()
    ret$table = data.frame(matrix(c(dr.avg, paste0("(", dr.sd, ")"),
                                    far.avg, paste0("(", far.sd, ")")), ncol = 2),
                           row.names = c("mean", "sd"))
    colnames(ret$table)     = c("detection.rate", "false.alarm.rate")
    ret$tot.correct.cluster = sum(eval(parse(text = clust)))
    ret$correct.cluster     = eval(parse(text = clust))
    
    ret$time = data.frame(matrix(c(avg.time, paste0("(", sd.time, ")"), 
                                   p1.avg.time, paste0("(", p1.sd.time, ")"),
                                   p2.avg.time, paste0("(", p2.sd.time, ")")), nrow = 2),
                          row.names = c("mean", "sd"))
    colnames(ret$time)      = c("overall", "phase.1", "phase.2")
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
# k30.nden = eval.detect(dat, n = 100, method = "density.norm")
# save(k30.euc, k30.weuc, k30.man, k30.che, k30.min, k30.mah, k30.den, k30.nden, file = "evaluation.results.RData")
# load(file = "evaluation.results.RData")

## a <- detect(dat, seed = 1, method = "density")
# sapply(1:34, function(i) {
#     cat(i, sep = "\n")
#     overlap(a[[1]][a[[3]] == 3, i], a[[2]][, i])
# })
# a[[1]][a[[3]] == 3, 18]
# a[[2]][, 18]

# a[[1]][a[[3]] == 4, 20]
# a[[2]][, 20]

# overlap(a[[1]][a[[3]] == 1, 2], a[[2]][, 2])
# 
# plot(density(a[[1]][a[[3]] == 1, 2]))
# plot(density(a[[2]][, 2]))
# 
# plot(density(a[[1]][a[[3]] == 1, 2]))
# lines(density(a[[2]][, 2]))


































