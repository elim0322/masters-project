source("evaluation.R")
source("kmeans.R")

detect = function(data, k = 0.3, n_normal = 3500, n_attack = 1500, seed = 1) {
    
    invisible(gc())
    
    # =========
    # Sampling
    # =========
    cat(paste0("Initializing samples........ "))
    set.seed(seed)
    normal = sample(which(dat$attack_type == "normal."), size = n_normal, replace = FALSE)
    
    set.seed(seed)
    attack = sample(which(dat$attack_type != "normal."), size = n_attack, replace = FALSE)
    
    category = c(2,3,4,7,12,21,22)#,42)
    testset  = data[c(normal, attack), -category]
    cat(paste0("done", "\n"))
    
    # ============
    # Phase 1 LOF
    # ============
    k_min = k * (n_normal + n_attack)
    k_max = k * (n_normal + n_attack)
    
    cat(paste0("Running LOF algorithm....... "))
    lof = LOF(testset, control = Weka_control(min = k_min, max = k_max, "num-slots" = 2))[,"LOF"]; invisible(gc())
    names(lof) <- data[c(normal, attack), "attack_type"]
    cat(paste0("done", "\n"))
    
    # ==================
    # Phase 1 detection
    # ==================
    cat(paste0("Running phase 1 detection... "))
    detected.ind = experiment(lof, testset)$detected
    cat(paste0("done", "\n"))
    
    # ================
    # Phase 2 X-means
    # ================
    detected.df = testset[detected.ind, ]
    
    xmeans.res = xmeans1(detected.df, "normalise")
    centers = gsub("^.+?n(0.+)}.+$", "\\1", capture.output(xmeans.res$clusterer$getClusterCenters()))
    centers = eval(parse(text = paste("c(", gsub("\\\\n", ",", centers), ")")))
    
    n = length(centers)
    x = 1:n
    split.list  = split(x, ceiling(x / (n / xmeans.res$clusterer$numberOfClusters())))
    centers.detected = sapply(split.list, function(x) centers[x])
    rownames(centers.detected) <- xmeans.res$feature
    
    # ==========================
    # Phase 2 center comparison
    # ==========================
    normal.df = preproc(testset[-detected.ind, ], "normalise")
    centers.normal = colMeans(normal.df[, names(normal.df)!="attack_type"])
    centers.normal = centers.normal[which(names(centers.normal) %in% rownames(centers.detected))]
    
    #d = apply(centers.detected, 2, function(x) sum( (x - centers.normal)^2 ))
    #d = apply(centers.detected, 2, function(x) sum(x * centers.normal) / (sqrt(sum(x^2)) * sqrt(sum(centers.normal^2))) )
    d = apply(centers.detected, 2, function(x) sum( abs(x - centers.normal) ))
    normal.clust = which.min(d) - 1 # -1 as cluster ids start from 0
    #normal.clust = which.max(d) - 1
    normal.ind = which(xmeans.res$class_ids == normal.clust)
    
    #return(summary(normal.df$attack_type))
    #return(summary(detected.df$attack_type))
    detected.final = detected.df$attack_type[-normal.ind]
    
    #return(summary(detected.df$attack_type[-normal.ind]))
    
    ## which cluster actually consists of normal
    actual.clust = which.max(sapply(xmeans.res$table, function(x) x["normal."] / sum(x))) - 1
    
    result = list()
    result$detection.rate   = sum(detected.final != "normal.") / n_attack
    result$false.alarm.rate = sum(detected.final == "normal.") / n_normal
    result$correctly.identified  = ifelse(actual.clust == normal.clust, TRUE, FALSE)
    result$correct.cluster = actual.clust
    #return(list(centers.detected,centers.normal, result$correct.cluster))
    return(result)
}
eval.detect = function(data, k = 0.3, n_normal = 3500, n_attack = 1500, seed = 1, n = 5) {
    
    result = list()
    for (i in 1:n) {
        result[[i]] = detect(data=data, k=k, n_normal=n_normal, n_attack=n_attack, seed=i)
    }
    
    dr  = paste0("c(", paste0("result[[", 1:n, "]]$detection.rate", collapse = ", "), ")")
    far = paste0("c(", paste0("result[[", 1:n, "]]$false.alarm.rate", collapse = ", "), ")")
    clust = paste0("c(", paste0("result[[", 1:n, "]]$correctly.identified", collapse = ", "), ")")
    
    dr.avg  = round(mean(eval(parse(text = dr))), digits = 5)
    far.avg = round(mean(eval(parse(text = far))), digits = 5)
    
    dr.sd  = round(sd(eval(parse(text = dr))), digits = 5)
    far.sd = round(sd(eval(parse(text = far))), digits = 5)
    
    ret = list()
    ret$table = data.frame(matrix(c(dr.avg, paste0("(", dr.sd, ")"), far.avg, paste0("(", far.sd, ")")), ncol = 2), row.names = c("mean", "sd"))
    colnames(ret$table) = c("detection.rate", "false.alarm.rate")
    ret$correct.cluster = sum(eval(parse(text = clust)))
    
    ret
    
}

k30 = eval.detect(dat, k = 0.3, n = 100)
k30.cos = eval.detect(dat, k = 0.3, n = 100)
k30.man = eval.detect(dat, k = 0.3, n = 100)

k40 = eval.detect(dat, k = 0.4, n = 100)
k50 = eval.detect(dat, k = 0.5, n = 100)

a = detect(dat, seed = 10)
apply(a[[1]], 2, function(x) sum( (x - a[[2]])^2 ))
apply(a[[1]], 2, function(x) sum( abs(x - a[[2]]) ))
apply(a[[1]], 2, function(x) sum(x*a[[2]]) / (sqrt(sum(x^2)) * sqrt(sum(a[[2]]^2))) )




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







