#--- evaluate detection rate and false alarm rate ---#
evaluate = function(score, test, e = 0.1) {
    
    threshold = threshold(score, e)
    
    # n = sum(score >= d$x[local_max] & score <= d$x[local_min])
    
    ## floor() to be generous
    dr = sum(test$attack_type[score >= threshold] != "normal.") / sum(test$attack_type != "normal.")
    fa = sum(test$attack_type[score >= threshold] == "normal.") / sum(test$attack_type == "normal.")
    
    ret = list(threshold = threshold, detection.rate = dr, false.alarm = fa, detected = which(score >= threshold))
    
    return(ret[-4])
    
}

# 
#--- find threshold based on density of score ---#
threshold = function(score, e = 0.1, graph = FALSE) {
    
    ## density
    d = density(score, n = 10 * length(score))
    
    ## find extrema, minima and maxima (less than 2)
    extrema = diff(sign(diff(d$y)))
    extrema = c(0, extrema, 0)  # to make the length equal to d
    maxima  = which(extrema[d$x < 2.5] == -2)
    minima  = which(extrema[d$x < 2.5] ==  2)
    
    ## find prob for each curvature
    prob = numeric()
    require(sfsmisc)
    for (i in 1:(length(minima)-1)) {
        if (i == 1) { prob[1] = integrate.xy(d$x[1:minima[1]], d$y[1:minima[1]]) }
        prob[i+1] = integrate.xy(d$x[minima[i]:minima[i+1]], d$y[minima[i]:minima[i+1]])
    }
    
    ## first index at which the cumulative sum of prob is greater than 0.5
    ind = which(cumsum(prob) > 0.5)[1]
    
    ## find the local minimum, maximum and threshold
    maximum   = maxima[ind]
    minimum   = minima[ind]
    threshold = d$x[minimum] - e * diff(c(d$x[maximum], d$x[minimum]))
    
    if (graph) {
        plot(d, main = "Density curve of LOF scores")
        abline(v = threshold, col = "blue", lty = 2)
        legend("topright", "threshold", lty = 2, col = "blue")
    }
    
    return(threshold)
    
}


#--- evaluate detection rate and false alarm rate ---#
experiment = function(score, test = NULL) {
    d = density(score, n = 10 * length(score))
    ## diff(diff(x)) essentially computes the discrete analogue of the second derivative
    ## so should be negative at local maxima and positive at local minima.
    
    extrema   = diff(sign(diff(d$y)))
    local_min = min(which(extrema ==  2) + 1) # should be the first local minimum after 1
    local_max = min(which(extrema == -2) + 1) # should be the global maximum at 1
#     local_min = min(which(extrema ==   2)[-1] + 1)
#     local_max = min(which(extrema ==  -2)[-1] + 1)
    
#     return(list(c(local_max, local_min), c(d$x[local_max], d$x[local_min]),
#                 sfsmisc::integrate.xy(d$x[1:local_min], d$y[1:local_min]),
#                 sfsmisc::integrate.xy(d$x[local_max:local_min], d$y[local_max:local_min]),
#                 sfsmisc::integrate.xy(d$x[local_min:length(d$x)], d$y[local_min:length(d$y)])
#                 ))
    
    #threshold = d$x[local_min] - diff(c(d$x[local_max], d$x[local_min])) * 0.1
    threshold = d$x[local_max]
    n = length(d$x[local_max] <= score & score <= d$x[local_min])
    
    if (!is.null(test)) {
        dr = sum(test$attack_type[score >= threshold] != "normal.") / sum(test$attack_type != "normal.")
        fa = sum(test$attack_type[score >= threshold] == "normal.") / sum(test$attack_type == "normal.")
        ret = list(threshold = threshold, detection.rate = dr, false.alarm = fa, detected = which(score >= threshold), n = n)
        return(ret)
    }
    
    return(list(threshold = threshold, n = n))
}

## compute finite difference estimation on density values
# finite_difference = function(density) {
#     fp1 = numeric()
#     n  = length(density$x)
#     
#     ## forward difference is used
#     for (i in 1:(n-1)) {
#         fx0 = density$y[i]
#         fx1 = density$y[i+1]
#         h   = density$x[i+1] - density$x[i]
#         fp1[i] = (fx1 - fx0) / h
#     }
#     
#     return(fp1)
# }

## subset the finite difference estimates for the interval from the first local
## maximum (around 1) to the first local minimum (around 1.5 depending on samples)
# slope = function(fp) {
#     allneg   = which(fp < 0)
#     neg.last = allneg[min(which(diff(allneg) != 1)) - 1]
#     neg.ind = allneg[1]:neg.last
#     
#     n = length(neg.ind)
#     trim = ceiling(0.5 * n)
#     trimmed.ind = neg.ind[trim:n]
#     
#     return(index = trimmed.ind)
# }

## repeated experiment
# experiment1 = function(n) {
#     category = c(2,3,4,7,12,21,22)
#     result   = list()
#     
#     for (i in 1:n) {
#         cat(paste0("starting [", i, "]", "\n"))
#         
#         invisible(gc())
#         set.seed(i)
#         
#         normal = sample(which(dat$attack_type == "normal."), size = 3500, replace = FALSE)
#         attack = sample(which(dat$attack_type != "normal."), size = 1500, replace = FALSE)
#         test   = dat[c(normal, attack), -category]
#         
#         cat(paste0("  starting  k = 10% ..."))
#         lof_10p = LOF(test, control = Weka_control(min = 500, max = 500, "num-slots" = 2))[,"LOF"]; invisible(gc())
#         cat(paste0(" completed.", "\n"))
#         
#         cat(paste0("  starting  k = 20% ..."))
#         lof_20p = LOF(test, control = Weka_control(min = 1000, max = 1000, "num-slots" = 2))[,"LOF"]; invisible(gc())
#         cat(paste0(" completed.", "\n"))
#         
#         cat(paste0("  starting  k = 30% ..."))
#         lof_30p = LOF(test, control = Weka_control(min = 1500, max = 1500, "num-slots" = 2))[,"LOF"]; invisible(gc())
#         cat(paste0(" completed.", "\n"))
#         
#         cat(paste0("  starting  k = 40% ..."))
#         lof_40p = LOF(test, control = Weka_control(min = 2000, max = 2000, "num-slots" = 2))[,"LOF"]; invisible(gc())
#         cat(paste0(" completed.", "\n"))
#         
#         cat(paste0("  starting  k = 50% ..."))
#         lof_50p = LOF(test, control = Weka_control(min = 2500, max = 2500, "num-slots" = 2))[,"LOF"]; invisible(gc())
#         cat(paste0(" completed.", "\n"))
#         
#         names(lof_10p) <- dat[c(normal, attack), "attack_type"]
#         names(lof_20p) <- dat[c(normal, attack), "attack_type"]
#         names(lof_30p) <- dat[c(normal, attack), "attack_type"]
#         names(lof_40p) <- dat[c(normal, attack), "attack_type"]
#         names(lof_50p) <- dat[c(normal, attack), "attack_type"]
#         
#         result[[i]] = sapply(list(lof_10p, lof_20p, lof_30p, lof_40p, lof_50p), function (x) {
#             experiment(x)[-4]
#         })
#         
#         cat(paste0("completed", "\n", "\n"))
#         invisible(gc())
#     }
#     
#     return(result)
#     
# }

# make.table = function(result) {
#     tab = vector()
#     for (i in 1:15) {
#         txt = paste0("c(", paste0("result[[", 1:100, "]][[", i, "]]", collapse = ", "), ")")
#         tab[i] = paste(round(mean(eval(parse(text = txt))), digits = 3),
#                        paste0("(", round(sd(eval(parse(text = txt))), digits = 3), ")"))
#     }
#     
#     result.tab = data.frame(matrix(tab, nrow = 3), row.names = c("threshold", "detection.rate", "false.alarm.rate"))
#     colnames(result.tab) = c("k = 10%", "k = 20%", "k = 30%", "k = 40%", "k = 50%")
#     result.tab
# }

# t0 = Sys.time()
# results = experiment1(n = 100)
# results.table = make.table(results)
# cat(Sys.time() - t0)
# save(results, file = "results.table.RData")

