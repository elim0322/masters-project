## evaluate detection rate and false alarm rate
experiment = function(score, test) {
    d = density(score, n = 10 * length(score))
#     fp = finite_difference(d)
#     neg = slope(fp)
#     #ind = neg[which.min(diff(fp[neg]))]
#     #threshold = d$x[ind]
#     
#     ## this part requires improvement
#     threshold = d$x[neg[length(neg)]] - diff(c(d$x[neg[1]], d$x[neg[length(neg)]])) * 0.1
    
    
    ## diff(diff(x)) essentially computes the discrete analogue of the second derivative
    ## so should be negative at local maxima and positive at local minima.
    local_min = min(which(diff(sign(diff(d$y))) ==  2) + 1) # should be the first local minimum after 1
    local_max = min(which(diff(sign(diff(d$y))) == -2) + 1) # should be the global maximum at 1
    
    threshold = d$x[local_min] - diff(c(d$x[local_max], d$x[local_min])) * 0.1
    
    ## floor() to be generous
    dr = sum(test$attack_type[score >= threshold] != "normal.") / sum(test$attack_type != "normal.")
    fa = sum(test$attack_type[score >= threshold] == "normal.") / sum(test$attack_type == "normal.")
    
    ret = list(threshold = threshold, detection.rate = dr, false.alarm = fa, detected = which(score >= threshold))
    return(ret)
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

