load(file = "Data/System_results/results_10a.RData")
load(file = "Data/System_results/results_20a.RData")
load(file = "Data/System_results/results_30a.RData")
load(file = "Data/System_results/results_40a.RData")

processResults = function(results, graph = FALSE) {
    d = numeric()
    f = numeric()
    for (i in 1:100) {
        dif     = diff(results$p2.proportion.within.cluster[i, 2:10])
        dif.neg = which(dif < 0)
        mn      = which.min(dif[dif.neg]) + 2
        d       = c(d, results$system.detection.rate[i, mn])
        f       = c(f, results$system.false.alarm.rate[i, mn])
    }
    
    if (graph) {
        avg.TPR = colMeans(results$system.detection.rate)
        avg.FPR = colMeans(results$system.false.alarm.rate)
        avg.pwc = colMeans(results$p2.proportion.within.cluster)
        avg.pws = colMeans(results$p2.proportion.within.sample)
        plot(x = 0:10, y = seq(0, 1, by = 0.1), type = "n")
        lines(avg.TPR, col = "blue")
        lines(avg.FPR, col = "red")
        lines(avg.pwc)
        lines(avg.pws)
    }
    
    return(list(TPR = mean(d), FPR = mean(f)))
}

processResults(system.results_dat1_10a10k, graph = T)
processResults(system.results_dat1_10a20k, graph = T)
processResults(system.results_dat1_10a30k, graph = T)
processResults(system.results_dat1_10a40k, graph = T)

processResults(system.results_dat1_20a10k, graph = T)
processResults(system.results_dat1_20a20k, graph = T)
processResults(system.results_dat1_20a30k, graph = T)
processResults(system.results_dat1_20a40k, graph = T)

processResults(system.results_dat1_30a10k, graph = T)
processResults(system.results_dat1_30a20k, graph = T)
processResults(system.results_dat1_30a30k, graph = T)
processResults(system.results_dat1_30a40k, graph = T)

processResults(system.results_dat1_40a10k, graph = T)
processResults(system.results_dat1_40a20k, graph = T)
processResults(system.results_dat1_40a30k, graph = T)
processResults(system.results_dat1_40a40k, graph = T)




d = diff(system.results_dat1_10a40k$p2.proportion.within.cluster[i, 1:10])
which(d < 0)
j = which(d < 0)[which.min(d[which(d < 0)])]

system.results_dat1_10a40k$system.detection.rate[i, j]
system.results_dat1_10a40k$system.false.alarm.rate[i, j]


k = 4
plot(x = 0:10, y = seq(0,1, by = 0.1), type = "n")
lines(system.results_dat1_10a40k$p2.proportion.within.cluster[k, ])
lines(system.results_dat1_10a40k$p2.proportion.within.sample[k, ])
lines(system.results_dat1_10a40k$system.detection.rate[k, ], col = "blue")
lines(system.results_dat1_10a40k$system.false.alarm.rate[k, ], col = "red")

system.results_dat

for (i in 1:100) {
    plot(x = 0:10, y = seq(0,1, by = 0.1), type = "n")
    lines(system.results_dat1_10a40k$p2.proportion.within.cluster[i, ])
    lines(system.results_dat1_10a40k$p2.proportion.within.sample[i, ])
    lines(system.results_dat1_10a40k$system.detection.rate[i, ], col = "blue")
    lines(system.results_dat1_10a40k$system.false.alarm.rate[i, ], col = "red")
    Sys.sleep(2)
}


