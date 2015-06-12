overlap = function(v1, v2, graph = FALSE, ablines = FALSE) {
    
    ## if both variables consist of only one unique value
    u1 = unique(v1)
    u2 = unique(v2)
    if (length(u1) == 1 & length(u2) == 1) {
        if (u1 == u2) {
            return(1)
        } else {
            return(0)
        }
    }
    
    ## initial (temporary) density
    tmp.d1 = density(v1, n = 1000)
    tmp.d2 = density(v2, n = 1000)
    
    ## find the x range so that we are only comparing the relevant range
    mn = max(c(min(tmp.d1$x), min(tmp.d2$x)))
    mx = min(c(max(tmp.d1$x), max(tmp.d2$x)))
    
    d1 = density(v1, n = 1000, from = mn, to = mx)
    d2 = density(v2, n = 1000, from = mn, to = mx)
    
    intrs       = c(0, diff(d1$y < d2$y))
    which.intrs = which(intrs != 0)
    n           = length(which.intrs)
    
    if (graph) {
        if (max(d1$y) > max(d2$y)) {
            plot(d1)
            lines(d2, col = "grey")
        } else {
            plot(d2)
            lines(d1, col = "grey")
        }
    }
    if (graph & ablines) {
        abline(v = d1$x[which.intrs], lty = 2)
    }
    
    require(sfsmisc)
    for (i in 1:(n+1)) {
        if (i == 1) {
            tmp = which.intrs[i]
            if (intrs[tmp] == -1) {
                pArea = integrate.xy(d1$x[1:tmp], d1$y[1:tmp])
            } else if (intrs[tmp] == 1) {
                pArea = integrate.xy(d2$x[1:tmp], d2$y[1:tmp])
            }
        } else if (i > 1 & i <= n) {
            tmp1 = which.intrs[i-1]
            tmp2 = which.intrs[i]
            if (intrs[tmp1] == -1 & intrs[tmp2] == 1) {
                pArea = pArea + integrate.xy(d2$x[tmp1:tmp2], d2$y[tmp1:tmp2])
            } else if (intrs[tmp1] == 1 & intrs[tmp2] == -1) {
                pArea = pArea + integrate.xy(d1$x[tmp1:tmp2], d1$y[tmp1:tmp2])
            }
        } else if (i == (n+1)) {
            tmp1 = which.intrs[i-1]
            tmp2 = length(intrs)
            if (intrs[tmp1] == -1) {
                pArea = pArea + integrate.xy(d2$x[tmp1:tmp2], d2$y[tmp1:tmp2])
            } else if (intrs[tmp1] == 1) {
                pArea = pArea + integrate.xy(d1$x[tmp1:tmp2], d1$y[tmp1:tmp2])
            }
        }
    }
    
    pArea
    
}

