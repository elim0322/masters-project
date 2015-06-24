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
    
    v1.range = range(v1)
    v2.range = range(v2)
    if (min(v1.range) > max(v2.range)) {
        if (graph) {
            if (max(tmp.d1$y) > max(tmp.d2$y)) {
                plot(d1)
                lines(d2, col = "blue")
            } else if (max(tmp.d1$y < max(tmp.d2$y))) {
                plot(d2)
                lines(d1, col = "blue")
            }
        }
        return(0)
    } else if (max(v1.range) < min(v2.range)) {
        if (graph) {
            if (max(tmp.d1$y) > max(tmp.d2$y)) {
                plot(d2)
                lines(d1, col = "blue")
            } else if (max(tmp.d1$y) < max(tmp.d2$y)) {
                plot(d1)
                lines(d2, col = "blue")
            }
        }
        return(0)
    }
    
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
            plot(d1, type = "n")
        } else {
            plot(d2, type = "n")
        }
    }
    if (graph & ablines) {
        abline(v = d1$x[which.intrs], lty = 2)
    }
    
    ## when density plots don't overlap
    if (all(intrs == 0)) {
        if (all(d1$y < d2$y)) {
            pArea = integrate.xy(d1$x, d1$y)
            polygon(
                x = c(d1$x[1], d1$x, d1$x[length(d1$x)]),
                y = c(0,       d1$y, 0),
                col = "skyblue", border = "skyblue"
            )
            lines(d1, col = "blue")
            lines(d2)
        } else if (all(d1$y > d2$y)) {
            pArea = integrate.xy(d2$x, d2$y)
            polygon(
                x = c(d2$x[1], d2$x, d2$x[length(d2$x)]),
                y = c(0,       d2$y, 0),
                col = "skyblue", border = "skyblue"
            )
            lines(d1)
            lines(d2, col = "blue")
        }
        return(pArea)
    }
    
    require(sfsmisc)
    for (i in 1:(n+1)) {
        if (i == 1) {
            tmp = which.intrs[i]
            if (intrs[tmp] == -1) {
                pArea = integrate.xy(d1$x[1:tmp], d1$y[1:tmp])
                if (graph) {
                    polygon(
                        x = c(d1$x[1], d1$x[1:tmp], d1$x[tmp]),
                        y = c(0,       d1$y[1:tmp], 0),
                        col = "skyblue", border = "skyblue"
                    )
                }
            } else if (intrs[tmp] == 1) {
                pArea = integrate.xy(d2$x[1:tmp], d2$y[1:tmp])
                if (graph) {
                    polygon(
                        x = c(d2$x[1], d2$x[1:tmp], d2$x[tmp]),
                        y = c(0,       d2$y[1:tmp], 0),
                        col = "skyblue", border = "skyblue"
                    )
                }
            }
        } else if (i > 1 & i <= n) {
            tmp1 = which.intrs[i-1]
            tmp2 = which.intrs[i]
            if (intrs[tmp1] == -1 & intrs[tmp2] == 1) {
                pArea = pArea + integrate.xy(d2$x[tmp1:tmp2], d2$y[tmp1:tmp2])
                if (graph) {
                    polygon(
                        x = c(d2$x[tmp1], d2$x[tmp1], d2$x[tmp1:tmp2], d2$x[tmp2]),
                        y = c(0,          d2$y[tmp1], d2$y[tmp1:tmp2], 0),
                        col = "skyblue", border = "skyblue"
                    )
                }
            } else if (intrs[tmp1] == 1 & intrs[tmp2] == -1) {
                pArea = pArea + integrate.xy(d1$x[tmp1:tmp2], d1$y[tmp1:tmp2])
                if (graph) {
                    polygon(
                        x = c(d1$x[tmp1], d1$x[tmp1], d1$x[tmp1:tmp2], d1$x[tmp2]),
                        y = c(0,          d1$y[tmp1], d1$y[tmp1:tmp2], 0),
                        col = "skyblue", border = "skyblue"
                    )
                }
            }
        } else if (i == (n+1)) {
            tmp1 = which.intrs[i-1]
            tmp2 = length(intrs)
            
            ## if tmp1 != tmp2:
            ##   then the last point of intersection is not the last point
            ##   for the density curves (ie, the curves end after)
            ## if tmp1 == tmp2:
            ##   the last point of intersection is where the curves end
            if (tmp1 != tmp2) {
                if (intrs[tmp1] == -1) {
                    pArea = pArea + integrate.xy(d2$x[tmp1:tmp2], d2$y[tmp1:tmp2])
                    if (graph) {
                        polygon(
                            x = c(d2$x[tmp1], d2$x[tmp1], d2$x[tmp1:tmp2], d2$x[tmp2]),
                            y = c(0,          d2$y[tmp1], d2$y[tmp1:tmp2], 0),
                            col = "skyblue", border = "skyblue"
                        )
                    }
                } else if (intrs[tmp1] == 1) {
                    pArea = pArea + integrate.xy(d1$x[tmp1:tmp2], d1$y[tmp1:tmp2])
                    if (graph) {
                        polygon(
                            x = c(d1$x[tmp1], d1$x[tmp1], d1$x[tmp1:tmp2], d1$x[tmp2]),
                            y = c(0,          d1$y[tmp1], d1$y[tmp1:tmp2], 0),
                            col = "skyblue", border = "skyblue"
                        )
                    }
                }
            }
            ## if (tmp1 == tmp2) do nothing as the previous iteration has
            ## already computed pArea
        }
    }
    
    if (graph) {
        if (max(d1$y) > max(d2$y)) {
            lines(d1)
            lines(d2, col = "blue")
        } else {
            lines(d2)
            lines(d1, col = "blue")
        }
    }
    
    return(pArea)
    
}

