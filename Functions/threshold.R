# -----------------------------------------------------
# Function to find threshold based on density of score
# -----------------------------------------------------

threshold = function(score, e = 0.1, graph = FALSE) {
    
    # -----------------------------------------------------------------------
    # ARGUMENTS
    #     score: LOF scores
    #     e:     parameter for adjusting the "generousness" of the threshold
    #     graph: logical to whether output a graph
    # -----------------------------------------------------------------------
    
    ## density
    d = density(score, n = 10 * length(score))
    
    ## find extrema, minima and maxima (less than 2)
    extrema = diff(sign(diff(d$y)))
    extrema = c(0, extrema, 0)  # to make the length equal to d
    maxima  = which(extrema[d$x < 5] == -2)
    minima  = which(extrema[d$x < 5] ==  2)
    
    ## find prob for each curvature
    prob = numeric()
    require(sfsmisc)
    
    if (length(minima) == 1) {
        prob = integrate.xy(d$x[1:minima[1]], d$y[1:minima[1]])
    } else {
        for (i in 1:(length(minima)-1)) {
            if (i == 1) { prob[1] = integrate.xy(d$x[1:minima[1]], d$y[1:minima[1]]) }
            prob[i+1] = integrate.xy(d$x[minima[i]:minima[i+1]], d$y[minima[i]:minima[i+1]])
        }
    }
    
    ## first index at which the cumulative sum of prob is greater than 0.4
    ## (This probability depends on how many patterns of normal there are)
    ind = which(cumsum(prob) >= 0.4)[1]
    
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

