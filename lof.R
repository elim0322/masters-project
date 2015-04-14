# ===========================================================================
# My implementation of finding local outlier factors.
# The only existing LOF implementation in R is the lofactor() in DMwR,
# which incorrectly selects k-distance sets (definition 4 is not satisfied).
# ===========================================================================

lof = function(data, k) {
    data = as.matrix(data)
    knn_distances = dist.knn.mine(data, k)
    reach_list = reachability.mine(knn_distances)
    lof = (reach_list[["summed_lrd"]] / reach_list[["lrd"]]) / reach_list[["nneighbours"]]
    return(lof)
}

dist.knn.mine <- function(data, k) {
    n_dist = list()
    #dist_mat = as.matrix(dist(data, method))
    for (i in 1:nrow(data)) {
        centred = scale(data, center = data[i, ], scale = FALSE)
        dist_i  = sqrt(rowSums(centred^2))
        
        # find obervations that make up the k-distance neighborhood for observation dataset[i,]
        n_dist[[i]] = knn(dist_i, k)
        
        cat(paste("knn_dist:", "i =", i, "done", "\n"))
    }
    return(n_dist)
}

knn <- function(distance, k) {
    ordered_ind = order(distance)
    nn_dist      = distance[ordered_ind]
    
    # find distance to k-nearest neighbor
    # uses k+1 since first distance in vector is a 0
    knn_dist <- nn_dist[k+1]
    
    # find neighborhood
    # eliminate first row of zeros from neighborhood
    # (because of square rooting, we can't use == see .Machine$double.eps)
    nb <- nn_dist[nn_dist - knn_dist < 1e-15][-1]
    
    # find indexes of each neighbor in the neighborhood
    nb_n   = length(nb)
    nb_ind = ordered_ind[(1:nb_n) + 1]
    
    result = list(index=nb_ind, dist=nb, kdist=knn_dist)
    return(result)
}

reachability.mine = function(n_dist) {
    k_distances = sapply(n_dist, function(x) x$kdist)
    lrd = numeric()
    N_k = numeric()
    for (i in 1:length(n_dist)) {
        kdist = k_distances[n_dist[[i]]$index]
        dist  = n_dist[[i]]$dist
        reach_dist = pmax(kdist, dist)
        
        lrd[i] = 1 / (sum(reach_dist) / length(reach_dist))
        
        cat(paste("lrd:", "i =", i, "done", "\n"))
    }
    
    lrd_sum = sapply(n_dist, function(x) sum(lrd[x$index]))
    N_k     = sapply(n_dist, function(x) length(x$index))
    
    result = list(lrd=lrd, summed_lrd=lrd_sum, nneighbours=N_k)
    return(result)
}
