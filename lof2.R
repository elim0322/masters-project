lof2 = function(train, test,
                minPts.lower, minPts.upper, minPts.step = 1,
                theta, ...) {
    
    # ==============
    # For train set
    # ==============
    dist_mat = as.matrix(dist(train, ...)) # distance matrix
    k_val = seq(minPts.lower, minPts.upper, by = minPts.step)
    k_num = length(k_val)
    
    N = nrow(dist_mat)
    k_distances  = matrix(nrow=N, ncol=k_num)
    k_neighbours = list()
    
    for (i in 1:N) {
        i_dist = dist_mat[i, ]
        ordInd = order(i_dist)
        s_dist = i_dist[ordInd]
        
        k_distances[i, ]  = s_dist[k_val]
        k_neighbours[[i]] = sapply(k_val, function(x) {
            ordInd[s_dist - s_dist[x] < 1e-15][-1]
        })
    }
    
    # =============
    # For test set
    # =============
    tdist_mat = as.matrix(dist(test, ...))
    tN = nrow(tdist_mat)
    tk_distances  = matrix(nrow=tN, ncol=k_num)
    tk_neighbours = list()
    
    for (i in 1:2) {
        i_dist = tdist_mat[i, ]
        ordInd = order(i_dist)
        s_dist = i_dist[ordInd]
        
        tk_neighbours[[i]] = sapply(k_val, function(x) {
            ordInd[s_dist - s_dist[x] < 1e-15][-1]
        })
        
        for (j in 1:3){#k_num) {
            neighbours  = tk_neighbours[[i]][[j]]
            num_neighbours = length(tk_neighbours[[i]][[j]])
            #cat(k_distances[neighbours, j])
            #cat(i_dist[neighbours])
            
            # reachability-distance of the neighbours (in test set) with respect to
            # 
            reach_dist = max(k_distances[neighbours, j], i_dist[neighbours])
            lrd = 1 / (sum(reach_dist) / num_neighbours)
            cat(paste(lrd), "\n")
            
            
        }
    }
    
}


lof2(train=iris[,-5], test=iris[,-5], minPts.lower = 5, minPts.upper = 7)

