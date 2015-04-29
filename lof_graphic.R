# ===============================================
# Find detection rate through graphical approach
# ===============================================

plot(test_d_30p)
lines(train1_d_30p, col = "blue",   lty = 3)
lines(density(train1_lof_30p, bw=0.2, n=1000,from=0,to=5), col="red", lty=3)
lines(density(test.df[,4], bw=0.1, n=1000,from=0,to=5),col="red",lty=3)


computeRates1(train1_d_30p, test_d_30p)
computeRates1(train1_d_30p, density(test.df[,4], bw=0.055, n=1000,from=0,to=5))
computeRates1(train1_d_30p, density(test.df[,4], bw=0.085, n=1000,from=0,to=5))
computeRates1(train1_d_30p, density(test.df[,4], bw=0.1, n=1000,from=0,to=5))


lines(train2_d_30p, col = "red",    lty = 3)
lines(train3_d_30p, col = "green",  lty = 3)
lines(train4_d_30p, col = "orange", lty = 3)
lines(train5_d_30p, col = "grey",   lty = 3)

computeRates = function() {
    trains  = paste0(c("train1", "train2", "train3", "train4", "train5"), "_d_30p")    
    diffs.cmd  = paste0("c(0, diff(", paste0(trains, "$y"), " < test_d_30p$y))")
    diffs.list = lapply(diffs.cmd, function (x) eval(parse(text = x)))
    
    dr  = numeric()
    far = numeric()
    
    for (i in 1:length(diffs.list)) {
        
        diffs  = diffs.list[[i]]
        inters = which(diffs != 0)
        
        x.str = paste0(trains[i], "$x")
        y.str = paste0(trains[i], "$y")
        
        for (j in 1:(length(inters)-1)) {
            if (j == 1) {
                ## for the 1st iteration only
                if (diffs[inters][1] == -1) {
                    cmd    = paste0("integrate.xy(", x.str, "[1:inters[1]], ", y.str, "[1:inters[1]])")
                    dr[i]  = eval(parse(text = cmd))
                    far[i] = dr[i] - integrate.xy(test_d_30p$x[1:inters[1]], test_d_30p$y[1:inters[1]])
                } else {
                    dr[i]  = integrate.xy(test_d_30p$x[1:inters[1]], test_d_30p$y[1:inters[1]])
                    far[i] = 0
                }
            } else {
                ## for the rest of the iterations
                if (diffs[inters][j] == -1 & diffs[inters][j+1] == 1) {
                    cmd    = paste0("integrate.xy(", x.str, "[inters[j]:inters[(j+1)]], ", y.str, "[inters[j]:inters[(j+1)]])")
                    dr.j   = eval(parse(text = cmd))
                    dr[i]  = dr[i] + dr.j
                    far[i] = far[i] + dr.j - integrate.xy(test_d_30p$x[inters[j]:inters[(j+1)]],
                                                          test_d_30p$y[inters[j]:inters[(j+1)]])
                } else {
                    dr[i] = dr[i] + integrate.xy(test_d_30p$x[inters[j]:inters[(j+1)]],
                                                 test_d_30p$y[inters[j]:inters[(j+1)]])
                }
            }
        }
    }
    return(list("detection rate" = dr, "false alarm rate" = far))
}

computeRates1 = function(train.density, test.density) {
    diffs  = c(0, diff(train.density$y < test.density$y))
    inters = which(diffs != 0)
    
    for (i in 1:(length(inters)-1)) {
        if (i == 1) {
            if (diffs[inters][1] == -1) {
                dr  = integrate.xy(train.density$x[1:inters[1]], train.density$y[1:inters[1]])
                far = dr - integrate.xy(test.density$x[1:inters[1]], test.density$y[1:inters[1]])
            } else {
                dr  = integrate.xy(test.density$x[1:inters[1]], test.density$y[1:inters[1]])
                far = 0
            }
        } else {
            if (diffs[inters][i]==-1 & diffs[inters][i+1]==1) {
                dr_i = integrate.xy(train.density$x[inters[i]:inters[(i+1)]],
                                     train.density$y[inters[i]:inters[(i+1)]])
                dr   = dr + dr_i
                far  = far + dr_i - integrate.xy(test.density$x[inters[i]:inters[(i+1)]],
                                                 test.density$y[inters[i]:inters[(i+1)]])
            } else {
                dr_i = integrate.xy(test.density$x[inters[i]:inters[(i+1)]],
                                    test.density$y[inters[i]:inters[(i+1)]])
                dr   = dr + dr_i
            }
        }
    }
    return(list(dr, far))
}

param = function(train, test, n=100, false.alarm.rate) {
    
    #train.density = density(train, n = n, from = 0, to = 5)
    test.density  = density(test,  n = n, from = 0, to = 5)
    
    #train.bw = seq(train.density$bw*0.3, train.density$bw*3, length.out=n)
    test.bw  = seq(test.density$bw*0.1,  test.density$bw*3,  length.out=n)
    
    res1  = matrix(nrow = n, ncol = 2)
    res2  = matrix(nrow = n, ncol = 2)
    res3  = matrix(nrow = n, ncol = 2)
    res4  = matrix(nrow = n, ncol = 2)
    res5  = matrix(nrow = n, ncol = 2)
    for (i in 1:n) {
        
        #train.newd = density(train, bw = train.bw[i], n=1000, from=0, to=5)
        test.newd  = density(test,  bw = test.bw[i],   n=1000, from=0, to=5)
        
        rates1 = computeRates1(train1_d_30p, test.newd)
        res1[i, 1] = rates1[[1]]
        res1[i, 2] = rates1[[2]]
        
        rates2 = computeRates1(train2_d_30p, test.newd)
        res2[i, 1] = rates2[[1]]
        res2[i, 2] = rates2[[2]]
        
        rates3 = computeRates1(train3_d_30p, test.newd)
        res3[i, 1] = rates3[[1]]
        res3[i, 2] = rates3[[2]]
        
        rates4 = computeRates1(train4_d_30p, test.newd)
        res4[i, 1] = rates4[[1]]
        res4[i, 2] = rates4[[2]]
        
        rates5 = computeRates1(train5_d_30p, test.newd)
        res5[i, 1] = rates5[[1]]
        res5[i, 2] = rates5[[2]]
        
    }
    
    dr_10 = c(res1[which(abs(res1[,2]-0.1) < 0.01),1],
              res2[which(abs(res2[,2]-0.1) < 0.01),1],
              res3[which(abs(res3[,2]-0.1) < 0.01),1],
              res4[which(abs(res4[,2]-0.1) < 0.01),1],
              res5[which(abs(res5[,2]-0.1) < 0.01),1])
    
    dr_8 = c(res1[which(abs(res1[,2]-0.08) < 0.01),1],
             res2[which(abs(res2[,2]-0.08) < 0.01),1],
             res3[which(abs(res3[,2]-0.08) < 0.01),1],
             res4[which(abs(res4[,2]-0.08) < 0.01),1],
             res5[which(abs(res5[,2]-0.08) < 0.01),1])
    dr_8 = dr_8[dr_8 < mean(dr_10)]
    
    dr_6 = c(res1[which(abs(res1[,2]-0.06) < 0.02),1],
             res2[which(abs(res2[,2]-0.06) < 0.02),1],
             res3[which(abs(res3[,2]-0.06) < 0.02),1],
             res4[which(abs(res4[,2]-0.06) < 0.02),1],
             res5[which(abs(res5[,2]-0.06) < 0.02),1])
    dr_6 = dr_6[dr_6 < mean(dr_8)]
    
    dr_4 = c(res1[which(abs(res1[,2]-0.04) < 0.02),1],
             res2[which(abs(res2[,2]-0.04) < 0.02),1],
             res3[which(abs(res3[,2]-0.04) < 0.02),1],
             res4[which(abs(res4[,2]-0.04) < 0.02),1],
             res5[which(abs(res5[,2]-0.04) < 0.02),1])
    dr_4 = dr_4[dr_4 < mean(dr_6)]
    
    dr_2 = c(res1[which(abs(res1[,2]-0.02) < 0.02),1],
             res2[which(abs(res2[,2]-0.02) < 0.02),1],
             res3[which(abs(res3[,2]-0.02) < 0.02),1],
             res4[which(abs(res4[,2]-0.02) < 0.02),1],
             res5[which(abs(res5[,2]-0.02) < 0.02),1])
    dr_2 = dr_2[dr_2 < mean(dr_4)]
    
    dr_1 = c(res1[which(abs(res1[,2]-0.01) < 0.02),1],
             res2[which(abs(res2[,2]-0.01) < 0.02),1],
             res3[which(abs(res3[,2]-0.01) < 0.02),1],
             res4[which(abs(res4[,2]-0.01) < 0.02),1],
             res5[which(abs(res5[,2]-0.01) < 0.02),1])
    dr_1 = dr_1[dr_1 < mean(dr_2)]
    
    return(c(mean(dr_10), mean(dr_8), mean(dr_6), mean(dr_4), mean(dr_2), mean(dr_1)))
    
}





# computeRates1(train1_d_30p, test_d_3)
# 
# a=seq(test_d_30p$bw * 0.3, test_d_30p$bw * 3, length.out = 10)
# c=seq(train1_d_30p$bw * 0.3, train1_d_30p$bw * 3, length.out = 10)
# plot(test_d_30p)
# for (i in 1:10) {
#     plot(test_d_30p)
#     lines(density(train1_lof_30p, bw=c[i], n=1000, from=0, to=5))
#     #lines(density(test.df[,4], bw=a[i], n=1000, from=0, to=5), col="blue", lty=3)
#     print(computeRates1(density(train1_lof_30p, bw=c[i], n=1000, from=0, to=5),
#                       test_d_30p))
#     Sys.sleep(time = 1)
#     dev.off(dev.list()["RStudioGD"])
# }
# 
# computeRates1(density(train1_lof_30p, bw=c[i], n=1000, from=0, to=5),
#               density(test.df[,4], bw=a[i], n=1000, from=0, to=5))
# 
# 
# 
# 
# b=param(train1_lof_30p, test.df[,4], q=0.3,n=10)
# b[which(abs(b[,2] - 0.08) < 0.001), ]









