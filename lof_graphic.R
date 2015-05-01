# ===============================================
# Find detection rate through graphical approach
# ===============================================
# train1_d_30p = density(train1_lof_30p, n = 10000)
# train2_d_30p = density(train2_lof_30p, n = 10000)
# train3_d_30p = density(train3_lof_30p, n = 10000, from = 0, to = 5)
# train4_d_30p = density(train4_lof_30p, n = 10000, from = 0, to = 5)
# train5_d_30p = density(train5_lof_30p, n = 10000, from = 0, to = 5)
# 
# test_d_30p = density(test.df[, 4], n = 10000)
# plot(test_d_30p)
# #lines(density(train1_lof_30p, n=10000), lty=3, col = "red")
# lines(density(train1_lof_30p, n=10000, from=min(test_d_30p$x), to=max(test_d_30p$x)), lty=3, col = "red")
# 
# 
# 
# plot(test_d_30p)
# lines(train1_d_30p, col = "blue",   lty = 3)
# 
# 
# lines(train2_d_30p, col = "red",    lty = 3)
# lines(train3_d_30p, col = "green",  lty = 3)
# lines(train4_d_30p, col = "orange", lty = 3)
# lines(train5_d_30p, col = "grey",   lty = 3)

library(sfsmisc)

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

computeRates1 = function(train, test) {
    
    test.density  = density(test,  n = 10000)
    train.density = density(train, n = 10000, from = min(test.density$x), to = max(test.density$x))
    
    diffs  = c(0, diff(train.density$y < test.density$y))
    inters = which(diffs != 0)
    
    #for (i in 1:(length(inters)-1)) {
    for (i in 5:6) {
        if (i == 1) {
            if (diffs[inters][1] == -1) {
                # test density curve is higher than the train (ie, anomaly)
                test.p  = integrate.xy(test.density$x[1:inters[1]], test.density$y[1:inters[1]])
                train.p = integrate.xy(train.density$x[1:inters[1]], train.density$y[1:inters[1]])
                diff.p  = test.p - train.p
                abs.p   = diff.p / test.p
                
                detected = 0
            } else {
                diff.p = 0
                abs.p  = 0
                detected = 0
            }
        } else {
            if (diffs[inters][i] == 1 & diffs[inters][i+1] == -1) {
                ## where test density curve is higher than the train density curve
                test.x = test.density$x[inters[i]:inters[i+1]]
                test.y = test.density$y[inters[i]:inters[i+1]]
                
                train.x  = train.density$x[inters[i]:inters[i+1]]
                train.y  = train.density$y[inters[i]:inters[i+1]]
                
                test.p  = integrate.xy(test.x,  test.y)
                train.p = integrate.xy(train.x, train.y)
#                 diff.p = 0 # delete this later
#                 diff.p  = diff.p + (test.p - train.p)
                
                diff.p = test.p - train.p
                abs.p  = diff.p / test.p
                
                detected = length(which(test.x[1] < test & test < test.x[length(test.x)])) * abs.p
                
            }
        }
    }
    
    return(detected)
    return(abs.p)
    
}

computeRates1(train1_lof_30p, test.df[, 4])




# result = param(test=test.df[,4])
# plot(rev(result[[1]]), type = "b", ylim = c(0, 1), xaxt = "n", 
#      main = "Using k = 30%", xlab = "false alarm rate", ylab = "detection rate")
# axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))


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









