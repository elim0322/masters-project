#============
# Load RWeka
#============
## increase the default heap size (512mb) to 4g
options(java.parameters = "-Xmx2g")
library(RWeka)

## need to re-install every time..
#WPM("install-package", "localOutlierFactor")
#WPM("load-package", "localOutlierFactor")

## make an R interface to use LOF
LOF = make_Weka_filter("weka/filters/unsupervised/attribute/LOF")


#============================
# train: systematic sampling
#============================
## sample 5000 random "normal" connections and find the LOF scores for them
sample1 = sys.sample(nrow(dat), 5000); sample1[1]
sample2 = sys.sample(nrow(dat), 5000); sample2[1]
sample3 = sys.sample(nrow(dat), 5000); sample3[1]
sample4 = sys.sample(nrow(dat), 5000); sample4[1]
sample5 = sys.sample(nrow(dat), 5000); sample5[1]

train1 = dat[which(dat$attack_type == "normal.")[sample1], -c(2,3,4,7,12,21,22,42)]
train2 = dat[which(dat$attack_type == "normal.")[sample2], -c(2,3,4,7,12,21,22,42)]
train3 = dat[which(dat$attack_type == "normal.")[sample3], -c(2,3,4,7,12,21,22,42)]
train4 = dat[which(dat$attack_type == "normal.")[sample4], -c(2,3,4,7,12,21,22,42)]
train5 = dat[which(dat$attack_type == "normal.")[sample5], -c(2,3,4,7,12,21,22,42)]

train1_lof_10p = LOF(train1, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
train2_lof_10p = LOF(train2, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
train3_lof_10p = LOF(train3, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
train4_lof_10p = LOF(train4, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
train5_lof_10p = LOF(train5, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]

train1_lof_20p = LOF(train1, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
train2_lof_20p = LOF(train2, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
train3_lof_20p = LOF(train3, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
train4_lof_20p = LOF(train4, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
train5_lof_20p = LOF(train5, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]

train1_lof_30p = LOF(train1, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
train2_lof_30p = LOF(train2, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
train3_lof_30p = LOF(train3, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
train4_lof_30p = LOF(train4, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
train5_lof_30p = LOF(train5, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]

train1_lof_40p = LOF(train1, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
train2_lof_40p = LOF(train2, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
train3_lof_40p = LOF(train3, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
train4_lof_40p = LOF(train4, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
train5_lof_40p = LOF(train5, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]

## density distribution of the scores
train1_d_10p = density(train1_lof_10p)
train2_d_10p = density(train2_lof_10p)
train3_d_10p = density(train3_lof_10p)
train4_d_10p = density(train4_lof_10p)
train5_d_10p = density(train5_lof_10p)

train1_d_20p = density(train1_lof_20p)
train2_d_20p = density(train2_lof_20p)
train3_d_20p = density(train3_lof_20p)
train4_d_20p = density(train4_lof_20p)
train5_d_20p = density(train5_lof_20p)

train1_d_30p = density(train1_lof_30p, n = 1000, from = 0, to = 5)
train2_d_30p = density(train2_lof_30p, n = 1000, from = 0, to = 5)
train3_d_30p = density(train3_lof_30p, n = 1000, from = 0, to = 5)
train4_d_30p = density(train4_lof_30p, n = 1000, from = 0, to = 5)
train5_d_30p = density(train5_lof_30p, n = 1000, from = 0, to = 5)

train1_d_40p = density(train1_lof_40p)
train2_d_40p = density(train2_lof_40p)
train3_d_40p = density(train3_lof_40p)
train4_d_40p = density(train4_lof_40p)
train5_d_40p = density(train5_lof_40p)

## density plot
x_max_10p = c(max(d1_10p$x), max(d2_10p$x), max(d3_10p$x), max(d4_10p$x), max(d5_10p$x))
x_min_10p = c(min(d1_10p$x), min(d2_10p$x), min(d3_10p$x), min(d4_10p$x), min(d5_10p$x))
y_max_10p = c(max(d1_10p$y), max(d2_10p$y), max(d3_10p$y), max(d4_10p$y), max(d5_10p$y))

x_max_20p = c(max(d1_20p$x), max(d2_20p$x), max(d3_20p$x), max(d4_20p$x), max(d5_20p$x))
x_min_20p = c(min(d1_20p$x), min(d2_20p$x), min(d3_20p$x), min(d4_20p$x), min(d5_20p$x))
y_max_20p = c(max(d1_20p$y), max(d2_20p$y), max(d3_20p$y), max(d4_20p$y), max(d5_20p$y))

x_max_30p = c(max(d1_30p$x), max(d2_30p$x), max(d3_30p$x), max(d4_30p$x), max(d5_30p$x))
x_min_30p = c(min(d1_30p$x), min(d2_30p$x), min(d3_30p$x), min(d4_30p$x), min(d5_30p$x))
y_max_30p = c(max(d1_30p$y), max(d2_30p$y), max(d3_30p$y), max(d4_30p$y), max(d5_30p$y))

x_max_40p = c(max(d1_40p$x), max(d2_40p$x), max(d3_40p$x), max(d4_40p$x), max(d5_40p$x))
x_min_40p = c(min(d1_40p$x), min(d2_40p$x), min(d3_40p$x), min(d4_40p$x), min(d5_40p$x))
y_max_40p = c(max(d1_40p$y), max(d2_40p$y), max(d3_40p$y), max(d4_40p$y), max(d5_40p$y))

par(mfrow = c(2, 2))
plot.new()
plot.window(xlim = c(x_min_10p[which.min(x_min_10p)], x_max_10p[which.max(x_max_10p)]), ylim = c(0, y_max_10p[which.max(y_max_10p)]))
lines(density(train1_lof_10p))
lines(density(train2_lof_10p), col = "blue")
lines(density(train3_lof_10p), col = "red")
lines(density(train4_lof_10p), col = "green")
lines(density(train5_lof_10p), col = "grey")
box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 10%", xlab = "LOF", ylab = "Density")
#legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)

plot.new()
plot.window(xlim = c(x_min_20p[which.min(x_min_20p)], x_max_20p[which.max(x_max_20p)]), ylim = c(0, y_max_20p[which.max(y_max_20p)]))
lines(density(train1_lof_20p))
lines(density(train2_lof_20p), col = "blue")
lines(density(train3_lof_20p), col = "red")
lines(density(train4_lof_20p), col = "green")
lines(density(train5_lof_20p), col = "grey")
box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 20%", xlab = "LOF", ylab = "Density")
#legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)

plot.new()
plot.window(xlim = c(x_min_30p[which.min(x_min_30p)], x_max_30p[which.max(x_max_30p)]), ylim = c(0, y_max_30p[which.max(y_max_30p)]))
lines(density(train1_lof_30p))
lines(density(train2_lof_30p), col = "blue")
lines(density(train3_lof_30p), col = "red")
lines(density(train4_lof_30p), col = "green")
lines(density(train5_lof_30p), col = "grey")
box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 30%", xlab = "LOF", ylab = "Density")
#legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)

plot.new()
plot.window(xlim = c(x_min_40p[which.min(x_min_40p)], x_max_40p[which.max(x_max_40p)]), ylim = c(0, y_max_40p[which.max(y_max_40p)]))
lines(density(train1_lof_40p))
lines(density(train2_lof_40p), col = "blue")
lines(density(train3_lof_40p), col = "red")
lines(density(train4_lof_40p), col = "green")
lines(density(train5_lof_40p), col = "grey")
box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 40%", xlab = "LOF", ylab = "Density")
#legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)



#========================
# train: random sampling
#========================
# rsample1 = sample(which(dat$attack_type == "normal."), size = 5000, replace = FALSE)
# rsample2 = sample(which(dat$attack_type == "normal."), size = 5000, replace = FALSE)
# rsample3 = sample(which(dat$attack_type == "normal."), size = 5000, replace = FALSE)
# rsample4 = sample(which(dat$attack_type == "normal."), size = 5000, replace = FALSE)
# rsample5 = sample(which(dat$attack_type == "normal."), size = 5000, replace = FALSE)
# 
# rtrain1 = dat[rsample1, -c(2,3,4,7,12,21,22,42)]
# rtrain2 = dat[rsample2, -c(2,3,4,7,12,21,22,42)]
# rtrain3 = dat[rsample3, -c(2,3,4,7,12,21,22,42)]
# rtrain4 = dat[rsample4, -c(2,3,4,7,12,21,22,42)]
# rtrain5 = dat[rsample5, -c(2,3,4,7,12,21,22,42)]
# 
# rtrain1_lof_10p = LOF(rtrain1, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
# rtrain2_lof_10p = LOF(rtrain2, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
# rtrain3_lof_10p = LOF(rtrain3, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
# rtrain4_lof_10p = LOF(rtrain4, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
# rtrain5_lof_10p = LOF(rtrain5, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
# 
# rtrain1_lof_20p = LOF(rtrain1, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
# rtrain2_lof_20p = LOF(rtrain2, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
# rtrain3_lof_20p = LOF(rtrain3, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
# rtrain4_lof_20p = LOF(rtrain4, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
# rtrain5_lof_20p = LOF(rtrain5, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
# 
# rtrain1_lof_30p = LOF(rtrain1, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
# rtrain2_lof_30p = LOF(rtrain2, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
# rtrain3_lof_30p = LOF(rtrain3, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
# rtrain4_lof_30p = LOF(rtrain4, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
# rtrain5_lof_30p = LOF(rtrain5, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
# 
# rtrain1_lof_40p = LOF(rtrain1, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
# rtrain2_lof_40p = LOF(rtrain2, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
# rtrain3_lof_40p = LOF(rtrain3, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
# rtrain4_lof_40p = LOF(rtrain4, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
# rtrain5_lof_40p = LOF(rtrain5, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
# 
# rd1_10p = density(rtrain1_lof_10p)
# rd2_10p = density(rtrain2_lof_10p)
# rd3_10p = density(rtrain3_lof_10p)
# rd4_10p = density(rtrain4_lof_10p)
# rd5_10p = density(rtrain5_lof_10p)
# 
# rd1_20p = density(rtrain1_lof_20p)
# rd2_20p = density(rtrain2_lof_20p)
# rd3_20p = density(rtrain3_lof_20p)
# rd4_20p = density(rtrain4_lof_20p)
# rd5_20p = density(rtrain5_lof_20p)
# 
# rd1_30p = density(rtrain1_lof_30p)
# rd2_30p = density(rtrain2_lof_30p)
# rd3_30p = density(rtrain3_lof_30p)
# rd4_30p = density(rtrain4_lof_30p)
# rd5_30p = density(rtrain5_lof_30p)
# 
# rd1_40p = density(rtrain1_lof_40p)
# rd2_40p = density(rtrain2_lof_40p)
# rd3_40p = density(rtrain3_lof_40p)
# rd4_40p = density(rtrain4_lof_40p)
# rd5_40p = density(rtrain5_lof_40p)
# 
# rx_max_10p = c(max(rd1_10p$x), max(rd2_10p$x), max(rd3_10p$x), max(rd4_10p$x), max(rd5_10p$x))
# rx_min_10p = c(min(rd1_10p$x), min(rd2_10p$x), min(rd3_10p$x), min(rd4_10p$x), min(rd5_10p$x))
# ry_max_10p = c(max(rd1_10p$y), max(rd2_10p$y), max(rd3_10p$y), max(rd4_10p$y), max(rd5_10p$y))
# 
# rx_max_20p = c(max(rd1_20p$x), max(rd2_20p$x), max(rd3_20p$x), max(rd4_20p$x), max(rd5_20p$x))
# rx_min_20p = c(min(rd1_20p$x), min(rd2_20p$x), min(rd3_20p$x), min(rd4_20p$x), min(rd5_20p$x))
# ry_max_20p = c(max(rd1_20p$y), max(rd2_20p$y), max(rd3_20p$y), max(rd4_20p$y), max(rd5_20p$y))
# 
# rx_max_30p = c(max(rd1_30p$x), max(rd2_30p$x), max(rd3_30p$x), max(rd4_30p$x), max(rd5_30p$x))
# rx_min_30p = c(min(rd1_30p$x), min(rd2_30p$x), min(rd3_30p$x), min(rd4_30p$x), min(rd5_30p$x))
# ry_max_30p = c(max(rd1_30p$y), max(rd2_30p$y), max(rd3_30p$y), max(rd4_30p$y), max(rd5_30p$y))
# 
# rx_max_40p = c(max(rd1_40p$x), max(rd2_40p$x), max(rd3_40p$x), max(rd4_40p$x), max(rd5_40p$x))
# rx_min_40p = c(min(rd1_40p$x), min(rd2_40p$x), min(rd3_40p$x), min(rd4_40p$x), min(rd5_40p$x))
# ry_max_40p = c(max(rd1_40p$y), max(rd2_40p$y), max(rd3_40p$y), max(rd4_40p$y), max(rd5_40p$y))
# 
# par(mfrow = c(2, 2))
# plot.new()
# plot.window(xlim = c(rx_min_10p[which.min(rx_min_10p)], rx_max_10p[which.max(rx_max_10p)]), ylim = c(0, ry_max_10p[which.max(ry_max_10p)]))
# lines(density(rtrain1_lof_10p))
# lines(density(rtrain2_lof_10p), col = "blue")
# lines(density(rtrain3_lof_10p), col = "red")
# lines(density(rtrain4_lof_10p), col = "green")
# lines(density(rtrain5_lof_10p), col = "grey")
# box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 10%", xlab = "LOF", ylab = "Density")
# #legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)
# 
# plot.new()
# plot.window(xlim = c(rx_min_20p[which.min(rx_min_20p)], rx_max_20p[which.max(rx_max_20p)]), ylim = c(0, ry_max_20p[which.max(ry_max_20p)]))
# lines(density(rtrain1_lof_20p))
# lines(density(rtrain2_lof_20p), col = "blue")
# lines(density(rtrain3_lof_20p), col = "red")
# lines(density(rtrain4_lof_20p), col = "green")
# lines(density(rtrain5_lof_20p), col = "grey")
# box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 20%", xlab = "LOF", ylab = "Density")
# #legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)
# 
# plot.new()
# plot.window(xlim = c(rx_min_30p[which.min(rx_min_30p)], rx_max_30p[which.max(rx_max_30p)]), ylim = c(0, ry_max_30p[which.max(ry_max_30p)]))
# lines(density(rtrain1_lof_30p))
# lines(density(rtrain2_lof_30p), col = "blue")
# lines(density(rtrain3_lof_30p), col = "red")
# lines(density(rtrain4_lof_30p), col = "green")
# lines(density(rtrain5_lof_30p), col = "grey")
# box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 30%", xlab = "LOF", ylab = "Density")
# #legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)
# 
# plot.new()
# plot.window(xlim = c(rx_min_40p[which.min(rx_min_40p)], rx_max_40p[which.max(rx_max_40p)]), ylim = c(0, ry_max_40p[which.max(ry_max_40p)]))
# lines(density(rtrain1_lof_40p))
# lines(density(rtrain2_lof_40p), col = "blue")
# lines(density(rtrain3_lof_40p), col = "red")
# lines(density(rtrain4_lof_40p), col = "green")
# lines(density(rtrain5_lof_40p), col = "grey")
# box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 40%", xlab = "LOF", ylab = "Density")
# #legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)





#=======
# test
#=======
#normal = sample(which(dat$attack_type == "normal."), size = 92279, replace = TRUE)
#attack = sample(which(dat$attack_type != "normal."), size = 39674, replace = TRUE)

## 70% "normal" and 30% attacks
normal   = sample(which(dat$attack_type == "normal."), size = 7000, replace = TRUE)
attack   = sample(which(dat$attack_type != "normal."), size = 3000, replace = TRUE)
category = c(2,3,4,7,12,21,22)#,42)
test     = dat[c(normal, attack), -category]

test_lof_5p  = LOF(test[,-35], control = Weka_control(min = 0.05*10000, max = 0.05*10000, "num-slots" = 2))[,"LOF"]; gc()
test_lof_10p = LOF(test[,-35], control = Weka_control(min = 0.1*10000,  max = 0.1*10000, "num-slots" = 2))[,"LOF"]; gc()
test_lof_20p = LOF(test[,-35], control = Weka_control(min = 0.2*10000,  max = 0.2*10000, "num-slots" = 2))[,"LOF"]; gc()
test_lof_30p = LOF(test[,-35], control = Weka_control(min = 0.3*10000,  max = 0.3*10000, "num-slots" = 2))[,"LOF"]; gc()
test_lof_40p = LOF(test[,-35], control = Weka_control(min = 0.4*10000,  max = 0.4*10000, "num-slots" = 2))[,"LOF"]; gc()
test_lof_50p = LOF(test[,-35], control = Weka_control(min = 0.5*10000,  max = 0.5*10000, "num-slots" = 2))[,"LOF"]; gc()


## label
test.df = data.frame(test_lof_5p, test_lof_10p, test_lof_20p, test_lof_30p, test_lof_40p,
                     test_lof_50p, "attack_type" = test$attack_type, "index" = c(normal, attack))
#save(test.df, file = "test_df.RData")
#load("test_df.RData")

## density
test_d_5p  = density(test.df[, 1])
test_d_10p = density(test.df[, 2])
test_d_20p = density(test.df[, 3])
test_d_30p = density(test.df[, 4], n = 1000, from = 0, to = 5)
test_d_40p = density(test.df[, 5])
test_d_50p = density(test.df[, 6])

par(mfrow = c(3, 2))
plot(test_d_5p, main = "k = 5%")
plot(test_d_10p, main = "k = 10%")
plot(test_d_20p, main = "k = 20%")
plot(test_d_30p, main = "k = 30%")
plot(test_d_40p, main = "k = 40%")
plot(test_d_50p, main = "k = 50%")


# ===============================================
# Find detection rate through graphical approach
# ===============================================
plot(test_d_30p)
lines(train1_d_30p, col = "blue",   lty = 3)
lines(train2_d_30p, col = "red",    lty = 3)
lines(train3_d_30p, col = "green",  lty = 3)
lines(train4_d_30p, col = "orange", lty = 3)
lines(train5_d_30p, col = "grey",   lty = 3)

diff1 = c(0, diff(train1_d_30p$y < test_d_30p$y))
intersX <- train1_d_30p$x[as.logical(abs(diff1))]
intersY <- train1_d_30p$y[as.logical(abs(diff1))]
abline(v = intersX, lty = 3)
#abline(h = intersY)

library(sfsmisc)
intersInd1 = which(diff1!=0)
#a = 0
for (i in 1:(length(intersInd1)-1)) {
    if (i == 1) {
        if (diff1[intersInd1][1] == -1) {
            dr1  = integrate.xy(train1_d_30p$x[1:intersInd1[1]], train1_d_30p$y[1:intersInd1[1]])
            far1 = dr1 - integrate.xy(test_d_30p$x[1:intersInd1[1]], test_d_30p$y[1:intersInd1[1]])
        } else {
            dr1 = integrate.xy(test_d_30p$x[1:intersInd1[1]], test_d_30p$y[1:intersInd1[1]])
        }
    } else {
        if (diff1[intersInd1][i]==-1 & diff1[intersInd1][i+1]==1) {
            dr1_i = integrate.xy(train1_d_30p$x[intersInd1[i]:intersInd1[(i+1)]],
                                train1_d_30p$y[intersInd1[i]:intersInd1[(i+1)]])
            dr1   = dr1 + dr1_i
            far1  = far1 + dr1_i - integrate.xy(test_d_30p$x[intersInd1[i]:intersInd1[(i+1)]],
                                                test_d_30p$y[intersInd1[i]:intersInd1[(i+1)]])
        } else {
            dr1_i = integrate.xy(test_d_30p$x[intersInd1[i]:intersInd1[(i+1)]],
                                 test_d_30p$y[intersInd1[i]:intersInd1[(i+1)]])
            dr1   = dr1 + dr1_i
#             a    = a + dr1_i - integrate.xy(train1_d_30p$x[intersInd1[i]:intersInd1[(i+1)]],
#                                             train1_d_30p$y[intersInd1[i]:intersInd1[(i+1)]])
        }
    }
}

diff2 = c(0, diff(train2_d_30p$y < test_d_30p$y))
intersInd2 = which(diff2 != 0)
for (i in 1:(length(intersInd2)-1)) {
    if (i == 1) {
        if (diff2[intersInd2][1] == -1) {
            dr2  = integrate.xy(train2_d_30p$x[1:intersInd2[1]], train2_d_30p$y[1:intersInd2[1]])
            far2 = dr2 - integrate.xy(test_d_30p$x[1:intersInd2[1]], test_d_30p$y[1:intersInd2[1]])
        } else {
            dr2 = integrate.xy(test_d_30p$x[1:intersInd2[1]], test_d_30p$y[1:intersInd2[1]])
            far2 = 0
        }
    } else {
        if (diff2[intersInd2][i]==-1 & diff2[intersInd2][i+1]==1) {
            dr2_i = integrate.xy(train2_d_30p$x[intersInd2[i]:intersInd2[(i+1)]],
                                train2_d_30p$y[intersInd2[i]:intersInd2[(i+1)]])
            dr2   = dr2 + dr2_i
            far2  = far2 + dr2_i - integrate.xy(test_d_30p$x[intersInd2[i]:intersInd2[(i+1)]],
                                             test_d_30p$y[intersInd2[i]:intersInd2[(i+1)]])
        } else {
            dr2_i = integrate.xy(test_d_30p$x[intersInd2[i]:intersInd2[(i+1)]],
                                test_d_30p$y[intersInd2[i]:intersInd2[(i+1)]])
            dr2   = dr2 + dr2_i
        }
    }
}








for (i in 1:ncol(test)) {
    u = unique(test[, i])
    t = table(test[, i])
    
    if (length(u) == 1) {
        cat(paste(i, "unique", "\n"))
    }
    
    if (any(t > 0.5*nrow(test))) {
        cat(paste(i, "more than half", "\n"))
    }
}

pcout(test[complete.cases(test), 2:4])










# ==============================================================
# find the threshold at which the specified false alarm rates
# (1%, 2%, 4%, 6%, 8%, 10%) occur and return the detection rate
# for the threshold
# ==============================================================
# detection_rate = function(index) {
#     parameters = seq(1, 8, by = 0.1)
#     falseAlarm = c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1)
#     d = matrix(nrow = length(parameters), ncol = length(falseAlarm))
#     
#     for (i in 1:71) {
#         rate = sum(test.df[test.df[[index]] > parameters[i], "attack_type"] == "normal.") / 7000
#         d[i, 1] = rate
#         
#         for (j in 1:length(falseAlarm)) {
#             d[i, j] = abs(rate - falseAlarm[j])
#         }
#     }
#     
#     (threshold = parameters[apply(d, 2, which.min)])
#     detection_rate = numeric(length(threshold))
#     
#     for (k in 1:length(threshold)) {
#         detection_rate[k] = sum(test.df[test.df[[index]] > threshold[k], "attack_type"] != "normal.") / 3000
#     }
#     
#     return(detection_rate)
# }
# 
# par(mfrow = c(3, 2))
# plot(detection_rate(1), type = "b", ylim = c(0, 1), xaxt = "n", 
#      main = "k = 5%", xlab = "false alarm rate", ylab = "detection rate")
# axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))
# 
# plot(detection_rate(2), type = "b", ylim = c(0, 1), xaxt = "n", 
#      main = "k = 10%", xlab = "false alarm rate", ylab = "detection rate")
# axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))
# 
# plot(detection_rate(3), type = "b", ylim = c(0, 1), xaxt = "n", 
#      main = "k = 20%", xlab = "false alarm rate", ylab = "detection rate")
# axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))
# 
# plot(detection_rate(4), type = "b", ylim = c(0, 1), xaxt = "n", 
#      main = "k = 30%", xlab = "false alarm rate", ylab = "detection rate")
# axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))
# 
# plot(detection_rate(5), type = "b", ylim = c(0, 1), xaxt = "n", 
#      main = "k = 40%", xlab = "false alarm rate", ylab = "detection rate")
# axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))
# 
# plot(detection_rate(6), type = "b", ylim = c(0, 1), xaxt = "n", 
#      main = "k = 50%", xlab = "false alarm rate", ylab = "detection rate")
# axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))








