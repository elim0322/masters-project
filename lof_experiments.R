#============
# Load RWeka
#============
## increase the default heap size (512mb) to 4g
options(java.parameters = "-Xmx4g")
library(RWeka)

## need to re-install every time..
WPM("install-package", "localOutlierFactor")
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
d1_10p = density(train1_lof_10p)
d2_10p = density(train2_lof_10p)
d3_10p = density(train3_lof_10p)
d4_10p = density(train4_lof_10p)
d5_10p = density(train5_lof_10p)

d1_20p = density(train1_lof_20p)
d2_20p = density(train2_lof_20p)
d3_20p = density(train3_lof_20p)
d4_20p = density(train4_lof_20p)
d5_20p = density(train5_lof_20p)

d1_30p = density(train1_lof_30p)
d2_30p = density(train2_lof_30p)
d3_30p = density(train3_lof_30p)
d4_30p = density(train4_lof_30p)
d5_30p = density(train5_lof_30p)

d1_40p = density(train1_lof_40p)
d2_40p = density(train2_lof_40p)
d3_40p = density(train3_lof_40p)
d4_40p = density(train4_lof_40p)
d5_40p = density(train5_lof_40p)

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
legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)

plot.new()
plot.window(xlim = c(x_min_20p[which.min(x_min_20p)], x_max_20p[which.max(x_max_20p)]), ylim = c(0, y_max_20p[which.max(y_max_20p)]))
lines(density(train1_lof_20p))
lines(density(train2_lof_20p), col = "blue")
lines(density(train3_lof_20p), col = "red")
lines(density(train4_lof_20p), col = "green")
lines(density(train5_lof_20p), col = "grey")
box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 20%", xlab = "LOF", ylab = "Density")
legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)

plot.new()
plot.window(xlim = c(x_min_30p[which.min(x_min_10p)], x_max_10p[which.max(x_max_10p)]), ylim = c(0, y_max_10p[which.max(y_max_10p)]))
lines(density(train1_lof_10p))
lines(density(train2_lof_10p), col = "blue")
lines(density(train3_lof_10p), col = "red")
lines(density(train4_lof_10p), col = "green")
lines(density(train5_lof_10p), col = "grey")
box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 30%", xlab = "LOF", ylab = "Density")
legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)

plot.new()
plot.window(xlim = c(x_min_40p[which.min(x_min_40p)], x_max_40p[which.max(x_max_40p)]), ylim = c(0, y_max_40p[which.max(y_max_40p)]))
lines(density(train1_lof_40p))
lines(density(train2_lof_40p), col = "blue")
lines(density(train3_lof_40p), col = "red")
lines(density(train4_lof_40p), col = "green")
lines(density(train5_lof_40p), col = "grey")
box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 40%", xlab = "LOF", ylab = "Density")
legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)

## 90th percentile of the scores
#quantile(train2_lof, probs = 0.90)








#========================
# train: random sampling
#========================
rsample1 = sample(which(dat$attack_type == "normal."), size = 5000, replace = FALSE)
rsample2 = sample(which(dat$attack_type == "normal."), size = 5000, replace = FALSE)
rsample3 = sample(which(dat$attack_type == "normal."), size = 5000, replace = FALSE)
rsample4 = sample(which(dat$attack_type == "normal."), size = 5000, replace = FALSE)
rsample5 = sample(which(dat$attack_type == "normal."), size = 5000, replace = FALSE)

rtrain1 = dat[rsample1, -c(2,3,4,7,12,21,22,42)]
rtrain2 = dat[rsample2, -c(2,3,4,7,12,21,22,42)]
rtrain3 = dat[rsample3, -c(2,3,4,7,12,21,22,42)]
rtrain4 = dat[rsample4, -c(2,3,4,7,12,21,22,42)]
rtrain5 = dat[rsample5, -c(2,3,4,7,12,21,22,42)]

rtrain1_lof_10p = LOF(rtrain1, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
rtrain2_lof_10p = LOF(rtrain2, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
rtrain3_lof_10p = LOF(rtrain3, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
rtrain4_lof_10p = LOF(rtrain4, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]
rtrain5_lof_10p = LOF(rtrain5, control = Weka_control(min = 100, max = 100, "num-slots" = 2))[,"LOF"]

rtrain1_lof_20p = LOF(rtrain1, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
rtrain2_lof_20p = LOF(rtrain2, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
rtrain3_lof_20p = LOF(rtrain3, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
rtrain4_lof_20p = LOF(rtrain4, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]
rtrain5_lof_20p = LOF(rtrain5, control = Weka_control(min = 200, max = 200, "num-slots" = 2))[,"LOF"]

rtrain1_lof_30p = LOF(rtrain1, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
rtrain2_lof_30p = LOF(rtrain2, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
rtrain3_lof_30p = LOF(rtrain3, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
rtrain4_lof_30p = LOF(rtrain4, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]
rtrain5_lof_30p = LOF(rtrain5, control = Weka_control(min = 300, max = 300, "num-slots" = 2))[,"LOF"]

rtrain1_lof_40p = LOF(rtrain1, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
rtrain2_lof_40p = LOF(rtrain2, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
rtrain3_lof_40p = LOF(rtrain3, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
rtrain4_lof_40p = LOF(rtrain4, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]
rtrain5_lof_40p = LOF(rtrain5, control = Weka_control(min = 400, max = 400, "num-slots" = 2))[,"LOF"]

rd1_10p = density(rtrain1_lof_10p)
rd2_10p = density(rtrain2_lof_10p)
rd3_10p = density(rtrain3_lof_10p)
rd4_10p = density(rtrain4_lof_10p)
rd5_10p = density(rtrain5_lof_10p)

rd1_20p = density(rtrain1_lof_20p)
rd2_20p = density(rtrain2_lof_20p)
rd3_20p = density(rtrain3_lof_20p)
rd4_20p = density(rtrain4_lof_20p)
rd5_20p = density(rtrain5_lof_20p)

rd1_30p = density(rtrain1_lof_30p)
rd2_30p = density(rtrain2_lof_30p)
rd3_30p = density(rtrain3_lof_30p)
rd4_30p = density(rtrain4_lof_30p)
rd5_30p = density(rtrain5_lof_30p)

rd1_40p = density(rtrain1_lof_40p)
rd2_40p = density(rtrain2_lof_40p)
rd3_40p = density(rtrain3_lof_40p)
rd4_40p = density(rtrain4_lof_40p)
rd5_40p = density(rtrain5_lof_40p)

rx_max_10p = c(max(rd1_10p$x), max(rd2_10p$x), max(rd3_10p$x), max(rd4_10p$x), max(rd5_10p$x))
rx_min_10p = c(min(rd1_10p$x), min(rd2_10p$x), min(rd3_10p$x), min(rd4_10p$x), min(rd5_10p$x))
ry_max_10p = c(max(rd1_10p$y), max(rd2_10p$y), max(rd3_10p$y), max(rd4_10p$y), max(rd5_10p$y))

rx_max_20p = c(max(rd1_20p$x), max(rd2_20p$x), max(rd3_20p$x), max(rd4_20p$x), max(rd5_20p$x))
rx_min_20p = c(min(rd1_20p$x), min(rd2_20p$x), min(rd3_20p$x), min(rd4_20p$x), min(rd5_20p$x))
ry_max_20p = c(max(rd1_20p$y), max(rd2_20p$y), max(rd3_20p$y), max(rd4_20p$y), max(rd5_20p$y))

rx_max_30p = c(max(rd1_30p$x), max(rd2_30p$x), max(rd3_30p$x), max(rd4_30p$x), max(rd5_30p$x))
rx_min_30p = c(min(rd1_30p$x), min(rd2_30p$x), min(rd3_30p$x), min(rd4_30p$x), min(rd5_30p$x))
ry_max_30p = c(max(rd1_30p$y), max(rd2_30p$y), max(rd3_30p$y), max(rd4_30p$y), max(rd5_30p$y))

rx_max_40p = c(max(rd1_40p$x), max(rd2_40p$x), max(rd3_40p$x), max(rd4_40p$x), max(rd5_40p$x))
rx_min_40p = c(min(rd1_40p$x), min(rd2_40p$x), min(rd3_40p$x), min(rd4_40p$x), min(rd5_40p$x))
ry_max_40p = c(max(rd1_40p$y), max(rd2_40p$y), max(rd3_40p$y), max(rd4_40p$y), max(rd5_40p$y))

par(mfrow = c(2, 2))
plot.new()
plot.window(xlim = c(rx_min_10p[which.min(rx_min_10p)], rx_max_10p[which.max(rx_max_10p)]), ylim = c(0, ry_max_10p[which.max(ry_max_10p)]))
lines(density(rtrain1_lof_10p))
lines(density(rtrain2_lof_10p), col = "blue")
lines(density(rtrain3_lof_10p), col = "red")
lines(density(rtrain4_lof_10p), col = "green")
lines(density(rtrain5_lof_10p), col = "grey")
box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 10%", xlab = "LOF", ylab = "Density")
legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)

plot.new()
plot.window(xlim = c(rx_min_20p[which.min(rx_min_20p)], rx_max_20p[which.max(rx_max_20p)]), ylim = c(0, ry_max_20p[which.max(ry_max_20p)]))
lines(density(rtrain1_lof_20p))
lines(density(rtrain2_lof_20p), col = "blue")
lines(density(rtrain3_lof_20p), col = "red")
lines(density(rtrain4_lof_20p), col = "green")
lines(density(rtrain5_lof_20p), col = "grey")
box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 20%", xlab = "LOF", ylab = "Density")
legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)

plot.new()
plot.window(xlim = c(rx_min_30p[which.min(rx_min_30p)], rx_max_30p[which.max(rx_max_30p)]), ylim = c(0, ry_max_30p[which.max(ry_max_30p)]))
lines(density(rtrain1_lof_30p))
lines(density(rtrain2_lof_30p), col = "blue")
lines(density(rtrain3_lof_30p), col = "red")
lines(density(rtrain4_lof_30p), col = "green")
lines(density(rtrain5_lof_30p), col = "grey")
box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 30%", xlab = "LOF", ylab = "Density")
legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)

plot.new()
plot.window(xlim = c(rx_min_40p[which.min(rx_min_40p)], rx_max_40p[which.max(rx_max_40p)]), ylim = c(0, ry_max_40p[which.max(ry_max_40p)]))
lines(density(rtrain1_lof_40p))
lines(density(rtrain2_lof_40p), col = "blue")
lines(density(rtrain3_lof_40p), col = "red")
lines(density(rtrain4_lof_40p), col = "green")
lines(density(rtrain5_lof_40p), col = "grey")
box(); abline(h = 0, col = "grey"); axis(side = 1); axis(side = 2); title(main = "k = 40%", xlab = "LOF", ylab = "Density")
legend("topright", lty = 1, legend = c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5"), col = c("black", "blue", "red", "green", "grey"), cex = 0.7)











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
test_lof_10p = LOF(test[,-35], control = Weka_control(min = 0.1*10000, max = 0.1*10000, "num-slots" = 2))[,"LOF"]; gc()
test_lof_20p = LOF(test[,-35], control = Weka_control(min = 0.2*10000, max = 0.2*10000, "num-slots" = 2))[,"LOF"]; gc()
test_lof_30p = LOF(test[,-35], control = Weka_control(min = 0.3*10000, max = 0.3*10000, "num-slots" = 2))[,"LOF"]; gc()
test_lof_40p = LOF(test[,-35], control = Weka_control(min = 0.4*10000, max = 0.4*10000, "num-slots" = 2))[,"LOF"]; gc()
test_lof_50p = LOF(test[,-35], control = Weka_control(min = 0.5*10000, max = 0.5*10000, "num-slots" = 2))[,"LOF"]; gc()


## label
test.df = data.frame(test_lof_5p, test_lof_10p, test_lof_20p, test_lof_30p, test_lof_40p, test_lof_50p, "attack_type" = test$attack_type)
#save(test.df, file = "test_df.RData")
#load("test_df.RData")

## density
par(mfrow = c(3, 2))
plot(density(test.df[, 1]), main = "k = 2%")
plot(density(test.df[, 2]), main = "k = 10%")
plot(density(test.df[, 3]), main = "k = 20%")
plot(density(test.df[, 4]), main = "k = 30%")
plot(density(test.df[, 5]), main = "k = 40%")
plot(density(test.df[, 6]), main = "k = 50%")


# ==============================================================
# find the threshold at which the specified false alarm rates
# (1%, 2%, 4%, 6%, 8%, 10%) occur and return the detection rate
# for the threshold
# ==============================================================
detection_rate = function(index) {
    parameters = seq(1, 8, by = 0.1)
    falseAlarm = c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1)
    d = matrix(nrow = length(parameters), ncol = length(falseAlarm))
    
    for (i in 1:71) {
        rate = sum(test.df[test.df[[index]] > parameters[i], "attack_type"] == "normal.") / 7000
        d[i, 1] = rate
        
        for (j in 1:length(falseAlarm)) {
            d[i, j] = abs(rate - falseAlarm[j])
        }
    }
    
    (threshold = parameters[apply(d, 2, which.min)])
    detection_rate = numeric(length(threshold))
    
    for (k in 1:length(threshold)) {
        detection_rate[k] = sum(test.df[test.df[[index]] > threshold[k], "attack_type"] != "normal.") / 3000
    }
    
    return(detection_rate)
}


par(mfrow = c(3, 2))
plot(detection_rate(1), type = "b", ylim = c(0, 1), xaxt = "n", 
     main = "k = 5%", xlab = "false alarm rate", ylab = "detection rate")
axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))

plot(detection_rate(2), type = "b", ylim = c(0, 1), xaxt = "n", 
     main = "k = 10%", xlab = "false alarm rate", ylab = "detection rate")
axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))

plot(detection_rate(3), type = "b", ylim = c(0, 1), xaxt = "n", 
     main = "k = 20%", xlab = "false alarm rate", ylab = "detection rate")
axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))

plot(detection_rate(4), type = "b", ylim = c(0, 1), xaxt = "n", 
     main = "k = 30%", xlab = "false alarm rate", ylab = "detection rate")
axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))

plot(detection_rate(5), type = "b", ylim = c(0, 1), xaxt = "n", 
     main = "k = 40%", xlab = "false alarm rate", ylab = "detection rate")
axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))

plot(detection_rate(6), type = "b", ylim = c(0, 1), xaxt = "n", 
     main = "k = 50%", xlab = "false alarm rate", ylab = "detection rate")
axis(1, at = 1:6, labels = c("1%", "2%", "4%", "6%", "8%", "10%"))








