options(java.parameters = "-Xmx6g")
library(RWeka)
WPM("install-package", "localOutlierFactor")
WPM("install-package", "XMeans")
LOF = make_Weka_filter("weka/filters/unsupervised/attribute/LOF")
dat = read.csv("H:/Documents/kddcup.data_10_percent_corrected", header = FALSE)
names(dat) = c("duration", "protocol_type", "service", "flag",
               "src_bytes", "dst_bytes", "land", "wrong_fragment",
               "urgent", "hot", "num_failed_logins", "logged_in",
               "num_compromised", "root_shell", "su_attempted",
               "num_root", "num_file_creations", "num_shells",
               "num_access_files", "num_outbound_cmds", "is_host_login",
               "is_guest_login", "count", "srv_count", "serror_rate",
               "srv_serror_rate", "rerror_rate", "srv_rerror_rate",
               "same_srv_rate", "diff_srv_rate", "srv_diff_host_rate",
               "dst_host_count", "dst_host_srv_count", "dst_host_same_srv_rate",
               "dst_host_diff_srv_rate", "dst_host_same_src_port_rate",
               "dst_host_srv_diff_host_rate", "dst_host_serror_rate",
               "dst_host_srv_serror_rate", "dst_host_rerror_rate",
               "dst_host_srv_rerror_rate", "attack_type")

set.seed(1)
normal   = sample(which(dat$attack_type == "normal."), size = 3500, replace = FALSE)
set.seed(1)
attack   = sample(which(dat$attack_type != "normal."), size = 1500, replace = FALSE)
category = c(2,3,4,7,12,21,22)#,42)
test1    = dat[c(normal, attack), -category]
train1   = dat[sample(which(dat$attack_type == "normal."), 5000), -category]

cat(paste0("starting  k = 10%", "\n"))
lof_10p = LOF(test1, control = Weka_control(min = 500, max = 500, "num-slots" = 2))[,"LOF"]; invisible(gc())
cat(paste0("completed k = 10%", "\n", "\n"))

cat(paste0("starting  k = 20%", "\n"))
lof_20p = LOF(test1, control = Weka_control(min = 1000, max = 1000, "num-slots" = 2))[,"LOF"]; invisible(gc())
cat(paste0("completed k = 20%", "\n", "\n"))

cat(paste0("starting  k = 30%", "\n"))
lof_30p = LOF(test1, control = Weka_control(min = 1500, max = 1500, "num-slots" = 2))[,"LOF"]; invisible(gc())
cat(paste0("completed k = 30%", "\n", "\n"))

cat(paste0("starting  k = 40%", "\n"))
lof_40p = LOF(test1, control = Weka_control(min = 2000, max = 2000, "num-slots" = 2))[,"LOF"]; invisible(gc())
cat(paste0("completed k = 40%", "\n", "\n"))

cat(paste0("starting  k = 50%", "\n"))
lof_50p = LOF(test1, control = Weka_control(min = 2500, max = 2500, "num-slots" = 2))[,"LOF"]; invisible(gc())
cat(paste0("completed k = 50%", "\n", "\n"))

names(lof_10p) <- dat[c(normal, attack), "attack_type"]
names(lof_20p) <- dat[c(normal, attack), "attack_type"]
names(lof_30p) <- dat[c(normal, attack), "attack_type"]
names(lof_40p) <- dat[c(normal, attack), "attack_type"]
names(lof_50p) <- dat[c(normal, attack), "attack_type"]

# plot(density(lof_10p, from = 0, to = 5))
# lines(density(lof_20p), lty = 3, col = "red")
# lines(density(lof_30p), lty = 3, col = "blue")
# lines(density(lof_40p), lty = 3, col = "green")
# lines(density(lof_50p), lty = 3, col = "grey")
# d = density(lof_50p, n = 10000)

# plot(density(lof_10p))
# sum(names(lof_10p[lof_10p > 1.1]) != "normal.")
# sum(names(lof_10p[lof_10p > 1.1]) == "normal.")

finite_difference = function(density) {
  fp1 = numeric()
  #fp2 = numeric()
  n  = length(density$x)
  
  ## forward difference is used
  for (i in 1:(n-1)) {
    fx0 = density$y[i]
    fx1 = density$y[i+1]
    h   = density$x[i+1] - density$x[i]
    fp1[i] = (fx1 - fx0) / h
  }
  
#   for (j in 1:(n-2)) {
#     fx2 = density$y[j+2]
#     fx1 = density$y[j+1]
#     fx0 = density$y[j]
#     h   = density$x[j+1] - density$x[j]
#     fp2[j] = (fx2 - 2 * fx1 + fx0) / h^2
#   }
#   return(list(fp1 = fp1, fp2 = fp2))
  
  return(fp1)
}
slope = function(fp) {
  allneg  = which(fp < 0)
  neg.ind = allneg[1:(min(which(diff(allneg) != 1)) - 1)]
  
  n = length(neg.ind)
  trim = floor(0.5 * n)
  trimmed.ind = neg.ind[trim:n]
  
  return(index = trimmed.ind)
}
experiment = function(data) {
  d = density(data, n = 10 * length(data))
  fp = finite_difference(d)
  neg = slope(fp)
  
  #ind = neg[which.min(diff(fp[neg]))]
  #threshold = d$x[ind]
  
  threshold = d$x[neg[length(neg)]] - diff(c(d$x[neg[1]], d$x[neg[length(neg)]])) * 0.1
  
  ## floor to be generous
  dr = sum(names(data[data >= threshold]) != "normal.") / sum(test1$attack_type != "normal.")
  fa = sum(names(data[data >= threshold]) == "normal.") / sum(test1$attack_type == "normal.")
  
  ret = list(threshold = threshold, detection.rate = dr, false.alarm = fa, detected = which(data >= threshold))
  return(ret)
}

p10 = experiment(lof_10p)
p20 = experiment(lof_20p)
p30 = experiment(lof_30p)
p40 = experiment(lof_40p)
p50 = experiment(lof_50p)
result = data.frame(cbind("k = 10%" = p10[-4], "k = 20%" = p20[-4], "k = 30%" = p30[-4], "k = 40%" = p40[-4], "k = 50%" = p50[-4]))
print(result)

trim1 = test1[p30$detected, ];  trim1 = trim1[, sapply(trim1, function(x) length(unique(x)))!=1]
trim2 = test1[-p30$detected, ]; trim2 = trim2[, sapply(trim2, function(x) length(unique(x)))!=1]

# trim2 = test1[p40$detected, ]
# trim3 = test1[p50$detected, ]

# obsolete2 = numeric()
# for (i in 1:ncol(train1)) {
#   u = unique(train1[, i])
#   if (length(u) == 1) {
#     obsolete2 = c(obsolete, i)
#   }
# }

library(FactoMineR)
res1 = PCA(trim1[, -which(names(trim1) == "attack_type")], graph = FALSE)

# res2 = PCA(trim2[, -c(obsolete, which(names(trim2) == "attack_type"))], ncp = 35, graph = FALSE)
# train.res = PCA(train1[, -c(obsolete2, which(names(train1) == "attack_type"))], ncp = 35, graph = FALSE)
# res3 = PCA(trim3[, -c(obsolete, which(names(trim3) == "attack_type"))], ncp = 35, graph = FALSE)
# res4 = PCA(trim4[, -c(obsolete, which(names(trim4) == "attack_type"))], graph = FALSE)
# 
# # par(mfrow = c(2, 2))
# # library(MASS)
# # cols2 = ifelse(trim1$attack_type == "normal.", "black", "red")
# # eqscplot(res1$ind$coord[,1:2], col = cols2); abline(h = 0, v = 0)
# # eqscplot(res2$ind$coord[,1:2], col = cols2); abline(h = 0, v = 0)
# # eqscplot(res3$ind$coord[,1:2], col = cols2); abline(h = 0, v = 0)
# # eqscplot(res4$ind$coord[,1:2], col = cols2); abline(h = 0, v = 0)
# 
# dim90p = min(which(res2$eig[[3]] > 90))
# a = kmeans(res2$ind$coord[,1:dim90p], centers = 23, nstart = 20)
# for (i in 1:23) {
#   print(summary(trim2[which(a$cluster == i), "attack_type"])[summary(trim2[which(a$cluster == i), "attack_type"]) != 0])
# }
# 
# result = FAMD(trim2[, -obsolete])
# result$quali.var$coord[1, 1:2]
# 
# which(result$ind$coord[,1]-result$quali.var$coord[1, 1] < 1e-10)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# b = kmeans(res2$ind$coord[, 1:2], centers = 4, nstart = 20)
# cols = rep("black", dim(res2$ind$coord)[1])
# cols[b$cluster == 2] = "red"
# cols[b$cluster == 3] = "green"
# cols[b$cluster == 4] = "blue"
# #cols[b$cluster == 4] = "grey"
# # # cols[a$cluster == 5] = "blue"
# # # cols[a$cluster == 6] = "navy"
# # # cols[a$cluster == 7] = "purple"
# # # cols[a$cluster == 8] = "grey"
# # # cols[a$cluster == 9] = "aquamarine"
# 
# eqscplot(res$ind$coord[,1:2], col = cols); abline(h = 0, v = 0)
# 
# cols2 = ifelse(trim2$attack_type == "normal.", "black", "red")
# eqscplot(res2$ind$coord[,1:2], col = cols2); abline(h = 0, v = 0)
# 
# par(mfrow = c(1, 2))
# eqscplot(res2$ind$coord[,1:2], col = cols); abline(h = 0, v = 0)
# eqscplot(res2$ind$coord[,1:2], col = cols2); abline(h = 0, v = 0)
# 
# c1 = res$ind$coord[which(a$cluster == 1), 1:2]
# c2 = res$ind$coord[which(a$cluster == 2), 1:2]
# c3 = res$ind$coord[which(a$cluster == 3), 1:2]
# c4 = res$ind$coord[which(a$cluster == 4), 1:2]
# c5 = res$ind$coord[which(a$cluster == 5), 1:2]
# 
# (max(c1[, 1]) - min(c1[, 1])) * (max(c1[, 2]) - min(c1[, 2]))
# (max(c2[, 1]) - min(c2[, 1])) * (max(c2[, 2]) - min(c2[, 2]))
# (max(c3[, 1]) - min(c3[, 1])) * (max(c3[, 2]) - min(c3[, 2]))
# (max(c4[, 1]) - min(c4[, 1])) * (max(c4[, 2]) - min(c4[, 2]))
# (max(c5[, 1]) - min(c5[, 1])) * (max(c5[, 2]) - min(c5[, 2]))
# 
# sum(trim1[c(which(a$cluster == 1), which(a$cluster == 2)), "attack_type"] != "normal.")
# 
# 
# 
# 
# for (i in 1:ncol(trim4)) {
#     u = unique(trim4[,i])
#     if (length(u) == 1) {
#         cat(paste(i, "\n"))
#     }
# }
# c = kmeans(trim4[,-35], centers = 5); c$size
# sum(trim4[which(c$cluster == 4), "attack_type"] != "normal.")
# 






