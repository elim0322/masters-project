# ==============================
# Principal component classifier
# ==============================
library(MASS)
library(FactoMineR)

## Read data
dat = read.csv("../Desktop/kddcup.data_10_percent_corrected", header = FALSE)
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

## Systematic sampling
sys.sample = function(N,n){
    k = floor(N/n)    
    r = sample(1:k, 1)
    sys.samp = seq(r, r + k*(n-1), k)
    return(sys.samp)
}

## sample 5000 normal connections as a train set
sample.ind = sys.sample(sum(dat$attack_type == "normal."), 5000)
train = dat[which(dat$attack_type=="normal.")[sample.ind], -c(2,3,4,7,12,20,21,22,42)]

## compute Mahalanobis distances
m2 = mahalanobis(train, colMeans(train), ginv(cov(train)), inverted = TRUE)

## multivariate trimming (using n*0.005)
train2 = train[-order(m2, decreasing = TRUE)[1:(nrow(train)*0.005)], ]

## PCA
train2.pca   = PCA(train2, ncp = ncol(train2), graph = FALSE)
majorComp    = range(which(train2.pca$eig[, 3] <= 50))
minorComp    = range(which(train2.pca$eig[, 1] <= 0.2))
scores_major = train2.pca$ind$coord[, majorComp[1]:majorComp[2]]^2
scores_minor = train2.pca$ind$coord[, minorComp[1]:minorComp[2]]^2
eigen_major  = train2.pca$eig[majorComp[1]:majorComp[2], 1]
eigen_minor  = train2.pca$eig[minorComp[1]:minorComp[2], 1]
m2_major = numeric()
m2_minor = numeric()
for (i in 1:nrow(train2)) {
    m2_major[i] = sum(scores_major[i, ] / eigen_major)
    m2_minor[i] = sum(scores_minor[i, ] / eigen_minor)
}

## empirical distributions
Fn_1 = ecdf(m2_major)
Fn_2 = ecdf(m2_minor)

## PCC_1 and PCC_2 with 0.9899 quantiles
c1 = quantile(Fn_1, probs = 0.9899)
c2 = quantile(Fn_2, probs = 0.9899)



# =======================
# Testing the classifiers
# =======================

## test with randomly selected normal (92279) and attacks (39674)
test = rbind(dat[sample(which(dat$attack_type == "normal."), 92279), -c(2,3,4,7,12,20,21,22)],
             dat[sample(which(dat$attack_type != "normal."), 39674), -c(2,3,4,7,12,20,21,22)])
test.pca = PCA(test[, -34], ncp = ncol(test[, -34]), graph = FALSE)
test.majorComp = range(which(test.pca$eig[, 3] <= 50))
test.minorComp = range(which(test.pca$eig[, 1] <= 0.2))
test.scores_major = test.pca$ind$coord[, test.majorComp[1]:test.majorComp[2]]^2
test.scores_minor = test.pca$ind$coord[, test.minorComp[1]:test.minorComp[2]]^2
test.eigen_major  = test.pca$eig[test.majorComp[1]:test.majorComp[2], 1]
test.eigen_minor  = test.pca$eig[test.minorComp[1]:test.minorComp[2], 1]
test.m2_major = numeric()
test.m2_minor = numeric()
for (i in 1:nrow(test)) {
    test.m2_major[i] = sum(test.scores_major[i, ] / test.eigen_major)
    test.m2_minor[i] = sum(test.scores_minor[i, ] / test.eigen_minor)
}

## detection rate (NEEDS WORK)
## NOTE: the classifiers are wrong?
sum(test.m2_major > c1 | test.m2_minor > c2) / sum(test$attack_type != "normal.")

