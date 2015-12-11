dat1 = read.csv("//Users/eric/Desktop/kddcup.data_10_percent", header = FALSE)
names(dat1)  = c("duration", "protocol_type", "service", "flag", "src_bytes", "dst_bytes", "land", "wrong_fragment", "urgent", "hot", "num_failed_logins", "logged_in", "num_compromised", "root_shell", "su_attempted", "num_root", "num_file_creations", "num_shells", "num_access_files", "num_outbound_cmds", "is_host_login", "is_guest_login", "count", "srv_count", "serror_rate", "srv_serror_rate", "rerror_rate", "srv_rerror_rate", "same_srv_rate", "diff_srv_rate", "srv_diff_host_rate", "dst_host_count", "dst_host_srv_count", "dst_host_same_srv_rate", "dst_host_diff_srv_rate", "dst_host_same_src_port_rate", "dst_host_srv_diff_host_rate", "dst_host_serror_rate", "dst_host_srv_serror_rate", "dst_host_rerror_rate", "dst_host_srv_rerror_rate", "attack_type")

set.seed(3); normal = sample(which(dat1$attack_type == "normal."), size = 4500, replace = FALSE)
set.seed(3); attack = sample(which(dat1$attack_type != "normal."), size = 500,  replace = FALSE)
## testset excludes any categorical/binary features
testset = dat1[c(normal, attack), -c(2,3,4,7,12,21,22)]

library(FactoMineR)
pca.res = PCA(testset[, -35], graph = FALSE, ncp = 34)
plot(pca.res$eig$eigenvalue)

## number of PCs to keep
library(nFactors)
nPC = nMreg(pca.res$eig$eigenvalue)$nFactors["b"]
pc = pca.res$ind$coord[, 1:nPC]

library(RWeka)
WPM("load-package", "XMeans")
XMeans = make_Weka_clusterer("weka/clusterers/XMeans")
xmeans.res = XMeans(as.data.frame(pc))
cMax = names(which.max(table(xmeans.res$class_ids)))  # largest cluster
table(testset[xmeans.res$class_ids != cMax, "attack_type"])







