dat = read.csv("../kddcup.data_10_percent_corrected", header = FALSE)
dat2 = read.csv("../kddcup.testdata_10_percent_corrected", header = FALSE)
names(dat2) = c("duration", "protocol_type", "service", "flag",
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
#load(file = "../Desktop/kddcup.data_10_percent_corrected.RData")
options(java.parameters = "-Xmx4g")
library(RWeka)
Sys.setenv(WEKA_HOME = "C:\\Users\\OEM\\wekafiles")
WPM("load-package", "localOutlierFactor")
WPM("load-package", "XMeans")
LOF = make_Weka_filter("weka/filters/unsupervised/attribute/LOF")
# WPM("install-package", "localOutlierFactor")
# WPM("install-package", "XMeans")

#save(dat, file = "../kddcup.data_10_percent_corrected.RData")


############
## Normal ##
############
dat[dat$attack_type == "normal.", "attack_type_main"] = "normal"

#######################
## Denial of Service ##
#######################
dos_attacks = dat$attack_type == "back."    | dat$attack_type == "land." |
              dat$attack_type == "neptune." | dat$attack_type == "pod." |
              dat$attack_type == "smurf."   | dat$attack_type == "teardrop."

## CHECK (note: 391458 DoS attacks in the data)
sum(dat$attack_type == "back.") + sum(dat$attack_type == "land.") + sum(dat$attack_type == "neptune.") + sum(dat$attack_type == "pod.") + sum(dat$attack_type == "smurf.") + sum(dat$attack_type == "teardrop.") == sum(dos_attacks)

## label "dos" attacks
dat[dos_attacks, "attack_type_main"] = "dos"

##################
## User to Root ##
##################
## 52 U2R attacks
u2r_attacks = dat$attack_type == "buffer_overflow." | dat$attack_type == "loadmodule." |
              dat$attack_type == "perl." | dat$attack_type == "rootkit."

sum(dat$attack_type == "buffer_overflow.") + sum(dat$attack_type == "loadmodule.") + sum(dat$attack_type == "perl.") + sum(dat$attack_type == "rootkit.") == sum(u2r_attacks)

dat[u2r_attacks, "attack_type_main"] = "u2r"

#####################
## Remote to Local ##
#####################
## 1126 R2L attacks
r2l_attacks = dat$attack_type == "ftp_write." | dat$attack_type == "guess_passwd." |dat$attack_type == "imap." | dat$attack_type == "multihop." | dat$attack_type == "phf." | dat$attack_type == "spy." | dat$attack_type == "warezclient." | dat$attack_type == "warezmaster."

sum(dat$attack_type == "ftp_write.") + sum(dat$attack_type == "guess_passwd.") + sum(dat$attack_type == "imap.") + sum(dat$attack_type == "multihop.") + sum(dat$attack_type == "phf.") + sum(dat$attack_type == "spy.") + sum(dat$attack_type == "warezclient.") + sum(dat$attack_type == "warezmaster.") == sum(r2l_attacks)

dat[r2l_attacks, "attack_type_main"] = "r2l"

###########
## Probe ##
###########
## 4107 attacks
probe_attacks = dat$attack_type == "ipsweep." | dat$attack_type == "nmap." | dat$attack_type == "portsweep." | dat$attack_type == "satan."

sum(dat$attack_type == "ipsweep.") + sum(dat$attack_type == "nmap.") + sum(dat$attack_type == "portsweep.") + sum(dat$attack_type == "satan.") == sum(probe_attacks)

dat[probe_attacks, "attack_type_main"] = "probe"

## All 494021 observations are labelled.
sum(dat$attack_type_main == "probe") + sum(dat$attack_type_main == "u2r") + sum(dat$attack_type_main == "r2l") + sum(dat$attack_type_main == "dos") + sum(dat$attack_type_main == "normal") == dim(dat)[1]

## Coerce into a factor
dat$attack_type_main = as.factor(dat$attack_type_main)





## "num_outbound_cmds" & "is_host_login" contain only one unique value
## ie, they are essentially redundant so dropped from the data set
for (i in 1:ncol(dat)) {
    if (length(unique(dat[[i]])) == 1) {
        cat(paste(length(unique(dat[[i]])), names(dat)[i], "\n"))
    }
}
dat2 = subset(dat, select = -c(num_outbound_cmds, is_host_login))

## continous variables
# continous = c("duration", "src_bytes", "dst_bytes", "serror_rate",
#               "srv_serror_rate", "rerror_rate", "srv_rerror_rate",
#               "same_srv_rate", "diff_srv_rate", "srv_diff_host_rate",
#               "dst_host_same_srv_rate", "dst_host_diff_srv_rate",
#               "dst_host_same_src_port_rate", "dst_host_srv_diff_host_rate",
#               "dst_host_serror_rate", "dst_host_srv_serror_rate",
#               "dst_host_rerror_rate", "dst_host_srv_rerror_rate")
# for (i in 1:length(continous)) {
#     dat_test[[continous[i]]] = discretize(dat_test[[continous[i]]], 
#                                           method = "cluster", categories = 10)
#     cat(paste("done", continous[i], "\n"))
# }

## counts
# counts = c("wrong_fragment", "urgent", "hot", "num_failed_logins",
#            "num_compromised", "num_root", "num_file_creations",
#            "num_shells", "num_access_files", #"num_outbound_cmds", 
#            "count", "srv_count", "dst_host_count", "dst_host_srv_count")
# for (i in 1:length(counts)) {
#     if (length(unique(dat_test[[counts[i]]])) > 20) {
#         dat_test[[counts[i]]] = discretize(dat_test[[counts[i]]], 
#                                            method = "cluster", categories = 10)
#     } else {
#         dat_test[[counts[i]]] = as.factor(dat_test[[counts[i]]])
#     }
#     cat(paste("done", counts[i], "\n"))
# }

## logical variables
logical = c("land", "logged_in", "root_shell", "su_attempted",
            #"is_host_login",
            "is_guest_login")
for (i in 1:length(logical)) {
#     dat2[[logical[i]]] = as.logical(dat[[logical[i]]])
    dat2[[logical[i]]] = as.factor(dat[[logical[i]]])
    cat(paste("done", logical[i], "\n"))
}

## further processing
## "wrong_fragment", "urgent", "num_failed_logins", "num_shells":
# dat_test[, 8] = as.factor(dat_test[, 8])
# dat_test[, 9] = as.factor(dat_test[, 9])
# dat_test[, 11] = as.factor(dat_test[, 11])
# dat_test[, 18] = as.factor(dat_test[, 18])

## drop factor levels that are no longer used
drop_factor = function(data) {
    for (i in 1:ncol(data)) {
        if (is.factor(data[, i]))
            data[, i] = factor(data[, i])
    }
    data
}
dat_dos   = dat2[dat2[, "attack_type_main"] == "dos", ];   dat_dos = drop_factor(dat_dos)
dat_r2l   = dat2[dat2[, "attack_type_main"] == "r2l", ];   dat_r2l = drop_factor(dat_r2l)
dat_u2r   = dat2[dat2[, "attack_type_main"] == "u2r", ];   dat_u2r = drop_factor(dat_u2r)
dat_probe = dat2[dat2[, "attack_type_main"] == "probe", ]; dat_probe = drop_factor(dat_probe)]
dat_norm  = dat2[dat2[, "attack_type_main"] == "normal", ]; dat_norm = drop_factor(dat_norm)


num_unique = function(data) {
    for (i in 1:ncol(data)) {
        u = length(unique(data[, i]))
        if (u == 1) {
            cat(paste0(i, ", ", names(data)[i], ": ", unique(data[, i]), "\n"))
        }
    }
}
num_unique(dat_dos)
num_unique(dat_r2l)
num_unique(dat_u2r)
num_unique(dat_probe)
num_unique(dat_norm)

library(FactoMineR)
res_r2l = FAMD(dat_r2l[, -c(2,7,8,41)])
## contribution is in percentage adding up to 100
#head(contrib[order(contrib[,1], decreasing = TRUE),])
## so compare the coordinates (data are standardised so 0<=coord<=1)
head(res_r2l$var$coord[order(res_r2l$var$coord[,1], decreasing = TRUE), ])
head(res_r2l$var$coord[order(res_r2l$var$coord[,2], decreasing = TRUE), ])
test = res_r2l$var$coord[order(res_r2l$var$coord[,1], decreasing = TRUE), ]


a = dat_r2l[, -c(2,7,8,41)]
str(a[, grep("is_guest_login", names(a))])
str(a[, grep("dst_host_same_srv_rate", names(a))])


rvCoef = res_r2l$var$RV[-38, -38]
coord  = res_r2l$var$coord[, c(1,2)]  # first 2 dimensions
for (i in 1:(nrow(rvCoef)-1)) {
    rname = rownames(rvCoef)[i]
    rcoords = coord[rname, ]
    
    if (all(rcoords < 0.1)) {
        rcoordsString = paste0("dim", 1:2, ": ", round(rcoords, digits = 2), collapse = ", ")
        cat(paste(paste0("[", rname, "] ", rcoordsString), "\n"))
    }
    
    for (j in (i+1):ncol(rvCoef)) {
        ## rv coefficients (correlation coefficients) between variables:
        ## look for highly correalted variables
        #rv = rvCoef[i, j]
        cname = colnames(rvCoef)[j]
        ccoords = coord[cname, ]
        
        if (all(ccoords < 0.1)) {
            ccoordsString = paste0("dim", 1:2, ": ", round(ccoords, digits = 2), collapse = ", ")
            cat(paste(paste0("[", cname, "] ", ccoordsString), "\n"))
        }
#         if (rv > 0.9) {
#             #res.mat[i,j] = "high"
#             rname = rownames(rvCoef)[i]
#             cname = colnames(rvCoef)[j]
#             rcoords = coord[rname, ]
#             ccoords = coord[cname, ]
#             if (any(rcoords > 0.8) | any(ccoords > 0.8)) {
#                 rcoordsString = paste0("dim", 1:2, ": ", round(rcoords, digits = 2), collapse = ", ")
#                 ccoordsString = paste0("dim", 1:2, ": ", round(ccoords, digits = 2), collapse = ", ")
#                 cat(paste(paste0("[", rname, "] ", rcoordsString), " | ",
#                           paste0("[", cname, "] ", ccoordsString), "\n"))
#                 cat("\n")
#             }
#         }
        ## coordinates of variables on dimension 1 and 2:
        ## look for variables whose coordinates are close to 1
    }
}

vars2drop = character()
for (i in 1:nrow(coord)) {
    coord_i = round(coord[i, ], digits = 3)
    var_i   = rownames(coord)[i]
    if (all(coord_i < 0.1)) {
        cat(paste("[", var_i, "] ",
                  paste(coord_i, collapse = ", "),
                  "\n"), fill = TRUE)
        vars2drop = c(vars2drop, var_i)
    }
}
c = a[, !(names(a) %in% vars2drop)]
rpart_r2l2 = rpart(attack_type~., data = c)

b = a[, !(names(a) %in% c("rerror_rate", "dst_host_rerror_rate"))]
nnet_r2l = nnet(attack_type~., data = b, size = 45, maxit = 300, MaxNWts = 2500)
summary(nnet_r2l)
plot(nnet_r2l)

multinom_r2l = multinom(attack_type~., data = b)
summary(multinom_r2l)

library(rpart)
rpart_r2l = rpart(attack_type~., data = b)
library(maptree)
par(xpd = TRUE)
draw.tree(rpart_r2l)

rpart_r2l_1 = rpart(attack_type~., data = a)
par(xpd = TRUE)
draw.tree(rpart_r2l)

printcp(rpart_r2l)
printcp(rpart_r2l_1)




ind = sample(2, nrow(dat_norm), replace = TRUE, prob = c(0.05, 0.95))
dat_norm_sample = dat_norm[ind==1,]
res_norm = FAMD(dat_norm_sample[, -c(8,40,41)])
plot(res, habillage = 37)


library(rpart)
rpart_r2l = rpart(attack_type~., data = dat_r2l[, -c(2,7,8,41)])

a = rpart(attack_type~., data = dat2)
b = glm(attack_type~., data = dat2)



X <- data.frame(
    y=rep(1:4,25),
    Climate=rep(c("Tropical", "Arid", "Temperate", "Snow"),25)
)
f <- rpart(y~Climate, X)
plot(f)
text(f)







a=princomp(scale(iris[, -5]))
a$scores[1, 1]


x <- as.matrix(assn1.df[,3:11])
R> covMatrix <- cov(x)
R> e <- eigen(covMatrix, symmetric = TRUE)


data(iris)
iris_num = as.matrix(iris[, -5])
test1 = cov(iris_num)
test2 = eigen(test1)
test3 = iris_num %*% test2$vectors

test4 = svd(scale(iris_num))
head(test4$v %*% diag(test4$d))
test4$v

test5 = PCA(iris[, -5])
test5$var$coord[, c(1,2)]

test5 = princomp(iris[, -5])
biplot(test5)



data(iris)
iris = as.matrix(iris[, -5])
test_a = svd(scale(iris))
test_b = scale(iris) %*% test_a$v
test_c = test_a$u %*% diag(test_a$d)
#all(test_b - test_c < 10e-15)

eig = eigen(iris)







test_d = svd.triplet(scale(iris[,-5]))
diag(test_d$vs)

test_a$u[1:10, 1:2]
test_d$U[1:10, 1:2]




test_a$u[1:10, 1]
test_d$U[1:10, 1]

test_a$v
test_d$V

test_d = diag(test_a$d) %*% test_a$v
test_d


diag(test_d$vs) %*% test_d$V



test_a1 = princomp(scale(iris))
test_a1$scores[1:5, 1]
















hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
X <- hilbert(9)[, 1:6]
(s <- svd(X))
D <- diag(s$d)
s$u %*% D %*% t(s$v) #  X = U D V'
t(s$u) %*% X %*% s$v #  D = U' X V










### LOF scores from weka
iris_lof = read.csv("../lof.csv")
iris_lof = iris_lof[, "LOF"]
order(iris_lof)
order(lofactor(iris[,-5], k=10))
order(lofactor(iris[,-5], k=40))

sort(iris_lof)[1:10]
sort(lofactor(iris[,-5], k=10))[1:10]
sort(lofactor(iris[,-5], k=40))[1:10]

sampleInd = sample(which(dat$attack_type == "normal."), size = 5000, replace = TRUE)
dat2 = dat[sampleInd, ]
#dat2 = subset(dat, select=-c(2,3,4,7,12,14,20,21,22,42))
write.csv(dat2[,-c(2,3,4,7,12,14,20,21,22,42)], file="../kddcup_numeric.csv")








sys.sample = function(N, n) {
    k = ceiling(N/n)
    r = sample(1:k, 1)
    sys.samp = seq(r, r + K*(n-1), k)
    return(sys.samp)
}

normalSamples = sample(which(dat$attack_type == "normal."), size = 92279, replace = TRUE)
attackSamples = sample(which(dat$attack_type != "normal."), size = 39674, replace = TRUE)
category = c(2,3,4,7,12,21,22)#,42)
dat2 = dat[c(normalSamples, attackSamples), -category]
write.csv(dat2, file = "../kddcup_numeric2.csv")


time_current = Sys.time()
lof.res = lof(dat2, k = 10)
Sys.time() - time_current
# Time difference of 10.88129 hours
# Error in rowSums(centred^2) : 
#    'Calloc' could not allocate memory (131953 of 16 bytes)

obsInd = read.csv("../kddcup_numeric.csv")[, 1]

dat3 = read.csv("../kddcup_lof.csv")
lof_scores = dat3[, "LOF"]
sum(lof_scores > 1.3)


dat4 = dat[obsInd, ]
sum(dat4$attack_type != "normal.")
dat4$LOF = lof_scores

#sum(dat4$LOF >= 1.232705) / sum(dat4$attack_type != "normal.")
#sort(dat4$LOF, decreasing=T)[39674]
sum(dat4[order(dat4$LOF, decreasing=TRUE)[1:39674], "attack_type"] == "normal.")
dat4[order(dat4$LOF, decreasing=TRUE)[1], "LOF"]











