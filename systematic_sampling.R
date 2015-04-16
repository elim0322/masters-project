# ====================
# Systematic sampling
# ====================
sys.sample = function(N, n) {
    k_ceiling = ceiling(N/n)
    if (n*k_ceiling <= N) {
        k = k_ceiling
    } else {
        k = floor(N/n)
    }
    r = sample(1:k, 1)
    sys.samp = seq(r, r + k*(n-1), k)
    return(sys.samp)
}

## train
samp_5000norm = sample(which(dat$attack_type == "normal."), size = 5000, replace = FALSE)
train = dat[samp_5000norm, -c(2,3,4,7,12,21,22)]
foreign:::write.arff(train[,-35], file="train.arff")
#system('java -classpath "C:/Program Files (x86)/Weka-3-7/weka.jar" weka.filters.unsupervised.attribute.LOF -min 5 -max 5 -A "weka.core.neighboursearch.LinearNNSearch -A \"weka.core.EuclideanDistance -R first-last\"" -num-slots 1')
train2 = read.csv("train.csv")
quantile(train2$LOF, probs = 0.9)

## test
normal = sample(which(dat$attack_type == "normal."), size = 92279, replace = TRUE)
attack = sample(which(dat$attack_type != "normal."), size = 39674, replace = TRUE)
category = c(2,3,4,7,12,21,22)#,42)
dat2 = dat[c(normal, attack), -category]
foreign:::write.arff(dat2[,-35], file = "test.arff")





test$a
test = read.csv("../kddcup_lof.csv", as.is = TRUE)
test = test[, -1]

a = read.csv("../lof_k=5.csv")
b = read.csv("../kddcup_numeric.csv")
c = read.csv("../kddcup_numeric2.csv")

b = b[, -1]
c = c[, -1]

all(a[, 1] == b[, 1])


dim(a)



quantile(train2$LOF, probs = 0.95)
















