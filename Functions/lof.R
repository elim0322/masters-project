#source("Functions/init.R") # load in data & set up weka


# --------------------------------------------------------------
# Function to compute LOF scores and save them in "data" folder
# --------------------------------------------------------------

lofScores = function(data, n = 5000, i = 100, p.attack = 0.1, p.k = 0.3, file) {
    
    # -----------------------------------------------
    # ARGUMENTS
    #     data:     dataset
    #     n:        sample size
    #     i:        number of seeds/runs/experiments
    #     p.attack: percentage/ratio of attacks
    #     p.k:      percentage k
    #     file:     save directory
    # -----------------------------------------------
    
    ## normal, attack and k
    nSize = n * (1 - p.attack)
    aSize = n * p.attack
    k     = n * p.k
    
    ## iteration up to i
    for (j in 1:i) {
        ## force garbage collection
        invisible((gc(reset = TRUE)))
        
        ## print
        cat(paste(j, "..."))
        
        ## random sampling without replacement
        set.seed(j); normal = sample(which(data$attack_type == "normal."), size = nSize, replace = FALSE)
        set.seed(j); attack = sample(which(data$attack_type != "normal."), size = aSize, replace = FALSE)
        ## testset excludes any categorical/binary features
        testset = data[c(normal, attack), -c(2,3,4,7,12,21,22,42)]
        
        ## a vector of scores is stored as a row
        result   = t(as.matrix(LOF(testset, control = Weka_control(min = k, max = k, "num-slots" = 2))[,"LOF"]))
        ## write result into file (by row)
        write.table(result, file = file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
        
        ## print
        cat(paste0(" done.", "\n"))
    }
}


## run:
# 5% attack
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.05, p.k = 0.05, file = "Data/LOF_scores/dat1_05a_05k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.05, p.k = 0.1,  file = "Data/LOF_scores/dat1_05a_10k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.05, p.k = 0.2,  file = "Data/LOF_scores/dat1_05a_20k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.05, p.k = 0.3,  file = "Data/LOF_scores/dat1_05a_30k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.05, p.k = 0.4,  file = "Data/LOF_scores/dat1_05a_40k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.05, p.k = 0.5,  file = "Data/LOF_scores/dat1_05a_50k.csv")

# 10% attack
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.05, file = "Data/LOF_scores/dat1_10a_05k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.1,  file = "Data/LOF_scores/dat1_10a_10k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.2,  file = "Data/LOF_scores/dat1_10a_20k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.3,  file = "Data/LOF_scores/dat1_10a_30k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.4,  file = "Data/LOF_scores/dat1_10a_40k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.1, p.k = 0.5,  file = "Data/LOF_scores/dat1_10a_50k.csv")

# 20% attack
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.05, file = "Data/LOF_scores/dat1_20a_05k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.1,  file = "Data/LOF_scores/dat1_20a_10k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.2,  file = "Data/LOF_scores/dat1_20a_20k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.3,  file = "Data/LOF_scores/dat1_20a_30k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.4,  file = "Data/LOF_scores/dat1_20a_40k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.2, p.k = 0.5,  file = "Data/LOF_scores/dat1_20a_50k.csv")

# 30% attack
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.05, file = "Data/LOF_scores/dat1_30a_05k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.1,  file = "Data/LOF_scores/dat1_30a_10k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.2,  file = "Data/LOF_scores/dat1_30a_20k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.3,  file = "Data/LOF_scores/dat1_30a_30k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.4,  file = "Data/LOF_scores/dat1_30a_40k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.3, p.k = 0.5,  file = "Data/LOF_scores/dat1_30a_50k.csv")


# 40% attack
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.05, file = "Data/LOF_scores/dat1_40a_05k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.1,  file = "Data/LOF_scores/dat1_40a_10k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.2,  file = "Data/LOF_scores/dat1_40a_20k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.3,  file = "Data/LOF_scores/dat1_40a_30k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.4,  file = "Data/LOF_scores/dat1_40a_40k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.5,  file = "Data/LOF_scores/dat1_40a_50k.csv")

# 50% attack
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.05, file = "Data/LOF_scores/dat1_50a_05k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.1,  file = "Data/LOF_scores/dat1_50a_10k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.2,  file = "Data/LOF_scores/dat1_50a_20k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.3,  file = "Data/LOF_scores/dat1_50a_30k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.4,  file = "Data/LOF_scores/dat1_50a_40k.csv")
lofScores(data = dat1, n = 5000, i = 100, p.attack = 0.4, p.k = 0.5,  file = "Data/LOF_scores/dat1_50a_50k.csv")














