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
    s = seq(r, r + k*(n-1), k)
    return(s)
}
