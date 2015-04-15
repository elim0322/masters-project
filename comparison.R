b = Sys.time()
invisible(replicate(n = 10, lofactor(iris[,-5],5)))
Sys.time() - b


b = Sys.time()
invisible(replicate(n = 10, lof(iris[,-5],5)))
Sys.time() - b




b = Sys.time()
#invisible(replicate(n=10, dist.knn.mine(iris[,-5],5)))
invisible(replicate(n=10, dist.to.knn(as.matrix(iris[,-5]),5)))
Sys.time() - b




