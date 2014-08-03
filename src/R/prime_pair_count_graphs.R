max <- 1000
prime.pair.count <- read.csv("PrimePairCounts10000.csv")
colnames(prime.pair.count) <- c("n", "prime.pair.count")
rows <- prime.pair.count$n <= max
prime.pair.count <- prime.pair.count[rows,]
scatter.smooth(prime.pair.count$n, prime.pair.count$prime.pair.count, 
               lpars = list(col = "blue", lwd = 3), 
               main="Number of prime pairs", xlab="n", 
               ylab="number of prime pairs")
