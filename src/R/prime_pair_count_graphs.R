prime.pair.count.1000 <- read.csv("PrimePairCounts1000.csv")
scatter.smooth(prime.pair.count.1000$n, prime.pair.count.1000$prime.pair.count, 
               lpars = list(col = "blue", lwd = 3), 
               main="Number of prime pairs", xlab="n", 
               ylab="number of prime pairs")
