prime.pair.count <- read.csv("./data/triples100000.csv")
colnames(prime.pair.count) <- c("n", "count", "divisors")
scatter.smooth(prime.pair.count$n, prime.pair.count$prime.pair.count, 
               lpars = list(col = "blue", lwd = 3), 
               main="Number of prime pairs", xlab="n", 
               ylab="number of prime pairs")

plot(
    prime.pair.count$n, prime.pair.count$count, 
    col=c("red","blue", "green", "yellow", "cyan", "magenta")[prime.pair.count$divisors], 
    main="Number of prime pairs", xlab="n", ylab="number of prime pairs")

plot(
    prime.pair.count$n, prime.pair.count$count, 
    main="Number of prime pairs", xlab="n", ylab="number of prime pairs")
smooth <- loess.smooth(prime.pair.count$n, prime.pair.count$count)
lines(smooth, col="green")
lines(prime.pair.count$n, 50 * log(prime.pair.count$n), col="red")
