
library(stats4)
load("forest500.RData")

# forest500$V7 is a beta distribution but not by itself. Its range is not 0 to 1
# To fix this issue we can use the follow to change the variable itself.

beta <- (forest500$V3 - min(forest500$V3)) / max(forest500$V3)

beta <- beta[(beta < 1 & beta > 0)]

density_beta <- density(beta)

llbeta <- function (a, b) {
        ll <- ((log(gamma(a+b)) - log(gamma(a)) - log(gamma(b))) * length(beta)) + sum((a-1)*log(beta) + (b-1)*log(1-beta))
        return(-ll)
}

b <- mle(minuslogl=llbeta, start=list(a = 8, b = 2))
print(b)

# After estimation:
alpha <- 2.451904
beta <- 5.246793

pdf(file = "BetaMLE.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4)

curve(expr= (gamma(alpha + beta) * x^(alpha-1)*(1-x)^(beta-1) )/ (gamma(alpha)*gamma(beta)), from= 0, to = 1, main = "Beta MLE Graph",xlab = "forest500$V7", ylab = "Frequency")

lines(density_beta , lwd = 3, col = "blue")

dev.off()

