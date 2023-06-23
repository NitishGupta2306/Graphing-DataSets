library(stats4)

load("falldetection.RData", verbose=TRUE)

exp <- falldetection$SL
# Divide by 10000 as mle() function cant hangle super low lambdas
exp <- exp / 10000
n <- length(exp)

ll <- function (lamb) {
    if(lamb > 0){
        loglik <- n*log(lamb) - lamb*sum(exp)
    }

    return(-loglik)
}

# Running MLE
e <- mle(minuslogl=ll, start=list(lamb = 0.1))

pdf(file = "ExpoMLE.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4)

# Graphing using found estimate from e
lamb <- 0.1328538
curve(expr= (lamb * exp(-lamb*x)), from= 0, to = 250, main = "Exponential MLE Graph",xlab = "falldetection$SL", ylab = "Frequency")

# Graphing the actual data set
density_exp <- density(exp)
lines(density_exp , lwd = 1, col = "blue")

dev.off()
