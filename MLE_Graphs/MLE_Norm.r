library(stats4)

load("/Users/nitishgupta/Downloads/mlb1.RData")

norm<- mlb1$Weight

mean <- mean(norm)
var <- var(norm)
n <- length(norm)

norm_ll <- function(mean, var){
    nloglik <- n*log(sqrt(2*var*3.14)) + sum(((norm - mean)^2)/(2*var))
    return (nloglik)
}

n <- mle(minuslogl=norm_ll, start=list(mean = mean(norm), var = var(norm)))

# Taking the values found by estimation:

mean <- 201.3488
var <- 433.6010

pdf(file = "NormMLE.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4)

# (1/sqrt(2*3.14*var))*exp(-0.5*mean^2/var)

curve(expr= (exp(-0.5*(x - mean)^2/var)/sqrt(2*3.14*var)), from= 160, to = 280, main = "Normal MLE Graph",xlab = "mlb1$Weight", ylab = "Frequency")

density_norm <- density(norm) 

lines(density_norm , lwd = 3, col = "blue")

dev.off()

