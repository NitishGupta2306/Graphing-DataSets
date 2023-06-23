load("national.longitudinal.survey.rda")
library(stats4)

gamma <- national.longitudinal.survey$jobsnum90
n <- length(gamma)
tot_sum <- sum(gamma)

# Gamma formula from tb:  [λ^r * t^(r-1) * e^(-λt)] / gamma(r)
# log of this: (r*log(λ) + (r - 1)*log(t) - λ*tlog(e)) - log(gamma(r))
# simplified: (r*log(λ) + (r - 1)*log(t) - λ*t) - log(gamma(r))
# summing t: (r*log(λ) + (r - 1)* sum(log(t)) - λ*sum(t)) - n*log(gamma(r))
# (a is r, lamb is b)
ll <- function(r, lamb) {
  loglik <- n * r * log(lamb) + (r - 1) * sum(log(gamma)) - lamb * sum(gamma) - n * log(gamma(r))
  return(-loglik)
}

# MLE calculations start:

# Guessing values based on Graph
z <- mle(minuslogl = ll, start = list(r = 4, lamb = 1))

summary(z)

# From Estimation:
r = 3.1342574
lamb = 0.3913943

pdf(file = "GammaMLE.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4)

# Graphing:
curve(expr= ((lamb^r) * (x^(r-1)) * exp(-lamb*x))/(gamma(r)), from= 0, to = 40, main = "Gamma MLE Graph",xlab = "national.longitudinal.survey$jobsnum90", ylab = "Frequency")

lines(density(gamma) , lwd = 1, col = "blue")

dev.off()



