load("forest500.RData")
# Graph creation

beta <- (forest500$V3 - min(forest500$V3)) / max(forest500$V3)
beta <- beta[(beta < 1 & beta > 0)]

dx <- density(beta)

# Basic Formula Needs:
# mean = a/(a+b)
# variance = ab/((a+b)^2(a+b+1))

samplemean <- mean(beta)
samplevariance <- var(beta)

temp <- (samplemean^2)*(1-samplemean)/samplevariance
alpha <- temp - samplemean
# alpha <- samplemean*sqrt(temp) - samplemean/2 # beta dist variable 1
print(alpha)
beta <- (1/samplemean - 1)*alpha    # beta dist variable 2
print(beta)

pdf(file = "BetaMME.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4)

plot(dx, col = 'blue', main = "MME Beta: forest500$V3", x_lab = 'Density')

x_norm <- seq(0, 1, by = 0.0001) 
y_norm <- dbeta(x_norm, alpha, beta, ncp = 0) 

lines(x_norm, y_norm , lwd = 1, col = "black")

dev.off()
