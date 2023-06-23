load("mlb1.RData")

# Graph creation
dx <- density(mlb1$Weight)

# Basic Formula Needs:
samplemean <- mean(mlb1$Weight) # normal dist variable 1
standarddev <- sqrt(var(mlb1$Weight)) # normal dist variable 2

pdf(file = "BetaMME.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4)

plot(dx, col = 'blue', main = "MME Normal: mlb1$Weight ", x_lab = 'Density', y_lab = 'mlb1$Weight')

x_norm <- seq(0, 20000, by = 0.08) 
y_norm <- dnorm(x_norm, mean = samplemean , sd = standarddev) 

lines(x_norm, y_norm , lwd = 1, col = "black")

dev.off()
