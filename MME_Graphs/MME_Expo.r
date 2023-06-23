load("falldetection.RData")
# Graph creation
# Added divison by 10,000 to match the MLE data

dx <- density(falldetection$SL/10000)

# Basic Formula Needs:
# mean = 1/lambda

lambda <- 1/mean(falldetection$SL/10000) # exponential dist variable 1

pdf(file = "ExpoMME.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4)

plot(dx, col = 'blue', main = "MME Exponential: falldetection$SL", x_lab = 'Density')

x_norm <- seq(0, 250, by = 1/10000) 
y_norm <- dexp(x_norm, lambda) 

lines(x_norm, y_norm , lwd = 1, col = "black")

dev.off()


