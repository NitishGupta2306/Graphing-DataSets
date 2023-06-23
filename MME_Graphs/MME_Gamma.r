load("national.longitudinal.survey.rda")
# Graph creation
dx <- density(national.longitudinal.survey$jobsnum90)

# Basic Formula Needs:
samplemean <- mean(national.longitudinal.survey$jobsnum90)
sum <- 0
samplevariance <- var(national.longitudinal.survey$jobsnum90)
lambda <- samplemean / samplevariance # gamma dist variable 1
c <- lambda * samplemean  # gamma dist variable 2


print(lambda)
print(c)

pdf(file = "GammaMME.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4)

plot(dx, col = 'blue', main = "MME Gamma: national.longitudinal.survey$jobsnum90 ", x_lab = 'Density')

x_dgamma <- seq(0, 50, by = 0.001) 
y_gamma <- dgamma(x_dgamma, shape = c, rate = lambda) 

lines(x_dgamma, y_gamma , lwd = 1, col = "black")

dev.off()

