par(mfrow=c(2,1))
#q.1
x = rnorm(30, mean=5, sd=1)
x
#q.2
y = ((5*x)+2)
y
#q.3
# we would expect that the correlation between x and y will be y - there is
# a perfect linear connection between those 2 variables.
cor = cor(x,y)
cor 
#q.4 
# we would expect that the slope will be 5 - this is the relation between
# the amount of increasing in y as a dependent variable of x.
l = lm(y~x)
coefficients = coefficients(l)
slope = coefficients[2]
slope
#q.5
noise= rnorm(30,sd=1)
y<-y+noise
cor = cor(x,y)
cor
l = lm(y~x)
coefficients = coefficients(l)
slope = coefficients[2]
slope

#q.6
noise_v = c()
cor_v = c()
slope_v = c()
for(i in seq(0.5,10,0.5)){
  noise_v = c(noise_v, i)
  noise= rnorm(30,sd=i)
  y<-y+noise
  cor = cor(x,y)
  cor_v = c(cor_v, cor)
  l = lm(y~x)
  coefficients = coefficients(l)
  slope = coefficients[2]
  slope_v = c(slope_v,slope)
}
df <- data.frame(noise_v, cor_v, slope_v)
df

#q.7
plot(noise_v, cor_v ,xlab = "noise", ylab = "correlation", col="black")
plot(noise_v, slope_v ,xlab = "noise", ylab = "slope", col="black")

#q.8
# We can conclude that there is an inverse proportion between the noise and
# the covariance.
# farther more, the slope doesn't have any relation with the amount of noise 
# we add.
# from the following graphs we can determine that the LSR line isn't the most 
# reliable statistic tool. it can tell us if the proportion between the variables
# tend to be positive or negative but it can't give us the number precisely.


