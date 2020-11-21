Path = "C:/users/hadar/Desktop/stats/statistics_R/week_4/Age_And_Time.csv"
df = read.table(file = Path, header=T, sep=",")
par(mfrow = c(2,1))


#q.1
hist(df$Age, col="black", breaks= 15 ,xlab = "age", 
     ylab = "#", ylim = c(0,25), main = "Histogram of age",
     xlim = c(min(df$Age)-2,max(df$Age)+1))

hist(df$Time, col="black", breaks= 20 ,xlab = "time", 
     ylab = "#", ylim = c(0,25), main = "Histogram of time",
     xlim = c(min(df$Time)-2,max(df$Time)+1))

# The age distribution is approximately gaussian.
# The time distribution is approximately exponential.

#q.2
par(mfrow = c(1,1))
plot(x = df$Age ,y= df$Time, xlab = "age", ylab = "time",
     xlim = c(min(df$Age)-2,max(df$Age)+1))
l = lm(df$Time~df$Age-1)
abline(l)
y_hat = predict(l)
Coefficient_of_determination = 1-sum((df$Time - y_hat)^2/sum(df$Time^2))
Coefficient_of_determination 



#q.3

df2 = df[which(df$Time < 30),]
log_age = log(df2$Age)
par(mfrow = c(1,1))
l = lm(df2$Time~log_age-1)
y_hat = predict(l)
Coefficient_of_determination = 1-sum((df2$Time - y_hat)^2/sum(df2$Time^2))
Coefficient_of_determination 



log_time = log(df2$Time)
par(mfrow = c(1,1))
l = lm(log_time~df2$Age-1)
y_hat = predict(l)
Coefficient_of_determination = 1-sum((log_time - y_hat)^2/sum(log_time^2))
Coefficient_of_determination 

# we have determined that taking the log of the time bring us 
# to 0.8712446 which is much closer to 1 then the our previous R^2 (0.5889811)


#q.4
par(mfrow = c(1,1))
l = lm(log_time~df2$Age-1)
plot(x = df2$Age ,y= log_time, xlab = "age", ylab = "time",
     xlim = c(min(df2$Age)-2,max(df2$Age)+1))
y_hat = predict(l)
abline(l)
Coefficient_of_determination = 1-sum((log_time - y_hat)^2/sum(log_time^2))
Coefficient_of_determination 


#q.5
plot(x = df2$Age ,y= df2$Time , log = "y" , xlab = "age", ylab = "time",
     xlim = c(min(df2$Age)-2,max(df2$Age)+1))
f1 <- function(t) exp(l$coefficients[1]*t)

curve(f1,from = min(df2$Age)-10,to = max(df2$Age)+10, add=TRUE)



     