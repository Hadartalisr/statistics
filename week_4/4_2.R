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
abline(lm(df$Time~df$Age-1))
Coefficient_of_determination = cor(df$Time,df$Age)^2

#q.3
# We have checked some log transformations and we have not determined 
# any transformations which increase the correlation in a significant amount.

#q.4
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(x = df$Time ,y= df$Age, xlab = "time", ylab = "age")
Coefficient_of_determination = cor(df$Time,df$Age)^2


plot(x = sqrt(df$Age) ,y= sqrt(df$Time), xlab = "age", ylab = "time")
abline(lm(df$Age~df$Time-1))
Coefficient_of_determination = cor(df$Age,df$Age)^2

     