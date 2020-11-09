Path = "C:/users/hadar/Desktop/stats/statistics_R/week_3/heights.csv"
df = read.table(file = Path, header=T, sep=",")
par(mfrow=c(2,1))
#q.1
plot(df$HEIGHT, df$WEIGHT ,xlab = "Height", ylab = "WEIGHT", col="black")

#q.2
first = unname(quantile(df$HEIGHT,(1/3)))
first_df = df[which(df$HEIGHT <= first),]
first_y = mean(first_df$WEIGHT)
first_x = unname(quantile(df$HEIGHT,(1/6)))

second = unname(quantile(df$HEIGHT,(2/3)))
second_df = df[which(df$HEIGHT >= second),]
second_y = mean(second_df$WEIGHT)
second_x = unname(quantile(df$HEIGHT,(5/6)))
l = line(c(first_x, second_x), c(first_y, second_y))
n = unname(l$coefficients[1])
m = unname(l$coefficients[2])
abline(a=n, b=m, col="red")
l2 = lm(WEIGHT~HEIGHT, data=df)
n2 = unname(l2$coefficients[1])
m2 = unname(l2$coefficients[2])
abline(a=n2, b=m2, col="blue")

#q.3
# In out data, there is a mistake measurement - according to the data, 
# one of the student's height is 14 meters.
# From the previous question we can determine that the least squares line
# is very sensetive to mistakes in the observations - because of this one observation,
# the slope is very small and doesn't reflect the strong correlation between the 2 parameters.
# it's make sense - we have previous knowledge that LSR's asymptotic breaking point is 0.
# On the other hand, the resistant line has more robustness, and this one observation 
# didn't affect its shape at all.


#q.4
# LSR's slope is
unname(m2)
# correlation 
cor = cor(df$HEIGHT, df$WEIGHT)
cor
# Coefficient of determination
r_squerd = cor^2 
r_squerd
# According to those measurements, we wouldn't analyze
# that there is a linear relation between Height and Weight."

# q.5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df = df[which(df$HEIGHT<250),]
#q.1
plot(df$HEIGHT, df$WEIGHT ,xlab = "Height", ylab = "WEIGHT", col="black")

#q.2
first = unname(quantile(df$HEIGHT,(1/3)))
first_df = df[which(df$HEIGHT <= first),]
first_y = mean(first_df$WEIGHT)
first_x = unname(quantile(df$HEIGHT,(1/6)))

second = unname(quantile(df$HEIGHT,(2/3)))
second_df = df[which(df$HEIGHT >= second),]
second_y = mean(second_df$WEIGHT)
second_x = unname(quantile(df$HEIGHT,(5/6)))
l = line(c(first_x, second_x), c(first_y, second_y))
n = unname(l$coefficients[1])
m = unname(l$coefficients[2])
abline(a=n, b=m, col="red")
l2 = lm(WEIGHT~HEIGHT, data=df)
n2 = unname(l2$coefficients[1])
m2 = unname(l2$coefficients[2])
abline(a=n2, b=m2, col="blue")


#q.5.4
# LSR's slope is
unname(m2)
# correlation 
cor = cor(df$HEIGHT, df$WEIGHT)
cor
# Coefficient of determination
r_squerd = cor^2 
r_squerd
# According to those measurements, we would determine
# that there is a linear relation between Height and Weight."


#q.6
# we can conclude that extrem observations have major effect 
# on the r_squerd measurment 



