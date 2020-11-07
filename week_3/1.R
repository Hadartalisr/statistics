library(sqldf)
Path = "C:/users/hadar/Desktop/stats/statistics_R/week_3/heights.csv"
df = read.table(file = Path, header=T, sep=",")
par(mfrow=c(1,1))
#q.1
plot(df$HEIGHT, df$WEIGHT ,xlab = "Height", ylab = "WEIGHT", col="red")

#q.2
first = unname(quantile(df$HEIGHT,(1/3)))
first_df = df[which(df$HEIGHT <= first),]
first_y = mean(first_df$WEIGHT)
first_x = unname(quantile(df$HEIGHT,(1/6)))

second = unname(quantile(df$HEIGHT,(2/3)))
second_df = df[which(df$HEIGHT >= second),]
second_y = mean(second_df$WEIGHT)
second_x = unname(quantile(df$HEIGHT,(5/6)))
par(new=False)
l = line(c(first_x, second_x), c(first_y, second_y))
n = l$coefficients[1]
m = l$coefficients[2]
abline(a=n, b=m)


