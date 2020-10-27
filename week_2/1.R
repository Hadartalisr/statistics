library(sqldf)


#q.1
# according to the question in the forum, 
# it is possible to use advanced R functions
density.kernel <- function(x, h){
  plot(density(x,kernel = c("cosine"),window = kernel, bw = h))
}


#q.2
Path = "C:/users/hadar/Desktop/stats/statistics_R/week_2/grades.csv"
df = read.table(file = Path, header=T, sep=",")
math = df$math
par(mfrow=c(3,2))
hist(math, freq=FALSE, ylim = c(0,0.04))
for (h in c(0.5,1,1.5,2,2.5)){
  plot(density(math,kernel = c("cosine"),window = kernel, bw = h), main = h )
}
# From the following graphs we can conclude that the best h* is 1.5
# this h let us balance between the need to show as much details a possible
# and yet to present the main information which we can achieve from the data.


#q.3
# The distribution has 3 modals - at about 50, 75 and 90.


#q.4

a_math = sqldf('Select math from df a where a.school = "A"')$math
plot(density(a_math, kernel = c("cosine"),window = kernel, bw = 1.5),
     xlim=c(0,100), ylim=c(0,0.15))
b_math = sqldf('Select math from df a where a.school = "B"')$math
lines(density(b_math, kernel = c("cosine"),window = kernel, bw = 1.5),add=T)
c_math = sqldf('Select math from df a where a.school = "C"')$math
lines(density(c_math, kernel = c("cosine"),window = kernel, bw = 1.5),add=T)




