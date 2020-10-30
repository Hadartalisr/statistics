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
par(mfrow=c(3,1))
a_math = sqldf('Select math from df a where a.school = "A"')$math
plot(density(a_math, kernel = c("cosine"),window = kernel, bw = 1.5),
     xlim=c(0,100), ylim=c(0,0.2), col="orange", 
     main = "Math density graphs")
b_math = sqldf('Select math from df a where a.school = "B"')$math
lines(density(b_math, kernel = c("cosine"),window = kernel, bw = 1.5),
      add=T, col="red")
c_math = sqldf('Select math from df a where a.school = "C"')$math
lines(density(c_math, kernel = c("cosine"),window = kernel, bw = 1.5),
      add=T, col="blue")
legend("topleft", c("A","B","C"),fill=c("orange","red","blue"))
# C's students has the best knowledge in math, then A's students 
# and at the last place B's students. 
# When we inspect the size of the school we can figure out that there is 
# an inverse proportion between the size of the school to it's students
# math knowledge
plot(c(length(c_math), length(b_math), length(a_math)),
     c(mean(c_math), mean(b_math), mean(a_math))
     ,main="Average math score as a function of the school size",
     xlab = "# of students", ylab = "average math score", 
     col="red", ylim =c(40,100))


#q.5
a_gym = sqldf('Select gym from df a where a.school = "A"')$gym
plot(density(a_gym, kernel = c("cosine"),window = kernel, bw = 1.5),
     xlim=c(0,100), ylim=c(0,0.1), col="orange", 
     main = "gym density graphs")
b_gym = sqldf('Select gym from df a where a.school = "B"')$gym
lines(density(b_gym, kernel = c("cosine"),window = kernel, bw = 1.5),
      add=T, col="red")
c_gym = sqldf('Select gym from df a where a.school = "C"')$gym
lines(density(c_gym, kernel = c("cosine"),window = kernel, bw = 1.5),
      add=T, col="blue")
legend("topleft", c("A","B","C"),fill=c("orange","red","blue"))
# In the third graph there is not a major difference in the students
# grades between the different schools. 
# therefore we cannot get the same conclusions as those from the previous 
# question.






