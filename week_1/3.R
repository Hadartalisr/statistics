library(sqldf)
Path = "C:/stats/hw1/appendicitis.csv"
df = read.table(file = Path, header=T, sep=",")
head(df)
surgery_count = nrow(df)

#3.1
# P(unnecessary) = P(Pathology = 2)
unnecessary_count = nrow(df[which(df$Pathology == 2),])
unnecessary_probabilty = unnecessary_count/surgery_count
print(unnecessary_probabilty)


#3.2
table = table(df)
print(table)


#3.3
#P(unnecessary|Male) = P(unnecessary & Male)/P(Male)
unnecessary_male_count = nrow(df[which(df$Pathology == 2 & df$Sex == "Male"),])
male_count = nrow(df[which(df$Sex == "Male"),])
unnecessary_pro_if_Male = unnecessary_male_count / male_count
print(unnecessary_pro_if_Male)
#P(unnecessary|Female) = P(unnecessary & Female)/P(Female)
unnecessary_female_count = nrow(df[which(df$Pathology == 2 & df$Sex == "Female"),])
female_count = nrow(df[which(df$Sex == "Female"),])
unnecessary_pro_if_female = unnecessary_female_count / female_count
print(unnecessary_pro_if_female)


#3.4
max_age <- max(df$Age)
age = c(0:max_age)

sql = "Select Age From df where sex = 'Male'"
male_df = sqldf(sql)
maleEcdf = ecdf(male_df$Age)
male_dist = maleEcdf(age)
  

sql = "Select Age From df where sex = 'Female'"
female_df = sqldf(sql)
femaleEcdf = ecdf(female_df$Age)
female_dist = femaleEcdf(age)

plot(age, male_dist, type = "l", xlab = "??????",ylab = "?????????????? ???????????? ??????????????", 
     col="blue", main="?????????????? ???????????? ?????????????? ???? ?????? ?????? ?????? ???????????? ?????????? ????????????")

lines(age, female_dist, col="red")

legend("bottomleft", 
       legend = c("??????????", "????????"), 
       col = c("blue","red"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       inset = c(0.1, 0.1))


