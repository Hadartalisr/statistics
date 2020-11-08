library(sqldf)
Path = "C:/users/hadar/Desktop/stats/statistics_R/week_3/flatprices.csv"
df = read.table(file = Path, header=T, sep=",")
par(mfrow=c(2,1))
#q.1
plot(df$space, df$price ,xlab = "space", ylab = "price", col="black")

#q.2
l = lm(price~space, data=df)
n = unname(l$coefficients[1])
m = unname(l$coefficients[2])
abline(a=n, b=m, col="blue")

#q.3
# LSR 
m
# cor
cor = cor(df$space, df$price)
cor
# Coefficient of determination
cod = cor^2
cod
# sse
sse = 0
for (i in seq(1,length(df$space))){
  y = df[i,2]
  x = df[i,1]
  diff = (y - (n+m*x))^2
  sse = sse + diff
}
sse

sst = 0
mean = mean(df$price)
for (i in seq(1,length(df$space))){
  y = df[i,2]
  diff = (y - mean)^2
  sst = sst + diff
}
sst

#q.4
excpected_price = n+125*m
excpected_price


#q.5


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#q.6- first option
log_price_df = df 
log_price_df$price = log(log_price_df$price)
plot(log_price_df$space, log_price_df$price ,xlab = "space", ylab = "price", col="black")

  #q.6.2
  l = lm(price~space, data=log_price_df)
  n = unname(l$coefficients[1])
  m = unname(l$coefficients[2])
  abline(a=n, b=m, col="blue")
  
  #q.6.3
  # LSR 
  m
  # cor
  cor = cor(log_price_df$space, log_price_df$price)
  cor
  # Coefficient of determination
  cod = cor^2
  cod
  # sse
  sse = 0
  for (i in seq(1,length(log_price_df$space))){
    y = log_price_df[i,2]
    x = log_price_df[i,1]
    diff = (y - (n+m*x))^2
    sse = sse + diff
  }
  sse
  
  sst = 0
  mean = mean(df$price)
  for (i in seq(1,length(df$space))){
    y = df[i,2]
    diff = (y - mean)^2
    sst = sst + diff
  }
  sst
  
  #q.6.4
  excpected_price = exp(n+125*m)
  excpected_price 
  "I wouldn't use this for computing the price if i had buy the house - 
  it causes relatively big area houses' price to increase more then they should."


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
  #q.6 - second option
  log_price_df = df 
  log_price_df$price = log(log_price_df$price)
  plot(log_price_df$space, log_price_df$price ,xlab = "space", ylab = "price", col="black")
  
  #q.6.2
  l = lm(price~space, data=log_price_df)
  n = unname(l$coefficients[1])
  m = unname(l$coefficients[2])
  abline(a=n, b=m, col="blue")
  
  #q.6.3
  # LSR 
  m
  # cor
  cor = cor(log_price_df$space, log_price_df$price)
  cor
  # Coefficient of determination
  cod = cor^2
  cod
  # sse
  sse = 0
  for (i in seq(1,length(log_price_df$space))){
    y = log_price_df[i,2]
    x = log_price_df[i,1]
    diff = (y - (n+m*x))^2
    sse = sse + diff
  }
  sse
  
  sst = 0
  mean = mean(df$price)
  for (i in seq(1,length(df$space))){
    y = df[i,2]
    diff = (y - mean)^2
    sst = sst + diff
  }
  sst
  
  #q.6.4
  excpected_price = exp(n+125*m)
  excpected_price 
  "I wouldn't use this for computing the price if i had buy the house - 
  it causes relatively big area houses' price to increase more then they should."
  
  









