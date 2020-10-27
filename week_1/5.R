#5.1
a <- round(3/10, digits=2)
print(a)
b <- round(10/3, digits=2)
print(b)


#5.2
x <- seq(0, 5, 0.25)
print(x)
print(paste0("mean: ", mean(x), ", variance: ", var(x),
             ", median: ", median(x), ", standard deviation: ", sd(x), "."))

#5.3
print(length(x))
print(min(x))
print(max(x))


#5.4
mat <- matrix(0:24, nrow=5, ncol=5)
print(mat)
df <- as.data.frame(mat)
colnames(df) <-c("a","b","c","d","e")
rownames(df) <-c("f","g","h","i","j")
print(df)