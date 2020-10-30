#q.2
par(mfrow = c(1,1))
v = c(100 ,84,88 ,96 ,0 ,69 ,38 ,92)
#q.2.1
v_mean = mean(v)
v_mean
#q.2.2
mean_alpha_cut <- function(v, alpha){
  n = length(v)
  cut_per_side = floor(alpha*n)
  sorted_v = sort(v)
  m = mean((sorted_v[(1+cut_per_side):(length(v)-cut_per_side)]))
  return(m)
}
v_alpha_mean = mean_alpha_cut(v,0.2)
#q.2.3
v_median = median(v)
#q.2.4
v_65_percentile = quantile(v,0.65)
v_65_percentile



#q.2.2
new_v = c(100 ,84,88 ,96 ,0 ,69 ,38 ,200)
new_v_mean = mean(new_v)
new_v_mean
new_v_alpha_mean = mean_alpha_cut(new_v,0.2)
new_v_median = median(new_v)
new_v_65_percentile = quantile(new_v,0.65)
new_v_65_percentile


par(mfrow = c(4,1))
plot(c(v_mean, new_v_mean),c(1,2), xlim = c(70,100), 
     ylim=c(0.5,2.5) ,col="red", main = "Mean", xlab ="", ylab = "")
plot(c(v_alpha_mean, new_v_alpha_mean), xlim = c(70,100), 
     c(1,2),ylim =c(0.5,2.5), col="red", main = "Alpha_mean", xlab ="", ylab = "")
plot(c(v_median, new_v_median),c(1,2),xlim = c(70,100),
     ylim =c(0.5,2.5), col="red", main = "Median", xlab ="", ylab = "")
plot(c(v_65_percentile, new_v_65_percentile),c(1,2), 
     xlim = c(70,100), ylim =c(0.5,2.5), col="red", 
     main = "65 percentile", xlab ="", ylab = "")
# From the following graphs we can inspect the sensitivity 
# of each summary statistic.
# the Mean has changed the most - very small or large values 
# in some observations can lead to major changes.
# the alpha_mean has changed a bit - as we were expecting - 
# after we added the 200 value, the value that was in the previous 
# (1-alpha)*n place took part in the new mean. 
# that caused the mean to increase from the previous vector.
# this kind of summary statistic gives us good results, and has Robustness as we
# would like to.
# because the length of the vector is 8, the median is the mean of 84 and 88,
# which were not changed between the two observations.
# on the other hand, the 65_percentile is the weighted average of the 6_th and 
# 7_th place according to their sorted indexes distance from the 65%. 
# the value in the 7_th got increased, therefore the 65_percentile got increased.
# even though the change was not that big, this summary statistic has great
# sensitivity to the distance between following observations 
# in the order statistic .  

