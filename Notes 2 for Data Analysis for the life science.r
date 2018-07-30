# 7/26/18, 21:36
g = 9.8
n = 25 
tt = seq(0, 3.4, len=n)
f = 56.67 - 0.5*g*tt^2
y = f + rnorm(n, sd=1)
plot(tt, y)
lines(tt, f, col=3)

tt2 = tt^2
fit = lm(y~tt+tt2)
summary(fit)



x = list(a = 1, b = 1:3, c = 10:100)
lapply(x, sum)
sapply(x, FUN = sum)
mm = sapply(1:5, function(x) rnorm(9, x))
boxplot(mm)

rss = function(beta0, beta1, beta2) {
	r = y - (beta0+beta1*tt+beta2*tt^2)
	return(sum(r^2))
}
beta2s = seq(-10, 10, len=1000)
plot(beta2s, sapply(beta2s, rss, beta0=55, beta1=0))
lines(beta2s, sapply(beta2s, rss, beta0=65, beta1=0))

plot(tt, y)
(y - 4)^2
n = 55 - 19 * tt^2
lines(tt, n)
rnorm

