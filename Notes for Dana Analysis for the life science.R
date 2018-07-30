# 6/27/18, 17:55 Chapter 0 Introduction
install.packages("rafalib")
install.packages("downloader")
install.packages("devtools")
# fix pakage commands conflicts
detach("package:dplyr", character.only = TRUE)
library("dplyr", character.only = TRUE)

library(rafalib)
library(downloader)
library(devtools)
library(tidyverse)

getwd()
url = "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename = "femaleMiceWeights.csv"
download(url, destfile=filename)

install_github('genomicsclass/dagdata')
dir = system.file(package = "dagdata")
list.files(dir)
list.files(file.path(dir, "extdata"))
filename = file.path(dir, "extdata/femaleMiceWeights.csv")
dat = read.csv(filename)

dat[12,2]
dat$Bodyweight[11]
dat[dat$Diet == "hf",2] %>% mean()

set.seed(2); sample(13:24)

chow = filter(dat, Diet == "chow")
chowVals = select(chow, Bodyweight)
chowVals = filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist()

msleep %>%  filter(order == "Primates") %>% select("sleep_total") %>% unlist() %>% mean()

# asymptotic
onethird = function(x) sum(3/10^c(1:x))
1/3 - onethird(15)

# Integrals
f = dnorm
x = seq(-4, 4, length=100)
plot(x, f(x), type = "l")
x0 = x[x>2]
y0 = f(x0)
x0 = c(min(x0), x0, max(x0))
y0 = c(0, y0, 0)
polygon(x0, y0, col="grey")
width = 0.001
x = seq(2,4, width)
areaofbars = f(x)*width
sum(areaofbars)

# 6/28/18, 14:33 Chapter 1 Inference
dat %>% filter(Diet == "hf") %>% summary()
control = filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist
treatment = filter(dat, Diet == "hf") %>% select(Bodyweight) %>% unlist
obsdiff = mean(treatment) - mean(control)
url = url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename = "femaleControlsPopulation.csv"
download(url, filename)
population = read.csv(filename) %>% unlist
control = sample(population, 12)
# p value
control = sample(population, 12)
treatment = sample(population, 12)
n = 10000
null = vector("numeric", n)
for (i in 1:n) {
  control = sample(population, 12)
  treatment = sample(population, 12)
  null[i] = mean(treatment) - mean(control)
}
hist(null)
pval = mean(null>obsdiff)
install.packages("UsingR")
library(UsingR)
x = father.son$fheight
round(sample(x,10), 1)
# Cumulative distribution function (CDF)
smallest = floor(min(x))
largest = ceiling(max(x))
values = seq(smallest, largest, len=300)
heightecdf = ecdf(x)
plot(values, heightecdf(values), type = "l", xlab = "a (Height in inches)", ylab="Pr (x <= a)")
# Histogram
bins = seq(smallest,largest, 0.25)
hist(x, breaks=bins, xlab="Heigh (in inches)", main = "Adult men heights")
# Probability distribution
n = 100
library(rafalib)
nullplot(-5,5,1,30, xlab="Observed differences (g)", ylab="Frequency")
totals = vector("numeric", 11)
for (i in 1:n) {
  control = sample(population, 12)
  treatment = sample(population,12)
  nulldiff = mean(treatment) - mean(control)
  j = pmax(pmin(round(nulldiff)+6, 11), 1)
  totals[j] = totals[j] + 1
  text(j-6, totals[j], pch=15, round(nulldiff, 1))
}
hist(null, freq=T)
abline(v=obsdiff, col="red", lwd=2)
1 - pnorm(obsdiff, mean(null), sd(null))
# population mean and standard deviation
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
x %>% mean()
set.seed(1); sample(x, 5) %>% mean() - mean(x)
set.seed(5); sample(x, 5) %>% mean() - mean(x) 

n = 1000
set.seed(1)
meantest = vector("numeric", n)
for (i in 1:n) {
  meantest[i] = sample(x, 50) %>% mean()
}
hist(meantest)
sum(meantest >= 23 & meantest <=25)/n*100

pnorm(25, mean = 23.9, sd = 0.43) - pnorm(23, mean = 23.9, sd = 0.43)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- "mice_pheno.csv"
download(url,destfile=filename)
dat <- read.csv(filename)
mean(controlPopulation) - mean(hfPopulation)
sd(controlPopulation)
sd(hfPopulation)
str(dat)
dat = na.omit(dat)
x = filter(dat, Sex == 'M' & Diet == 'chow') %>% select(Bodyweight) %>% unlist() 
y = filter(dat, Sex == 'M' & Diet == 'hf') %>% select(Bodyweight) %>% unlist() 
mean(x) # 30.96381
mean(y) # 34.84793
library(rafalib)
popsd(x) # 4.420501
mean(x) - mean(y)
set.seed(1)
mean(sample(x, 25)) - mean(sample(x, 25))
# Central limit theorem
mean(x)
sd(x)
x[x >= (mean(x)-sd(x)) & x <= (mean(x)+sd(x))] %>% length()/length(x)
n = 1000
meantest = vector(mode = "numeric", n)
for (i in 1:1000) {
  meantest[i] = mean(sample(x, 25))
}
hist(meantest)
sd(meantest)
sd(x)/sqrt(25)

X = seq(1,10)
Y = seq(1,20, by=2)
Z = X - Y

mean(X)
mean(Y)
mean(Z)
var(X)
var(Y)
var(Z)

dat = read.csv("mice_pheno.csv")
controlPopulation <- filter(dat,Sex == "F" & Diet == "chow") %>%  
  select(Bodyweight) %>% unlist
hfPopulation <- filter(dat,Sex == "F" & Diet == "hf") %>%  
  select(Bodyweight) %>% unlist
mypar(1,2)
hist(hfPopulation)
hist(controlPopulation)
mypar(1,2, brewer.n = 8)
qqnorm(hfPopulation)
qqline(hfPopulation)
qqnorm(controlPopulation)
qqline(controlPopulation)
dat = na.omit(dat)
pnorm(1) - pnorm(-1)
pnorm(2) - pnorm(-2)
pnorm(3) - pnorm(-3)
sum(controlPopulation >=  (mean(controlPopulation)-2*popsd(controlPopulation)) & controlPopulation <= (mean(controlPopulation)+2*popsd(controlPopulation)))/length(controlPopulation)
fchow = filter(dat, Sex == "F" & Diet == "chow") %>% select(Bodyweight) %>% unlist
fhf = filter(dat, Sex == "F" & Diet == "hf") %>% select(Bodyweight) %>% unlist
mchow = filter(dat, Sex == "M" & Diet == "chow") %>% select(Bodyweight) %>% unlist
mhf = filter(dat, Sex == "M" & Diet == "hf") %>% select(Bodyweight) %>% unlist
mypar(2,2)
qqnorm(fchow)
qqnorm(fhf)
qqnorm(mchow)
qqnorm(mhf)
avgs = replicate(10000, mean(sample(mchow, 2)))
mypar(1,2)
hist(avgs)
qqnorm(avgs); qqline(avgs)
mean(avgs)
mean(mchow)
sd(avgs)
popsd(mchow)/sqrt(25)
sds = replicate(10000, sd(sample(mchow,30)))
mypar(1,2)
hist(sds)
qqnorm(sds)
qqline(sds)
sum(sds < 3.5)/length(sds)

x=seq(0.0001, 0.9999, len=300)
hist(qt(x, 10))
hist(qnorm(x))

# Central limit theorem in practice
mean(fchow) - mean(fhf)
N = length(fchow)
fchowvar = mean((fchow - mean(fchow))^2)
var(fchow)*(N-1)/N
popvar(fchow)
Ns = c(3,12,25,50)
B = 10000
res = sapply(Ns, function(n) {
  replicate(B,mean(sample(fhf, n)) - mean(sample(fchow, n)))
})
mypar(2,2)
for (i in seq(along=Ns)) {
  titleavg <- signif(mean(res[,i]),3)
  titlesd <- signif(popsd(res[,i]),3)
  title <- paste0("N=",Ns[i]," Avg=",titleavg," SD=",titlesd)
  qqnorm(res[,i],main=title)
  qqline(res[,i],col=2)
}

Ns = c(3, 12, 18, 50)
B = 10000
computetstat = function(n) {
  y = sample(fhf, n)
  x = sample(fchow, n)
  (mean(y)-mean(x))/sqrt(var(y)/n+var(x)/n)
}
res = sapply(Ns, function(n) {
  replicate(B, computetstat(n))
})

xxx = replicate(100000, sample(fhf, 1)+sample(fchow, 1))
mean(xxx); mean(fhf)-mean(fchow)
var(xxx); var(fhf); var(fchow)

qqnorm(fchow)

sd(sample(fchow, 10))
sd(fchow)/sqrt(9)

test = replicate(1000, mean(sample(fchow, 10)))
mean(sample(fchow, 15))
sd(sample(fchow, 10))
mean = replicate(1000, mean(sample(fchow, 10)))

x = replicate(1000, sample(fchow,10)); mean(x); sd(x)
qqnorm(x)

Ns = c(3, 8, 12, 18, 30)
diff = sapply(Ns, function(n) {
  y = sample(fhf, n)
  x = sample(fchow, n)
  replicate(10000, (mean(x)-mean(y))/sqrt(var(y)/n+var(x)/n))
})
mypar(3,2)
for (i in seq(along=Ns)) {
  qqnorm(diff[, i])
  qqline(diff[, i])
}

computet = function(n) {
  y = sample(fhf, n)
  x = sample(fchow, n)
  (mean(y)-mean(x))/sqrt(var(y)/n+var(x)/n)
}
difft = sapply(Ns, function(n) {
  replicate(10000, computet(n))
})
mypar(3,2)
for (i in seq(along=Ns)) {
  qqnorm(difft[, i])
  qqline(difft[, i])
}



mean(x==6)

set.seed(1)
dicecompute = function() {
  x = sample(1:6, n, replace = TRUE)
  (mean(x==6) - 1/6)/sqrt((1/6 * 5/6)/n)
}

x = sample(1:6, 10000, replace = T)
mean(x==6)



p = 0.5
n = 5
dicecompute = function(n) {
  x = sample(1:6, n, replace = T)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}
set.seed(1)
chance = replicate(n, dicecompute(n))
mean(chance >= 2)
hist(chance)
qqnorm(chance)
qqline(chance)
dat = read.csv('femaleMiceWeights.csv')
chow = filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist
hf = filter(dat, Diet == "hf") %>% select(Bodyweight) %>% unlist
mean(hf) - mean(chow)
z = sqrt(12) * 5.21/sd(chow)
pnorm(z)
se = sqrt(sd(chow)^2/12 + sd(hf)^2/12)
1-pnorm((mean(hf)-mean(chow))/se)
t.test(chow, hf)

dat = read.csv("femaleMiceWeights.csv")
chow = filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist()
hf = filter(dat, Diet == "hf") %>% select(Bodyweight) %>% unlist()
diff = mean(hf) - mean(chow)
se = sqrt(var(hf)/length(hf) + var(chow)/length(chow))
tstat = diff/se
qqnorm(hf)
qqline(hf, col=5)

tse = sqrt(var(hf)/(length(hf)-1) + var(chow)/(length(chow)-1))
(1-pnorm(diff/tse))*2
t.test(hf,chow)  

dat = read.csv("femaleMiceWeights.csv")
control = filter(dat, Diet == "chow") %>% drop_na() %>% select(Bodyweight) %>% unlist()
treatment = filter(dat, Diet == "hf") %>% drop_na() %>% select(Bodyweight) %>% unlist()
t.test(treatment, control)

se = sqrt(var(control)/length(control) + var(treatment)/length(treatment))
diff <- mean(treatment) - mean(control)
tstat = diff/se
1 - pnorm(abs(tstat)) + pnorm(-abs(tstat))
length(control) + length(treatment) - 2 

dat = read.csv("mice_pheno.csv")
chowPopulation = dat[dat$Sex == "F" & dat$Diet == "chow", 3]
mu_chow = mean(chowPopulation)
N = 30
chow = sample(chowPopulation, N)
mean_sample = mean(chow)
se = sd(chow)/sqrt(N)
Q = qnorm(1-0.05/2)
interval = c(mean_sample - Q*se, mean_sample + Q*se)


B = 250 
N = 8
Q = qt(1-0.05/2, df=N-1)
mypar()
plot(mean(chowPopulation)+c(-7,7), c(1,1), type="n", xlab='weight', ylab="interval", ylim=c(1,B))
abline(v=mean(chowPopulation))
for (i in 1:B) {
  chow = sample(chowPopulation, N)
  se = sd(chow)/sqrt(N)
  interval = c(mean(chow)-Q*se, mean(chow)+Q*se)
  covered = mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color = ifelse(covered, 1, 2)
  lines(interval, c(i,i), col=color)
}
t.test(treatment, control, conf.level = 0.9)$conf.int

dat = read.csv("mice_pheno.csv")
controlPopulation = dat[dat$Sex == "F" & dat$Diet == "chow", 3]
hfPopulation = dat[dat$Sex == "F" & dat$Diet == "hf", 3]
mu_hf = mean(hfPopulation)
mu_control = mean(controlPopulation)
set.seed(1)
N = 5
hf = sample(hfPopulation, N)
control = sample(controlPopulation, N)
t.test(hf, control)
t.test(hfPopulation, controlPopulation)

# 14.76% Power Calculation
set.seed(1)
library(tidyverse)
dat = read.csv("mice_pheno.csv")
controlPopulation = dat[dat$Sex == "F" & dat$Diet == "chow", 3]
hfPopulation = dat[dat$Sex == "F" & dat$Diet == "hf", 3]
N = 12
alpha = 0.05
B = 2000
reject = function(N, alpha=0.05) {
  hf = sample(hfPopulation, N)
  control = sample(controlPopulation, N)
  pval = t.test(hf, control)$p.value
  pval < alpha
}
reject(12)
rejections = replicate(B, reject(N))
mean(rejections)
Ns = seq(5, 50, 3)
power = sapply(Ns, function(N) {
  rejections = replicate(B, reject(N))
  mean(rejections)
})
plot(Ns, power, type="b")

N = 30
alphas = c(0.1, 0.05, 0.01, 0.001, 0.0001, 0.00001)
power = sapply(alphas, function(alpha) {
  rejections = replicate(B, reject(N, alpha=alpha))
  mean(rejections)
})
plot(alphas, power, type="b", log="x")



# 7/12/18, 04:27
N = 3
thresh = 0.05
rep = 1000
t.test(sample(hfPopulation, N), sample(controlPopulation))$p.value > thresh

mean(replicate(rep, t.test(sample(hfPopulation, N), sample(controlPopulation))$p.value > thresh))

Ns = seq(10, 200, 10)
powers = sapply(Ns, function(n) {
  mean(replicate(rep, t.test(sample(hfPopulation, n), sample(controlPopulation, n))$p.value < thresh))
})
plot(Ns, pvalues, type = "b")

Ns_rep = rep(Ns, each=10)
ps = sapply(Ns_rep, function(n) {
  t.test(sample(hfPopulation, n), sample(controlPopulation, n))$p.value
})
plot(Ns_rep, ps, log="y")

Ns <- seq(10,200,by=10)
Ns_rep <- rep(Ns, each=10)
calculatePvalue <- function(N) {
   hf <- sample(hfPopulation,N) 
   control <- sample(controlPopulation,N)
   t.test(hf,control)$p.value
}
pvalues <- sapply(Ns_rep, calculatePvalue)
plot(Ns_rep, pvalues, log="y", xlab="sample size",
     ylab="p-values")
abline(h=c(.01, .05), col="red", lwd=2)

control = sample(controlPopulation, 12)
hf = sample(hfPopulation,12)
t.test(hf, control)$conf.int / mean(control) * 100

# 7/12/18, 22:25
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke = babies[babies$smoke == 0, 1]
bwt.smoke = babies[babies$smoke == 1, 1]
library(rafalib)
mean(bwt.nonsmoke) - mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
t.test(bwt.nonsmoke, bwt.smoke)
set.seed(1)
N = 5
dat.ns = sample(bwt.nonsmoke, N)
dat.s = sample(bwt.smoke, N)
tval = (mean(dat.ns)-mean(dat.s)) * sqrt(N)/sqrt(var(dat.ns) + var(dat.s))
pval = pnorm(tval) - pnorm(-tval)
diff = mean(dat.ns)-mean(dat.s)
sd.diff = sqrt((var(dat.ns) + var(dat.s))/N)
(mean(dat.ns)-mean(dat.s)) /sd.diff
qnorm(1-0.01/2)
interval = c(diff - qnorm(1-0.01/2)*sd.diff, diff + qnorm(1-0.01/2)*sd.diff)
Q = qt(1 - 0.05/2, df=2*N-2)
interval.t = c(diff - Q*sd.diff, diff + Q*sd.diff)
t.test(dat.ns, dat.s)
computepvals = function(N) {
  dat.ns = sample(bwt.nonsmoke, N)
  dat.s = sample(bwt.smoke, N)
  t.test(dat.ns, dat.s, conf.level = 0.99)$p.value
}
pval = replicate(10000, computepvals(60))
mean(pvals > 0.05)
Ns = c(5, 30, 60, 90, 120)
sapply(Ns, function(n) {
  pval = replicate(1000, computepvals(n))
  mean(pval > 0.01)
})


# 7/13/18, 08:45
dat = read.csv("mice_pheno.csv")
controlPopulation = filter(dat, Sex == "F" & Diet == "chow") %>% select(Bodyweight)  %>% unlist()
ttestgenerator =  function(n) {
  cases = sample(controlPopulation, n)
  controls = sample(controlPopulation, n)
  tstat = (mean(cases) - mean(controls))/sqrt(var(cases)/n + var(controls)/n)
  return(tstat)
}
ttests = replicate(1000, ttestgenerator(6))
hist(ttests)
qqnorm(ttests); qqline(ttests)
qt(0.05, df=10)

controls = rnorm(5000, mean = 24, sd = 3.5)

ttestgenerator = function(n, mean=24, sd=3.5) {
  cases = rnorm(n, mean, sd)
  controls = rnorm(n, mean, sd)
  tstat = (mean(cases)-mean(controls))/sqrt(var(cases)/n + var(controls)/n)
  return(tstat)
}
ttestgenerator(10)

set.seed(1)
prac = rnorm(5)
t = mean(prac)/sqrt(var(prac)/5)

set.seed(1)
B = 1000
computetstat = function(n) {
  prac = rnorm(n)
  t = mean(prac)/sqrt(var(prac)/n)
  return(t)
}
pracdata = replicate(B, computetstat(30))
mean(pracdata > 2)
1-pt(2, df=4)
t.test(pracdata)
qqnorm(pracdata)

computetvals = function(n, mean=0) {
  prac1 = rnorm(n, mean)
  prac2 = rnorm(n, mean)
  tvals = t.test(prac1, prac2, var.equal = TRUE)
  return((tvals$statistic))
}
pracdata = replicate(B, computetvals(5))
qqnorm(pracdata)

X = rnorm(15)
X2 = rbinom(n=15, size = 1, prob = 0.5)
hist(X)
hist(X2)

computebi = function(n=15) {
  X = rbinom(n, size =1, prob = 0.5)
  tstat = sqrt(15)*mean(X)/sd(X)
}
pracbi = replicate(1000, computebi())
hist(pracbi)
qqnorm(pracbi)

dat = read.csv("femaleMiceWeights.csv")
control = dat  %>% filter(Diet == "chow") %>% select(Bodyweight) %>% unlist()
hf = dat  %>% filter(Diet == "hf") %>% select(Bodyweight) %>% unlist()
obsdiff = mean(hf) - mean(control)
sample(c(control, hf))
A = c(1,2,3,4,5,6,7)
B = c(11,22,33,44,55,66,77)
test = sample(c(A,B))
str(test)

babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
N = 10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)
shuffling = function() {
  dat <- c(smokers,nonsmokers)
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean(smokersstar)-mean(nonsmokersstar)
}
permutation = replicate(1000, shuffling())
hist(permutation)
abline(v=obs, col='red')
(sum(permutation < obs) + 1)/(length(permutation)+1)

obs <- median(smokers) - median(nonsmokers)
shuffling = function() {
  dat <- c(smokers,nonsmokers)
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar)-median(nonsmokersstar)
}
permutation = replicate(1000, shuffling())
hist(permutation)
abline(v=obs, col='red')
(sum(permutation < obs) + 1)/(length(permutation)+1)

# 7/13/18, 19:01
factorial(3)
factorial(16)/factorial(16-3)

# 7/14/18, 10:47
tab = matrix(c(3,1,1,3),2,2)
rownames(tab) = c("Before", "After")
colnames(tab) = c("Guessed before", "Guessed after")
fisher.test(tab, alternative = "greater")


food = factor(c("low", "medium", "high", "medium", "low", "high", "high", "low"))
food2 = factor(c("low", "medium", "high", "medium", "low", "high", "high", "low"), levels=c("low", "medium", "high"), ordered=T)

disease=factor(c(rep(0,180),rep(1,20),rep(0,40),rep(1,10)), labels=c("Yes","normal"))

disease=factor(c(rep(0,180),rep(1,20),rep(0,40),rep(1,10)),
               labels=c("normal","sick"))
genotype=factor(c(rep("wildtype",200),rep("mutant",50)),
                levels=c("wildtype","mutant"))
dat = data.frame(disease, genotype)
summary(dat)
dat = dat[sample(nrow(dat)), ]
table(genotype)
table(disease)
tab=table(genotype, disease)
head(dat)
sum(dat$disease == "control" & dat$genotype == "aa")
p=mean(disease=="sick")
expected = rbind(c(1-p, p) * sum(genotype=="wildtype"), 
                  c(1-p,p) * sum(genotype=="mutant"))
dimnames(expected) = dimnames(tab)
chisq.test(tab)$p.value
chisq.test(expected)
tab2 = tab*10
chisq.test(tab2)
fit <- glm(disease~genotype,family="binomial",data=dat)
coeftab = summary(fit)$coef
ci = coeftab[2,1] + c(-2,2)*coeftab[2,2]

library(downloader)
url = "https://studio.edx.org/c4x/HarvardX/PH525.1x/asset/assoctest.csv"
download(url, "assoctest.csv")
dat = read.csv("assoctest.csv")
genotype = factor(dat$allele, labels = c("wildtype", "mutant"))
disease = factor(dat$case, labels = c("normal", "sick"))
tab = table(genotype, disease)
chisq.test(tab)
fisher.test(tab)
tabr = matrix(c(17,17,10,28), 2, 2)
colnames(tabr) = c("wildtype", "mutant")
rownames(tabr) = c("normal", "sick")
chisq.test(tabr)
fisher.test(tabr)

install.packages("UsingR")
library(UsingR)
library(rafalib)
install.packages("rafalib")
x = father.son$fheight
hist(x)
ps = (seq(0, 99) + 0.5)/100
qs = quantile(x, ps)
plot(qs)
normalqs = qnorm(ps, mean(x), popsd(x))
plot(normalqs)
plot(normalqs, qs)
abline(0, 1)
qqnorm(x)
qqline(x)
mean(x)
popsd(x)

n = 1000
x = rnorm(n)
qqnorm(x)
qqline(x)
rt(1000)
display.brewer.all()
bigpar(1, 2, brewer.n = 8)
hist(exec.pay)
qqnorm(exec.pay)
boxplot(exec.pay)

library(UsingR)
data("father.son")
x=father.son$fheight
y=father.son$sheight
plot(x,y, main=paste("correlation = ", signif(cor(x,y), 2)))

groups = split(y, seq(50,100, length=10))
boxplot(groups)

# 7/16/18, 07:33
A = c(1,2,3,4,5,6,7,8,9,10)
B= c(10,9,8,7,6,5,4,3,2,1)
plot(A,B)
cor(A,B)
a2 = sum((A-mean(A))^2)
b2 = sum((B - mean(B))^2)
sum((A-mean(A))*(B-mean(B)))

groups = split(y, round(x))
boxplot(groups)
library(rafalib)
mypar(2,2)
for (i in c(5,8,11,14)) {
  qqnorm(groups[[i]])
  qqline(groups[[i]])
}



x1 = (x-mean(x))/sd(x)
y1 = (y-mean(y))/sd(y)
mypar(2,1)
plot(x,y)
plot(x1,y1)
cor(x,y)
cor(x1,y1)
means = tapply(y1, round(x1*10)/10, mean)
fatherheights = as.numeric(names(means))
mypar(1,1)
plot(fatherheights, means)
abline(0, cor(x1,y1))

library(downloader)
filename <- "fig1.RData"
url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig1.RData"
if (!file.exists(filename)) download(url,filename)
load(filename)
library(rafalib)
mypar()
dat = list(Treatment=x, Control=y)
boxplot(dat,xlab="Group",ylab="Response", cex=0)
stripchart(dat, vertical=TRUE, method="jitter",pch=16,add=TRUE)

url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig3.RData"
filename <- "fig3.RData"
if (!file.exists(filename)) download(url, filename)
load(filename)
dat = list(Treatment=x, Control=y)
barplot(c(mean(x), mean(y)))
boxplot(dat)
boxplot(dat, log="y")

url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig4.RData"
filename <- "fig4.RData"
if (!file.exists(filename)) download(url, filename)
load(filename)
mypar(1,2)
plot(x,y, lwd=2)
fit = lm(y~x)
abline(fit$coef)

set.seed(12201970)
before = runif(300, 5, 8)
after = rnorm(300, before*1.05, 2)
li = range(c(before, after))
ymx = max(abs(after-before))
library(rafalib)
mypar(1,2)
plot(before, after)
fit = lm(after~before)
abline(fit$coef)
cor(before,after)
plot(before, after-before)
fit2 = lm(after-before~before)
abline(fit2$coef)
z = rep(c(0, 1), rep(300,2))
plot(z, c(before, after), segments(rep(0,300), before, rep(1,300), after), col=2)
boxplot(before, after)
library(downloader)
filename <- "fig8dat.csv"
url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig8dat.csv"
if (!file.exists(filename)) download(url, filename)
x <- read.table(filename, sep=",", header=TRUE)
plot(x[,1],x[,2],xlab="log Dose",ylab="Proportion survived",ylim=c(0,1), type="l",lwd=2,col=1)
plot(x[,1],x[,2], type='l', col=1, ylim=c(min(x[,-1]),max(x[,-1])))
lines(x[,1], x[,3], col=2)
lines(x[,1], x[,4], col=3)
legend(1, 0.4, c("A", "B", "C"), lwd=2, col=1:3)
library(UsingR)
library(tidyverse)

head(InsectSprays)

for (i in unique(InsectSprays$spray)) {
  x[i] = InsectSprays[1, InsectSprays==i]
}

load("skew.RData")
str(dat)
boxplot(dat)

library(UsingR)
data(nym.2002)
nym.2002
library(tidyverse)
x = filter(nym.2002, gender=="Male")  %>% select(time)
y = filter(nym.2002, gender=="Female")  %>% select(time)
z = list(x,y)
boxplot(z)
log(10)
log2(10)
log(1/10)

x = rnorm(1000)
y = rnorm(1000)
ratio = x/y
logratio = log(ratio)
hist(ratio)
hist(logratio)

set.seed(779)
N=25
x = rnorm(N, 0,1)
y = rnorm(N, 0, 1)
x[1] = 5
x[2] = 7
hist(x)
plot(x, y)
boxplot(x,y)
t.test(x,y)
wilcox.test(x,y)
library(rafalib)
mypar(1,2)
stripchart(list(x,y), vertical=TRUE, ylim=c(-7,7))
abline(h=0)
xrank = rank(c(x,y))[seq(along=x)]
yrank = rank(c(x,y))[-seq(along=y)]
stripchart(list(xrank, yrank), vertical=TRUE)
ws = sapply(x, function(z) rank(c(z, y))[1]-1)

data(ChickWeight)
str(ChickWeight)
plot(ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
head(ChickWeight)
chick = reshape(ChickWeight, idvar = c("Chick", "Diet"), timevar="Time", direction="wide")
head(chick)

InsectSprays
reshape(InsectSprays, timevar="spray", direction = "wide")

x = c(3,4,5)
y = c(3,6,7)
x^-1
library()