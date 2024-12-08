## -----------------------------------------------------------------------------
data(precip)
hist(precip,
     main = "美国城市年降水量",
     xlab = "降水量",
     ylab = "频数"
     )


## -----------------------------------------------------------------------------
data(airmiles)
plot(airmiles)

## -----------------------------------------------------------------------------
df<-data.frame(Year = as.numeric(time(airmiles)),Miles = as.numeric(airmiles))
model <- lm(Miles~Year,data =df)
plot(df$Year, 
     df$Miles, 
     main="Air Miles Over Time",
     xlab="Year", 
     ylab="Miles",
     col="blue"
     )
abline(model, col="red")

## -----------------------------------------------------------------------------
x<-rweibull(10000,shape=1,scale=0.06)
hist(x,
     xlab='频数',
     ylab='取值',
     density=10,
     nclass=100)

## -----------------------------------------------------------------------------
set.seed(122)
n<- 1000
t1<-1 #σ's value
t2<-2
t3<-3
u<-runif(n,min=0,1-0.0001)
x1<-sqrt(-2*(t1**2)*log(1-u))
x2<-sqrt(-2*(t2**2)*log(1-u))
x3<-sqrt(-2*(t3**2)*log(1-u))
y<-seq(0,10,.01)
par(mfrow=c(1,3))
hist(x1,prob=TRUE,breaks=20,main ='σ=1')
lines(y,y/(t1**2)*exp(-y**2/(2*t1**2)))
hist(x2,prob=TRUE,breaks=20,main ='σ=2')
lines(y,y/(t2**2)*exp(-y**2/(2*t2**2)))
hist(x3,prob=TRUE,breaks=20,main ='σ=3')
lines(y,y/(t3**2)*exp(-y**2/(2*t3**2)))

## -----------------------------------------------------------------------------
set.seed(123)
p11=0.75
p21=0.5
p31=0.25
p12=1-p11
p22=1-p21
p32=1-p31
n<-1e4
x1<-rnorm(n,0,1)
x2<-rnorm(n,3,1)
r1<-sample(c(0,1),size = n,replace = TRUE,prob = c(p12,p11))
r2<-sample(c(0,1),size = n,replace = TRUE,prob = c(p22,p21))
r3<-sample(c(0,1),size = n,replace = TRUE,prob = c(p32,p31))
z1<-r1*x1+(1-r1)*x2
z2<-r2*x1+(1-r2)*x2
z3<-r3*x1+(1-r3)*x2
par(mfrow=c(1,3))
hist(z1,breaks = 50,main ="p1=0.75")
hist(z2,breaks = 50,main ="p1=0.5")
hist(z3,breaks = 50,main ="p1=0.25")

## -----------------------------------------------------------------------------
size<-1e4
t=10
lambda =3#the parameter of poisson process
shape<-1
scale<-1#the parameters of Gamma distribution
n<-qpois(1-1e-9,lambda =lambda*t)#the largest number which the poisson process can get
poip=function(z){
  
}
xn<-rpois(size,lambda*t)
xs<-c()
for(i in 1:size){
  yi=cumsum(c(rgamma(n,shape = shape,scale = scale)))
  xs<-c(xs,yi[xn[i]])
}
print("Mean and Variance of the sample")
print(mean(xs))
print(var(xs))
print("Theoretical mean and variance")
print(lambda*t*shape*scale)
print(lambda*t*(shape+1)*shape*scale**2)

## -----------------------------------------------------------------------------
n=10000
Beta.cdf = function(x){
  y=runif(n)
  return(mean(dbeta(x*y,3,3))*x)
}
t<-seq(0.1,0.9,0.1)
x1<-sapply(t,function(t) pbeta(t,3,3))
x2<-sapply(t,function(t) Beta.cdf(t))
round(rbind(x1,x2),4)

## -----------------------------------------------------------------------------
n=10000
a = 2#value of σ
pdf1.Rayleigh = function(x){
  u1=runif(n/2)
  return(a*(-2*log(1-u1))**0.5)
}
pdf2.Rayleigh = function(x){
  u2=runif(n/2)
  return(a*(-2*log(u2))**0.5)
}

## -----------------------------------------------------------------------------
g = function (x) {
  x^2/sqrt(2*pi)*exp(-x^2/2)
}
x = seq(0,10,0.1)
y = g(x)
plot(x,y,type="l")


## -----------------------------------------------------------------------------
g = function (x) {
  x^2/sqrt(2*pi)*exp(-x^2/2)
}
x = seq(1,10,0.1)
y = g(x)
plot(x,y,type="l",ylim=c(0,1))
y_norm<-dnorm(x,mean = 1.5, sd= 1)
y_weibull<-dweibull(x,shape = 2,scale = 2)
lines(x,y_norm,col="red",ylim=c(0,1))
lines(x,y_weibull,col="blue",ylim=c(0,1))
sd_norm<-sd(y/y_norm)
sd_weibull<-sd(y/y_weibull)
rbind(sd_norm,sd_weibull)

## ----chunk-name, eval=FALSE, echo=TRUE,results='asis'-------------------------
#  n=10000
#  t<-c(n,2*n,4*n,6*n,8*n)
#  
#  ##快速排序算法
#  quicksort=function(x){
#      if(length(x)<=1)return(x)
#      x0<-x[1]
#      loc<-1
#      low<-1
#      n<-length(x)
#      high<-n
#      while(low!=high){
#          if(loc==low){
#              if(x[high]<x[loc]){
#                  tmp<-x[high]
#                  x[high]<-x[loc]
#                  x[loc]<-tmp
#                  low=low+1
#                  loc=high
#              }else{
#                      high=high-1
#                  }
#              }else{
#                  if(x[low]>x[loc]){
#                      tmp<-x[low]
#                      x[low]<-x[loc]
#                      x[loc]<-tmp
#                      high=high-1
#                      loc=low
#                  }else{
#                      low=low+1
#                  }
#             }
#          }
#      L=c()
#      R=c()
#      if(low>1) L=x[1:(low-1)]
#      if(low<length(x)) R=x[(low+1):n]
#      return(c(quicksort(L),x[low],quicksort(R)))
#  }
#  
#  time<-numeric(5)
#  k<-0
#  for(i in t){
#    k<-k+1
#    random_sample=sample(1:n,i,replace=TRUE)
#    x<-numeric(100)
#    for(j in 1:100){
#      start<-Sys.time()
#      y<-quicksort(random_sample)
#      end<-Sys.time()
#      x[j]<-(end-start)
#    }
#    time[k]<-mean(x)
#  }
#  ##回归分析
#  x_log_x<-t*log(t)
#  model<-lm(time ~ x_log_x)
#  y_pred<-predict(model)
#  polt_data<-data.frame(x_log_x=x_log_x,y=time,y_pred=y_pred)
#  ggplot(plot_data,aes(x=x_log_x))+
#    geom_point(aes(y=y),color="blue")+
#    geom_line(aes(y=y_pred),color="red") +
#    labs(title="Regression of y on x * log(x)",
#         x="x * log(x)",
#         y="y")+
#    theme_minimal()

## -----------------------------------------------------------------------------
n<-30
n_sim<-10000
skew_value<-numeric(n_sim)
for(i in 1:n_sim){
  sample_data<-rnorm(n)
  skew_value[i]<-sum((sample_data-mean(sample_data))^3)*(n^0.5)/((sd(sample_data)*(n-1)^0.5)^3)
}
quantiles<-quantile(skew_value,probs=c(0.025,0.05,0.95,0.975))
standard_error<-sqrt(6/n)
cat("estimate quantiles of skewness: \n")
print(quantiles)
cat("standard error:\n")
print(standard_error)
theory_quantiles<-qnorm(c(0.025,0.05,0.95,0.975),mean=0,sd=(6/n)^0.5)
cat("theoretical quantiles of large sample approximation:\n")
print(theory_quantiles)

## -----------------------------------------------------------------------------
count5test=function(x,y){
  X<-x-mean(x)
  Y<-y-mean(y)
  outx<-sum(X>max(Y))+sum(X<min(Y))
  outy<-sum(Y>max(X))+sum(Y<min(X))
  return(as.integer(max(c(outx,outy))>5))
}
sigma1 <- 1
sigma2 <- 1.5
alpha<-0.055
num<-c(5,20,100)
m<-1000
power_five<-power_F<-numeric(3)
count_five_power<-F_test_power<-numeric(3)
for(i in 1:3){
  count_five_reject<-0
  f_test_reject<-0
  for(j in 1:m){
    x<-rnorm(num[i], 0, sigma1)
    y<-rnorm(num[i], 0, sigma2)
    
    count_five_reject<-count_five_reject+count5test(x,y)
    f_test_reject<-f_test_reject+(var.test(x,y)$p.value<alpha)
  }
  count_five_power[i]<-count_five_reject/m
  F_test_power[i]<-f_test_reject/m
}
rbind(count_five_power,F_test_power)

## -----------------------------------------------------------------------------
N<-1000
n_null<-950
n_alt<-50
alpha<-0.1
m<-1e4

results<-matrix(0, nrow = 3, ncol = 2)
rownames(results)<-c("FWER", "FDR", "TPR")
colnames(results)<-c("Bonferroni correction", "B-H correction")

for(i in 1:m){
  p_null<-runif(n_null)
  p_alt<-rbeta(n_alt,0.1,1)
  p_value<-c(p_null,p_alt)
  
  bonf_p<-p.adjust(p_value,method = "bonferroni")
  bh_p<-p.adjust(p_value,method="BH")
  
  reject_bonf<-bonf_p<alpha
  true_positive_bonf<-sum(reject_bonf[(n_null+1):N])
  false_positive_bonf<-sum(reject_bonf[1:n_null])
  fwer_bonf<-ifelse(false_positive_bonf>0,1,0)
  fdr_bonf<-ifelse(sum(reject_bonf)>0,false_positive_bonf/sum(reject_bonf),0)
  tpr_bonf<-true_positive_bonf/n_alt
  
  reject_bh<-bh_p<alpha
  true_positive_bh<-sum(reject_bh[(n_null+1):N])
  false_positive_bh<-sum(reject_bh[1:n_null])
  fwer_bh<-ifelse(false_positive_bh>0,1,0)
  fdr_bh<-ifelse(sum(reject_bh)>0,false_positive_bh/sum(reject_bh),0)
  tpr_bh<-true_positive_bh/n_alt
  
  results["FWER", "Bonferroni correction"]<-results["FWER", "Bonferroni correction"]+fwer_bonf
  results["FDR", "Bonferroni correction"]<-results["FDR", "Bonferroni correction"]+fdr_bonf
  results["TPR", "Bonferroni correction"]<-results["TPR", "Bonferroni correction"]+tpr_bonf
  
  results["FWER", "B-H correction"]<-results["FWER", "B-H correction"]+fwer_bh
  results["FDR", "B-H correction"]<-results["FDR", "B-H correction"]+fdr_bh
  results["TPR", "B-H correction"]<-results["TPR", "B-H correction"]+tpr_bh
}
result<-results/m
print(round(result,4))

## -----------------------------------------------------------------------------
library(boot)
set.seed(111)
hours<-c(3,5,7,18,43,85,91,98,100,130,230,487)
n<-12
B<-1000

mle=function(value,indices){
  return(length(value[indices])/sum(value[indices]))
}
lambda_hat<-mle(hours)
lambda_b<-boot(data = hours,statistic = mle,R = B)

round(c(bias=mean(lambda_b$t)-lambda_hat,standard_error=sd(lambda_b$t)),4)

## -----------------------------------------------------------------------------
set.seed(111)

mle2=function(value,indices){
  return(sum(value[indices])/length(value[indices]))
}
lambda_b_back<-boot(data = hours,statistic = mle2,R = B)
bootstrap_ci <- boot.ci(lambda_b_back,conf =0.95, type = c("norm","basic", "perc", "bca"))

print(bootstrap_ci)

## -----------------------------------------------------------------------------
library(bootstrap)
sigma<-cov(scor)
eigenvalue<-eigen(sigma)
value<-eigenvalue$values[order(-(eigenvalue$values))]
theta<-value[1]/sum(value)

n<-88
theta.hat<-numeric(n)
for(i in 1:n){
  jack.scor<-scor[-i,]
  sigma.hat<-cov(jack.scor)
  ss<-eigen(sigma.hat)
  vv<-ss$values[order(-(ss$values))]
  theta.hat[i]<-vv[1]/sum(vv)
}
jack.bias<-(n-1)*(mean(theta.hat)-theta)
jack.se<-sqrt((n-1)*mean((theta.hat-theta)**2))
print(round(c(jackknife.estimates.of.bias=jack.bias,jackknife.estimates.of.standerd.error=jack.se),4))

## -----------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
n<-length(magnetic)   
e1<-e2<-e3<-e4<-numeric(n)
yhat1<-yhat2<-yhat3<-yhat4<-numeric(n)
SSR1<-SSR2<-SSR3<-SSR4<-SST1<-SST2<-SST3<-SST4<-numeric(n)
ybar<-mean(magnetic)
for (k in 1:n) {
  y<-magnetic[-k]
  x<-chemical[-k]

  J1<-lm(y ~ x)
  yhat1[k]<-J1$coef[1]+J1$coef[2]*chemical[k]
  e1[k]<-magnetic[k]-yhat1[k]
  SSR1[k]<-(yhat1[k]-ybar)^2
  SST1[k]<-(magnetic[k]-ybar)^2

  J2<-lm(y ~ x + I(x^2)) 
  yhat2[k]<-J2$coef[1]+J2$coef[2]*chemical[k]+
    J2$coef[3]*chemical[k]^2
  e2[k]<-magnetic[k]-yhat2[k]
  SSR2[k]<-(yhat2[k]-ybar)^2
  SST2[k]<-(magnetic[k]-ybar)^2

  J3<-lm(log(y) ~ x) 
  logyhat3<-J3$coef[1]+J3$coef[2]*chemical[k]
  yhat3[k]<-exp(logyhat3)
  e3[k]<-magnetic[k]-yhat3[k]
  SSR3[k]<-(yhat3[k]-ybar)^2
  SST3[k]<-(magnetic[k]-ybar)^2

  J4<-lm(y ~ x+I(x^2)+I(x^3)) 
  yhat4[k]<-J4$coef[1]+J4$coef[2]*chemical[k] +
  J4$coef[3]*chemical[k]^2+J4$coef[4]*chemical[k]^3
  e4[k]<-magnetic[k]-yhat4[k]
  SSR4[k]<-(yhat4[k]-ybar)^2
  SST4[k]<-(magnetic[k]-ybar)^2
}
print(c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2)))

print(c(51/50*sum(SSR1)/sum(SST1),51/49*sum(SSR2)/sum(SST2),51/50*sum(SSR3)/sum(SST3),51/48*sum(SSR4)/sum(SST4))) 

## ----eval = FALSE-------------------------------------------------------------
#  
#  library(RVAideMemoire)
#  attach(chickwts)
#  
#  x1=sort(weight[feed =="soybean"])
#  ys=sort(weight[feed == "linseed"])
#  xs=sample(x1, size = length(ys))
#  rep=1000
#  
#  zs=c(xs, ys)
#  n1=length(xs)
#  n=n1*2
#  
#  Ts=numeric(rep)
#  for (i in 1:rep) {
#    ks=sample(1:n, n1, replace = FALSE)
#    zs1=zs[ks]
#    zs2=zs[-ks]
#    Ts[i]=cramer.test(zs1, zs2)$statistic
#  }
#  
#  (cvm=cramer.test(x, y))
#  T.hat=cvm$statistic
#  (p.hat=mean(abs(T.hat) < abs(Ts)))
#  
#  hist(Ts)

## -----------------------------------------------------------------------------
soybean=chickwts$weight[chickwts$feed=="soybean"]
linseed =chickwts$weight[chickwts$feed=="linseed"]
n=length(soybean)
m=length(linseed)

tmp=min(n, m)
soybean=sort(soybean[1:tmp])
linseed=sort(linseed[1:tmp])

zs=c(soybean, linseed)
spearman.cor.test=cor.test(x = soybean, y = linseed, method = "spearman")

B=1000
k=length(zs)
rep<-1000
rhos=numeric(rep)

for(b in 1:B) {
  i=sample(1:k, k/2, replace = FALSE)
  xs=zs[i]
  ys=zs[-i]
  rhos[b]=cor(x = xs, y = ys, method = "spearman")
}

hist(rhos, breaks = 100)
(theta.hat=spearman.cor.test$estimate)
spearman.cor.test$p.value
(p.hat=mean(abs(rhos) > abs(theta.hat)))
(alpha=0.05)

## -----------------------------------------------------------------------------
set.seed(111)
theta=1
eta=0


f<-function(x){
  stopifnot(theta > 0)
  return(1/(theta*pi*(1+((x-eta)/theta)**2)))
}

m<-1e4
k<-0
u<-runif(m)
x<-numeric(m)

x[1]<-rnorm(1,mean = 1)
for(i in 2:m){
  xt<-x[i-1]
  y<-rnorm(1,mean = xt)
  r=f(y)*dnorm(x=xt,mean=y)/(f(xt)*dnorm(x=y,mean = xt))
  if(u[i]<=r){
    x[i]=y
  }else{
    k=k+1
    x[i]=xt
  }
}
is = 5001:5500
par(mfrow = c(1,2))
plot(is, x[is], type="l")
a<-ppoints(100)
QR<-tan(pi*(a-0.5))
Q<-quantile(x, a)
qqplot(QR, Q, main="",
        xlab="Cauty Quantiles", ylab="Sample Quantiles")
abline(0,1,col='blue',lwd=2)
hist(x, probability = TRUE, breaks = 100)
plot.x = seq(min(x), max(x), 0.01)
lines(plot.x, f(plot.x))
par(mfrow = c(1,1))

## -----------------------------------------------------------------------------
n<-100
a<-20
b<-30
f=function(x,y){
  return(factorial(n)/(factorial(x)*factorial(n-x))*(y**(x+a-1))*((1-y)**(n-x+b+1)))
}
m<-5000
X=matrix(0,nrow = m,ncol = 2)
for(i in 2:m){
  xt=X[i-1,]
  xt[1]=rbinom(1,n,xt[2])
  xt[2]=rbeta(1,xt[1]+a,n-xt[1]+b)
  X[i,]=xt
}
x<-X[1001:m,]
cat('Means: ',round(colMeans(x),2))
cat('Standard errors: ',round(apply(x,2,sd),2))
cat('Correlation coefficients: ', round(cor(x[,1],x[,2]),2))

plot(x[,1],type='l',col=1,lwd=2,xlab='Index',ylab='Random numbers')
lines(x[,2],col=2,lwd=2)
legend('bottomright',c(expression(X[1]),expression(X[2])),col=1:2,lwd=2)

## -----------------------------------------------------------------------------
a<-c(1,2)
k_term = function(a,k){
  d<-length(a)
  return((-1)^k * exp((2*k+2)*log(norm(a,type="2"))-lgamma(k+1)-k*log(2)-log(2*k+1)-log(2*k+2)+lgamma((d+1)/2)+lgamma(k+3/2)-lgamma(k+d/2+1)))
}
n<-500
z<-numeric(n)
thesum = function(a){
  for (i in 1:n) {
    z[i]<-k_term(a,i-1)
  }
  return(sum(z))
}
thesum(a)

## -----------------------------------------------------------------------------
m <- c(5:25, 100, 500, 1000)
n <- c(4:25, 100)
A <- numeric(24)
B <- numeric(23)

for (k in m) {
f <- function(x){
       return(exp(log(2) + lgamma(x/2) - (1/2)*(log(pi*(x-1))) - lgamma((x-1)/2)))
}

g <- function(a){
       return((f(k)*integrate(function(x)(1 + (x^2)/(k-1))^(-k/2),lower = sqrt((a^2)*(k-1)/(k-a^2)), upper = Inf)$value) - (f(k+1)*integrate(function(x)(1 + (x^2)/k)^(-(k+1)/2), lower = sqrt((a^2)*k/(k+1-a^2)), upper = Inf)$value))
}
A[k] <- uniroot(g, lower = 0.01, upper = 1+sqrt(k)/2)$root
}

for (k in n) {
f <- function(x){
       return(exp(log(2) + lgamma(x/2) - (1/2)*(log(pi*(x-1))) - lgamma((x-1)/2)))
}
h <- function(a){
       return((f(k)*integrate(function(x)(1 + (x^2)/(k-1))^(-k/2), lower = 0, upper = sqrt((a^2)*(k-1)/(k-a^2)))$value) - (f(k+1)*integrate(function(x)(1 + (x^2)/k)^(-(k+1)/2), lower = 0, upper = sqrt((a^2)*k/(k+1-a^2)))$value))
}
B[k] <- uniroot(h, lower = 0.01, upper = 1+sqrt(k)/2)$root
}

A <- c(NA, A[m])
B <- c(B[n], NA, NA)
k <- c(4:25, 100, 500, 1000)
result <- data.frame(k, A, B)
knitr::kable(result)

## -----------------------------------------------------------------------------
Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85) 
tau <- 1
censored <- Y == tau 
n <- length(Y)  

lambda <- 1 

tolerance <- 1e-6  
max_iter <- 1000  
for (iter in 1:max_iter) {
  Y_complete <- Y
  Y_complete[censored] <- tau + 1 / lambda
  lambda_new <- mean(1/Y_complete)
  
  if (abs(lambda - lambda_new) < tolerance) {
    cat("steps:", iter, "\n")
    break
  }

  lambda <- lambda_new
}


cat("lambda：", lambda, "\n")

lambda_mle_observed <- mean(1/Y)
cat("MLE：", lambda_mle_observed, "\n")


## ----warning=FALSE------------------------------------------------------------
library(lpSolve)

objective <- c(4, 2, 9)

constraints <- matrix(c(2, 1, 1,
                        1, -1, 3), 
                      nrow = 2, byrow = TRUE)

rhs <- c(2, 3)

direction <- c("<=", "<=")

solution <- lp("min", objective, constraints, direction, rhs, compute.sens = TRUE)

cat("Optimal value:", solution$objval, "\n")
cat("Optimal solution (x, y, z):", solution$solution, "\n")


## -----------------------------------------------------------------------------
formulas <- list(
 mpg ~ disp,
 mpg ~ I(1 / disp),
 mpg ~ disp + wt,
 mpg ~ I(1 / disp) + wt
 )

v1<-vector("list",4)
for (i in 1:4){
v1[i] <- lapply(i,function(i) {lm(formulas[[i]], data = mtcars)})
}

## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
 rows <- sample(1:nrow(mtcars), rep = TRUE)
 mtcars[rows, ]
})

v2<-vector("list",10)
for (i in 1:10){
v2[i] <- lapply(bootstraps[i], lm, formula = mpg ~ disp)
}

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared
# question 3
sapply(v1, rsq)
#question 4
sapply(v2, rsq)

## -----------------------------------------------------------------------------
trials <- replicate(
         100,
         t.test(rpois(10, 10), rpois(7, 10)),
         simplify = FALSE
       )
sapply(trials, function(x) x[["p.value"]])
sapply(trials, "[[", "p.value")

## -----------------------------------------------------------------------------
chisq_test_fast <- function(x, y) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both inputs must be numeric vectors")
  }
  if (any(is.na(x)) || any(is.na(y))) {
    stop("Input vectors must not contain missing values")
  }
  n <- rbind(x, y)
  n1 <- rowSums(n)
  n2 <- colSums(n)
  k <- sum(n)
  m <- tcrossprod(n1,n2) / k

  x_stat <- sum((n - m)^2 / m)
  df <- (length(n1) - 1) * (length(n2) - 1)
  p.value <- pchisq(x_stat, df = df, lower.tail = FALSE)

  list(x_stat = x_stat, df = df, p.value = p.value)
}

## -----------------------------------------------------------------------------
library(fastmatch)
fast_table <- function(x, y) {
  if (!is.integer(x)) x <- as.integer(x)
  if (!is.integer(y)) y <- as.integer(y)
  
  a_sort <- sort(unique(a))
  b_ssort <- sort(unique(b))
  
  a_len <- length(a_sort)
  b_len <- length(b_sort)
  
  dim <- c(a_len, b_len)
  pr <- a_len * b_len
  dn <- list(a = a_sort, b = b_sort)
  
  bin <- fmatch(a, a_sort) + a_len * fmatch(b, b_sort) - a_len
  y <- tabulate(bin, pr)
  
  y <- array(y, dim = dim, dimnames = dn)
  class(y) <- "table"
  
  y
}

## -----------------------------------------------------------------------------
library(Rcpp)
library(SA24204180)
# 参数设置
n = 100
a = 10

# 运行R和Rcpp版本

x_r = gibbsR(n, a)
x_cpp = gibbsC(n, a)

qqplot(x_r[,1], x_cpp[,1], main = "QQ Plot for X", 
       xlab = "R Gibbs Samples", ylab = "Rcpp Gibbs Samples")
abline(0, 1, col = "red")

qqplot(x_r[,2], x_cpp[,2], main = "QQ Plot for Y", 
       xlab = "R Gibbs Samples", ylab = "Rcpp Gibbs Samples")
abline(0, 1, col = "red")

library(microbenchmark)

benchmark_results = microbenchmark(
  R_version = gibbsR(n, a),
  Rcpp_version = gibbsC(n, a),
  times = 10
)

print(benchmark_results)


