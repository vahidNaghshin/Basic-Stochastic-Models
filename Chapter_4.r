png(
  "exprd.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
w <- rexp(1000)
x <- seq(0,10, length = 1000)
hist(w, prob = T)
points(x, dexp(x), type = "l")

dev.off()
browseURL("exprd.png")


set.seed(1)
alpha = 0.9
x <- w <- rnorm(100)
for (t in 2:100) x[t] <- alpha*x[t - 1] + w[t]
x.ar <- ar(x, method = "mle")
print(cbind("The estimated parameter: ", x.ar$ar))
print("The 95% confidence interval for parameter estiamted: ")
print( x.ar$ar + c(-2, 2) * sqrt(x.ar$asy.var))

set.seed(1)
alpha = 1.02
x <- w <- rnorm(100)
for (t in 2:100) x[t] <- alpha*x[t - 1] + w[t]
x.ar <- ar(x)
print(cbind("The estimated parameter: ", x.ar$ar))
print("The 95% confidence interval for parameter estiamted: ")
print( x.ar$ar + c(-2, 2) * sqrt(x.ar$asy.var))


set.seed(1)
x <- w <- rnorm(1000)
for (t in 3:1000) x[t] <- (5/6)*x[t - 1] - (1/6)*x[t-2] + w[t]

mu = mean(x)
png(
  "acf.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
acf(x, main="(5/6)*x[t - 1] - (1/6)*x[t-2] + w[t]")
dev.off()
browseURL("acf.png")

png(
  "pacf.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
pacf(x, main="(5/6)*x[t - 1] - (1/6)*x[t-2] + w[t]")
dev.off()
browseURL("pacf.png")

 x.ar <- ar(x, method = "mle")
print(cbind("The model order: ", x.ar$order))
print(cbind("The estimated parameter: ", x.ar$ar))
print( x.ar$ar[1] + c(-2, 2) * sqrt(x.ar$asy.var[1,1]))
print( x.ar$ar[2] + c(-2, 2) * sqrt(x.ar$asy.var[1,1]))
#Yes they are stationary as the roots are out of unit circle (i.e., the abs values of roots are greater than unit)
print(abs(polyroot(c(1,-x.ar$ar[1],-x.ar$ar[2]))))

png(
  "acf_res.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)

acf(x.ar$res[-(1:x.ar$order)], main="residual error correlogram", lag = 50)
dev.off()
browseURL("acf_res.png")


set.seed(1)
x <- w <- rnorm(1000)
for (t in 3:1000) x[t] <- (3/2)*x[t - 1] - (1/2)*x[t-2] + w[t]
y = diff(x)
print(length(y))
y.ar <- ar(y-mean(y), method = "mle")
print(cbind("The model order: ", y.ar$order))
print(cbind("The estimated parameter: ", y.ar$ar))
for (t in 1:y.ar$order){
print( y.ar$ar[t] + c(-2, 2) * sqrt(y.ar$asy.var[t,t]))
}
png(
  "acf_res_diff_x.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
acf(y.ar$res[-(1:y.ar$order)], main="residual error correlogram of y=diff(x) for x[t] <- (3/2)*x[t - 1] - (1/2)*x[t-2] + w[t]", lag = 50)
dev.off()
browseURL("acf_res_diff_x.png")
www <-file.path(getwd(), 'global.dat')
Global = scan(www)
Global.ts = ts(Global, st = c(1856, 1), end = c(2005, 12),fr = 12)
Global.agg <- aggregate(Global.ts, FUN = mean)
Global.ar <- ar(Global.agg, method = "mle")
mu = mean(Global.agg)

x <- rep(0,length(Global.agg))

x[1] <- Global.agg[1]
x[2] <- Global.agg[2]
x[3] <- Global.agg[3]
x[4] <- Global.agg[4]

for (t in (Global.ar$order+1):length(Global.agg)) 
{
  x[t] <- mu + Global.ar$ar[1]*(x[t-1]-mu)+Global.ar$ar[2]*(x[t-2]-mu)+Global.ar$ar[3]*(x[t-3]-mu)+Global.ar$ar[4]*(x[t-4]-mu)
}

png(
  "res_x_ar.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
acf(Global.ar$res[-(1:Global.ar$order)])
dev.off()
browseURL("res_x_ar.png")

png(
  "acf_globl_mean.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
acf(Global.agg)
dev.off()
browseURL("acf_globl_mean.png")

png(
  "pacf_globl_mean.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
pacf(Global.agg)
dev.off()
browseURL("pacf_globl_mean.png")

global.predict = predict(Global.ar, n.ahead = 100)

png(
  "pred_globl_mean.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
ts.plot(Global.agg, global.predict$pred, lty = 1:2, lwd=1, main="Predicted Global temp for 100 years")
abline(h=mu, col="blue")
dev.off()
browseURL("pred_globl_mean.png")