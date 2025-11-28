## Exercise 3 
## 3.1
{r}
df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))
df
## 3.2

nll_lm <- function(par, data)
{
  b0 <- par[1]
  b1 <- par[2]
  b2 <- par[3]
  b3 <- par[4]
  sig <- par[5]
  y <- data$y
  mu <-b0 + b1*data$x1+ b2*data$x2 + b3*data$x3
  num <- y-mu
  y1 <- -sum(dnorm(x=num,mean= 0, sd=sig, log=TRUE))
  y1
}
## 3.3

num <- c(mean(df$y),0,0,0, sd(df$y))
optim(par = num, 
      fn = nll_lm, 
      data = df, 
      method = "L-BFGS-B", 
      lower = -Inf, 
      upper = Inf)

## 3.5
x <- cbind(1,df$x1,df$x2,df$x3)
y <- df$y
num<- solve(crossprod(x), crossprod(x,y))
num
## 3.6
row <- nrow(df)
col <- ncol(df)

hat <- y-x %*% num
sqrt(crossprod(hat)/row)
sqrt(crossprod(hat)/col)

## 3.8
num <- c(mean(df$y),0,0,0, sd(df$y))
s <- optim(par = num, 
           fn = nll_lm, 
           data = df, 
           method = "L-BFGS-B", 
           lower = -Inf, 
           upper = Inf,
           hessian = TRUE)
mat <- solve(s$hessian)
sqrt(diag(mat))[1:4]
lm <- lm(y ~ x1 + x2 + x3, data = df)

betahat_lm  <- coef(fit_lm)
betahat_lm
sigmahat_lm <- summary(fit_lm)$sigma
sigmahat_lm