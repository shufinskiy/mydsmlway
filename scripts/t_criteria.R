## Расчёт t-критерия для двух независимых выборок
set.seed(42)
dist1 <- rnorm(20, mean = 100, sd = 8)
dist2 <- rnorm(20, mean = 90, sd = 7)

t_test <- t.test(dist1, dist2)
CI <- function(distribution, 
               type = c("beta", "binom", "cauchy", "chisq", "exp", "f", "gamma",
                        "geom", "hyper", "logis", "lnorm", "nbinom", "norm", "pois",
                        "t", "tukey", "unif", "weibull", "wilcox", "signrank"), 
               acc, err, ...){
  match.arg(type)
  m <- mean(distribution, na.rm = TRUE)
  s <- sd(distribution, na.rm = TRUE)
  n <- length(distribution)
  z <- do.call(paste0("q", type), list((1 - (1-acc)/2), ...))
  error <- z*s/sqrt(n)
  if(err){
    return(error)
  }
  left <- m - error
  right <- m + error
  return(c(left, right))
}
ci.1 <- CI(dist1, "t", 0.95, err = FALSE, df = (length(dist1)-1))
ci.2 <- CI(dist2, "t", 0.95, err = FALSE, df = (length(dist2)-1))
me <- sapply(list(dist1, dist2), mean)
ci <- sapply(list(dist1, dist2), CI, "t", 0.95, err = TRUE, df = 19)
boxplot(dist1, dist2)
x <- 1:2
# Plot
par(2, 2, 3, 2)
plot(me~x, ylim = c(80, 110), xlab="x", ylab="y", pch=16, cex=2, xaxt="none")
axis(1, seq(1,2,1))
# Add error bars
arrows(x0=x, y0=me-ci, x1=x, y1=me+ci, code=3, angle=90, length=0.1)
