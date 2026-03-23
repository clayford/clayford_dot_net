

rout <- replicate(n = 1000, expr = {
  n <- 500
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  lp <- 0.2 + 0.5*x1
  y <- rbinom(n = n, size = 1, prob = plogis(lp))
  m1 <- glm(y ~ x1, family = binomial)
  m2 <- glm(y ~ x1 + x2, family = binomial)
  out <- anova(m1, m2)
  out$Deviance[2]})
mean(rout)
hist(rout, freq = F, breaks = 50)
curve(expr = dchisq(x, df = 1), from = 0, to = 20, add = TRUE, col = "red", n = 401, lwd = 2)


library(ggplot2)
d <- data.frame(dev = rout)
ggplot(d) +
  aes(x = dev) +
  geom_density() +
  geom_function(fun = dchisq, args = list(df = 1), color = "red", xlim = c(0.1, 20))


rout2 <- replicate(n = 1000, expr = {
  n <- 500
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  lp <- 0.2 + 0.5*x1
  y <- rpois(n = n, lambda = exp(lp))
  m1 <- glm(y ~ x1, family = poisson)
  m2 <- glm(y ~ x1 + x2, family = poisson)
  out <- anova(m1, m2)
  out$Deviance[2]})

mean(rout2)
hist(rout2, freq = F, breaks = 50)
curve(expr = dchisq(x, df = 1), from = 0, to = 20, add = TRUE, col = "red", n = 401, lwd = 2)
