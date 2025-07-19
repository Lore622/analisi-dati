k <- 500
nt <- 100

dat <- expand.grid(
    id = 1:k,
    trial = 1:nt
)

dat$x <- ifelse(dat$trial <= 50, 0, 1)
dat$xc <- ifelse(dat$x == 0, -0.5, 0.5)

dat <- arrange(dat, id)

# b0

b0 <- log(500)
b1 <- log(600/500)

dat$b0 <- b0
dat$b1 <- b1

b0i <- rnorm(k, 0, 0.1)
b1i <- rnorm(k, 0, 0.1)

dat$b0i <- b0i[dat$id]
dat$b1i <- b1i[dat$id]

dat$lp <- with(dat, b0 + b0i + (b1 + b1i)*xc)
mu <- exp(dat$lp)
shape <- 10
rate <- shape / mu
dat$rt <- rgamma(k * nt, shape = shape, rate = rate)

library(glmmTMB)

fit <- glmmTMB(rt ~ xc + (xc|id), data = dat, family = Gamma(link = "log"))
summary(fit)

dat |> 
    filter(id %in% sample(id, 30)) |> 
    select(lp, x, id) |> 
    mutate(rt = exp(lp)) |> 
    distinct() |> 
    ggplot(aes(x = x, y = rt)) +
    geom_line(aes(group = id))
