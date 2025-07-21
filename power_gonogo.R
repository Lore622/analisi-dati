# power go no go

# y=B0+B1I+B2G+B3IG

rm(list=ls())


N=10
n_trial=100
b0 <- -0.7   #500 ms alla baseline--> CNTRL - SCONOSCIUTO
b1 <- 0.183   #exp=1.2 significa che effetto I quando =TU aumenta RT 20% rispettp sconosciuto
b2 <- 0       #effetto nullo
b3 <- 0.11
#b3->Nei soggetti HSAM, l’effetto TU (vs SCONOSCIUTO) è piùforte 11% rispetto ai controlli.

df <- expand.grid(trial = 1:n_trial, id = 1:N)
df <- df[, c("id", "trial")]

#inseriamo le condizioni
df$G<-ifelse(df$id <= N/2,0,1)
df$I<-ifelse(df$trial<51,0,1)


#parametri:
df$b0<-rep(b0,nrow(df))
df$b1<-rep(b1,nrow(df))
df$b2<-rep(b2,nrow(df))
df$b3<-rep(b3,nrow(df))

#inseriamo intercetta e slope random correlati .25 tra lloro
# b0i<-rnorm(N,0,0.1)
# b1i<-rnorm(N,0,0.1)

library(MASS)
# matrice di covarianza con correlazione 0.25
Sigma <- matrix(c(0.01, 0.0025,
                  0.0025, 0.01), nrow = 2)
rand_eff <- mvrnorm(n = N, mu = c(0, 0), Sigma = Sigma)
b0i <- rand_eff[, 1]
b1i <- rand_eff[, 2]

df$b0i <- b0i[df$id]
df$b1i <- b1i[df$id]

#shape=17 shape=mu/rate
#rate*shape=mu
#rate=mu/shape
#calcoliamo mu e poi rate
shape=17
log_mu <- with(df, b0 + b1*I + b2*G + b3*I*G +b0i + b1i * I)
mu <- exp(log_mu)
rate<-shape/mu


df$y  <- rgamma(nrow(df), shape = shape, scale = mu / shape)


hist(df$y)
summary(df$y)

#testiamo:
library(glmmTMB)

# modello con distribuzione Gamma e link log
fit_power <- glmmTMB(
  y ~ I* G+ (1 + I | id),
  data = df,
  family = Gamma(link = "log")
)
summary(fit_power)

saveRDS(fit_power,file="power fit")
####fitta bene i parametri
#in intercetta:Prior molto larga (= weakly informative):prima dei dati
library(brms)

#priorB3<-a prior ammette che l’effetto differenziale possa andare da circa –30 % a +40 %, con maggior densità vicino a 0. 
#Contiene comodamente il valore vero simulato (+11 %).
  myPRIORS <- c(
    set_prior("student_t(3, 0, 1)", class = "Intercept"),
    
  set_prior( paste0("student_t(3, 0, .08)"), class = "b", 
             coef = "I"),
  set_prior( paste0("student_t(3, 0, .08)"), class = "b", 
             coef = "G"),
  #set_prior( paste0("student_t(3, 0, .0055)"), class = "b", 
  set_prior( paste0("student_t(3, 0, .08)"), class = "b", 
             coef = "I:G"),
  set_prior( paste0("student_t(3, 0, 1)"), class = "sd" ),
  set_prior(paste0("gamma(", shape, ", 17)"), class = "shape"))
  


#prior precedenti:
  # myPRIORS <- c(
  #   set_prior("student_t(3, -0.51, 0.3)", class = "Intercept"),
  #   
  #   set_prior( paste0("student_t(3, 0, .08)"), class = "b", 
  #              coef = "I"),
  #   set_prior( paste0("student_t(3, 0, .08)"), class = "b", 
  #              coef = "G"),
  #   #set_prior( paste0("student_t(3, 0, .0055)"), class = "b", 
  #   set_prior( paste0("student_t(3, 0, .08)"), class = "b", 
  #              coef = "I:G"),
  #   set_prior( "student_t(3, 0, 1)", class = "sd" ),
  #   set_prior("gamma(17, 17)", class = "shape")
  # )
  

  
#vediamo bene prior b3

deg  <- 3        
mu  <- 0        
sigma <- 0.08   
rope   <- 0.05       # metà-ampiezza (±0.05 log-unit ≈ ±5 %)

p_in_rope <- pt(( rope - mu)/sigma, deg) -
  pt((-rope - mu)/sigma, deg) #42%

## --- probabilità nella direzione attesa -------
##     (β3 > rope  →  positivo e fuori dalla ROPE)
p_pos_out <- 1 - pt(( rope - mu)/sigma, deg) #0.2881411

library(ggplot2)

theta <- seq(-0.4, 0.4, length.out = 1000)
dens  <- dt((theta - mu)/sigma, deg) / sigma

ggplot(data.frame(theta, dens), aes(theta, dens)) +
  geom_line(colour = "steelblue", linewidth = 1) +
  geom_vline(xintercept = c(-rope, rope), linetype = "dashed",
             colour = "grey50") +
  labs(title = expression(paste("Prior Student-t(3, 0, 0.08) per ", beta[3])),
       subtitle = sprintf("ROPE = ±%.2f  (≈ ±%.0f %% in RT)",
                          rope, (exp(rope)-1)*100),
       x = expression(beta[3]~~"(log-units)"),
       y = "Densità") +
  theme_minimal(base_size = 13)


#facciamo prior predictive check:
library(brms)


form <- bf(
  y ~ I * G + (1 + I | id),
  family = Gamma(link = "log")
)


fit_prior <- brm(
  formula       = form,
  data          = df,
  prior         = myPRIORS,
  sample_prior  = "only",   # <- nessun dato usato per aggiornare
  chains        = 4,
  iter          = 2000,
  cores         = 4,
  seed          = 123
)


summary(fit_prior)
saveRDS(fit_prior,file="fit_prior.rds")
fit_prior<-readRDS("fit_prior.rds")
#ppcheck sul fit_prior
#Se credessi davvero nelle prior che ho scelto, 
#che tipo di dati simulerei dal mio modello? Sono plausibili?
#dà info anche sulla variabilità delle prior 

pp_check(fit_prior, ndraws = 1000) + coord_cartesian(xlim = c(0, 1))

#se l’intervallo credibile al 90% non include lo zero o è 
#interamente fuori dalla ROPE
#per il numero totale di iterazioni

fit1<- brm(
  formula = form,
  data = df,
  prior = myPRIORS,
  chains = 4,
  cores = parallel::detectCores(),
  iter = 4000,
  warmup = 1000,
  seed = 1,
  control = list(adapt_delta = 0.99)
  )

summary(fit1)
####################
# Simulazione
library(brms)
library(posterior)
library(MASS)

# calcolo Power= n volte in cui CI non contiene zero per n_tot_sim
# Parametri principali
  BE <- c(-0.7, 0.183, 0, 0.11)
  
  LEVEL <- .90
  
  PROBS <- c((1 - LEVEL) / 2, 1 - (1 - LEVEL) / 2)
  
  #sample_size <- c(20, 50, 70, 100, 120, 150)
  sample_size <- c(20, 50, 70, 100, 120)
  n_sim <- 10
  n_trial <- 30
  shape <- 17
  Sigma <- matrix(c(0.01, 0.0025, 0.0025, 0.01), 2, 2)
  SCENARIO <- "go_no_go"
  datadir <- "./"  # o dove vuoi salvare i file
  LETTER <- "A"
  
  form <- bf(y ~ I * G + (1 + I | id), family = Gamma(link = "log"))
  EXP2_all <- NULL  
  
  # Funzione per simulare i dati
  for (n in sample_size) {
    EXP2 <- OBS <- NULL
    for (b in 1:n_sim) {
      cat(paste0("n = ", n, " (", b, "/", n_sim, ")\n"))
      
      df <- expand.grid(trial = 1:n_trial, id = 1:n)
      df$G <- ifelse(df$id <= n / 2, 0, 1)
      df$I <- ifelse(df$trial <= (n_trial / 2), 0, 1)
      
      rand_eff <- mvrnorm(n = n, mu = c(0, 0), Sigma = Sigma)
      b0i <- rand_eff[, 1]
      b1i <- rand_eff[, 2]
      
      df$b0i <- b0i[df$id]
      df$b1i <- b1i[df$id]
      
      log_mu <- with(df, BE[1] + BE[2]*I + BE[3]*G + BE[4]*I*G + b0i + b1i * I)
      mu <- exp(log_mu)
      df$y <- rgamma(nrow(df), shape = shape, scale = mu / shape)
      # -------- fit/update ------------
      if (b == 1) {
        base_fit <- brm(form, data = df, prior = myPRIORS,
                        chains = 4, iter = 1500, warmup = 500,
                        backend = "cmdstanr", refresh = 0)
      }
      
      try_fit <- tryCatch(
        update(base_fit, newdata = df, recompile = FALSE, refresh = 0),
        error = function(e) e
      )
      if (inherits(try_fit, "error")) next
      fit <- try_fit
      
      converged <- all(rhat(as_draws_array(fit)) <= 1.05)
      
      
      est <- data.frame(fixef(fit, probs = PROBS))
      
      est$detected <- FALSE  # inizializza tutta la colonna
      ci_b3 <- est["I:G", c("Q5", "Q95")]  #CI_low, CI_high
      est["I:G", "detected"] <- (ci_b3[1] > 0 | ci_b3[2] < 0) #CI_low>0  CI_high<0
      est$converged <- converged
      est$par <- rownames(est)
      est$n <- n
      est$iter <- b
      est$scenario <- SCENARIO
      
      EXP2 <- rbind(EXP2, est)
   
      
    }
    
    save(EXP2, file = paste0(datadir, "power_sim_", SCENARIO, "_n", n, LETTER, ".rda"))
    EXP2_all <- rbind(EXP2_all, EXP2)  
  }
  
  
  EXP2_b3 <- subset(EXP2_all, par == "I:G"& converged == TRUE)
  
  
  power_by_n <- aggregate(detected ~ n, data = EXP2_b3, FUN = mean)
  
  # dopo il subset EXP2_b3
  if (any(is.nan(power_by_n$detected))) {
    warning("🤔 Potenza non calcolata per qualche n")
  }
  save(EXP2_all, file = paste0(datadir, "power_sim_", SCENARIO, "_ALL", LETTER, ".rda"))
  
  print(power_by_n)
  # Visualizza potenza:


pow<-ggplot(power_by_n, aes(x = n, y = detected)) +
  geom_line() +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Numero di soggetti", y = "Potenza (CI 90% ≠ 0)",
       title = "Potenza Bayesiana per b₃ (interazione TU × Gruppo)") +
  theme_minimal(base_size = 14)

####### visualizziamo
theta<-seq(0,1.1, by=.1)
pr_g<-plot(dgamma(theta, shape = 17, rate = 34),lwd=2,type="h")
point(dgamma(x, shape = 100, rate = 200),lwd=2,type="h")
