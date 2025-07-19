
rm(list=ls())
set.seed(1)

setwd("/mnt/data/users/massimiliano.pastore")
#setwd("~/MEGAsync/lavori/giovanni/")
datadir <- "data/"

# ++++++++++++++++++++ 
## simulation parameters
LEVEL <- c(.7,.8,.9) # credibility level
PROBS <- c( (1-LEVEL)/2,1-(1-LEVEL)/2 )
Nvec <- c(50,100,300) # sample size per group
B <- 300

BE_exp1_sc2 <- c(-.163, -.125, .000, -.069)
BE <- BE_exp1_sc2
DELTA <- c(0.20,0.20)
rDELTA <- .17
SHAPE <- 17
NTRIAL <- 50
SCENARIO <- 2
LETTER <- "A" # "B"

# UTILITY FUNCTION
#' @title dataSim
#' @description Simula RT in un disegno 2 (condizioni) X 2 (gruppi)
#' @param N = sample size per gruppo
#' @param Ntrial = numero di trial per condizione 
#' @param BE = vettore parametri fissi
#' @param delta = deviazioni standard intercette e slopes
#' @param rdelta = correlazione tra effetti random
#' @param shape = parametro di shape 
dataSim <- function(N = 10, Ntrial = 15, BE, delta = c(0,0), rdelta = 0, shape) {
  
  require(MASS)
  require(lavaan)
  simData <- NULL
  
  DESIGN <- expand.grid( 
    K = c("sconosciuto", "tu"), 
    G = c("control","target"))
  DESIGN$Y <- BE
  
  MM <- model.matrix( Y ~ K*G, data = DESIGN)
  SHAPE <- shape
  
  Rdelta <- matrix(c(1,rdelta,rdelta,1),2,2)
  Sdelta <- cor2cov(Rdelta,delta)
  
  s <- 0
  for (n in 1:N) {
    
    LAM <- mvrnorm(1,c(0,0),Sdelta)
    
    BEr <- BE
    BEr[1] <- BEr[1] + LAM[1]
    BEr[2] <- BEr[2] + LAM[2]
    MU <- MM %*% BEr
    
    LAMBDA <- SHAPE/exp(MU)
    y00 <- rgamma(Ntrial,SHAPE,LAMBDA[1])
    y10 <- rgamma(Ntrial,SHAPE,LAMBDA[2])  
    y01 <- rgamma(Ntrial,SHAPE,LAMBDA[3])
    y11 <- rgamma(Ntrial,SHAPE,LAMBDA[4]) 
    
    s <- s+1
    S0 <- data.frame(
      RT = c(y00,y10),
      I = rep(c("sconosciuto","tu"),each = Ntrial ),
      G = "control",
      ID = s,
      trial = 1:length(y00)
    )
    
    s <- s+1
    S1 <- data.frame(
      RT = c(y01,y11),
      I = rep(c("sconosciuto","tu"),each = Ntrial ),
      G = "target",
      ID = s,
      trial = 1:length(y00)
    )
    
    S <- rbind(S0,S1)
    simData <- rbind( simData, S )
    
  }
  
  simData[,-1] <- lapply( simData[,-1], FUN = factor )
  return(simData)
  
}

# +++++++++++++++++++++++++++
# SIMULATION
library(brms)

myPRIORS <- c(
  set_prior( paste0("student_t(3, 0, 1)"), class = "Intercept"), 
  set_prior( paste0("student_t(3, 0, .08)"), class = "b", 
             coef = "Itu"),
  set_prior( paste0("student_t(3, 0, .08)"), class = "b", 
             coef = "Gtarget"),
  #set_prior( paste0("student_t(3, 0, .0055)"), class = "b", 
  set_prior( paste0("student_t(3, 0, .08)"), class = "b", 
             coef = "Itu:Gtarget"),
  set_prior( "student_t(3, 0, 1)", class = "sd" ),
  set_prior( paste0("gamma(",SHAPE,", ",SHAPE, ")"), class = "shape")
)


for (n in Nvec) {
  
  EXP1 <- OBS <- NULL
  for (b in 1:B) {
    cat(paste0("n = ",n," (",b,"/",B,")"),"\n")
    
    Z <- dataSim( n, NTRIAL, BE = BE, 
        delta = DELTA, rdelta = rDELTA, shape = SHAPE )  
    
    obsMX <- aggregate(RT ~ I:G, data = Z, FUN = mean)
    obsMX$n <- n
    obsMX$iter <- b
    obsMX$scenario <- SCENARIO
    OBS <- rbind(OBS, obsMX)
    
    if (b == 1) {
      base_fit <- brm( RT ~ I*G + (I|ID), data = Z, 
                       prior = myPRIORS,
                       cores = parallel::detectCores(), 
                       family = "gamma", refresh = 0 )
    } 
    
    fit <- update( base_fit, newdata = Z, 
                   recompile = FALSE, refresh = 0 )
    
    if ( sum( rhat( fit ) > 1.05 ) > 0 ) {
      converged <- FALSE
    } else {
      converged <- TRUE
    }  
    
    POST <- as_draws_df(fit, 
          variable = c("shape","sd_ID__Intercept","sd_ID__Itu","cor_ID__Intercept__Itu"))
    QQ <- apply(POST[,1:4], 2, quantile, prob = PROBS)
    MX <- apply(POST[,1:4],2,mean)
    SX <- apply(POST[,1:4],2,sd)
    
    POST <- cbind(MX, SX, t(QQ))
    
    est <- data.frame( fixef( fit, probs = PROBS ) )
    colnames(POST) <- colnames(est)
    est <- rbind( est, POST)
    
    est$converged <- converged
    est$par <- rownames(est)
    est$n <- n  
    est$iter <- b
    est$scenario <- SCENARIO
    
    EXP1 <- rbind(EXP1,est)
    save( EXP1, OBS, myPRIORS, BE, SHAPE, DELTA, NTRIAL,
          file = paste0(datadir, "R03_exp1_sc",SCENARIO,"_n",n,LETTER,".rda"))
  }  
  
}


