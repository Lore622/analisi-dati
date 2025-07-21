rm(list = ls())

df_m <- read.csv("C:/Users/39348/Desktop/DATI/matching_task_unificato.csv", stringsAsFactors = FALSE)

aggregate(rt ~ id_subj, data = df_m, FUN = length) #toglie gli Na
table(df_m$id_subj)

df<-df_m[df_m$acc==1 & (df_m$identity=="matching SCONOSCIUTO" | df_m$identity=="matching TU"),]

#come check:
# table(df$acc)
# table(df$identity)
# table(df$identity, df$acc)  # tabella incrociata
# any(df$acc == 0)   # TRUE se c'è almeno un acc = 0
# all(df$acc == 1)     # TRUE se sono tutti 1
# which(df_m$acc == 0)  # quali righe hanno acc = 0

## grafico
library(ggplot2)

# Mappa per rendere i nomi più leggibili (facoltativo)
df$identity_label <- ifelse(df$identity == "matching TU", "TU", "SCONOSCIUTO")




# crea funzione che per ogni partecipante cacoli media ed sd rt corretti
# trial matching poi elimini e stampi numero di rt eliminati sopra o sotto 
# le 3 sd
# 
#mn<-aggregate(rt ~ id_subj+ identity, data = df, FUN = mean, na.rm=TRUE)
#st<-aggregate(rt ~ id_subj+ identity, data = df, FUN = function(x) sd(x, na.rm = TRUE))
# 
# bo<-rbind(mean,sd)

soggetti <- unique(df$id_subj)
condizioni <- unique(df$identity_label)

sub<-data.frame(
  id_subj = character(),
  identity_label = character(),
  mean_rt = numeric(),
  sd_rt = numeric(),
  stringsAsFactors = FALSE
)

pulizia <- function(df){
  df_cleaned <- data.frame() 
  sub <- data.frame()
  soggetti <- unique(df$id_subj)
  condizioni <- unique(df$identity_label)
  
  for (s in soggetti) {
    for (c in condizioni) {
      sottoinsieme <- df[df$id_subj == s & df$identity_label == c, ]
      
      if (nrow(sottoinsieme) == 0) next
      
      media_rt <- mean(sottoinsieme$rt, na.rm = TRUE)
      sd_rt <- sd(sottoinsieme$rt, na.rm = TRUE)
      
      lower <- media_rt - 3 * sd_rt
      upper <- media_rt + 3 * sd_rt
      
      sottoinsieme_pulito <- sottoinsieme[sottoinsieme$rt >= lower & sottoinsieme$rt <= upper, ]
      n_rimossi <- nrow(sottoinsieme) - nrow(sottoinsieme_pulito)
      
      if (n_rimossi > 0) {
        cat(" Rimossi", n_rimossi, "outlier per il soggetto", s, "nella condizione", c, "\n")
      } else {
        cat("Nessun outlier per", s, "-", c, "\n")
      }
      
      df_cleaned <- rbind(df_cleaned, sottoinsieme_pulito)
      
      sub <<- rbind(sub, data.frame(
        id_subj = s,
        identity_label = c,
        mean_rt = media_rt,
        sd_rt = sd_rt,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(df_cleaned)
}



df_pulito <- pulizia(df)


# Grafico con densità sovrapposte
ggplot(df_pulito, aes(x = rt, fill = identity_label, color = identity_label)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("TU" = "cyan", "SCONOSCIUTO" = "salmon")) +
  scale_color_manual(values = c("TU" = "cyan4", "SCONOSCIUTO" = "red3")) +
  labs(
    x = "RT",
    y = "Densità",
    fill = NULL,
    color = NULL,
    title = "RT Matching TU vs SCONOSCIUTO"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right"
  )

#length(df_pulito$rt[df_pulito$id_subj=="CNTRL01"])

#aggiungiamo match_id:


# soggetti <- sort(unique(df_pulito$id_subj)) 
# 
# estrai_numero <- function(id) {
#   as.numeric(gsub("\\D", "", id))
# }
# 
# matching_table <- data.frame(
#   id_subj = soggetti,
#   gruppo = ifelse(grepl("CNTRL", soggetti), "control", "target"),
#   numero = estrai_numero(soggetti)
# )
# 
# matching_table$match_id <- paste0("match", sprintf("%02d", matching_table$numero))
# matching_table <- matching_table[, c("id_subj", "match_id")]
# matching_table
# 
# 
# df_pulito <- merge(df_pulito, matching_table, by = "id_subj", all.x = TRUE)
# table(df_pulito$id_subj, df_pulito$match_id)



## controllo:
# df_pulito$group <- factor(df_pulito$group, levels = c("control", "target"))
# df_pulito$identity <- factor(df_pulito$identity, levels = c("SCONOSCIUTO", "TU"))
# df_pulito$match_id <- as.factor(df_pulito$match_id)
# df_pulito$id_subj <- as.factor(df_pulito$id_subj)   

df_pulito$identity_label <- factor(df_pulito$identity_label, levels = c("SCONOSCIUTO", "TU"))
df_pulito$group <- factor(df_pulito$group, levels = c("CTRL", "HSAM"))



#vediamo medie:
# Calcola media e sd
media <- tapply(df_pulito$rt, df_pulito$identity_label, mean)
dev_std <- tapply(df_pulito$rt, df_pulito$identity_label, sd)

# Arrotonda a una cifra decimale
media <- round(media, 3)
dev_std <- round(dev_std, 3)
 

#ora vediamo le medie sia per condizione che per gruppo
#quindi hsam tu vs sconosciuto:
#cntrl tu vs sconosciuto:

df_hsam<-subset(df_pulito,df_pulito$group=="HSAM",select=c(rt,id_subj,group,acc,identity_label))
media_hsam<-tapply(df_hsam$rt,df_hsam$identity_label,mean)
sd_hsam<-tapply(df_hsam$rt,df_hsam$identity_label,sd)

df_cntrl<-subset(df_pulito,df_pulito$group=="CTRL",select=c(rt,id_subj,group,acc,identity_label))
media_cntrl<-tapply(df_cntrl$rt,df_cntrl$identity_label,mean)
sd_cntrl<-tapply(df_cntrl$rt,df_cntrl$identity_label,sd)

# Filtra solo le prove TU
df_tu <- df_pulito[df_pulito$identity_label == "TU", ]

# Grafico: RT matching-TU confronto tra gruppi
ggplot(df_tu, aes(x = rt, fill = group, color = group)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("HSAM" = "steelblue", "CTRL" = "darkgrey")) +
  scale_color_manual(values = c("HSAM" = "steelblue4", "CTRL" = "darkgrey")) +
  labs(
    x = "RT",
    y = "Densità",
    fill = NULL,
    color = NULL,
    title = "RT Matching-TU: gruppo HSAM vs CNTRL"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")










library(brms)

formula_brm <- bf(
  rt ~ identity_label * group + (1 + identity_label | id_subj),
  family = Gamma(link = "log")
)

priori <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept"),
  set_prior("student_t(3, 0, 0.08)", class = "b", coef = "identity_labelTU"),
  set_prior("student_t(3, 0, 0.08)", class = "b", coef = "groupHSAM"),
  set_prior("student_t(3, 0, 0.08)", class = "b", coef = "identity_labelTU:groupHSAM"),
  set_prior("student_t(3, 0, 1)", class = "sd"),  # per tutti gli effetti casuali
  set_prior("gamma(17,17)", class = "shape")      # basata su pilot
)

library(brms)
fit_no_outlier<- brm(
  formula = formula_brm,
  data = df_pulito,
  prior = priori,
  chains = 4,
  cores = parallel::detectCores(),
  iter = 4000,
  warmup = 1000,
  seed = 1,
  control = list(adapt_delta = 0.99)
  
)

summary(fit_no_outlier)

pp_check(fit_no_outlier,ndraws = 100)

# stampa con CI al 90%
summary(fit_no_outlier, prob = 0.90)  

# LEVEL <- .9 # credibility level
# 
# PROBS <- c( (1-LEVEL)/2,1-(1-LEVEL)/2 )

post_b3<-plot(fit_no_outlier, pars = "identity_labelTU:groupHSAM")
#calcola prob a posteriori che b3 sia positivo:

# Estrai i campioni posteriori come data.frame
posteriori <- as.data.frame(fit_no_outlier)

# Estrai la variabile beta3 (b3 = interaction)
b3 <- posteriori$`b_identity_labelTU:groupHSAM`

# Calcola area sotto la curva per b3 > 0
area_b3_positive <- mean(b3 > 0)

# Mostra il risultato
cat("Area (probabilità) sotto la curva per β₃ > 0:", round(area_b3_positive, 3), "\n")

# Calcola la probabilità che b3 cada nell'intervallo [-0.14, -0.07]
area_target<-mean(b3 > -0.14 & b3 < -0.07)
#mean(b3<0) 95% prob che sia neg



# prob β3 cada nell'intervallo nullo [-0.013, 0.013]. +-10ms
area_b3_NI<-mean(b3 > -0.013 & b3 < 0.013)
area_b3_NI









###################### GRAFICO:

# Crea dataframe per ggplot
df_b3 <- data.frame(b3 = b3)
dens <- density(b3)

library(ggplot2)
library(dplyr)

# Calcolo densità
dens <- density(b3)
df_dens <- data.frame(x = dens$x, y = dens$y)

# Aree di interesse
df_target <- df_dens %>% filter(x > -0.14 & x < -0.07)
df_null <- df_dens %>% filter(x > -0.013 & x < 0.013)

# Plot
ggplot(df_dens, aes(x = x, y = y)) +
  geom_area(data = df_target, aes(x = x, y = y), fill = "lightgreen", alpha = 0.8) +
  geom_area(data = df_null, aes(x = x, y = y), fill = "red", alpha = 0.6) +
  geom_line(color = "black", size = 0.6) +
  
  # Etichette fuori dalla curva
  annotate("label", x = -0.18, y = max(df_dens$y) + 0.4,
           label = paste0("P(-0.14 < β₃ < -0.07) = ", round(area_target, 3)),
           fill = "lightgreen", color = "black", size = 4, label.size = NA) +
  
  annotate("label", x = 0.045, y = max(df_dens$y) + 0.4,
           label = paste0("P(-0.013 < β₃ < 0.013) = ", round(area_b3_NI, 3)),
           fill = "red", color = "white", size = 4, label.size = NA) +
  
  labs(title = expression("Posterior di " * beta[3] * " (interazione I × G)"),
       x = expression(beta[3]),
       y = "Densità") +
  theme_minimal(base_size = 14) +
  coord_cartesian(ylim = c(0, max(df_dens$y) + 1.5))


#frequentista:

#y=b0+b1I+b2G+b3IG

library(glmmTMB)

fit_gamma_no_outlier <- glmmTMB(
  rt ~ identity_label * group + (1 + identity_label | id_subj),
  data = df_pulito,
  family = Gamma(link = "log")
)

summary(fit_gamma_no_outlier)
#togliere match_id

 
saveRDS(fit_no_outlier, file = "fit_bayes_no_outlier.rds")
saveRDS(fit_gamma_no_outlier, file = "fit_glmmTMB_no_outlier.rds")
saveRDS(df_pulito, file = "dati_puliti_no_outlier.rds")

# per ricaricarli:
# df_pulito <- readRDS("dati_puliti.rds")
# fit <- readRDS("fit_bayes.rds")
# fit_gamma <- readRDS("fit_glmmTMB.rds")

fit_gamma <- readRDS("fit_glmmTMB_no_outlier.rds")
summary( fit_gamma)

fit_bayes_no_outlier <- readRDS("fit_bayes_no_outlier.rds")
summary(fit_bayes_no_outlier, prob=.9)

#proviamo a vedere se cambia qualcosa non escludendo RT ma non credo

fit_si_outlier<- brm(
  formula = formula_brm,
  data = df,
  prior = priori,
  chains = 4,
  cores = parallel::detectCores(),
  iter = 4000,
  warmup = 1000,
  seed = 1,
  control = list(adapt_delta = 0.99)
  
)

summary(fit_si_outlier,  prob = 0.90)
saveRDS(fit_si_outlier, file = "fit_bayes_si_outlier.rds")

# Estrazione posteriori
posteriori_so <- as.data.frame(fit_si_outlier)
b3_so <- posteriori_so$`b_identity_labelTU:groupHSAM`

# Calcolo probabilità
area_b3_positive_so <- mean(b3_so > 0)
area_target_so <- mean(b3_so > -0.14 & b3_so < -0.07)

# Output testuale
cat("Area (P(β₃ > 0)):", round(area_b3_positive_so, 3), "\n")
cat("Area (P(-0.14 < β₃ < -0.07)):", round(area_target_so, 3), "\n")

# Costruzione densità
df_b3_so <- data.frame(b3 = b3_so)
dens_so <- density(b3_so)

# Grafico con ggplot
post_b3_so <- ggplot(df_b3_so, aes(x = b3)) +
  # Curva principale
  geom_density(fill = "lightblue", color = "black", alpha = 0.3) +
  
  # Area target [-0.14, -0.07]
  stat_function(fun = function(x) {
    d <- density(b3_so)
    approx(d$x, d$y, xout = x)$y * as.numeric(x > -0.14 & x < -0.07)
  }, geom = "area", fill = "orange", alpha = 0.5) +
  
  # Area b3 > 0
  stat_function(fun = function(x) {
    d <- density(b3_so)
    approx(d$x, d$y, xout = x)$y * as.numeric(x > 0)
  }, geom = "area", fill = "red", alpha = 0.3) +
  
  # Annotazioni
  annotate("text", x = -0.27, y = 0.8 * max(dens_so$y),
           label = paste0("P(-0.14 < β3 < -0.07) = ", round(area_target_so, 3)),
           color = "darkorange", hjust = 0) +
  
  annotate("text", x = 0.01, y = 0.6 * max(dens_so$y),
           label = paste0("P(β3 > 0) = ", round(area_b3_positive_so, 3)),
           color = "red", hjust = 0) +
  
  # Titoli e tema
  labs(title = expression("Posterior di " * beta[3] * " (interazione TU × HSAM) — con outlier"),
       x = expression(beta[3]),
       y = "Densità") +
  theme_minimal(base_size = 13)

# Visualizza il grafico
print(post_b3_so)
