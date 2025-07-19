rm(list = ls())
df_go<- read.csv("C:/Users/39348/Desktop/DATI/go_unificato.csv", stringsAsFactors = FALSE)

table(df_go$id_subj, df_go$block_order)     


df_go_acc<-df_go[df_go$acc==1 & df_go$identity=="go",]
table(df_go_acc$id_subj, df_go_acc$block_order)  

#come si può notare cntrl13 ha sbagliato tutto
# quindi va eliminato e con lui l'accoppiamento con HM13
#eliminiamo CNTRL13 e HM13
df_go_acc <-df_go_acc[!df_go_acc$id_subj %in% c("CNTRL13"), ]
#controllo
unique(df_go_acc$id_subj)
table(df_go_acc$id_subj, df_go_acc$block_order)  


df_go_acc$ordine<-ifelse(df_go_acc$block_order=="self-first", "TU","SCONOSCIUTO")
#grafico
library(ggplot2)

ggplot(df_go_acc, aes(x = rt, fill = ordine, color = ordine)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("TU" = "cyan", "SCONOSCIUTO" = "salmon")) +
  scale_color_manual(values = c("TU" = "cyan4", "SCONOSCIUTO" = "red3")) +
  labs(
    x = "RT",
    y = "Densità",
    fill = NULL,
    color = NULL,
    title = "RT Go-TU vs Go-SCONOSCIUTO"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right"
  )


soggetti<-unique(df_go_acc$id_subj)
ordine<-unique(df_go_acc$block_order)

df_go_pulito<-data.frame()
numero_totale_out<-0
for(s in soggetti){
  for(i in ordine){
    df_tr<-df_go_acc[df_go_acc$id_subj==s & df_go_acc$block_order==i,]
    media<-mean(df_tr$rt,na.rm=T)
    ds<-sd(df_tr$rt,na.rm=T)
    lower<-media-3*ds
    high<-media+3*ds
    
    df_clean <- df_tr[df_tr$rt >= lower & df_tr$rt <= high, ]
    n_out<-nrow(df_tr) - nrow(df_clean)
    numero_totale_out <- numero_totale_out + n_out
    if (n_out > 0) {
      cat("Soggetto", s, "- Blocco", i, ": eliminati", n_out, "outlier\n")
     } 
    df_go_pulito <-  rbind(df_go_pulito,df_clean)
    }
  
}
cat("Totale outlier eliminati:", numero_totale_out, "\n")

df_go_pulito$block_order<-NULL


# #aggiungiamo match id:
# 
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
# df_go_pulito <- merge(df_go_pulito, matching_table, by = "id_subj", all.x = TRUE)
# table(df_go_pulito$id_subj, df_go_pulito$match_id)
# 
# 

df_go_pulito$ordine <- factor(df_go_pulito$ordine, levels = c("SCONOSCIUTO", "TU"))
df_go_pulito$group  <- factor(df_go_pulito$group,  levels = c("CTRL", "HSAM"))
 levels(df_go_pulito$ordine)
 levels(df_go_pulito$group)

library(glmmTMB)

fit_gamma <- glmmTMB(
  rt ~ ordine * group + (1 + ordine | id_subj),
  data = df_go_pulito,
  family = Gamma(link = "log")
)

summary(fit_gamma)
saveRDS(fit_gamma,file="fit_gamma_freq")
fit_gamma<- readRDS(fit_gamma,file="fit_gamma_freq")

library(brms)

form<-bf(rt~ordine*group + (1+ordine|id_subj),
         family = Gamma(link = "log"))


myPRIORS <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept"),
  
  set_prior( paste0("student_t(3, 0, .08)"), class = "b", 
             coef = "ordineTU"),
  set_prior( paste0("student_t(3, 0, .08)"), class = "b", 
             coef = "groupHSAM"),
  #set_prior( paste0("student_t(3, 0, .0055)"), class = "b", 
  set_prior( paste0("student_t(3, 0, .08)"), class = "b", 
             coef = "ordineTU:groupHSAM"),
  set_prior( paste0("student_t(3, 0, 1)"), class = "sd" ),
  set_prior(paste0("gamma(17, 34)"), class = "shape"))

fit_bayes<- brm(
  formula = form,
  data = df_go_pulito,
  prior = myPRIORS,
  chains = 4,
  cores = parallel::detectCores(),
  iter = 4000,
  warmup = 1000,
  seed = 1,
  control = list(adapt_delta = 0.99)
)

summary(fit_bayes, prob=.9)
saveRDS(fit_bayes,file="fit_go_bayes")
fit_bayes<-readRDS("fit_go_bayes")
fit_bayes<-readRDS(file="fit_go_bayes")
post_b3<-plot(fit_bayes,pars="ordineTU:groupHSAM")



posteriori_go <- as.data.frame(fit_bayes)

b3 <- posteriori_go$`b_ordineTU:groupHSAM`
  
area_b3_positive <- mean(b3 > 0)

#si vede dal modello che |match_id non è informativo
#Correlazione tra intercetta e slope = −0.25 → soggetti 
#con RT medi più alti tendono ad avere un effetto TU più piccolo.
#dispersione-->(sigma^2): 0.0291 





library(ggplot2)
library(dplyr)

# Supponendo che tu abbia già:
# fit_bayes <- brm(...)


# Calcolo delle densità
dens <- density(b3)
df_dens <- data.frame(x = dens$x, y = dens$y)

# Intervallo nullo (NI: ±0.02)
df_null <- df_dens %>% filter(x > -0.02 & x < 0.02)



library(ggplot2)
library(dplyr)

# Supponendo che tu abbia già:
# fit_bayes <- brm(...)
posterior_samples <- as.data.frame(fit_bayes)
b3 <- posterior_samples[,"b_ordineTU:groupHSAM"]

# Calcolo densità
dens <- density(b3)
df_dens <- data.frame(x = dens$x, y = dens$y)

# Intervallo nullo (NI: ±0.02)
df_null <- df_dens %>% filter(x > -0.02 & x < 0.02)

# Intervallo positivo (β₃ > 0)
df_pos <- df_dens %>% filter(x > 0)

# Probabilità già calcolate
area_NI <- mean(b3 > -0.020 & b3 < 0.020)
area_pos_b3 <- mean(b3 > 0)

# Plot aggiornato
ggplot(df_dens, aes(x = x, y = y)) +
  geom_area(data = df_pos, aes(x = x, y = y), fill = "lightblue", alpha = 0.5) +
  geom_area(data = df_null, aes(x = x, y = y), fill = "red", alpha = 0.6) +
  geom_line(color = "black", size = 0.6) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey30") +
  
  # Etichetta area nulla: spostata a sinistra (x = -0.03)
  annotate("label", x = -0.03, y = max(df_dens$y) + 0.8,
           label = paste0("P(-0.02 < β3 < 0.02) = ", round(area_NI, 3)),
           fill = "red", color = "white", size = 4, label.size = NA) +
  
  # Etichetta area positiva: spostata più a sinistra (x = 0.07)
  annotate("label", x = 0.07, y = max(df_dens$y) + 0.8,
           label = paste0("P(β3 > 0) = ", round(area_pos_b3, 3)),
           fill = "lightblue", color = "black", size = 4, label.size = NA) +
  
  labs(title = expression("Distribuzione a Posteriori di " * beta[3]),
       x = expression(beta[3] * " (interazione TU × Gruppo)"),
       y = "Densità") +
  theme_minimal(base_size = 14) +
  coord_cartesian(ylim = c(0, max(df_dens$y) + 1.5))


