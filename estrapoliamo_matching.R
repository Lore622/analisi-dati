rm(list=ls())

df_mt <- data.frame(
  id_subj = character(),
  group = character(),
  n_trial= numeric(),
  rt = numeric(), 
  acc = numeric(),
  identity = character(),
  stringsAsFactors = FALSE
)




#carichiamo HM01
HM01 <- read.csv("C://Users//39348//Desktop//ANALISI_1_STUDIO//analisi_HSAM//HM01.csv",
                 stringsAsFactors = FALSE,
                 sep = ";",        # ← Prova prima con ; poi con , se non funziona
                 header = TRUE)


# Normalizza nomi colonna
names(HM01) <- tolower(trimws(gsub(" ", "_", names(HM01))))

# Filtro solo righe sperimentali
df_exp <- HM01[HM01$exp_or_prac == "exp", ]


# Crea sotto-dataframe per HM01 con solo i primi 200 trial
df_exp <- df_exp[1:200, ]  # Limita subito a 200 trial

df_HM01 <- data.frame(
  id_subj = "HM01",
  group = "HSAM",
  n_trial = seq(1, 200),
  rt = df_exp$keytrialresp.rt,
  acc = df_exp$trial_acc,
  identity = paste(df_exp$trial_type, df_exp$trial_lettera, sep = " "),
  stringsAsFactors = FALSE
)

# Aggiungilo al dataframe finale
df_mt <- rbind(df_mt, df_HM01)
View(df_mt)
