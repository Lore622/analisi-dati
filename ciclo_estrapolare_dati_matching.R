rm(list=ls())

# Crea il dataframe finale vuoto
df_mt <- data.frame(
  id_subj = character(),
  group = character(),
  n_trial = numeric(),
  rt = numeric(), 
  acc = numeric(),
  identity = character(),
  stringsAsFactors = FALSE
)

# Directory dove sono i file
dir_path <- "C://Users//39348//Desktop//DATI"

# Prende solo i .csv
files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)

# Ciclo su ogni file
for (file in files) {
  cat("\n📄 Sto elaborando:", basename(file), "...\n")
  
  # Prova a leggere il file
  tryCatch({
    df <- read.csv(file, stringsAsFactors = FALSE, sep = ",")
    names(df) <- tolower(trimws(gsub(" ", "_", names(df))))
    
    if (!("exp_or_prac" %in% names(df))) {
      warning("⚠️ Colonna 'exp_or_prac' mancante. File saltato.")
      next
    }
    
    df_exp <- df[df$exp_or_prac == "exp" & df$phase=="matching_task", ]
    
    # Verifica colonne necessarie
    if (!all(c("keytrialresp.rt", "trial_acc", "trial_type", "trial_lettera") %in% names(df_exp))) {
      warning("⚠️ Una o più colonne necessarie mancanti in", basename(file), ". File saltato.")
      next
    }
    
    # Crea dataframe soggetto
    n_trials <- nrow(df_exp)
    subj_id <- tools::file_path_sans_ext(basename(file))
    
    df_s <- data.frame(
      id_subj = subj_id,
      group = ifelse(grepl("HM", subj_id), "HSAM", "CTRL"),
      n_trial = 1:n_trials,
      rt = df_exp$keytrialresp.rt,
      acc = df_exp$trial_acc,
      identity = paste(df_exp$trial_type, df_exp$trial_lettera, sep = " "),
      stringsAsFactors = FALSE
    )
    
    # Aggiungi
    df_mt <- rbind(df_mt, df_s)
    cat("✅ File aggiunto: ", subj_id, "\n")
    
  }, error = function(e) {
    cat("❌ Errore con il file", basename(file), ":", e$message, "\n")
  })
}

# 👀 Anteprima
head(df_mt)
tail(df_mt)

# 📁 Salva se vuoi
write.csv(df_mt, file = file.path(dir_path, "matching_task_unificato.csv"), row.names = FALSE)
