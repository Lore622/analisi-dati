rm(list=ls())

# Dataframe finale vuoto
df_go <- data.frame(
  id_subj = character(),
  group = character(),
  n_trial = numeric(),
  rt = numeric(), 
  acc = numeric(),
  identity = character(),
  pressed_key = character(),
  link = character(),
  block_order = character(),
  stringsAsFactors = FALSE
)

# Directory dei file
dir_path <- "C://Users//39348//Desktop//DATI"
files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)

# Escludi il file unificato dal ciclo
files <- files[!basename(files) %in% c("matching_task_unificato.csv")]

# Ciclo sui file
for (file in files) {
  cat("\n📄 Sto elaborando:", basename(file), "...\n")
  
  tryCatch({
    df <- read.csv(file, stringsAsFactors = FALSE, sep = ",")
    names(df) <- tolower(trimws(gsub(" ", "_", names(df))))
    
    if (!("exp_or_prac" %in% names(df))) {
      warning("⚠️ Colonna 'exp_or_prac' mancante. File saltato.")
      next
    }
    
    df_exp <- df[df$exp_or_prac == "exp" & df$phase == "go-no-go", ]
    
    required_cols <- c("keygngresp.rt", "trial_acc", "trial_type", "keygngresp.keys", "expname")
    if (!all(required_cols %in% names(df_exp))) {
      warning("⚠️ Una o più colonne richieste mancano in", basename(file), ". File saltato.")
      next
    }
    
    n_trials <- nrow(df_exp)
    subj_id <- tools::file_path_sans_ext(basename(file))
    exp_version <- tolower(df_exp$expname[1])
    link <- exp_version  # ✅ Corretto uso del nome della variabile
    
    block_order <- if (exp_version %in% c("hsam_q1", "hsam_t1")) {
      rep(c("self-first", "other-first"), each = 100)
    } else if (exp_version %in% c("hsam_q2", "hsam_t2")) {
      rep(c("other-first", "self-first"), each = 100)
    } else {
      rep(NA, n_trials)
    }
    
    df_s <- data.frame(
      id_subj = subj_id,
      group = ifelse(grepl("HM", subj_id), "HSAM", "CTRL"),
      n_trial = 1:n_trials,
      rt = df_exp$keygngresp.rt,
      acc = df_exp$trial_acc,
      identity = df_exp$trial_type,
      pressed_key = df_exp$keygngresp.keys,
      link = link,
      block_order = block_order,
      stringsAsFactors = FALSE
    )
    
    df_go <- rbind(df_go, df_s)
    cat("✅ File aggiunto:", subj_id, "→", exp_version, "\n")
    
  }, error = function(e) {
    cat("❌ Errore con il file", basename(file), ":", e$message, "\n")
  })
}

#Anteprima
View(df_go)


# 📁 Salva
write.csv(df_go, file = file.path(dir_path, "go_unificato.csv"), row.names = FALSE)

