###############################################################################
# PROIECT SERII DE TIMP – COMPONENTA MULTIVARIATĂ (COD COMPLET)
# Metodologie: VAR / VECM (Johansen)
#
# Variabile (mai 2007 – feb 2026, 226 obs. lunare):
#   HICP_RENT   – Indice chirii efective România (Eurostat, 2015=100)
#   SR          – Salariu real (WAGE_NET / HICP_core × 100)
#   IR_APRC     – Rata APRC credite ipotecare noi RON (BNR/BCE)
#   FX_NOMINAL  – Curs nominal EUR/RON (câți lei face 1 euro, BNR)
#
# MOTIVARE CURS NOMINAL vs. REAL:
#   Chiriile din România sunt negociate în EUR și plătite în RON.
#   Cursul NOMINAL EUR/RON captează direct mecanismul de indexare:
#   EUR/RON crește → chiriile în RON cresc automat.
#   Coeficient așteptat POZITIV în vectorul de cointegrare.
###############################################################################

# ─── 0. PACHETE ───────────────────────────────────────────────────────────────
packages_needed <- c(
  "readxl", "zoo", "ggplot2", "dplyr", "tidyr",
  "tseries", "urca", "vars", "lmtest",
  "forecast", "scales", "patchwork", "FinTS", "gridExtra", "grid"
)
new_pkg <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if (length(new_pkg) > 0) install.packages(new_pkg)
invisible(lapply(packages_needed, library, character.only = TRUE))


###############################################################################
# 1. IMPORT DATE ȘI CONSTRUIREA SERIILOR
###############################################################################

df_raw <- read_excel("date_multivariat.xlsx")
colnames(df_raw) <- c("Date", "HICP_RENT", "SR", "IR_APRC", "RCR", "FX_NOMINAL")

cat("=== VERIFICARE DATE ===\n")
cat("Dimensiune:", nrow(df_raw), "×", ncol(df_raw), "\n")
cat("NaN per coloană:\n"); print(colSums(is.na(df_raw)))
cat("\nSummary:\n"); print(summary(df_raw))

df <- df_raw
cat("\nPerioadă:", as.character(df$Date[1]), "–", as.character(df$Date[nrow(df)]), "\n")
cat("Observații:", nrow(df), "\n")

# ── Construim seriile ts ──────────────────────────────────────────────────────
# log(HICP_RENT):  variabila dependentă
# log(SR):         salariu real deflat cu HICP core (exclus chirii) → I(1)
# IR_APRC:         rată % — NU logaritmăm (3–15%) → verificăm I(0) sau I(1)
# log(FX_NOMINAL): curs nominal EUR/RON — câți lei face 1 euro
#                  Coeficient așteptat POZITIV: EUR/RON crește → chirii RON cresc
#                  Înlocuiește cursul real (RCR) care dădea semn negativ
#                  din cauza efectelor de putere de cumpărare

log_rent <- ts(log(df$HICP_RENT),  start = c(2007, 5), frequency = 12)
log_sr   <- ts(log(df$SR),         start = c(2007, 5), frequency = 12)
ir_aprc  <- ts(df$IR_APRC,         start = c(2007, 5), frequency = 12)
log_fx   <- ts(log(df$FX_NOMINAL), start = c(2007, 5), frequency = 12)

cat("\n=== SERII CONSTRUITE ===\n")
cat("log_rent: min=", round(min(log_rent),3), "max=", round(max(log_rent),3), "\n")
cat("log_sr:   min=", round(min(log_sr),3),   "max=", round(max(log_sr),3),   "\n")
cat("ir_aprc:  min=", round(min(ir_aprc),3),  "max=", round(max(ir_aprc),3),  "\n")
cat("log_fx:   min=", round(min(log_fx),3),   "max=", round(max(log_fx),3),   "\n")

cat("\nPrimele 3 valori FX_NOMINAL:", round(head(df$FX_NOMINAL, 3), 4), "\n")
cat("Ultimele 3 valori FX_NOMINAL:", round(tail(df$FX_NOMINAL, 3), 4), "\n")
# Verificare: ~3.2 (2007) → ~5.1 (2026) — depreciere nominală lentă a leului


###############################################################################
# 2. ANALIZĂ EXPLORATORIE
###############################################################################

# ── 2.1 Grafice serii originale ───────────────────────────────────────────────
df_long <- df %>%
  pivot_longer(cols = c(HICP_RENT, SR, IR_APRC, FX_NOMINAL),
               names_to = "variabila", values_to = "valoare")

print(
  ggplot(df_long, aes(x = as.Date(as.yearmon(Date, "%Y-%m")),
                      y = valoare, color = variabila)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~variabila, ncol = 2, scales = "free_y",
               labeller = labeller(variabila = c(
                 HICP_RENT  = "HICP Chirii (2015=100)",
                 SR         = "Salariu real (RON)",
                 IR_APRC    = "Rata dobânzii la credite ipotecare noi (%)",
                 FX_NOMINAL = "Curs nominal EUR/RON"
               ))) +
    labs(title    = "Evoluția variabilelor – mai 2007 – feb 2026",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold"))
)

# ── 2.2 Grafice serii în forma de intrare în model ────────────────────────────
df_model_plot <- data.frame(
  timp     = as.yearmon(time(log_rent)),
  log_RENT = as.numeric(log_rent),
  log_SR   = as.numeric(log_sr),
  IR_APRC  = as.numeric(ir_aprc),
  log_FX   = as.numeric(log_fx)
)

df_log_long <- df_model_plot %>%
  pivot_longer(-timp, names_to = "variabila", values_to = "valoare")

print(
  ggplot(df_log_long, aes(x = as.Date(timp), y = valoare)) +
    geom_line(color = "#005088", linewidth = 0.8) +
    geom_hline(data = df_log_long %>% group_by(variabila) %>%
                 summarise(m = mean(valoare, na.rm = TRUE), .groups = "drop"),
               aes(yintercept = m), linetype = "dashed", color = "red") +
    facet_wrap(~variabila, ncol = 2, scales = "free_y") +
    labs(title = "Seriile în forma de intrare în model (log sau nivel)",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(strip.text = element_text(face = "bold"))
)

# ── 2.3 Grafice sezoniere ─────────────────────────────────────────────────────
print(ggseasonplot(log_rent, year.labels=FALSE) +
        labs(title="Grafic sezonier – log(HICP_RENT)") + theme_bw())
print(ggseasonplot(log_sr,   year.labels=FALSE) +
        labs(title="Grafic sezonier – log(Salariu real)") + theme_bw())
print(ggseasonplot(ir_aprc,  year.labels=FALSE) +
        labs(title="Grafic sezonier – IR_APRC") + theme_bw())
print(ggseasonplot(log_fx,   year.labels=FALSE) +
        labs(title="Grafic sezonier – log(Curs nominal EUR/RON)") + theme_bw())

print(ggsubseriesplot(log_rent) +
        labs(title="Subserii sezoniere – log(HICP_RENT)") + theme_bw())
print(ggsubseriesplot(log_fx)   +
        labs(title="Subserii sezoniere – log(Curs nominal EUR/RON)") + theme_bw())

cat("\n=== NSDIFFS (sezonalitate stochastică) ===\n")
cat("log_rent:", nsdiffs(log_rent), "\n")
cat("log_sr:  ", nsdiffs(log_sr),   "\n")
cat("ir_aprc: ", nsdiffs(ir_aprc),  "\n")
cat("log_fx:  ", nsdiffs(log_fx),   "\n")

# ── 2.4 Corelograme individuale ───────────────────────────────────────────────
ggtsdisplay(log_rent, lag.max=36, main="log(HICP_RENT) – Autocorelare")
ggtsdisplay(log_sr,   lag.max=36, main="log(Salariu real) – Autocorelare")
ggtsdisplay(ir_aprc,  lag.max=36, main="IR_APRC – Autocorelare")
ggtsdisplay(log_fx,   lag.max=36, main="log(Curs nominal EUR/RON) – Autocorelare")

# ── 2.5 Matrice corelații și scatterplots ─────────────────────────────────────
df_calcul <- data.frame(
  log_RENT = as.numeric(log_rent),
  log_SR   = as.numeric(log_sr),
  IR_APRC  = as.numeric(ir_aprc),
  log_FX   = as.numeric(log_fx)
)

pairs(df_calcul, main="Scatterplots – variabile în forma din model", col="#005088")

cat("\n=== MATRICE CORELAȚII ===\n")
print(round(cor(df_calcul, use="complete.obs"), 4))

# ── 2.6 CCF cu variabila dependentă ──────────────────────────────────────────
plot_ccf_luni <- function(x, y, xname, yname) {
  ccf_res <- ccf(x, y, plot=FALSE, lag.max=24)
  df_ccf  <- data.frame(
    lag = as.numeric(ccf_res$lag) * 12,
    ccf = as.numeric(ccf_res$acf)
  )
  ci <- 2 / sqrt(length(x))
  ggplot(df_ccf, aes(x=lag, y=ccf)) +
    geom_bar(stat="identity", fill="steelblue", width=0.6) +
    geom_hline(yintercept=c(ci,-ci), linetype="dashed",
               color="red", linewidth=0.6) +
    geom_hline(yintercept=0) +
    scale_x_continuous(breaks=seq(-24,24,by=6)) +
    labs(title=paste0(xname, " vs. ", yname),
         x="Lag (luni)", y="Corelație") +
    theme_minimal(base_size=10) +
    theme(plot.title=element_text(face="bold", size=11),
          panel.grid.minor=element_blank())
}

g_ccf1 <- plot_ccf_luni(ir_aprc, log_rent, "IR_APRC",          "log(RENT)")
g_ccf2 <- plot_ccf_luni(log_sr,  log_rent, "log(SR)",           "log(RENT)")
g_ccf3 <- plot_ccf_luni(log_fx,  log_rent, "log(Curs nominal)", "log(RENT)")

grid.arrange(g_ccf1, g_ccf2, g_ccf3, ncol=3,
             top=textGrob("CCF cu Variabila Dependentă log(RENT)",
                          gp=gpar(fontsize=13, font=2, col="#005088")))

# ── 2.7 Statistici descriptive ────────────────────────────────────────────────
calcul_stats <- function(x) {
  c(Media=mean(x), Mediana=median(x), Min=min(x), Max=max(x),
    SD=sd(x), CV_proc=(sd(x)/abs(mean(x)))*100)
}

tabel_desc <- data.frame(
  Variabila = c("RENT (original)", "log_RENT",
                "SR (original)",   "log_SR",
                "IR_APRC",
                "FX_NOMINAL",      "log_FX"),
  rbind(
    round(calcul_stats(exp(as.numeric(log_rent))), 4),
    round(calcul_stats(as.numeric(log_rent)),      4),
    round(calcul_stats(exp(as.numeric(log_sr))),   4),
    round(calcul_stats(as.numeric(log_sr)),        4),
    round(calcul_stats(as.numeric(ir_aprc)),       4),
    round(calcul_stats(df$FX_NOMINAL),             4),
    round(calcul_stats(as.numeric(log_fx)),        4)
  )
)

cat("\n=== STATISTICI DESCRIPTIVE ===\n")
print(tabel_desc, row.names=FALSE)


###############################################################################
# 3. TESTAREA STACIONARITĂȚII
###############################################################################

run_stat <- function(serie, nume, diferentiere=FALSE) {
  if (diferentiere) { serie <- diff(serie); prefix <- paste0("Δ", nume) }
  else              { prefix <- paste0(nume, " în nivel") }
  cat("\n", paste(rep("═",55),collapse=""), "\n")
  cat(" STACIONARITATE:", prefix, "\n")
  cat(paste(rep("═",55),collapse=""), "\n")
  cat("  ADF (none)  → "); print(summary(ur.df(serie, type="none",  selectlags="AIC")))
  cat("  ADF (drift) → "); print(summary(ur.df(serie, type="drift", selectlags="AIC")))
  cat("  ADF (trend) → "); print(summary(ur.df(serie, type="trend", selectlags="AIC")))
  cat("  KPSS        → "); print(summary(ur.kpss(serie, type="mu",  lags="long")))
  cat("  PP          → "); print(pp.test(serie, lshort=FALSE))
}

run_stat(log_rent, "log(RENT)")
run_stat(log_rent, "log(RENT)", diferentiere=TRUE)

run_stat(log_sr, "log(SR)")
run_stat(log_sr, "log(SR)", diferentiere=TRUE)

run_stat(ir_aprc, "IR_APRC")
run_stat(ir_aprc, "IR_APRC", diferentiere=TRUE)
# ► CHEIE: dacă ADF respinge H0 în nivel → IR_APRC este I(0)!

run_stat(log_fx, "log(FX_NOMINAL)")
run_stat(log_fx, "log(FX_NOMINAL)", diferentiere=TRUE)
# Așteptat: I(1) — cursul nominal are trend ascendent (depreciere lentă a leului)

# Zivot-Andrews
cat("\n=== ZIVOT-ANDREWS – IR_APRC ===\n")
za_ir   <- ur.za(ir_aprc, model="both", lag=1)
summary(za_ir); plot(za_ir)

cat("\n=== ZIVOT-ANDREWS – log(RENT) ===\n")
za_rent <- ur.za(log_rent, model="both", lag=1)
summary(za_rent); plot(za_rent)

# ► SETEAZĂ după rezultatele testelor:
ir_aprc_stacionar <- FALSE   # TRUE dacă ADF respinge H0 în nivel pentru IR_APRC


###############################################################################
# 4. CONSTRUIREA SISTEMULUI + DUMMY-URI
###############################################################################

timp <- time(log_rent)

covid_dummy <- ifelse(timp >= 2020+2/12 & timp <= 2021+5/12, 1, 0)
inf_dummy   <- ifelse(timp >= 2022+1/12 & timp <= 2023+11/12, 1, 0)
dum_mat     <- cbind(covid_dummy, inf_dummy)

# Ordinea Cholesky: cel mai exogen → cel mai endogen
# IR_APRC → log_FX → log_SR → log_RENT
# Justificare:
#   IR_APRC: determinată de BCE/BNR, cel mai exogen
#   log_FX:  determinată de piețe valutare internaționale
#   log_SR:  răspunde la condiții macro cu lag
#   log_RENT: cel mai endogen — outcome al tuturor celorlalte

if (!ir_aprc_stacionar) {
  Y <- cbind(
    IR_APRC  = ir_aprc,
    log_FX   = log_fx,
    log_SR   = log_sr,
    log_RENT = log_rent
  )
  cat("\n► Scenariu A: Johansen pe 4 variabile\n")
} else {
  Y <- cbind(
    log_FX   = log_fx,
    log_SR   = log_sr,
    log_RENT = log_rent
  )
  cat("\n► Scenariu B: Johansen pe 3 variabile, IR_APRC exogenă\n")
}

cat("Variabile în sistem:", colnames(Y), "\n")
var_names <- colnames(Y)


###############################################################################
# 5. SELECTAREA LAGULUI OPTIM
###############################################################################

cat("\n=== SELECTARE LAG OPTIM – SERII ÎN NIVEL ===\n")
lag_select <- VARselect(Y, lag.max=12, type="const")
print(lag_select)

p_aic <- as.numeric(lag_select$selection["AIC(n)"])
p_bic <- as.numeric(lag_select$selection["SC(n)"])
p_hq  <- as.numeric(lag_select$selection["HQ(n)"])
cat("\nLag AIC:", p_aic, "| BIC:", p_bic, "| HQ:", p_hq, "\n")

# AIC pentru date lunare — captează mai bine dinamici complexe
# Johansen necesită K ≥ 2 obligatoriu
p_opt <- max(p_aic, 2)
cat("Lag ales (AIC, min 2):", p_opt, "\n")


###############################################################################
# 6. TESTUL DE COINTEGRARE JOHANSEN
###############################################################################

cat("\n=== TEST JOHANSEN – TRACE ===\n")
johansen_trace <- ca.jo(Y,
                        type   = "trace",
                        ecdet  = "const",
                        K      = p_opt,
                        dumvar = dum_mat,
                        season = 12)
summary(johansen_trace)

cat("\n=== TEST JOHANSEN – MAXEIGEN ===\n")
johansen_max <- ca.jo(Y,
                      type   = "eigen",
                      ecdet  = "const",
                      K      = p_opt,
                      dumvar = dum_mat,
                      season = 12)
summary(johansen_max)

# ► MODIFICĂ după tabelul Johansen:
# r=0 respins, r≤1 nerespins → r_coint=1
# r=0 și r≤1 respinse, r≤2 nerespins → r_coint=2
# r=0 nerespins → r_coint=0 → VAR pe diferențe
r_coint <- 1

cat("\n► Rang ales: r =", r_coint, "\n")

###############################################################################
# 7. ESTIMARE VECM (NORMALIZAT METODOLOGIC PE VARIABILA ȚINTĂ: log_RENT)
###############################################################################

if (r_coint >= 1) {
  
  cat("\n══════════════════════════════════════════════════════\n")
  cat(" 7A. ESTIMARE VECM (r =", r_coint, ") - NORMALIZAT PE log_RENT\n")
  cat("══════════════════════════════════════════════════════\n")
  
  # Pasul 1: Estimarea brută VECM din testul Johansen
  vecm_brut <- cajorls(johansen_trace, r=r_coint)
  
  # Pasul 2: RE-NORMALIZARE MANUALĂ (Ca în cursuri)
  # Extragem vectorul beta brut pentru a identifica poziția variabilei log_RENT
  beta_brut <- vecm_brut$beta
  
  # Aflăm valoarea coeficientului actual de la log_RENT pentru a împărți tot vectorul la el
  coef_rent_brut <- beta_brut["log_RENT.l12", 1]
  
  # Generăm noul vector Beta normalizat, unde log_RENT devine exact 1.0000
  beta_normalized <- beta_brut / coef_rent_brut
  
  cat("\n=== VECTORUL DE COINTEGRARE (β) NORMALIZAT PE log_RENT ===\n")
  print(round(beta_normalized, 5))
  
  # Pasul 3: Recalcularea coeficienților de ajustare dinamică (α) conform noii normalizări
  # În econometrie, când re-normalizăm Beta, trebuie să înmulțim Alpha cu acel coeficient pentru a păstra echilibrul
  alpha_brut <- coef(vecm_brut$rlm)[paste0("ect", 1:r_coint), , drop=FALSE]
  alpha_normalized <- alpha_brut * coef_rent_brut
  
  cat("\n=== COEFICIENȚI DE AJUSTARE (α) RECALCULAȚI ===\n")
  colnames(alpha_normalized) <- c("IR_APRC.d", "log_FX.d", "log_SR.d", "log_RENT.d")
  print(round(alpha_normalized, 6))
  
  # Pasul 4: Extragerea ecuației dinamice complete STRICT pentru log_RENT
  cat("\n══════════════════════════════════════════════════════\n")
  cat(" === ECUAȚIA DINAMICĂ DETALIATĂ: Δlog(RENT) ===\n")
  cat("══════════════════════════════════════════════════════\n")
  
  # Identificăm indexul ecuației log_RENT în model (poziția 4 în sistemul tău)
  print(summary(vecm_brut$rlm)[[4]])
  
  # Pasul 5: Conversia automată VECM -> VAR pentru utilizarea în IRF, FEVD și Prognoze
  var_model <- vec2var(johansen_trace, r=r_coint)
  
  # Pasul 6: Generarea seriei reziduurilor ECT re-normalizate pentru grafic
  ect_ts <- ts(
    as.matrix(vecm_brut$rlm$model[, paste0("ect", 1:r_coint)]) * coef_rent_brut,
    start     = time(log_rent)[p_opt+2],
    frequency = 12
  )
  
  df_ect <- data.frame(
    timp    = as.Date(as.yearmon(time(ect_ts))),
    Valoare = as.numeric(ect_ts)
  )
  
  # Afișare Grafic ECT curat
  print(
    ggplot(df_ect, aes(x=timp, y=Valoare)) +
      geom_line(color="#005088", linewidth=0.8) +
      geom_hline(yintercept=0, linetype="dashed", color="#CC0000", linewidth=0.7) +
      geom_hline(yintercept=mean(df_ect$Valoare, na.rm=TRUE), linetype="dotted", color="grey50") +
      labs(title    = "Termenul de Corecție al Erorii (ECT) Normalizat pe log(RENT)",
           subtitle = "Evoluția abaterilor și tendința de revenire la echilibrul structural",
           x=NULL, y="Valori Reziduale ECT") +
      theme_minimal(base_size=11) +
      theme(plot.title=element_text(face="bold", color="#005088"))
  )
  
  # Testul de validare a staționalității pe noile reziduuri
  cat("\n=== TEST STACIONARITATE ECT (ADF) ===\n")
  print(summary(ur.df(ect_ts, type="drift", selectlags="AIC")))
  
} # end VECM


###############################################################################
# 8. DIAGNOSTIC REZIDUURI
###############################################################################

cat("\n══════════════════════════════════════════════════════\n")
cat(" 8. DIAGNOSTIC REZIDUURI\n")
cat("══════════════════════════════════════════════════════\n")

# Autocorelare Portmanteau (lags.pt > p OBLIGATORIU)
cat("\n=== AUTOCORELARE (Portmanteau) ===\n")
cat("lag 24: "); print(serial.test(var_model, lags.pt=24, type="PT.asymptotic"))
cat("lag 36: "); print(serial.test(var_model, lags.pt=36, type="PT.asymptotic"))
cat("lag 48: "); print(serial.test(var_model, lags.pt=48, type="PT.asymptotic"))

# ARCH multivariate
cat("\n=== ARCH MULTIVARIATE ===\n")
cat("lag 12: "); print(arch.test(var_model, lags.multi=12))
cat("lag 24: "); print(arch.test(var_model, lags.multi=24))

# Normalitate
cat("\n=== NORMALITATE REZIDUURI ===\n")
print(normality.test(var_model))

# Stabilitate (VAR pe diferențe)
if (r_coint == 0) {
  cat("\n=== STABILITATE VAR ===\n")
  print(roots(var_model))
  plot(stability(var_model, type="OLS-CUSUM"), main="Test stabilitate OLS-CUSUM")
}

# ── Grafice reziduuri ────────────────────────────────────────────────────────
resid_var <- residuals(var_model)

if (r_coint >= 1) {
  col_rent <- "resids of log_RENT"
  col_sr   <- "resids of log_SR"
  col_fx   <- "resids of log_FX"
  col_ir   <- "resids of IR_APRC"
  t_start  <- time(log_rent)[p_opt+2]
} else {
  col_rent <- "log_RENT"
  col_sr   <- "log_SR"
  col_fx   <- "log_FX"
  col_ir   <- "IR_APRC"
  t_start  <- time(log_rent)[p_opt_d1+2]
}

res_rent <- ts(resid_var[,col_rent], start=t_start, frequency=12)
res_sr   <- ts(resid_var[,col_sr],   start=t_start, frequency=12)
res_fx   <- ts(resid_var[,col_fx],   start=t_start, frequency=12)
res_ir   <- ts(resid_var[,col_ir],   start=t_start, frequency=12)

p1 <- autoplot(res_rent) + geom_hline(yintercept=0,linetype="dashed",color="red") +
  labs(title="Reziduuri – log(RENT)", x=NULL, y="") + theme_bw()
p2 <- autoplot(res_sr)   + geom_hline(yintercept=0,linetype="dashed",color="red") +
  labs(title="Reziduuri – log(SR)",   x=NULL, y="") + theme_bw()
p3 <- autoplot(res_fx)   + geom_hline(yintercept=0,linetype="dashed",color="red") +
  labs(title="Reziduuri – log(FX nominal)", x=NULL, y="") + theme_bw()
p4 <- autoplot(res_ir)   + geom_hline(yintercept=0,linetype="dashed",color="red") +
  labs(title="Reziduuri – IR_APRC",   x=NULL, y="") + theme_bw()

print((p1+p2)/(p3+p4))

ggAcf(res_rent, lag.max=36) + ggtitle("ACF reziduuri – log(RENT)") + theme_bw()
ggAcf(res_sr,   lag.max=36) + ggtitle("ACF reziduuri – log(SR)")   + theme_bw()
ggAcf(res_fx,   lag.max=36) + ggtitle("ACF reziduuri – log(FX)")   + theme_bw()
ggAcf(res_ir,   lag.max=36) + ggtitle("ACF reziduuri – IR_APRC")   + theme_bw()


###############################################################################
# 9. CAUZALITATE GRANGER
###############################################################################

cat("\n══════════════════════════════════════════════════════\n")
cat(" 9. CAUZALITATE GRANGER\n")
cat("══════════════════════════════════════════════════════\n")

if (r_coint == 0) {
  print(causality(var_model, cause="IR_APRC"))
  print(causality(var_model, cause="log_FX"))
  print(causality(var_model, cause="log_SR"))
  print(causality(var_model, cause="log_RENT"))

} else {
  coef_mat <- coef(vecm_model$rlm)

  idx_rent <- grep("^log_RENT\\.dl", rownames(coef_mat))
  idx_sr   <- grep("^log_SR\\.dl",   rownames(coef_mat))
  idx_fx   <- grep("^log_FX\\.dl",   rownames(coef_mat))
  idx_ir   <- grep("^IR_APRC\\.dl",  rownames(coef_mat))

  granger_wald <- function(model_mlm, idx_reg, eq_name) {
    if (length(idx_reg)==0) { cat("  (niciun lag)\n"); return(invisible(NULL)) }
    b       <- coef(model_mlm)[, eq_name]
    V       <- vcov(model_mlm)
    n_coef  <- length(b)
    eq_pos  <- which(colnames(coef(model_mlm))==eq_name)
    row_idx <- ((eq_pos-1)*n_coef+1):(eq_pos*n_coef)
    V_eq    <- V[row_idx, row_idx]
    b_sub   <- b[idx_reg]; V_sub <- V_eq[idx_reg,idx_reg]
    W       <- as.numeric(t(b_sub) %*% solve(V_sub) %*% b_sub)
    df_w    <- length(idx_reg)
    pval    <- pchisq(W, df=df_w, lower.tail=FALSE)
    stars   <- ifelse(pval<0.01,"***",ifelse(pval<0.05,"**",
                      ifelse(pval<0.10,".","ns")))
    cat("  Chi²=",round(W,4),"| df=",df_w,"| p=",round(pval,4),stars,"\n")
    invisible(list(statistic=W, df=df_w, p.value=pval))
  }

  # ── Ecuația Δlog(RENT) ───────────────────────────────────────────────────
  cat("\n--- Δlog(RENT): cine cauzează chiriile? ---\n")
  cat("H0: log_SR  NU → "); granger_wald(vecm_model$rlm, idx_sr,  "log_RENT.d")
  cat("H0: log_FX  NU → "); granger_wald(vecm_model$rlm, idx_fx,  "log_RENT.d")
  cat("H0: IR_APRC NU → "); granger_wald(vecm_model$rlm, idx_ir,  "log_RENT.d")

  # ── Ecuația Δlog(SR) ─────────────────────────────────────────────────────
  cat("\n--- Δlog(SR): cine cauzează salariul? ---\n")
  cat("H0: log_RENT NU → "); granger_wald(vecm_model$rlm, idx_rent, "log_SR.d")
  cat("H0: log_FX   NU → "); granger_wald(vecm_model$rlm, idx_fx,   "log_SR.d")
  cat("H0: IR_APRC  NU → "); granger_wald(vecm_model$rlm, idx_ir,   "log_SR.d")

  # ── Ecuația Δlog(FX) ─────────────────────────────────────────────────────
  cat("\n--- Δlog(FX): cine cauzează cursul? ---\n")
  cat("H0: log_RENT NU → "); granger_wald(vecm_model$rlm, idx_rent, "log_FX.d")
  cat("H0: log_SR   NU → "); granger_wald(vecm_model$rlm, idx_sr,   "log_FX.d")
  cat("H0: IR_APRC  NU → "); granger_wald(vecm_model$rlm, idx_ir,   "log_FX.d")

  # ── Ecuația ΔIR_APRC ─────────────────────────────────────────────────────
  cat("\n--- ΔIR_APRC: cine cauzează dobânda? ---\n")
  cat("H0: log_RENT NU → "); granger_wald(vecm_model$rlm, idx_rent, "IR_APRC.d")
  cat("H0: log_SR   NU → "); granger_wald(vecm_model$rlm, idx_sr,   "IR_APRC.d")
  cat("H0: log_FX   NU → "); granger_wald(vecm_model$rlm, idx_fx,   "IR_APRC.d")

  # ── Cauzalitate termen lung (α) ──────────────────────────────────────────
  cat("\n--- Cauzalitate TERMEN LUNG (α / ECT) ---\n")
  cat("α negativ și semnificativ → ajustare activă la dezechilibru\n")
  print(round(alpha_mat, 6))
}


###############################################################################
# 9. CAUZALITATE GRANGER STRUCTURATĂ (AFIȘARE COMPLETĂ P-VALUE)
###############################################################################

cat("\n══════════════════════════════════════════════════════\n")
cat(" 9. CAUZALITATE GRANGER ȘI REZUMAT SISTEMIC\n")
cat("══════════════════════════════════════════════════════\n")

if (r_coint == 0) {
  print(causality(var_model, cause="IR_APRC"))
  print(causality(var_model, cause="log_FX"))
  print(causality(var_model, cause="log_SR"))
  print(causality(var_model, cause="log_RENT"))
} else {
  coef_mat <- coef(vecm_model$rlm)
  
  idx_rent <- grep("^log_RENT\\.dl", rownames(coef_mat))
  idx_sr   <- grep("^log_SR\\.dl",   rownames(coef_mat))
  idx_fx   <- grep("^log_FX\\.dl",   rownames(coef_mat))
  idx_ir   <- grep("^IR_APRC\\.dl",  rownames(coef_mat))
  
  # Funcție îmbunătățită pentru a afișa P-Value CURAT, fără format științific
  granger_wald_clear <- function(model_mlm, idx_reg, eq_name, var_cauza) {
    if (length(idx_reg)==0) { return(invisible(NULL)) }
    b       <- coef(model_mlm)[, eq_name]
    V       <- vcov(model_mlm)
    n_coef  <- length(b)
    eq_pos  <- which(colnames(coef(model_mlm))==eq_name)
    row_idx <- ((eq_pos-1)*n_coef+1):(eq_pos*n_coef)
    V_eq    <- V[row_idx, row_idx]
    b_sub   <- b[idx_reg]; V_sub <- V_eq[idx_reg,idx_reg]
    W       <- as.numeric(t(b_sub) %*% solve(V_sub) %*% b_sub)
    df_w    <- length(idx_reg)
    pval    <- pchisq(W, df=df_w, lower.tail=FALSE)
    
    stars   <- ifelse(pval<0.01,"***",ifelse(pval<0.05,"**", ifelse(pval<0.10,".","ns")))
    
    # Formatăm p-value să aibă mereu 4 zecimale clare, fără e-04
    pval_formatat <- sprintf("%.4f", pval)
    if(pval < 0.0001) pval_formatat <- "< 0.0001"
    
    cat(sprintf("  H0: %-10s nu cauzează %-10s → Chi² = %7.4f | p-value = %s %s\n", 
                var_cauza, eq_name, W, pval_formatat, stars))
  }
  
  cat("\n--- TERMEN SCURT: Cine cauzează Chiriile? ---\n")
  granger_wald_clear(vecm_model$rlm, idx_sr,  "log_RENT.d", "log_SR")
  granger_wald_clear(vecm_model$rlm, idx_fx,  "log_RENT.d", "log_FX")
  granger_wald_clear(vecm_model$rlm, idx_ir,  "log_RENT.d", "IR_APRC")
  
  cat("\n--- TERMEN SCURT: Cine cauzează Cursul de Schimb? ---\n")
  granger_wald_clear(vecm_model$rlm, idx_rent, "log_FX.d",   "log_RENT")
  granger_wald_clear(vecm_model$rlm, idx_sr,   "log_FX.d",   "log_SR")
  granger_wald_clear(vecm_model$rlm, idx_ir,   "log_FX.d",   "IR_APRC")
  
  cat("\n--- TERMEN SCURT: Cine cauzează Salariul? ---\n")
  granger_wald_clear(vecm_model$rlm, idx_rent, "log_SR.d",   "log_RENT")
  granger_wald_clear(vecm_model$rlm, idx_fx,   "log_SR.d",   "log_FX")
  granger_wald_clear(vecm_model$rlm, idx_ir,   "log_SR.d",   "IR_APRC")
  
  cat("\n--- TERMEN SCURT: Cine cauzează Dobânda? ---\n")
  granger_wald_clear(vecm_model$rlm, idx_rent, "IR_APRC.d",  "log_RENT")
  granger_wald_clear(vecm_model$rlm, idx_sr,   "IR_APRC.d",  "log_SR")
  granger_wald_clear(vecm_model$rlm, idx_fx,   "IR_APRC.d",  "log_FX")
  
  # ── COLECTARE AUTOMATĂ ȘI CURATĂ PENTRU TERMEN LUNG (α) ──
  cat("\n--- TERMEN LUNG: Viteza de ajustare structurală (α / ECT1) ---\n")
  sum_rlm <- summary(vecm_model$rlm)
  
  for(i in 1:4) {
    eq_name <- colnames(coef(vecm_model$rlm))[i]
    coef_ect <- sum_rlm[[i]]$coefficients["ect1", "Estimate"]
    pval_ect <- sum_rlm[[i]]$coefficients["ect1", "Pr(>|t|)"]
    
    stars_ect <- ifelse(pval_ect<0.01,"***",ifelse(pval_ect<0.05,"**", ifelse(pval_ect<0.10,".","ns")))
    pval_ect_form <- sprintf("%.4f", pval_ect)
    if(pval_ect < 0.0001) pval_ect_form <- "< 0.0001"
    
    cat(sprintf("  ECT1 → %-12s: Coeficient alpha = %9.6f | p-value = %s %s\n", 
                eq_name, coef_ect, pval_ect_form, stars_ect))
  }
}


###############################################################################
# 10. IRF – FUNCȚII RĂSPUNS LA IMPULS
###############################################################################

cat("\n══════════════════════════════════════════════════════\n")
cat(" 10. IRF\n")
cat("══════════════════════════════════════════════════════\n")

set.seed(42)
n_ahead_irf <- 24
impulsuri   <- var_names[var_names != "log_RENT"]

# Salvare PDF
pdf("Grafice_IRF_Chirii.pdf", width=8, height=6)
for (imp in impulsuri) {
  irf_obj <- irf(var_model, impulse=imp, response="log_RENT",
                 n.ahead=n_ahead_irf, boot=TRUE, ci=0.95, ortho=TRUE, runs=500)
  plot(irf_obj,
       main=paste0("IRF: răspunsul log(RENT) la șoc în ", imp),
       ylab="Variație log(RENT)", xlab="Orizont (luni)")
}
dev.off()
cat("[SALVAT] Grafice_IRF_Chirii.pdf\n")

# Vizualizare pe ecran în aceeași fereastră (3 rânduri, 1 coloană)
# Rulăm cu 500 runs pentru o acuratețe ridicată a intervalelor de încredere
par(mfrow=c(3,1))
set.seed(42)
for (imp in impulsuri) {
  irf_obj <- irf(var_model, impulse=imp, response="log_RENT",
                 n.ahead=n_ahead_irf, boot=TRUE, ci=0.95, ortho=TRUE, runs=500)
  plot(irf_obj, main=paste0("Impact ", imp, " → log_RENT"), 
       ylab="log(RENT)", xlab="Luni ahead")
}
par(mfrow=c(1,1)) # Resetare layout

# IRF complet sistem
irf_all <- irf(var_model, n.ahead=n_ahead_irf, boot=TRUE,
               ci=0.95, ortho=TRUE, runs=500)

# Grafice ggplot pentru IRF spre log_RENT
extract_irf <- function(irf_obj, impulse, response) {
  data.frame(
    perioada = 0:n_ahead_irf,
    raspuns  = irf_obj$irf[[impulse]][, response],
    lower    = irf_obj$Lower[[impulse]][, response],
    upper    = irf_obj$Upper[[impulse]][, response],
    impulse  = impulse
  )
}

df_irf_rent <- do.call(rbind, lapply(impulsuri,
                                     function(imp) extract_irf(irf_all, imp, "log_RENT")))
df_irf_rent$eticheta <- paste0("Șoc: ", df_irf_rent$impulse, " → log(RENT)")

print(
  ggplot(df_irf_rent, aes(x=perioada)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), fill="#DEEAF1", alpha=0.7) +
    geom_line(aes(y=raspuns), color="#005088", linewidth=1) +
    geom_hline(yintercept=0, linetype="dashed", color="red", linewidth=0.5) +
    facet_wrap(~eticheta, ncol=2, scales="free_y") +
    labs(title    = "IRF: răspunsul log(Chirii) la șocuri în regressori",
         subtitle = paste0("Orizont: ", n_ahead_irf, " luni | IC 95% | 500 rulări"),
         x="Luni", y="Răspuns (≈ variație %)") +
    theme_minimal(base_size=11) +
    theme(strip.text=element_text(face="bold", size=9))
)





###############################################################################
# 11. FEVD – DESCOMPUNEREA VARIANȚEI (VERSIUNE OPTIMIZATĂ ȘI INTEGRATĂ)
###############################################################################

cat("\n══════════════════════════════════════════════════════\n")
cat(" 11. DESCOMPUNEREA VARIANȚEI (FEVD)\n")
cat("══════════════════════════════════════════════════════\n")

# Asigurăm încărcarea pachetului pentru formatarea procentelor în ggplot
library(scales)

# Resetăm layout-ul grafic și închidem ferestrele blocate anterior pentru a preveni erori de margini
if(!is.null(dev.list())) dev.off()

# Pasul 1: Rularea modelului de descompunere a varianței pe 24 de luni
fevd_res <- fevd(var_model, n.ahead=24)

# Pasul 2: Printarea tabelelor numerice brute în consolă (utile pentru anexe sau verificări)
cat("\n=== TABEL NUMERIC FEVD: log(RENT) ===\n")
print(round(as.data.frame(fevd_res$log_RENT), 4))

# Pasul 3: Pregătirea și transformarea datelor pentru formatul lung (ggplot)
fevd_rent_df          <- as.data.frame(fevd_res$log_RENT)
fevd_rent_df$Orizont  <- 1:nrow(fevd_rent_df)

fevd_long <- fevd_rent_df %>%
  pivot_longer(cols=-Orizont, names_to="Soc", values_to="Proportie") %>%
  mutate(
    Soc_Economic = case_when(
      Soc == "IR_APRC"  ~ "Dobândă APRC",
      Soc == "log_FX"   ~ "Curs Nominal (FX)",
      Soc == "log_SR"   ~ "Salariu Real",
      Soc == "log_RENT" ~ "Chirii (Inerție Proprie)",
      TRUE ~ Soc
    )
  )

# Pasul 4: Generarea Graficului INTEGRAT de tip bare stivuite (Stacked Bars)
# Acesta combină toate influențele într-o singură fereastră aerisită, până la 100%
grafic_fevd_bare <- ggplot(fevd_long, aes(x=Orizont, y=Proportie, fill=Soc_Economic)) +
  geom_col(position="stack", color="white", width=0.85) +
  scale_y_continuous(labels=percent_format(accuracy=1), expand=c(0,0)) +
  scale_x_continuous(breaks=seq(1, 24, by=2)) +
  scale_fill_manual(values=c("#003366", "#4A90E2", "#50E3C2", "#B8E986")) +
  labs(title    = "FEVD: Descompunerea Varianței Erorii de Prognoză pentru log(RENT)",
       x        = "Orizont de timp (Luni în viitor)", 
       y        = "Proporție din Varianță (%)", 
       fill     = "Sursa structurală a șocului:") +
  theme_minimal(base_size=12) +
  theme(
    plot.title       = element_text(face="bold", size=13, color="#111111"),
    plot.subtitle    = element_text(size=10, face="italic", color="#555555"),
    legend.position  = "bottom",
    legend.title     = element_text(face="bold", size=10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Afișare directă pe ecran în RStudio
print(grafic_fevd_bare)

cat("[SUCCES] Graficul integrat FEVD a fost randat fără erori de margini.\n")


###############################################################################
# 12. PROGNOZE + EVALUARE OUT-OF-SAMPLE
###############################################################################

cat("\n══════════════════════════════════════════════════════\n")
cat(" 12. PROGNOZE + EVALUARE OUT-OF-SAMPLE\n")
cat("══════════════════════════════════════════════════════\n")

h_test  <- 38    #ian 2023-feb 2026
n_total <- nrow(Y)
n_train <- n_total - h_test

Y_train <- window(Y, end   = time(Y)[n_train])
Y_test  <- window(Y, start = time(Y)[n_train+1])

cat("Training: mai 2007 –",
    format(as.yearmon(time(Y)[n_train]),   "%b %Y"), "|", n_train, "obs.\n")
cat("Test:    ",
    format(as.yearmon(time(Y)[n_train+1]), "%b %Y"), "–",
    format(as.yearmon(time(Y)[n_total]),   "%b %Y"), "|", h_test,  "obs.\n")

dum_train <- window(dum_mat, end   = time(Y)[n_train])
dum_test  <- window(dum_mat, start = time(Y_test)[1],
                    end             = tail(time(Y_test),1))

# ── Model 1: VECM ────────────────────────────────────────────────────────────
p_train_opt    <- max(p_opt, 2)
johansen_train <- ca.jo(Y_train, type="trace", ecdet="const",
                        K=p_train_opt, dumvar=dum_train, season=12)
var_train_vecm <- vec2var(johansen_train, r=r_coint)
fc_vecm        <- predict(var_train_vecm, n.ahead=h_test, ci=0.95,
                           dumvar=dum_test)

pred_vecm_rent <- fc_vecm$fcst$log_RENT[,1]
lo95_vecm_rent <- fc_vecm$fcst$log_RENT[,2]
hi95_vecm_rent <- fc_vecm$fcst$log_RENT[,3]

# ── Model 2: VAR pe diferențe ────────────────────────────────────────────────
Y_train_d1   <- diff(Y_train)
dum_d1_train <- window(dum_mat, start=time(Y_train_d1)[1],
                       end=tail(time(Y_train_d1),1))
var_train_d1 <- VAR(Y_train_d1, p=max(p_train_opt-1,1), type="const",
                    exogen=dum_d1_train, season=12)
fc_diff       <- predict(var_train_d1, n.ahead=h_test, ci=0.95,
                          dumvar=dum_test)

last_log_rent  <- as.numeric(tail(Y_train[,"log_RENT"],1))
pred_diff_rent <- last_log_rent + cumsum(fc_diff$fcst$log_RENT[,1])

# ── Realizat ─────────────────────────────────────────────────────────────────
actual_log_rent <- as.numeric(Y_test[,"log_RENT"])

# ── Metrici ──────────────────────────────────────────────────────────────────
eval_m <- function(actual, predicted) {
  err <- actual - predicted
  data.frame(RMSE = round(sqrt(mean(err^2)),5),
             MAE  = round(mean(abs(err)),5),
             `MAPE(%)` = round(mean(abs(err/actual))*100,4),
             check.names=FALSE)
}

cat("\n=== ACURATEȚE OUT-OF-SAMPLE ===\n")
acc_tab <- rbind(
  `VECM`              = eval_m(actual_log_rent, pred_vecm_rent),
  `VAR in Differences`= eval_m(actual_log_rent, pred_diff_rent)
)
print(acc_tab)
cat("Model optim (RMSE minim):", rownames(acc_tab)[which.min(acc_tab$RMSE)], "\n")

# ── Grafic prognoze vs. realizat ─────────────────────────────────────────────

time_test <- as.Date(as.yearmon(time(Y_test)))

# Pregătim tabelele de date originale
df_hist_p <- data.frame(timp=as.Date(as.yearmon(time(log_rent))), valoare=as.numeric(log_rent))
df_fc_p   <- data.frame(timp=time_test, VECM=pred_vecm_rent, VAR_Diff=pred_diff_rent, Actual=actual_log_rent)

# Transformăm datele de prognoză în format lung special pentru a genera legenda automată din ggplot
library(tidyr)
df_fc_long <- df_fc_p %>%
  pivot_longer(cols = c(Actual, VECM, VAR_Diff), names_to = "Model", values_to = "valoare") %>%
  mutate(Model_Eticheta = case_when(
    Model == "Actual"   ~ "Valori Realizate (Eșantion Test)",
    Model == "VECM"     ~ "Prognoză Model VECM Structural",
    Model == "VAR_Diff" ~ "Prognoză Model VAR în Diferențe"
  ))

df_fc_long$Model_Eticheta <- factor(df_fc_long$Model_Eticheta, levels = c(
  "Valori Realizate (Eșantion Test)", "Prognoză Model VECM Structural", "Prognoză Model VAR în Diferențe"
))

print(
  ggplot() +
    # 1. Linia istorică de fundal (In-Sample)
    geom_line(data=df_hist_p, aes(x=timp, y=valoare), color="grey7", linewidth=0.7) +
    
    # 2. Liniile continue de prognoză și realizat (FĂRĂ geom_ribbon / IC)
    geom_line(data=df_fc_long, aes(x=timp, y=valoare, color=Model_Eticheta, group=Model_Eticheta), 
              linetype="solid", linewidth=1.2) +
    
    # 3. Configurația cromatică cerută (Roșu, Albastru, Portocaliu)
    scale_color_manual(name = "Traiectorie", values = c(
      "Valori Realizate (Eșantion Test)" = "#CC0000",   # Roșu intens pentru realitate
      "Prognoză Model VECM Structural"   = "#005088",   # Albastru din codul tău
      "Prognoză Model VAR în Diferențe"  = "#E67E22"    # Portocaliu din codul tău
    )) +
    
    # 4. Axa timpului PĂSTRATĂ EXACT cu afișarea lunii și anului suprapuse (%b\n%Y) la fiecare 2 ani
    scale_x_date(breaks=seq(as.Date("2007-01-01"), as.Date("2027-01-01"), by="2 years"),
                 date_labels="%b\n%Y", expand=expansion(mult=c(0.01, 0.03))) +
    
    # 5. Titluri academice curate
    labs(title    = "Prognoze Out-of-Sample: VECM vs. VAR în Diferențe",
         subtitle = paste0("Analiză comparativă pe perioada de validare: ", 
                           format(min(time_test), "%b %Y"), " – ", format(max(time_test), "%b %Y")),
         x=NULL, y="log(RENT)") +
    
    # 6. Teme și mutarea legendei jos
    theme_minimal(base_size=11) +
    theme(axis.text.x=element_text(size=8), 
          panel.grid.minor=element_blank(),
          legend.position="bottom",
          legend.title=element_text(face="bold", size=9.5),
          plot.title=element_text(face="bold", size=12))
)

# Erori absolute
df_erori <- data.frame(timp=time_test,
                        VECM    = abs(actual_log_rent - pred_vecm_rent),
                        VAR_Diff= abs(actual_log_rent - pred_diff_rent)) %>%
  pivot_longer(-timp, names_to="Model", values_to="Eroare")

print(
  ggplot(df_erori, aes(x=timp, y=Eroare, color=Model)) +
    geom_line(linewidth=0.9) + geom_point(size=1.5) +
    scale_color_manual(values=c("#005088","#E67E22")) +
    labs(title="Erori absolute de prognoză – comparare modele",
         x=NULL, y="|y_t − ŷ_t|") +
    theme_minimal(base_size=11) +
    theme(legend.position="bottom")
)


###############################################################################
# 13. PROGNOZA FINALĂ 24 LUNI (mar 2026 – feb 2028)
###############################################################################

cat("\n══════════════════════════════════════════════════════\n")
cat(" 13. PROGNOZA FINALĂ: mar 2026 – feb 2028\n")
cat("══════════════════════════════════════════════════════\n")

h_final    <- 24
dum_viitor <- matrix(0, nrow=h_final, ncol=ncol(dum_mat))
colnames(dum_viitor) <- colnames(dum_mat)

fc_final    <- predict(var_model, n.ahead=h_final, ci=0.95, dumvar=dum_viitor)
pred_f_log  <- fc_final$fcst$log_RENT[,1]
pred_f_lo95 <- fc_final$fcst$log_RENT[,2]
pred_f_hi95 <- fc_final$fcst$log_RENT[,3]

if (r_coint == 0) {
  last_val    <- as.numeric(tail(log_rent,1))
  pred_f_log  <- last_val + cumsum(pred_f_log)
  pred_f_lo95 <- last_val + cumsum(pred_f_lo95)
  pred_f_hi95 <- last_val + cumsum(pred_f_hi95)
}

pred_f_niv      <- exp(pred_f_log)
pred_f_lo95_niv <- exp(pred_f_lo95)
pred_f_hi95_niv <- exp(pred_f_hi95)

date_fc <- seq(as.Date("2026-03-01"), by="month", length.out=h_final)

tabel_final <- data.frame(
  Luna     = format(date_fc, "%Y-%m"),
  log_RENT = round(pred_f_log,      4),
  Indice   = round(pred_f_niv,      2),
  IC95_inf = round(pred_f_lo95_niv, 2),
  IC95_sup = round(pred_f_hi95_niv, 2)
)
cat("\n=== VALORI PROGNOZATE (trimestrial) ===\n")
print(tabel_final[c(1,3,6,9,12,15,18,21,24),])

# Grafic final
df_ff <- data.frame(timp=date_fc, Progn=pred_f_niv,
                     Lo95=pred_f_lo95_niv, Hi95=pred_f_hi95_niv)
df_hist_orig <- data.frame(timp=as.Date(as.yearmon(time(log_rent))),
                            Indice=exp(as.numeric(log_rent)))

print(
  ggplot() +
    geom_ribbon(data=df_ff, aes(x=timp,ymin=Lo95,ymax=Hi95),
                fill="#DEEAF1", alpha=0.7) +
    geom_line(data=df_hist_orig, aes(x=timp,y=Indice),
              color="grey35", linewidth=0.75) +
    geom_line(data=df_ff, aes(x=timp,y=Progn),
              color="#005088", linewidth=1.2) +
    geom_vline(xintercept=as.Date("2026-03-01"),
               linetype="dashed", color="grey40", linewidth=0.6) +
    annotate("text", x=as.Date("2026-03-01"),
             y=max(df_hist_orig$Indice)*0.97,
             label="Debut\nprognoză", hjust=-0.1, size=3, color="grey40") +
    scale_x_date(
      breaks=seq(as.Date("2007-01-01"),as.Date("2029-01-01"),by="2 years"),
      date_labels="%b\n%Y", expand=expansion(mult=c(0.01,0.03))
    ) +
    labs(title    = "Prognoza 24 luni – Prețul chiriilor România | VECM",
         x=NULL, y="Indice HICP Chirii (2015=100)") +
    theme_minimal(base_size=11) +
    theme(axis.text.x=element_text(size=8),
          panel.grid.minor=element_blank(),
          plot.title=element_text(face="bold", color="#005088"))
)

ultima_val <- round(exp(as.numeric(tail(log_rent,1))),2)
cat("\n=== SUMAR EXECUTIV ===\n")
cat("Ultima valoare (feb 2026):", ultima_val, "(2015=100)\n")
cat("Prognoză mar 2026:",  round(pred_f_niv[1],  2), "\n")
cat("Prognoză feb 2027:",  round(pred_f_niv[12], 2), "\n")
cat("Prognoză feb 2028:",  round(pred_f_niv[24], 2), "\n")
cat("Variație 24 luni: +", round((pred_f_niv[24]/ultima_val-1)*100,2), "%\n")
cat("IC 95% feb 2028: [",
    round(pred_f_lo95_niv[24],2), ",",
    round(pred_f_hi95_niv[24],2), "]\n")

###############################################################################
# 13. PROGNOZA STRUCTURALĂ FINALĂ (AFIȘARE COMPLETĂ 24 LUNI & AXE ALINIATE)
###############################################################################

cat("\n══════════════════════════════════════════════════════\n")
cat(" 13. PROGNOZA STRUCTURALĂ FINALĂ: mar 2026 – feb 2028\n")
cat("══════════════════════════════════════════════════════\n")

h_final    <- 24  # Orizontul de prognoză pură în viitor
dum_viitor <- matrix(0, nrow=h_final, ncol=ncol(dum_mat))
colnames(dum_viitor) <- colnames(dum_mat)

# Generare prognoză pe modelul de bază VECM (var_model)
fc_final    <- predict(var_model, n.ahead=h_final, ci=0.95, dumvar=dum_viitor)
pred_f_log  <- fc_final$fcst$log_RENT[, 1]
pred_f_lo95 <- fc_final$fcst$log_RENT[, 2]
pred_f_hi95 <- fc_final$fcst$log_RENT[, 3]

if (r_coint == 0) {
  last_val    <- as.numeric(tail(log_rent, 1))
  pred_f_log  <- last_val + cumsum(pred_f_log)
  pred_f_lo95 <- last_val + cumsum(pred_f_lo95)
  pred_f_hi95 <- last_val + cumsum(pred_f_hi95)
}

# Reconversia econometrică corectă din logaritm în nivelul original (Index 2015=100)
pred_f_niv      <- exp(pred_f_log)
pred_f_lo95_niv <- exp(pred_f_lo95)
pred_f_hi95_niv <- exp(pred_f_hi95)

date_fc <- seq(as.Date("2026-03-01"), by="month", length.out=h_final)

# Crearea tabelului complet
tabel_final <- data.frame(
  Luna       = format(date_fc, "%Y-%m"),
  log_RENT   = round(pred_f_log,      4),
  Indice     = round(pred_f_niv,      2),
  IC95_inf   = round(pred_f_lo95_niv, 2),
  IC95_sup   = round(pred_f_hi95_niv, 2)
)

# --- MODIFICARE: Printează TOATE cele 24 de luni în consolă, fără nicio filtrare ---
cat("\n=== VALORI PROGNOZATE COMPLET (Toate cele 24 de luni, scala originală) ===\n")
print(tabel_final)  # Afișează tabelul întreg direct


# ── 13.1 Pregătirea Structurii de Date pentru Graficul de Curs / Licență
df_ff <- data.frame(timp = date_fc, Progn = pred_f_niv, Lo95 = pred_f_lo95_niv, Hi95 = pred_f_hi95_niv)
df_hist_orig <- data.frame(timp = as.Date(as.yearmon(time(log_rent))), Indice = exp(as.numeric(log_rent)), Model = "Date Istorice Realizate")
df_ff_line   <- data.frame(timp = date_fc, Indice = pred_f_niv, Model = "Scenariu Central de Prognoză (VECM)")

df_final_total <- rbind(df_hist_orig[, c("timp", "Indice", "Model")], df_ff_line)
df_final_total$Model <- factor(df_final_total$Model, levels = c("Date Istorice Realizate", "Scenariu Central de Prognoză (VECM)"))



# ── 13.3 Sumar Executiv Final în Consolă
ultima_val <- round(exp(as.numeric(tail(log_rent, 1))), 2)
cat("\n=== SUMAR EXECUTIV FINAL ===\n")
cat("Ultima valoare reală observată (feb 2026):", ultima_val, "(Index 2015=100)\n")
cat("Prognoza pe termen scurt (mar 2026)      :", round(pred_f_niv[1],  2), "\n")
cat("Prognoza la 12 luni (feb 2027)            :", round(pred_f_niv[12], 2), "\n")
cat("Prognoza la 24 luni (feb 2028)            :", round(pred_f_niv[24], 2), "\n")
cat("Dinamica acumulată pe orizontul predictiv  : +", round((pred_f_niv[24]/ultima_val - 1)*100, 2), "%\n")
cat("Interval de siguranță IC 95% (feb 2028)    : [", round(pred_f_lo95_niv[24], 2), ",", round(pred_f_hi95_niv[24], 2), "]\n")

