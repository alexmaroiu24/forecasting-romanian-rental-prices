###############################################################################
# PROIECT SERII DE TIMP – COMPONENTA UNIVARIATĂ
# Metodologie Box-Jenkins (conform flowchart)
# Seria: HICP – Chirii efective România | mai 2007 – feb 2026
#
# ORDINE METODOLOGICĂ CORECTĂ (conform flowchart):
#  1.  Import + construire serie
#  2.  Analiză exploratorie pe SERIA COMPLETĂ
#  3.  Delimitare train / test  ← DUPĂ explorare, ÎNAINTE de estimare
#  4.  Stationaritate + transformări pe TRAINING
#  5.  Identificare model (ACF/PACF) pe seria transformată
#  6.  Estimare modele candidate pe TRAINING
#  7.  Diagnostic reziduuri
#  8.  Selectare model optim
#  9.  Prognoze + evaluare pe TEST
#  10. Model alternativ ETS + comparare Diebold-Mariano
#  11. Prognoza finală pe toată seria
###############################################################################

# ─── Pachete necesare ────────────────────────────────────────────────────────
library(readxl)
library(forecast)
library(fpp2)
library(tseries)
library(urca)
library(uroot)
library(dplyr)
library(zoo)
library(ggplot2)
library(lmtest)
library(FinTS)
library(moments)
library(changepoint)
library(strucchange)
library(gridExtra)

###############################################################################
# 1. IMPORT DATE ȘI CONSTRUIREA SERIEI COMPLETE
###############################################################################

chirii_raw <- read_excel("date_univariat.xlsx", skip = 1, col_names = c("date","rent"))

# Convertim coloana dată
chirii_raw$date <- as.Date(as.yearmon(chirii_raw$date, "%Y-%m"))

# DECIZIE METODOLOGICĂ JUSTIFICATĂ:
# Ian–Apr 2007: salt metodologic Eurostat de ~+94% în mai 2007.
# Acesta NU reprezintă o evoluție economică reală a pieței chiriilor,
# ci o recalibrare a bazei de calcul Eurostat (schimbare metodologie HICP).
# Includerea acestor 4 observații ar distorsiona:
#   - media și varianța seriei
#   - testele de stationaritate (ADF, KPSS, PP)
#   - structura ACF/PACF → identificare greșită a modelului
#   - estimarea coeficienților AR/MA
# → Excludem ian–apr 2007 și începem seria din MAI 2007.

chirii <- chirii_raw %>% filter(date >= as.Date("2007-05-01"))

# Serie lunară ts: mai 2007 – feb 2026 (226 observații)
rent_ts <- ts(chirii$rent, start = c(2007, 5), frequency = 12)

cat("==========================================================\n")
cat(" SERIE: mai 2007 – feb 2026 |", length(rent_ts), "observații\n")
cat("==========================================================\n")
summary(rent_ts)


###############################################################################
# 2. ANALIZĂ EXPLORATORIE PE SERIA COMPLETĂ
#    Pasul: "Plot & Exploratory Analysis – Trend, Seasonality, Variance"
###############################################################################

# ── 2.1 Grafic evoluție cu șocuri macro ─────────────────────────────────────
socuri <- data.frame(
  data     = as.Date(c("2008-10-01", "2020-03-01", "2022-02-01")),
  eticheta = c("Criză financiară\noct. 2008",
               "Pandemie COVID-19\nmar. 2020",
               "Șoc inflaționist\nfeb. 2022"),
  culoare  = c("#1A5276", "#D35400", "#C0392B"),
  y_label  = c(98, 116, 155)
)

ggplot(chirii, aes(x = date, y = rent)) +
  annotate("rect",
           xmin = as.Date("2008-09-01"), xmax = as.Date("2010-01-01"),
           ymin = -Inf, ymax = Inf, fill = "#EAF2FF", alpha = 0.45) +
  annotate("rect",
           xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-01"),
           ymin = -Inf, ymax = Inf, fill = "#FEF9E7", alpha = 0.45) +
  annotate("rect",
           xmin = as.Date("2022-01-01"), xmax = as.Date("2024-06-01"),
           ymin = -Inf, ymax = Inf, fill = "#EAFAF1", alpha = 0.40) +
  geom_line(color = "#005088", linewidth = 0.85) +
  geom_vline(data = socuri,
             aes(xintercept = data, color = I(culoare)),
             linetype = "dashed", linewidth = 0.65, alpha = 0.9) +
  geom_label(data = socuri,
             aes(x = data, y = y_label, label = eticheta, color = I(culoare)),
             fill = "white", size = 2.6, fontface = "bold",
             label.padding = unit(0.2, "lines"), hjust = 0.5,
             show.legend = FALSE) +
  scale_x_date(
    breaks      = seq(as.Date("2007-01-01"), as.Date("2027-01-01"), by = "1 year"),
    date_labels = "%b\n%Y",
    expand      = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(breaks = seq(80, 175, 10)) +
  labs(
    title    = "Evoluția prețurilor chiriilor din România",
    subtitle = "Indicele HICP – chirii efective (mai 2007 – feb 2026)",
    caption  = "Sursă: Eurostat. Baza de comparație: 2015 = 100.",
    x = NULL, y = "Indice (2015 = 100)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 13, color = "#005088"),
    plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey40"),
    plot.caption  = element_text(size = 8, color = "grey55"),
    axis.text.x   = element_text(size = 7.5),
    panel.grid.minor = element_blank()
  )


# ── 2.2 Grafice sezoniere ────────────────────────────────────────────────────
ggseasonplot(rent_ts, year.labels = TRUE, year.labels.left = TRUE) +
  labs(
    title = "Grafic sezonier – Indice chirii România",
    y = "Indice (2015 = 100)", x = NULL
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsubseriesplot(rent_ts) +
  labs(
    title = "Subserii sezoniere – prețul chiriei pe luni",
    y = "Indice (2015 = 100)"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# ── 2.3 Descompunere STL pe seria completă ───────────────────────────────────
fit_stl <- stl(rent_ts, t.window = 13, s.window = "periodic", robust = TRUE)

autoplot(fit_stl) +
  labs(
    title = "Descompunere STL – Indice chirii România",
    x = "Timp", y = "Indice (2015 = 100)"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Forța componentelor STL (Hyndman & Athanasopoulos, 2021)
# Ft, Fs ∈ [0,1]: valorile apropiate de 1 → componentă dominantă
ft <- max(0, 1 - var(remainder(fit_stl)) /
            var(trendcycle(fit_stl) + remainder(fit_stl)))
fs <- max(0, 1 - var(remainder(fit_stl)) /
            var(seasonal(fit_stl) + remainder(fit_stl)))

cat("\n=== FORȚA COMPONENTELOR STL ===\n")
cat("Forța Trend:        ", round(ft, 4), "\n")
cat("Forța Sezonalitate: ", round(fs, 4), "\n")
cat("Interpretare: Ft > 0.6 → trend dominant; Fs > 0.6 → sezonalitate puternică\n")


# ── 2.4 Corelograma seriei originale ─────────────────────────────────────────
ggtsdisplay(rent_ts, lag.max = 60,
            main = "ACF și PACF – Serie originală (mai 2007 – feb 2026)")
# ACF descrește lent → indiciu puternic de nestationaritate
# PACF: primul lag aproape de 1 → comportament random walk / rădăcină unitară


# ── 2.5 ndiffs / nsdiffs orientative (pe seria completă) ────────────────────
cat("\n=== ndiffs / nsdiffs (seria completă – orientativ) ===\n")
cat("ndiffs:  ", ndiffs(rent_ts),  "\n")
cat("nsdiffs: ", nsdiffs(rent_ts), "\n")


###############################################################################
# 3. DELIMITARE TRAINING / TEST
#    ← DUPĂ analiza exploratorie, ÎNAINTE de orice estimare
###############################################################################
# Training: mai 2007 – dec 2022  → 187 obs. (82.7%)
# Test:     ian 2023 – feb 2026  →  38 obs. (16.8%)
#
# Motivare: setul de test acoperă ciclul inflaționist 2023-2026,
# perioadă relevantă și dificilă pentru orice model de prognoză.
###############################################################################

h        <- 38
train_ts <- window(rent_ts, end   = c(2022, 12))
test_ts  <- window(rent_ts, start = c(2023,  1))

cat("\n=== SPLIT TRAINING / TEST ===\n")
cat("Training: mai 2007 – dec 2022 |", length(train_ts), "obs.\n")
cat("Test:     ian 2023 – feb 2026 |", length(test_ts),  "obs.\n")

# Grafic split training vs. test
chirii_split <- chirii %>%
  mutate(Esantion = ifelse(
    date <= as.Date("2022-12-01"),
    "Training (mai 2007 – dec 2022)",
    "Test (ian 2023 – feb 2026)"
  ))

ggplot(chirii_split, aes(x = date, y = rent, color = Esantion)) +
  geom_line(linewidth = 0.9) +
  scale_x_date(
    breaks      = seq(as.Date("2007-01-01"), as.Date("2027-01-01"), by = "1 year"),
    date_labels = "%b\n%Y",
    expand      = expansion(mult = c(0.01, 0.02))
  ) +
  scale_color_manual(values = c(
    "Training (mai 2007 – dec 2022)" = "#005088",
    "Test (ian 2023 – feb 2026)"     = "#C0392B"
  )) +
  geom_vline(xintercept = as.Date("2023-01-01"),
             linetype = "dashed", color = "grey40", linewidth = 0.7) +
  annotate("text", x = as.Date("2023-01-01"), y = max(chirii$rent) * 0.97,
           label = "Limita\ntrain/test", hjust = -0.1, size = 3, color = "grey40") +
  labs(
    title    = "Segmentarea seriei: Training vs. Test",
    subtitle = "~83% training | ~17% test (metodologie Box-Jenkins)",
    x = NULL, y = "Indice (2015 = 100)", color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 13),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    legend.position  = "bottom",
    axis.text.x      = element_text(size = 7.5),
    panel.grid.minor = element_blank()
  )


###############################################################################
# 4. STATIONARITATE PE TRAINING
#    Pasul: "Stationary?" din flowchart
###############################################################################

# ── 4.1 Corelograma training ─────────────────────────────────────────────────
ggtsdisplay(train_ts, lag.max = 60,
            main = "Corelogramă – Training (mai 2007 – dec 2022)")


# ── 4.2 ADF (Augmented Dickey-Fuller) ────────────────────────────────────────
# H0: serie cu rădăcină unitară (nestacionară)
# H1: serie fără rădăcină unitară (stacionară)
# Dacă |t-stat| < |val. critică| → NU respingem H0 → nestacionară
cat("\n=== ADF – TRAINING ÎN NIVEL ===\n")
cat("--- Fără componentă deterministă ---\n")
summary(ur.df(train_ts, type = "none",  selectlags = "AIC"))
cat("--- Cu intercept (drift) ---\n")
summary(ur.df(train_ts, type = "drift", selectlags = "AIC"))
cat("--- Cu intercept + trend ---\n")
summary(ur.df(train_ts, type = "trend", selectlags = "AIC"))


# ── 4.3 KPSS ─────────────────────────────────────────────────────────────────
# H0: serie stacionară  ← OPUS față de ADF!
# H1: serie nestacionară
# Dacă stat. test > val. critică → RESPINGEM H0 → nestacionară
cat("\n=== KPSS – TRAINING ÎN NIVEL ===\n")
summary(ur.kpss(train_ts))


# ── 4.4 Phillips-Perron ───────────────────────────────────────────────────────
# H0: rădăcină unitară; robust la heteroscedasticitate
cat("\n=== PHILLIPS-PERRON – TRAINING ÎN NIVEL ===\n")
print(PP.test(train_ts))


# ── 4.5 Zivot-Andrews (ruptură structurală endogenă) ─────────────────────────
# Util când ADF ar putea fi distorsionat de o ruptură necunoscută în serie.
# H0: rădăcină unitară cu posibilă ruptură structurală
# H1: stacionaritate cu ruptură endogenă
# Dacă t-stat < val. critică (test left-tailed) → respingem H0
cat("\n=== ZIVOT-ANDREWS – TRAINING ===\n")
za <- ur.za(train_ts, model = "both", lag = 0)
summary(za)
plot(za)

# CONCLUZIE AȘTEPTATĂ: toate testele confirmă NESTACIONARITATE în nivel
# → este necesară diferențierea


###############################################################################
# 5. TRANSFORMAREA SERIEI
#    Pasul: "Transform Series – Detrend, Difference, Log, Seasonal Diff"
###############################################################################

# ── 5.1 Justificarea transformării logaritmice ────────────────────────────────
# Box-Cox: lambda ≈ 0 → log este transformarea optimă
# log stabilizează varianța (dacă aceasta crește odată cu nivelul)
# IMPORTANT: NU logaritmăm manual! Folosim lambda = 0 în Arima()
#   → prognozele sunt re-transformate automat cu bias adjustment
lam <- BoxCox.lambda(train_ts)
cat("\n=== BOX-COX LAMBDA ===\n")
cat("Lambda estimat:", round(lam, 4), "\n")
cat(ifelse(abs(lam) < 0.2,
           "→ log-transformare justificată (λ ≈ 0)\n",
           "→ verificați dacă log este necesară\n"))

# ── 5.2 Câte diferențe sunt necesare? ────────────────────────────────────────
cat("\n=== ndiffs / nsdiffs PE TRAINING (log) ===\n")
cat("ndiffs:  ", ndiffs(train_ts),  "\n")   # → 1 (un nivel de diferențiere)
cat("nsdiffs: ", nsdiffs(train_ts), "\n")   # → 0 (NU e necesară diff. sezonieră)
# nsdiffs = 0 → sezonalitatea este deterministă (slabă), nu stochastică
# → nu diferențiem sezonier (D = 0), d = 1 este suficient


# ── 5.3 Prima diferență a training ──────────────────────────────────────
rent_d1 <- diff(train_ts)

autoplot(rent_d1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Indice chirii – Prima diferență pe Training",
    x = "Timp", y = "Indice"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# ── 5.4 Retestare stationaritate pe seria transformată ───────────────────────
# Dacă toate testele confirmă stacionaritate → d = 1 este corect
cat("\n=== RETESTARE STACIONARITATE – Δlog(training) ===\n")
cat("--- ADF (none) ---\n")
summary(ur.df(rent_d1, type = "none",  selectlags = "AIC"))
cat("--- ADF (drift) ---\n")
summary(ur.df(rent_d1, type = "drift", selectlags = "AIC"))
cat("--- ADF (trend) ---\n")
summary(ur.df(rent_d1, type = "trend", selectlags = "AIC"))
cat("--- KPSS ---\n")
summary(ur.kpss(rent_d1))
cat("--- Phillips-Perron ---\n")
print(PP.test(rent_d1))

# CONCLUZIE AȘTEPTATĂ: training este STACIONARĂ → d = 1 confirmat ✓


###############################################################################
# 6. IDENTIFICARE MODEL – ACF / PACF
#    Pasul: "Identify Model – ACF / PACF" din flowchart
#    Analiza se face pe SERIA STACIONARĂ: Δlog(training)
###############################################################################

ggtsdisplay(rent_d1, lag.max = 60,
            main = "Identificare model: ACF și PACF")

ggAcf(rent_d1, lag.max = 60) +
  labs(title = "ACF – Indice chirii pe Training") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggPacf(rent_d1, lag.max = 60) +
  labs(title = "PACF – Indice chirii pe Training") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ── REGULI DE IDENTIFICARE ────────────────────────────────────────────────────
# ACF se taie brusc după lag q → componentă MA(q)
# PACF se taie brusc după lag p → componentă AR(p)
# Ambele descresc gradual → model mixt ARMA(p, q)
# Spike semnificativ la lag 12 în ACF → SMA(1) posibil (D=0 confirmat de nsdiffs)
#
# ORDINE MAXIMĂ REZONABILĂ:
# p ≤ 3 (laguri PACF semnificative)
# q ≤ 3 (laguri ACF semnificative)
# d = 1 (confirmat prin teste + ndiffs)
# D = 0 (nsdiffs = 0)
# P, Q ∈ {0, 1} dacă există pattern sezonier rezidual la lag 12


###############################################################################
# 7. ESTIMARE MODELE CANDIDATE PE TRAINING
#    Pasul: "Estimate Model" din flowchart
#    lambda = 0 → log intern Arima()
#    method = "ML" → comparare corectă AIC/BIC
###############################################################################

# ── 7.1 auto.arima ca punct de referință ─────────────────────────────────────
# auto.arima minimizează AICc automat, dar NU garantează coeficienți semnificativi
cat("\n=== AUTO.ARIMA – punct de referință ===\n")
m_auto <- auto.arima(train_ts, lambda = 0, stepwise = FALSE,
                     approximation = FALSE, trace = TRUE)
summary(m_auto)
coeftest(m_auto)

# modelul final
model_final <- m_auto

# diagnostic reziduuri
checkresiduals(model_final)


# MODEL FINAL
model_final <- m_auto


# 2. CHECKRESIDUALS (ACF + Ljung-Box)
checkresiduals(model_final)

# 3. TEST LUNGJUNG-BOX explicit
Box.test(residuals(model_final), lag = 24, type = "Ljung-Box")

# 4. TEST NORMALITATE (Jarque-Bera)
library(tseries)
jarque.bera.test(residuals(model_final))

# 5. TEST HETEROSCEDASTICITATE (ARCH)
library(FinTS)
ArchTest(residuals(model_final), lags = 12)

# 6. HISTOGRAMA + DENSITATE
hist(residuals(model_final), breaks=30, main="Histogramă reziduuri")
lines(density(residuals(model_final)), col="red")

# 7. QQ-PLOT (normalitate)
qqnorm(residuals(model_final))
qqline(residuals(model_final), col="red")


# ── 7.2 Modele ARIMA (fără componentă sezonieră explicită) ──────────────────
m1 <- Arima(train_ts, order = c(0,1,1), lambda = 0, method = "ML")
m2 <- Arima(train_ts, order = c(1,1,0), lambda = 0, method = "ML")
m3 <- Arima(train_ts, order = c(1,1,1), lambda = 0, method = "ML")
m4 <- Arima(train_ts, order = c(0,1,2), lambda = 0, method = "ML")
m5 <- Arima(train_ts, order = c(2,1,1), lambda = 0, method = "ML")
m6 <- Arima(train_ts, order = c(2,1,0), lambda = 0, method = "ML")
m7 <- Arima(train_ts, order = c(0,1,3), lambda = 0, method = "ML")
m8 <- Arima(train_ts, order = c(3,1,0), lambda = 0, method = "ML")
m9 <- Arima(train_ts, order = c(3,1,1), lambda = 0, method = "ML")


# ── 7.3 Modele SARIMA (cu componentă sezonieră, D = 0) ──────────────────────
# nsdiffs = 0 → D = 0; dar SMA(1) / SAR(1) pot capta autocorelare reziduală
# la lag 12 dacă există sezonalitate deterministă slabă
s1 <- Arima(train_ts, order = c(0,1,1), seasonal = c(0,0,1), lambda = 0, method = "ML")
s2 <- Arima(train_ts, order = c(1,1,1), seasonal = c(0,0,1), lambda = 0, method = "ML")
s3 <- Arima(train_ts, order = c(1,1,0), seasonal = c(0,0,1), lambda = 0, method = "ML")
s4 <- Arima(train_ts, order = c(0,1,2), seasonal = c(0,0,1), lambda = 0, method = "ML")
s5 <- Arima(train_ts, order = c(2,1,1), seasonal = c(0,0,1), lambda = 0, method = "ML")
s6 <- Arima(train_ts, order = c(0,1,1), seasonal = c(1,0,0), lambda = 0, method = "ML")
s7 <- Arima(train_ts, order = c(1,1,1), seasonal = c(1,0,0), lambda = 0, method = "ML")
s8 <- Arima(train_ts, order = c(2,1,0), seasonal = c(0,0,1), lambda = 0, method = "ML")
s9 <- Arima(train_ts, order = c(0,1,3), seasonal = c(0,0,1), lambda = 0, method = "ML")


# ── 7.4 Semnificativitatea coeficienților ────────────────────────────────────
# Eliminăm modelele cu coeficienți nesemnificativi (p > 0.05)
cat("\n=== MODELE ARIMA – OUTPUT COMPLET ===\n")

for (nm in paste0("m", 1:9)) {
  cat("\n==============================\n")
  cat("MODEL:", nm, "\n")
  cat("==============================\n")
  
  model <- get(nm)
  
  print(summary(model))        # ✔ output complet (AIC, sigma2, etc.)
  
  cat("\n--- COEFTST ---\n")
  print(coeftest(model))       # ✔ coef + p-value
}


# ── 7.5 Tabel criterii informaționale ────────────────────────────────────────
# Hannan-Quinn (HQ) = criteriu intermediar între AIC și BIC
cat("\n=== MODELE SARIMA – OUTPUT COMPLET ===\n")

for (nm in paste0("s", 1:9)) {
  cat("\n==============================\n")
  cat("MODEL:", nm, "\n")
  cat("==============================\n")
  
  model <- get(nm)
  
  print(summary(model))
  
  cat("\n--- COEFTST ---\n")
  print(coeftest(model))
}


cat("\n--- DIAGNOSTIC REZIDUURI ---\n")
checkresiduals(model)

criteria_df <- data.frame(
  Model = all_names,
  AIC   = round(sapply(all_fits, AIC), 3),
  BIC   = round(sapply(all_fits, BIC), 3),
  HQ    = round(sapply(all_fits, HQ),  3),
  LogLik = round(sapply(all_fits, function(m) as.numeric(logLik(m))), 3)
)
criteria_df <- criteria_df[order(criteria_df$AIC), ]
rownames(criteria_df) <- NULL

cat("\n=== CRITERII INFORMAȚIONALE (ordonat după AIC) ===\n")
print(criteria_df)
cat("\nModel cu AIC minim:", criteria_df$Model[1], "\n")
cat("Model cu BIC minim:", criteria_df$Model[which.min(criteria_df$BIC)], "\n")




# lista modele
all_models <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9,
                   s1,s2,s3,s4,s5,s6,s7,s8,s9, m_auto)

all_names <- c(
  "ARIMA(0,1,1)", "ARIMA(1,1,0)", "ARIMA(1,1,1)",
  "ARIMA(0,1,2)", "ARIMA(2,1,1)", "ARIMA(2,1,0)",
  "ARIMA(0,1,3)", "ARIMA(3,1,0)", "ARIMA(3,1,1)",
  "SARIMA(0,1,1)(0,0,1)", "SARIMA(1,1,1)(0,0,1)",
  "SARIMA(1,1,0)(0,0,1)", "SARIMA(0,1,2)(0,0,1)",
  "SARIMA(2,1,1)(0,0,1)", "SARIMA(0,1,1)(1,0,0)",
  "SARIMA(1,1,1)(1,0,0)", "SARIMA(2,1,0)(0,0,1)",
  "SARIMA(0,1,3)(0,0,1)", "SARIMA_auto"
)

# tabel erori
err_list <- lapply(all_models, function(mod){
  fc <- forecast(mod, h = length(test_ts))
  accuracy(fc, test_ts)[2, c("RMSE","MAE","MAPE","MASE")]
})

err_df <- do.call(rbind, err_list)
rownames(err_df) <- all_names

round(err_df, 3)



###############################################################################
# 8. DIAGNOSTIC REZIDUURI
#    Pasul: "Diagnostics & Residuals" → "Model Adequate?" din flowchart
#
#    Un model este ADECVAT dacă reziduurile sunt WHITE NOISE:
#    ✓ Fără autocorelare (Ljung-Box p > 0.05)
#    ✓ Fără efecte ARCH (ARCH-LM p > 0.05)
#    ✓ Medie ≈ 0
#    ✓ Normal distribuite (Jarque-Bera p > 0.05) – dezirabil, nu obligatoriu
###############################################################################

diagnostic_df <- data.frame(
  Model        = all_names,
  LjungBox_p12 = NA_real_,
  LjungBox_p24 = NA_real_,
  ARCH_p12     = NA_real_,
  JarqueBera_p = NA_real_,
  Ncoef        = NA_integer_
)

for (i in seq_along(all_fits)) {
  mod  <- all_fits[[i]]
  k    <- length(coef(mod))
  res  <- residuals(mod)
  lb12 <- Box.test(res, lag = 12, type = "Ljung-Box", fitdf = k)
  lb24 <- Box.test(res, lag = 24, type = "Ljung-Box", fitdf = k)
  arch <- ArchTest(res, lags = 12)
  jb   <- jarque.bera.test(res)
  diagnostic_df$LjungBox_p12[i] <- round(lb12$p.value, 4)
  diagnostic_df$LjungBox_p24[i] <- round(lb24$p.value, 4)
  diagnostic_df$ARCH_p12[i]     <- round(arch$p.value, 4)
  diagnostic_df$JarqueBera_p[i] <- round(jb$p.value,   4)
  diagnostic_df$Ncoef[i]        <- k
}

# Flag: reziduuri ok = LB lag24 > 0.05 (standard academic)
diagnostic_df$LB24_OK  <- ifelse(diagnostic_df$LjungBox_p24 > 0.05, "✓", "✗")
diagnostic_df$ARCH_OK  <- ifelse(diagnostic_df$ARCH_p12    > 0.05, "✓", "✗")
diagnostic_df$Norm_OK  <- ifelse(diagnostic_df$JarqueBera_p > 0.05, "✓", "✗")

cat("\n=== DIAGNOSTIC REZIDUURI – TOATE MODELELE ===\n")
print(diagnostic_df)

# checkresiduals pentru top modele (cele cu AIC cel mai mic)
checkresiduals(all_fits[[which(all_names == criteria_df$Model[1])]])
checkresiduals(all_fits[[which(all_names == criteria_df$Model[2])]])
checkresiduals(m_auto)


###############################################################################
# 9. SELECTAREA MODELULUI OPTIM
#    Pasul: "Select Best Model – AIC / BIC / HQ" din flowchart
#
#    CRITERIU DE SELECȚIE (în ordine de prioritate):
#    1. Toți coeficienții semnificativi (p < 0.05)
#    2. Reziduuri white noise (Ljung-Box lag 24 > 0.05)
#    3. AIC minim dintre modelele care trec criteriile 1 și 2
###############################################################################

# ► AJUSTAȚI m_best după rularea pașilor 7 și 8 de mai sus!
# Implicit: s1 = SARIMA(0,1,1)(0,0,1)[12] – candidat tipic pentru serii lunare
# cu sezonalitate slabă (nsdiffs=0) și un prim lag MA semnificativ

m_best <- m_auto   # ← ÎNLOCUIȚI după inspecția criteria_df + diagnostic_df

cat("\n=== MODEL OPTIM SELECTAT ===\n")
summary(m_best)
coeftest(m_best)

# Diagnostic detaliat – model optim
res_best <- residuals(m_best)

p_r1 <- autoplot(res_best) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Reziduuri în timp – Model optim", x = NULL, y = "Reziduu") +
  theme_bw() + theme(plot.title = element_text(face = "bold"))

p_r2 <- ggAcf(res_best, lag.max = 48) +
  labs(title = "ACF reziduuri – Model optim") +
  theme_bw() + theme(plot.title = element_text(face = "bold"))

p_r3 <- ggPacf(res_best, lag.max = 48) +
  labs(title = "PACF reziduuri – Model optim") +
  theme_bw() + theme(plot.title = element_text(face = "bold"))

p_r4 <- ggAcf(res_best^2, lag.max = 48) +
  labs(title = "ACF reziduuri² – Efecte ARCH") +
  theme_bw() + theme(plot.title = element_text(face = "bold"))

grid.arrange(p_r1, p_r2, p_r3, p_r4, ncol = 2)

# checkresiduals complet (ACF + histogramă + Ljung-Box)
checkresiduals(m_best)

# Ljung-Box la laguri multiple
cat("\n=== LJUNG-BOX DETALIAT – Model optim ===\n")
for (lg in c(1, 2, 6, 12, 18, 24, 36, 48)) {
  bt <- Box.test(res_best, lag = lg, type = "Ljung-Box",
                 fitdf = length(coef(m_best)))
  cat(sprintf("  Lag %2d: p = %.4f  %s\n", lg, bt$p.value,
              ifelse(bt$p.value > 0.05, "✓", "✗ autocorelare")))
}

# ARCH-LM la laguri multiple
cat("\n=== ARCH-LM DETALIAT – Model optim ===\n")
for (lg in c(1, 6, 12, 24)) {
  at <- ArchTest(res_best, lags = lg)
  cat(sprintf("  Lag %2d: p = %.4f  %s\n", lg, at$p.value,
              ifelse(at$p.value > 0.05, "✓", "✗ efecte ARCH")))
}

# Jarque-Bera
cat("\n=== NORMALITATE REZIDUURI (Jarque-Bera) ===\n")
print(jarque.bera.test(res_best))

# Rădăcini inverse (stationaritate + inversabilitate)
# → toate punctele trebuie să fie ÎN INTERIORUL cercului unitate
autoplot(m_best) + theme_bw() +
  labs(title = "Rădăcini inverse – Model optim (stationaritate + inversabilitate)")



###############################################################################
# 9. SELECTAREA MODELULUI OPTIM
###############################################################################

m_best <- m_auto
summary(m_best)
coeftest(m_best)
checkresiduals(m_best)

###############################################################################
# 10. MODEL ETS + PROGNOZE DE BAZĂ PE TEST
###############################################################################

h <- length(test_ts)

# Model SARIMA optim
fc_best <- forecast(
  m_best,
  h = h,
  level = c(80, 95),
  biasadj = TRUE
)

# Model ETS
ets_model <- ets(train_ts, lambda = 0)

summary(ets_model)
checkresiduals(ets_model)

fc_ets <- forecast(
  ets_model,
  h = h,
  level = c(80, 95),
  biasadj = TRUE
)


###############################################################################
# 10. MODEL ETS
###############################################################################
###############################################################################
# COD SUPLIMENTAR – ce lipsește din codul curent
# Adaugă aceste secțiuni DUPĂ pasul 10 (ETS) din codul existent
###############################################################################

###############################################################################
# A. NETEZIRE EXPONENȚIALĂ COMPLETĂ
#    Cerut explicit în cerințe: SES, Holt, Holt-Winters, ETS
###############################################################################

# ── A.1 Simple Exponential Smoothing (SES) ────────────────────────────────────
# Potrivit pentru serii fără trend și fără sezonalitate
# Prognoza = constantă (nivel), nu captează trendul
fc_ses <- ses(train_ts, h = h, level = c(80, 95))
summary(fc_ses)
# alpha aproape de 1 → reacționează rapid la observații recente

# ── A.2 Metoda Holt (trend exponențial) ───────────────────────────────────────
# Potrivit pentru serii cu trend, fără sezonalitate
# Prognoza = linie (nivel + trend)
fc_holt <- holt(train_ts, h = h, level = c(80, 95))
summary(fc_holt)

# Holt cu trend amortizat (damped trend) – mai prudent pentru prognoza lungă
fc_holt_damp <- holt(train_ts, damped = TRUE, h = h, level = c(80, 95))
summary(fc_holt_damp)

# ── A.3 Holt-Winters (trend + sezonalitate) ───────────────────────────────────
# Relevant chiar dacă sezonalitatea este slabă — testăm ambele variante
fc_hw_ad <- hw(train_ts, seasonal = "additive",      h = h, level = c(80, 95))
fc_hw_mu <- hw(train_ts, seasonal = "multiplicative", h = h, level = c(80, 95))
summary(fc_hw_ad)
summary(fc_hw_mu)

# ── A.4 ETS automat (deja în cod, recapitulăm pentru comparație) ──────────────
# ets_model = ETS(A,A,N) — trend aditiv, fără sezonalitate
# Cel mai bun pe test set (RMSE = 5.45)

# ── A.5 Diagnosticul reziduurilor pentru netezire ─────────────────────────────
checkresiduals(fc_ses)
checkresiduals(fc_holt)
checkresiduals(fc_holt_damp)
checkresiduals(fc_hw_ad)
checkresiduals(fc_hw_mu)

# ── A.6 Comparare metrici pe test – toate modelele de netezire ────────────────
cat("\n=== ACURATEȚE NETEZIRE EXPONENȚIALĂ – Test Set ===\n")
acc_netezire <- rbind(
  SES              = accuracy(fc_ses,       test_ts)["Test set", c("RMSE","MAE","MAPE","MASE")],
  Holt             = accuracy(fc_holt,      test_ts)["Test set", c("RMSE","MAE","MAPE","MASE")],
  `Holt damped`    = accuracy(fc_holt_damp, test_ts)["Test set", c("RMSE","MAE","MAPE","MASE")],
  `HW aditiv`      = accuracy(fc_hw_ad,     test_ts)["Test set", c("RMSE","MAE","MAPE","MASE")],
  `HW multiplicativ` = accuracy(fc_hw_mu,   test_ts)["Test set", c("RMSE","MAE","MAPE","MASE")],
  `ETS(A,A,N)`     = accuracy(fc_ets,       test_ts)["Test set", c("RMSE","MAE","MAPE","MASE")]
)
print(round(acc_netezire, 4))

# Grafic comparativ netezire
autoplot(train_ts, series = "Training") +
  autolayer(test_ts,           series = "Realizat (test)") +
  autolayer(fc_ses$mean,       series = "SES") +
  autolayer(fc_holt$mean,      series = "Holt") +
  autolayer(fc_holt_damp$mean, series = "Holt damped") +
  autolayer(fc_hw_ad$mean,     series = "HW aditiv") +
  autolayer(fc_ets$mean,       series = "ETS(A,A,N)") +
  scale_x_continuous(breaks = seq(2007, 2027, 1)) +
  labs(title = "Comparație modele de netezire exponențială – Test Set",
       x = "Timp", y = "Indice (2015=100)", colour = "Model") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")
diagnostic_ets <- function(fit, name){
  
  res <- residuals(fit)
  
  cat("\n==============================\n")
  cat("MODEL:", name, "\n")
  cat("==============================\n")
  
  # 1. Media reziduurilor
  cat("\n--- Media reziduuri ---\n")
  print(mean(res))
  
  # 2. Ljung-Box
  cat("\n--- Ljung-Box ---\n")
  print(Box.test(res, lag = 24, type = "Ljung-Box"))
  
  # 3. ACF
  acf(res, main = paste("ACF reziduuri -", name))
  
  # 4. Varianță / stabilitate
  plot(res, main = paste("Reziduuri în timp -", name))
  abline(h = 0, col = "red", lty = 2)
  
}

diagnostic_ets <- function(fit, name){
  
  res <- residuals(fit)
  
  cat("\n==============================\n")
  cat("MODEL:", name, "\n")
  cat("==============================\n")
  
  # 1. Media reziduurilor
  cat("\n--- Media reziduuri ---\n")
  print(mean(res))
  
  # 2. Ljung-Box
  cat("\n--- Ljung-Box ---\n")
  print(Box.test(res, lag = 24, type = "Ljung-Box"))
  
  # 3. ACF
  acf(res, main = paste("ACF reziduuri -", name))
  
  # 4. Varianță / stabilitate
  plot(res, main = paste("Reziduuri în timp -", name))
  abline(h = 0, col = "red", lty = 2)
  
}

diagnostic_ets(fc_ses, "SES")
diagnostic_ets(fc_holt, "Holt")
diagnostic_ets(fc_holt_damp, "Holt damped")
diagnostic_ets(fc_hw_ad, "HW aditiv")
diagnostic_ets(fc_hw_mu, "HW multiplicativ")
diagnostic_ets(fc_ets, "ETS(A,A,N)")


###############################################################################
# B. ARIMAX CORECTAT – doar cu regresor semnificativ
#    Crisis_2008 = semnificativ (p<0.001)
#    COVID_2020 + Inflation_2022 = nesemnificativi → se exclud
###############################################################################

# ── B.1 ARIMAX cu Crisis_2008 singur ──────────────────────────────────────────
ts2date <- function(x) as.Date(as.yearmon(time(x)))
date_train <- ts2date(train_ts)
date_test  <- ts2date(test_ts)

xreg_train_v2 <- matrix(
  ifelse(date_train >= as.Date("2008-10-01") &
           date_train <= as.Date("2010-12-01"), 1, 0),
  ncol = 1, dimnames = list(NULL, "Crisis_2008")
)

xreg_test_v2 <- matrix(
  ifelse(date_test >= as.Date("2008-10-01") &
           date_test <= as.Date("2010-12-01"), 1, 0),
  ncol = 1, dimnames = list(NULL, "Crisis_2008")
)

m_arimax_v2 <- Arima(
  train_ts,
  order    = c(1, 1, 0),
  seasonal = c(1, 0, 1),
  xreg     = xreg_train_v2,
  lambda   = 0,
  include.drift = TRUE,
  method   = "ML"
)

cat("\n=== ARIMAX v2 – doar Crisis_2008 ===\n")
summary(m_arimax_v2)
coeftest(m_arimax_v2)
# AIC trebuie să fie aproape de −1590 dar cu coeficienți toți semnificativi

checkresiduals(m_arimax_v2)

# Ljung-Box detaliat
for (lg in c(12, 24, 36)) {
  bt <- Box.test(residuals(m_arimax_v2), lag = lg, type = "Ljung-Box",
                 fitdf = length(coef(m_arimax_v2)))
  cat(sprintf("LB lag %2d: p = %.4f %s\n", lg, bt$p.value,
              ifelse(bt$p.value > 0.05, "✓", "✗")))
}

# Prognoze ARIMAX v2 pe test
fc_arimax_v2 <- forecast(m_arimax_v2, xreg = xreg_test_v2,
                         h = h, level = c(80, 95), biasadj = TRUE)

cat("\n=== Acuratețe ARIMAX v2 (Test Set) ===\n")
print(round(accuracy(fc_arimax_v2, test_ts)["Test set",
                                            c("RMSE","MAE","MAPE","MASE")], 4))

# Interpretare economică a coeficientului Crisis_2008:
# Coeficientul pozitiv (~0.009) pe log scale → criza a accelerat creșterea
# chiriilor cu aprox. 0.9% lunar în oct 2008–dec 2010.
# Posibil explicat prin presiunea pe chirii a celor care nu și-au mai permis credite.

###############################################################################
# ARIMAX v2 – doar Crisis_2008 + diagnostic complet pe reziduuri
###############################################################################

library(forecast)
library(lmtest)
library(tseries)
library(FinTS)
library(zoo)

ts2date <- function(x) as.Date(as.yearmon(time(x)))

date_train <- ts2date(train_ts)
date_test  <- ts2date(test_ts)

# Dummy Crisis_2008
xreg_train_v2 <- matrix(
  ifelse(date_train >= as.Date("2008-10-01") &
           date_train <= as.Date("2010-12-01"), 1, 0),
  ncol = 1,
  dimnames = list(NULL, "Crisis_2008")
)

xreg_test_v2 <- matrix(
  ifelse(date_test >= as.Date("2008-10-01") &
           date_test <= as.Date("2010-12-01"), 1, 0),
  ncol = 1,
  dimnames = list(NULL, "Crisis_2008")
)

# Estimare ARIMAX
m_arimax_v2 <- Arima(
  train_ts,
  order = c(1, 1, 0),
  seasonal = c(1, 0, 1),
  xreg = xreg_train_v2,
  lambda = 0,
  include.drift = TRUE,
  method = "ML"
)

cat("\n=== ARIMAX v2 – doar Crisis_2008 ===\n")
print(summary(m_arimax_v2))

cat("\n=== Coeficienți ARIMAX v2 ===\n")
print(coeftest(m_arimax_v2))

###############################################################################
# DIAGNOSTIC REZIDUURI
###############################################################################

res_arimax_v2 <- residuals(m_arimax_v2)

cat("\n=== CHECKRESIDUALS ===\n")
checkresiduals(m_arimax_v2)

cat("\n=== Ljung-Box detaliat ===\n")
for (lg in c(12, 24, 36)) {
  bt <- Box.test(
    res_arimax_v2,
    lag = lg,
    type = "Ljung-Box",
    fitdf = length(coef(m_arimax_v2))
  )
  
  cat(sprintf(
    "LB lag %2d: statistic = %.4f | p = %.4f %s\n",
    lg,
    bt$statistic,
    bt$p.value,
    ifelse(bt$p.value > 0.05, "✓ fără autocorelare", "✗ autocorelare")
  ))
}

cat("\n=== ARCH-LM pe reziduuri ===\n")
for (lg in c(1, 6, 12, 24)) {
  at <- ArchTest(res_arimax_v2, lags = lg)
  
  cat(sprintf(
    "ARCH lag %2d: statistic = %.4f | p = %.4f %s\n",
    lg,
    at$statistic,
    at$p.value,
    ifelse(at$p.value > 0.05, "✓ fără ARCH", "✗ efecte ARCH")
  ))
}

cat("\n=== Jarque-Bera normalitate reziduuri ===\n")
jb <- jarque.bera.test(res_arimax_v2)

cat(sprintf(
  "JB statistic = %.4f | p = %.4f %s\n",
  jb$statistic,
  jb$p.value,
  ifelse(jb$p.value > 0.05, "✓ normalitate", "✗ non-normalitate")
))

###############################################################################
# GRAFICE DIAGNOSTIC
###############################################################################

par(mfrow = c(2, 2))

plot(
  res_arimax_v2,
  main = "Reziduuri ARIMAX v2",
  ylab = "Reziduuri",
  xlab = "Timp"
)
abline(h = 0, col = "red", lty = 2)

acf(
  res_arimax_v2,
  main = "ACF reziduuri ARIMAX v2"
)

hist(
  res_arimax_v2,
  breaks = 30,
  main = "Histogramă reziduuri",
  xlab = "Reziduuri",
  probability = TRUE
)
lines(density(res_arimax_v2), col = "red", lwd = 2)

qqnorm(res_arimax_v2, main = "QQ-plot reziduuri")
qqline(res_arimax_v2, col = "red", lwd = 2)

par(mfrow = c(1, 1))

###############################################################################
# PROGNOZĂ PE TEST + ACURATEȚE
###############################################################################

fc_arimax_v2 <- forecast(
  m_arimax_v2,
  xreg = xreg_test_v2,
  h = length(test_ts),
  level = c(80, 95),
  biasadj = TRUE
)

cat("\n=== Acuratețe ARIMAX v2 pe Test Set ===\n")
acc_arimax_v2 <- accuracy(fc_arimax_v2, test_ts)["Test set",
                                                 c("RMSE", "MAE", "MAPE", "MASE")]
print(round(acc_arimax_v2, 4))

###############################################################################
# GRAFIC PROGNOZĂ VS REALIZAT
###############################################################################

autoplot(fc_arimax_v2) +
  autolayer(test_ts, series = "Valori reale") +
  ggtitle("ARIMAX v2 – Prognoză vs valori reale") +
  xlab("Timp") +
  ylab("Indice chirii") +
  guides(colour = guide_legend(title = "Serie"))

###############################################################################
# ARIMAX cu șocuri macroeconomice:
# Crisis_2008, COVID_2020, Inflation_2022
###############################################################################


ts2date <- function(x) as.Date(as.yearmon(time(x)))

date_train <- ts2date(train_ts)
date_test  <- ts2date(test_ts)

# Dummy-uri TRAINING
xreg_train_v4 <- cbind(
  
  Crisis_2008 = ifelse(
    date_train >= as.Date("2008-10-01") &
      date_train <= as.Date("2010-12-01"), 1, 0
  ),
  
  COVID_2020 = ifelse(
    date_train >= as.Date("2020-03-01") &
      date_train <= as.Date("2021-06-01"), 1, 0
  ),
  
  Inflation_2022 = ifelse(
    date_train >= as.Date("2022-02-01"), 1, 0   # ✔ persistent
  )
)

# Dummy-uri TEST
xreg_test_v4 <- cbind(
  
  Crisis_2008 = ifelse(
    date_test >= as.Date("2008-10-01") &
      date_test <= as.Date("2010-12-01"), 1, 0
  ),
  
  COVID_2020 = ifelse(
    date_test >= as.Date("2020-03-01") &
      date_test <= as.Date("2021-06-01"), 1, 0
  ),
  
  Inflation_2022 = ifelse(
    date_test >= as.Date("2022-02-01"), 1, 0
  )
)

# Model ARIMAX
m_arimax_v4 <- Arima(
  train_ts,
  order = c(1,1,0),
  seasonal = c(1,0,1),
  xreg = xreg_train_v4,
  lambda = 0,
  include.drift = TRUE,
  method = "ML"
)

cat("\n=== ARIMAX v4 – toate șocurile (corect definite) ===\n")
summary(m_arimax_v4)
coeftest(m_arimax_v4)

# Diagnostic reziduuri
checkresiduals(m_arimax_v4)

# Ljung-Box detaliat
for (lg in c(12, 24, 36)) {
  bt <- Box.test(
    residuals(m_arimax_v4),
    lag = lg,
    type = "Ljung-Box",
    fitdf = length(coef(m_arimax_v4))
  )
  
  cat(sprintf(
    "LB lag %2d: p = %.4f %s\n",
    lg,
    bt$p.value,
    ifelse(bt$p.value > 0.05, "✓", "✗")
  ))
}

# Forecast
fc_arimax_v4 <- forecast(
  m_arimax_v4,
  xreg = xreg_test_v4,
  h = length(test_ts),
  level = c(80, 95),
  biasadj = TRUE
)

cat("\n=== Acuratețe ARIMAX v4 (Test Set) ===\n")
print(round(
  accuracy(fc_arimax_v4, test_ts)["Test set", c("RMSE","MAE","MAPE","MASE")],
  4
))


###############################################################################
# DIAGNOSTIC REZIDUURI ARIMAX v2 – output complet ca în R
###############################################################################

res_arimax_v2 <- residuals(m_arimax_v2)

cat("\n==============================\n")
cat("CHECKRESIDUALS ARIMAX v2\n")
cat("==============================\n")
checkresiduals(m_arimax_v2)


cat("\n==============================\n")
cat("LJUNG-BOX TEST – lag 12\n")
cat("==============================\n")
print(Box.test(
  res_arimax_v2,
  lag = 12,
  type = "Ljung-Box",
  fitdf = length(coef(m_arimax_v2))
))


cat("\n==============================\n")
cat("LJUNG-BOX TEST – lag 24\n")
cat("==============================\n")
print(Box.test(
  res_arimax_v2,
  lag = 24,
  type = "Ljung-Box",
  fitdf = length(coef(m_arimax_v2))
))


cat("\n==============================\n")
cat("LJUNG-BOX TEST – lag 36\n")
cat("==============================\n")
print(Box.test(
  res_arimax_v2,
  lag = 36,
  type = "Ljung-Box",
  fitdf = length(coef(m_arimax_v2))
))


cat("\n==============================\n")
cat("ARCH-LM TEST – lag 1\n")
cat("==============================\n")
print(ArchTest(res_arimax_v2, lags = 1))


cat("\n==============================\n")
cat("ARCH-LM TEST – lag 6\n")
cat("==============================\n")
print(ArchTest(res_arimax_v2, lags = 6))


cat("\n==============================\n")
cat("ARCH-LM TEST – lag 12\n")
cat("==============================\n")
print(ArchTest(res_arimax_v2, lags = 12))


cat("\n==============================\n")
cat("ARCH-LM TEST – lag 24\n")
cat("==============================\n")
print(ArchTest(res_arimax_v2, lags = 24))


cat("\n==============================\n")
cat("JARQUE-BERA TEST\n")
cat("==============================\n")
print(jarque.bera.test(res_arimax_v2))


###############################################################################
# C. ARCH/GARCH PE REZIDUURILE SARIMA
#    Motivare: ARCH-LM lag 6-12 semnificativ → heteroscedasticitate condițională
#    Modelul ARIMA-GARCH captează și volatilitatea, nu doar media
###############################################################################

#install.packages("rugarch")  # dacă nu este instalat
library(rugarch)

# ── C.1 Extrage reziduurile standardizate ale modelului optim ─────────────────
res_best <- residuals(m_best)

# Vizualizare: ACF și ACF² pentru a confirma ARCH
par(mfrow = c(1, 2))
acf(res_best^2,  lag.max = 36, main = "ACF reziduuri²  (ARCH)")
pacf(res_best^2, lag.max = 36, main = "PACF reziduuri² (ARCH)")
par(mfrow = c(1, 1))

# ── C.2 Specificarea modelului ARIMA(1,1,0)(1,0,1) + GARCH(1,1) ───────────────
# mean.model = structura ARIMA pentru medie
# variance.model = structura GARCH pentru varianță
spec_garch <- ugarchspec(
  variance.model = list(
    model        = "sGARCH",   # GARCH standard
    garchOrder   = c(1, 1)     # GARCH(1,1)
  ),
  mean.model = list(
    armaOrder    = c(1, 0),    # AR(1) pe seria diferențiată
    include.mean = TRUE
  ),
  distribution.model = "std"   # distribuție t Student (robustă la cozi grele)
)

# Notă: ARIMA cu diferențiere + GARCH necesită diferențierea manuală a seriei
# lucrăm pe log-diff(train_ts) = rent_d1
fit_garch <- ugarchfit(spec = spec_garch, data = rent_d1, solver = "hybrid")
show(fit_garch)

# ── C.3 Diagnostic GARCH ──────────────────────────────────────────────────────
# Testul ARCH-LM pe reziduurile standardizate
cat("\n=== ARCH-LM pe reziduuri standardizate GARCH ===\n")
res_std <- residuals(fit_garch, standardize = TRUE)
for (lg in c(1, 6, 12)) {
  at <- ArchTest(res_std, lags = lg)
  cat(sprintf("Lag %2d: p = %.4f %s\n", lg, at$p.value,
              ifelse(at$p.value > 0.05, "✓", "✗")))
}

# Grafice diagnostic GARCH
plot(fit_garch, which = "all")

# ── C.4 Prognoze GARCH (volatilitate) ────────────────────────────────────────
fc_garch <- ugarchforecast(fit_garch, n.ahead = 24)
# Volatilitatea prognozată (σ_t)
sigma_fc <- sigma(fc_garch)
cat("\n=== Volatilitate prognozată GARCH (σ) – 24 luni ===\n")
print(round(as.numeric(sigma_fc), 6))

# NOTĂ: GARCH modelează volatilitatea reziduurilor, nu nivelul chiriilor.
# Este util pentru cuantificarea riscului/incertitudinii prognozei,
# nu pentru prognoze punctuale ale indexului.



###############################################################################
# DIAGNOSTIC COMPLET REZIDUURI GARCH
###############################################################################

# reziduuri standardizate
res_std <- residuals(fit_garch, standardize = TRUE)

cat("\n==============================\n")
cat("GARCH – REZIDUURI STANDARDIZATE\n")
cat("==============================\n")


# ── 1. Ljung-Box (autocorelare) ─────────────────────────────
cat("\n--- LJUNG-BOX (reziduuri) ---\n")

for (lg in c(12, 24, 36)) {
  print(Box.test(res_std, lag = lg, type = "Ljung-Box"))
}


# ── 2. Ljung-Box pe pătrate (volatilitate) ──────────────────
cat("\n--- LJUNG-BOX (reziduuri pătrate) ---\n")

for (lg in c(12, 24, 36)) {
  print(Box.test(res_std^2, lag = lg, type = "Ljung-Box"))
}


# ── 3. ARCH-LM ──────────────────────────────────────────────
cat("\n--- ARCH-LM TEST ---\n")

for (lg in c(1, 6, 12, 24)) {
  print(ArchTest(res_std, lags = lg))
}


# ── 4. Normalitate ──────────────────────────────────────────
cat("\n--- JARQUE-BERA ---\n")
print(jarque.bera.test(res_std))


# ── 5. Statistici descriptive ───────────────────────────────
cat("\n--- STATISTICI REZIDUURI ---\n")

cat("Mean:", mean(res_std), "\n")
cat("Std Dev:", sd(res_std), "\n")
cat("Skewness:", moments::skewness(res_std), "\n")
cat("Kurtosis:", moments::kurtosis(res_std), "\n")


# ── 6. Grafice ──────────────────────────────────────────────
par(mfrow = c(2,2))

plot(res_std, main="Reziduuri standardizate", col="blue")
abline(h=0, col="red", lty=2)

acf(res_std, main="ACF reziduuri")

acf(res_std^2, main="ACF reziduuri pătrate")

qqnorm(res_std)
qqline(res_std, col="red")

par(mfrow = c(1,1))



###############################################################################
# D. COMPARARE FINALĂ PE TEST SET – TOATE MODELELE
###############################################################################

library(forecast)
library(ggplot2)
library(zoo)

h_test <- length(test_ts)

# Recalculăm prognozele pe TEST ca să evităm obiecte suprascrise
fc_ses_test       <- ses(train_ts, h = h_test, level = c(80,95))
fc_holt_test      <- holt(train_ts, h = h_test, level = c(80,95))
fc_holt_damp_test <- holt(train_ts, damped = TRUE, h = h_test, level = c(80,95))
fc_hw_ad_test     <- hw(train_ts, seasonal = "additive", h = h_test, level = c(80,95))
fc_hw_mu_test     <- hw(train_ts, seasonal = "multiplicative", h = h_test, level = c(80,95))

ets_test_model <- ets(train_ts, lambda = 0)
fc_ets_test <- forecast(ets_test_model, h = h_test, level = c(80,95), biasadj = TRUE)

fc_arima111_test     <- forecast(m3, h = h_test, level = c(80,95), biasadj = TRUE)
fc_arima311_test     <- forecast(m9, h = h_test, level = c(80,95), biasadj = TRUE)
fc_sarima_auto_test  <- forecast(m_auto, h = h_test, level = c(80,95), biasadj = TRUE)

fc_arimax_test <- forecast(
  m_arimax_v2,
  xreg = xreg_test_v2,
  h = h_test,
  level = c(80,95),
  biasadj = TRUE
)

all_fc_final <- list(
  fc_ses_test,
  fc_holt_test,
  fc_holt_damp_test,
  fc_hw_ad_test,
  fc_hw_mu_test,
  fc_ets_test,
  fc_arima111_test,
  fc_arima311_test,
  fc_sarima_auto_test,
  fc_arimax_test
)

all_names_final <- c(
  "SES",
  "Holt",
  "Holt damped",
  "Holt-Winters aditiv",
  "Holt-Winters multiplicativ",
  "ETS(A,A,N)",
  "ARIMA(1,1,1)",
  "ARIMA(3,1,1)",
  "SARIMA auto",
  "ARIMAX + Crisis_2008"
)

# Tabel performanță pe test
tabel_final <- do.call(rbind, lapply(seq_along(all_fc_final), function(i) {
  acc <- accuracy(all_fc_final[[i]], test_ts)["Test set",
                                              c("RMSE","MAE","MAPE","MASE")]
  data.frame(
    Model = all_names_final[i],
    RMSE  = round(acc["RMSE"], 3),
    MAE   = round(acc["MAE"], 3),
    MAPE  = round(acc["MAPE"], 3),
    MASE  = round(acc["MASE"], 3)
  )
}))

tabel_final <- tabel_final[order(tabel_final$RMSE), ]
rownames(tabel_final) <- NULL

cat("\n=== TABEL FINAL – PERFORMANȚĂ PE TEST SET ===\n")
print(tabel_final)

best_model_name <- tabel_final$Model[1]

cat("\nModel cu RMSE minim:", best_model_name, "\n")
cat("Model cu MAPE minim:", tabel_final$Model[which.min(tabel_final$MAPE)], "\n")


###############################################################################
# E. GRAFIC COMPARATIV PE TEST SET – TOATE MODELELE
###############################################################################

date_train <- as.Date(as.yearmon(time(train_ts)))
date_test  <- as.Date(as.yearmon(time(test_ts)))

df_train <- data.frame(
  Data = date_train,
  Valoare = as.numeric(train_ts),
  Serie = "Training"
)

df_test <- data.frame(
  Data = date_test,
  Valoare = as.numeric(test_ts),
  Serie = "Realizat test"
)

df_fc_all <- do.call(rbind, lapply(seq_along(all_fc_final), function(i) {
  data.frame(
    Data = date_test,
    Valoare = as.numeric(all_fc_final[[i]]$mean),
    Serie = all_names_final[i]
  )
}))

ggplot() +
  geom_line(data = df_train,
            aes(x = Data, y = Valoare, color = Serie),
            linewidth = 0.8) +
  geom_line(data = df_test,
            aes(x = Data, y = Valoare, color = Serie),
            linewidth = 1.1) +
  geom_line(data = df_fc_all,
            aes(x = Data, y = Valoare, color = Serie),
            linewidth = 0.75, alpha = 0.85) +
  geom_vline(xintercept = as.Date("2023-01-01"),
             linetype = "dashed", color = "grey40", linewidth = 0.7) +
  labs(
    title = "Comparație prognoze pe setul de test",
    subtitle = "Modele ARIMA, SARIMA, ARIMAX și netezire exponențială",
    x = NULL,
    y = "Indice chirii (2015 = 100)",
    color = "Serie / Model"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13, color = "#005088"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    legend.position = "bottom",
    axis.text.x = element_text(size = 7.5),
    panel.grid.minor = element_blank()
  )


###############################################################################
# F. RE-ESTIMARE MODEL FINAL PE TOATĂ SERIA
###############################################################################

cat("\n=== MODEL FINAL ALES PENTRU PROGNOZĂ ===\n")
cat(best_model_name, "\n")

h_final <- 24

if (best_model_name == "Holt-Winters multiplicativ") {
  final_model <- hw(
    rent_ts,
    seasonal = "multiplicative",
    h = h_final,
    level = c(80,95)
  )
}

if (best_model_name == "Holt-Winters aditiv") {
  final_model <- hw(
    rent_ts,
    seasonal = "additive",
    h = h_final,
    level = c(80,95)
  )
}

if (best_model_name == "ETS(A,A,N)") {
  final_fit <- ets(rent_ts, lambda = 0)
  final_model <- forecast(
    final_fit,
    h = h_final,
    level = c(80,95),
    biasadj = TRUE
  )
}

if (best_model_name == "Holt") {
  final_model <- holt(
    rent_ts,
    h = h_final,
    level = c(80,95)
  )
}

if (best_model_name == "Holt damped") {
  final_model <- holt(
    rent_ts,
    damped = TRUE,
    h = h_final,
    level = c(80,95)
  )
}

if (best_model_name == "SES") {
  final_model <- ses(
    rent_ts,
    h = h_final,
    level = c(80,95)
  )
}

if (best_model_name == "ARIMA(1,1,1)") {
  final_fit <- Arima(
    rent_ts,
    order = c(1,1,1),
    lambda = 0,
    method = "ML"
  )
  final_model <- forecast(
    final_fit,
    h = h_final,
    level = c(80,95),
    biasadj = TRUE
  )
}

if (best_model_name == "ARIMA(3,1,1)") {
  final_fit <- Arima(
    rent_ts,
    order = c(3,1,1),
    lambda = 0,
    method = "ML"
  )
  final_model <- forecast(
    final_fit,
    h = h_final,
    level = c(80,95),
    biasadj = TRUE
  )
}

if (best_model_name == "SARIMA auto") {
  final_fit <- Arima(
    rent_ts,
    order = c(1,1,0),
    seasonal = c(1,0,1),
    lambda = 0,
    include.drift = TRUE,
    method = "ML"
  )
  final_model <- forecast(
    final_fit,
    h = h_final,
    level = c(80,95),
    biasadj = TRUE
  )
}


###############################################################################
# G. GRAFIC FINAL – PROGNOZĂ 24 LUNI ÎN ACELAȘI STIL
###############################################################################

date_fc_final <- seq(as.Date("2026-03-01"), by = "month", length.out = h_final)

df_fc_final <- data.frame(
  Data  = date_fc_final,
  Medie = as.numeric(final_model$mean),
  Lo80  = as.numeric(final_model$lower[,1]),
  Hi80  = as.numeric(final_model$upper[,1]),
  Lo95  = as.numeric(final_model$lower[,2]),
  Hi95  = as.numeric(final_model$upper[,2])
)

df_obs_final <- data.frame(
  Data = as.Date(as.yearmon(time(rent_ts))),
  Val  = as.numeric(rent_ts)
)

ggplot() +
  geom_ribbon(data = df_fc_final,
              aes(x = Data, ymin = Lo95, ymax = Hi95),
              fill = "#005088", alpha = 0.12) +
  geom_ribbon(data = df_fc_final,
              aes(x = Data, ymin = Lo80, ymax = Hi80),
              fill = "#005088", alpha = 0.22) +
  geom_line(data = df_obs_final,
            aes(x = Data, y = Val),
            color = "grey35", linewidth = 0.75) +
  geom_line(data = df_fc_final,
            aes(x = Data, y = Medie),
            color = "#005088", linewidth = 1.1) +
  geom_vline(xintercept = as.Date("2026-03-01"),
             linetype = "dashed", color = "grey50", linewidth = 0.6) +
  annotate("text",
           x = as.Date("2026-03-01"),
           y = max(df_obs_final$Val) * 0.95,
           label = "Start\nprognoză",
           hjust = -0.1, size = 3, color = "grey50") +
  scale_x_date(
    breaks = seq(as.Date("2007-01-01"), as.Date("2029-01-01"), by = "1 year"),
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(
    title = paste("Prognoza finală –", best_model_name),
    subtitle = "Re-estimare pe întreaga serie mai 2007 – feb 2026 | IC 80% și 95%",
    x = NULL,
    y = "Indice chirii (2015 = 100)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13, color = "#005088"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    plot.caption = element_text(size = 8, color = "grey55"),
    axis.text.x = element_text(size = 7.5),
    panel.grid.minor = element_blank()
  )


###############################################################################
# H. TABEL PROGNOZĂ FINALĂ 24 LUNI
###############################################################################

tabel_prognoza_finala <- data.frame(
  Luna     = format(date_fc_final, "%Y-%m"),
  Prognoza = round(as.numeric(final_model$mean), 2),
  IC80_lo  = round(as.numeric(final_model$lower[,1]), 2),
  IC80_hi  = round(as.numeric(final_model$upper[,1]), 2),
  IC95_lo  = round(as.numeric(final_model$lower[,2]), 2),
  IC95_hi  = round(as.numeric(final_model$upper[,2]), 2)
)

cat("\n=== TABEL PROGNOZĂ FINALĂ 24 LUNI ===\n")
print(tabel_prognoza_finala)




###############################################################################
# DIEBOLD-MARIANO TEST – comparație între prognoze
###############################################################################

# erori forecast
e_hw_mu  <- as.numeric(test_ts - fc_hw_mu_test$mean)
e_ets    <- as.numeric(test_ts - fc_ets_test$mean)
e_holt   <- as.numeric(test_ts - fc_holt_test$mean)
e_arima  <- as.numeric(test_ts - fc_arima111_test$mean)
e_sarima <- as.numeric(test_ts - fc_sarima_auto_test$mean)
e_arimax <- as.numeric(test_ts - fc_arimax_test$mean)

# HW multiplicativ ca model de referință
cat("\n=== DIEBOLD-MARIANO: HW multiplicativ vs alte modele ===\n")

dm_ets    <- dm.test(e_hw_mu, e_ets,    h = 1, power = 2)
dm_holt   <- dm.test(e_hw_mu, e_holt,   h = 1, power = 2)
dm_arima  <- dm.test(e_hw_mu, e_arima,  h = 1, power = 2)
dm_sarima <- dm.test(e_hw_mu, e_sarima, h = 1, power = 2)
dm_arimax <- dm.test(e_hw_mu, e_arimax, h = 1, power = 2)

dm_table <- data.frame(
  Comparatie = c(
    "HW multiplicativ vs ETS(A,A,N)",
    "HW multiplicativ vs Holt",
    "HW multiplicativ vs ARIMA(1,1,1)",
    "HW multiplicativ vs SARIMA auto",
    "HW multiplicativ vs ARIMAX + Crisis_2008"
  ),
  DM_statistic = round(c(
    dm_ets$statistic,
    dm_holt$statistic,
    dm_arima$statistic,
    dm_sarima$statistic,
    dm_arimax$statistic
  ), 4),
  p_value = round(c(
    dm_ets$p.value,
    dm_holt$p.value,
    dm_arima$p.value,
    dm_sarima$p.value,
    dm_arimax$p.value
  ), 4)
)

dm_table$Concluzie <- ifelse(
  dm_table$p_value < 0.05,
  "Diferență semnificativă",
  "Diferență nesemnificativă"
)

print(dm_table)