###### 3.2 ######
# Pakker ---------------------------------------------------
install.packages("ordinal")
library(ordinal)
library(dplyr)
library(ggplot2)

## =====================================================
## Opgave 3.2 – CLM med afkast, soliditet og balance
## =====================================================

## 1) Find kolonner til afkast, soliditet og balance
afkast_cols  <- grep("Afkastningsgrad", names(df), value = TRUE)
solid_cols   <- grep("Soliditetsgrad",  names(df), value = TRUE)
balance_cols <- grep("Balance",         names(df), value = TRUE)

## 2) Rens og konvertér til numerisk

# Afkast
df[afkast_cols] <- lapply(df[afkast_cols], function(x) {
  x <- as.character(x)
  x <- gsub(",", ".", x)          # , -> .
  x <- gsub("%", "", x)           # fjern %
  x <- gsub("[^0-9.+-]", "", x)   # fjern alt ikke-tal
  as.numeric(x)
})

# Soliditet
df[solid_cols] <- lapply(df[solid_cols], function(x) {
  x <- as.character(x)
  x <- gsub(",", ".", x)
  x <- gsub("%", "", x)
  x <- gsub("[^0-9.+-]", "", x)
  as.numeric(x)
})

# Balance
df[balance_cols] <- lapply(df[balance_cols], function(x) {
  x <- as.character(x)
  x <- gsub(",", ".", x)
  x <- gsub("[^0-9.+-]", "", x)
  as.numeric(x)
})

## 3) Beregn gennemsnit og log(balance)

df <- df %>%
  mutate(
    gennemsnit_afkast    = rowMeans(select(., all_of(afkast_cols)),  na.rm = TRUE),
    gennemsnit_soliditet = rowMeans(select(., all_of(solid_cols)),   na.rm = TRUE),
    gennemsnit_balance   = rowMeans(select(., all_of(balance_cols)), na.rm = TRUE),
    log_balance          = log(gennemsnit_balance + 1)
  )

## 4) Lav datasæt til CLM og SKALÉR forklarende variable

df_clm_full <- df %>%
  select(
    laane_muligheder,
    gennemsnit_afkast,
    gennemsnit_soliditet,
    log_balance
  ) %>%
  filter(
    !is.na(laane_muligheder),
    !is.na(gennemsnit_afkast),
    !is.na(gennemsnit_soliditet),
    !is.na(log_balance)
  ) %>%
  mutate(
    laane_muligheder = factor(
      laane_muligheder,
      levels = c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode"),
      ordered = TRUE
    ),
    afkast_s    = as.numeric(scale(gennemsnit_afkast)),
    soliditet_s = as.numeric(scale(gennemsnit_soliditet)),
    logbal_s    = as.numeric(scale(log_balance))
  )

# (valgfrit tjek)
# nrow(df_clm_full)
# summary(df_clm_full[, c("afkast_s", "soliditet_s", "logbal_s")])

## 5) Kør CLM med alle tre forklarende variable (skalerede)

model_full <- clm(
  laane_muligheder ~ afkast_s + soliditet_s + logbal_s,
  data = df_clm_full,
  link = "logit"
)

summary(model_full)



############################################################
## Opgave 3.2 – CLM med Afkast, Soliditet, Likviditet,
##              Balance og Gældsforpligtelser
############################################################

library(dplyr)
library(ordinal)

## 1) Start fra Regnskaber og lav låne-variablen  -----------

df <- Regnskaber %>%
  mutate(
    laan_raw = `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`
  ) %>%
  filter(!is.na(laan_raw),
         laan_raw != "Ved ikke")   # fjern evt. "Ved ikke"

# Ordnet faktor med 5 niveauer (juster hvis I kun bruger 3)
df$laane_muligheder <- factor(
  df$laan_raw,
  levels = c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode"),
  ordered = TRUE
)

## 2) Find kolonner til de 5 nøgletal ------------------------

afkast_cols  <- grep("Afkastningsgrad",     names(df), value = TRUE)
solid_cols   <- grep("Soliditetsgrad",      names(df), value = TRUE)
balance_cols <- grep("^Balance [0-9]{4}",   names(df), value = TRUE)
likvid_cols  <- grep("Likviditetsgrad",     names(df), value = TRUE)
gaeld_cols   <- grep("gæld|Gæld|Gaeld",     names(df), value = TRUE)

## 3) Hjælpefunktion: rens tal (%, komma, tusindtalsseparatorer)

clean_numeric <- function(x){
  x <- as.character(x)
  x <- gsub(",", ".", x)
  x <- gsub("%", "", x)
  x <- gsub("[^0-9.+-]", "", x)
  as.numeric(x)
}

## 4) Rens alle nøgletalskolonner ---------------------------

df[afkast_cols]  <- lapply(df[afkast_cols],  clean_numeric)
df[solid_cols]   <- lapply(df[solid_cols],   clean_numeric)
df[balance_cols] <- lapply(df[balance_cols], clean_numeric)
df[likvid_cols]  <- lapply(df[likvid_cols],  clean_numeric)
df[gaeld_cols]   <- lapply(df[gaeld_cols],   clean_numeric)

## 5) Lav gennemsnit og log-transformering ------------------

df <- df %>%
  mutate(
    gennemsnit_afkast    = rowMeans(select(., all_of(afkast_cols)),   na.rm = TRUE),
    gennemsnit_soliditet = rowMeans(select(., all_of(solid_cols)),    na.rm = TRUE),
    gennemsnit_balance   = rowMeans(select(., all_of(balance_cols)),  na.rm = TRUE),
    gennemsnit_likvid    = rowMeans(select(., all_of(likvid_cols)),   na.rm = TRUE),
    gennemsnit_gaeld     = rowMeans(select(., all_of(gaeld_cols)),    na.rm = TRUE),
    
    log_balance = log(gennemsnit_balance + 1),
    log_gaeld   = log(gennemsnit_gaeld   + 1)
  )

## 6) Datasæt til CLM + skalering af forklarende variable ---

df_clm_full <- df %>%
  select(
    laane_muligheder,
    gennemsnit_afkast,
    gennemsnit_soliditet,
    gennemsnit_likvid,
    log_balance,
    log_gaeld
  ) %>%
  filter(
    !is.na(laane_muligheder),
    complete.cases(.)
  ) %>%
  mutate(
    # sørg for at svarvariablen ER ordnet faktor
    laane_muligheder = factor(
      laane_muligheder,
      levels = c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode"),
      ordered = TRUE
    ),
    
    # skaler alle forklarende variable (mean=0, sd=1)
    afkast_s    = as.numeric(scale(gennemsnit_afkast)),
    soliditet_s = as.numeric(scale(gennemsnit_soliditet)),
    likvid_s    = as.numeric(scale(gennemsnit_likvid)),
    logbal_s    = as.numeric(scale(log_balance)),
    loggaeld_s  = as.numeric(scale(log_gaeld))
  )

# (valgfrit tjek)
# nrow(df_clm_full)
# table(df_clm_full$laane_muligheder)
# summary(df_clm_full[, c("afkast_s","soliditet_s","likvid_s","logbal_s","loggaeld_s")])

############################################################
## 7) CLM – hver variabel for sig
############################################################

model_afkast <- clm(
  laane_muligheder ~ afkast_s,
  data = df_clm_full,
  link = "logit"
)
summary(model_afkast)

model_soliditet <- clm(
  laane_muligheder ~ soliditet_s,
  data = df_clm_full,
  link = "logit"
)
summary(model_soliditet)

model_likviditet <- clm(
  laane_muligheder ~ likvid_s,
  data = df_clm_full,
  link = "logit"
)
summary(model_likviditet)

model_balance <- clm(
  laane_muligheder ~ logbal_s,
  data = df_clm_full,
  link = "logit"
)
summary(model_balance)

model_gaeld <- clm(
  laane_muligheder ~ loggaeld_s,
  data = df_clm_full,
  link = "logit"
)
summary(model_gaeld)

############################################################
## 8) CLM – alle fem forklarende variable samlet
############################################################

model_full5 <- clm(
  laane_muligheder ~ afkast_s + soliditet_s + likvid_s + logbal_s + loggaeld_s,
  data = df_clm_full,
  link = "logit"
)

summary(model_full5)
############################################################

######################## GGPLOTS ################33
############################################################
# Flotte DI-agtige grafer for alle 5 nøgletal
# Datasæt: df_clm_full fra opgave 3.2
############################################################

library(dplyr)
library(ggplot2)
library(tidyr)

# DI-farver
col_bad    <- "grey75"
col_neutral<- "grey40"
col_good   <- "#0099E6"

# Ens funktion til DI-plot ---------------------------------------------
di_plot <- function(data, var, title, ylab) {
  data %>%
    group_by(laane_muligheder) %>%
    summarise(mean_val = mean(.data[[var]], na.rm = TRUE)) %>%
    ggplot(aes(x = laane_muligheder, y = mean_val, fill = laane_muligheder)) +
    geom_col() +
    geom_text(aes(label = round(mean_val, 2)),
              vjust = -0.3, size = 4) +
    scale_fill_manual(values = c("Dårlige" = col_bad,
                                 "Neutrale" = col_neutral,
                                 "Gode"     = col_good)) +
    labs(
      title = title,
      x = "",
      y = ylab
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

# 1) Afkastningsgrad ----------------------------------------------------
plot_afkast_DI <- di_plot(
  df_clm_full, 
  "gennemsnit_afkast",
  "Afkastningsgrad og vurdering af lånemuligheder",
  "Gennemsnitlig afkastningsgrad (%)"
)

# 2) Soliditetsgrad ------------------------------------------------------
plot_solid_DI <- di_plot(
  df_clm_full,
  "gennemsnit_soliditet",
  "Soliditetsgrad og vurdering af lånemuligheder",
  "Gennemsnitlig soliditetsgrad (%)"
)

# 3) Likviditetsgrad ----------------------------------------------------
plot_likvid_DI <- di_plot(
  df_clm_full,
  "gennemsnit_likvid",
  "Likviditetsgrad og vurdering af lånemuligheder",
  "Gennemsnitlig likviditetsgrad (%)"
)

# 4) log(Balance) -------------------------------------------------------
plot_balance_DI <- di_plot(
  df_clm_full,
  "log_balance",
  "Virksomhedens størrelse og vurdering af lånemuligheder",
  "Gennemsnitlig log(balance)"
)

# 5) log(Gæld) ----------------------------------------------------------
plot_gaeld_DI <- di_plot(
  df_clm_full,
  "log_gaeld",
  "Gældsforpligtelser og vurdering af lånemuligheder",
  "Gennemsnitlig log(gæld)"
)

# Print enkeltvis
plot_afkast_DI
plot_solid_DI
plot_likvid_DI
plot_balance_DI
plot_gaeld_DI

# --------------------------------------------------------------
# --------------------------------------------------------------
# BONUS: ÉN samlet DI-style facet-figur
# --------------------------------------------------------------
plot_long <- df_clm_full %>%
  group_by(laane_muligheder) %>%
  summarise(
    Afkastningsgrad   = mean(gennemsnit_afkast, na.rm = TRUE),
    Soliditetsgrad    = mean(gennemsnit_soliditet, na.rm = TRUE),
    Likviditetsgrad   = mean(gennemsnit_likvid, na.rm = TRUE),
    `log(Balance)`    = mean(log_balance, na.rm = TRUE),
    `log(Gæld)`       = mean(log_gaeld, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -laane_muligheder,
    names_to = "noegletal",
    values_to = "mean_val"
  )

ggplot(plot_long, aes(x = laane_muligheder, y = mean_val, fill = laane_muligheder)) +
  geom_col() +
  geom_text(aes(label = round(mean_val, 2)), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("Dårlige" = col_bad,
                               "Neutrale" = col_neutral,
                               "Gode"     = col_good)) +
  facet_wrap(~ noegletal, scales = "free_y") +
  labs(
    title = "Regnskabstal og vurdering af lånemuligheder – DI-style",
    x = "Svarmulighed",
    y = "Gennemsnit pr. kategori"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )
      
      