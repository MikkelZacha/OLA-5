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
