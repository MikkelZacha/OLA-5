
install.packages("ordinal")   # kun første gang
library(ordinal)


# Vi laver 3 dataframes til at vise de virksomheder der har svaret hhv. God, Dårlig eller Neutral

df_gode     <- Regnskaber[Regnskaber$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Gode", ]
df_neutral     <- Regnskaber[Regnskaber$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Neutrale", ]
df_daarlige     <- Regnskaber[Regnskaber$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Dårlige", ]

Regnskaber$laane_muligheder <- as.ordered(Regnskaber$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)

library(dplyr)

# 1. Vælg de kolonner hvor navnet indeholder "Afkastningsgrad"
afkast_cols <- grep("Afkastningsgrad", names(Regnskaber), value = TRUE)

Regnskaber[afkast_cols] <- lapply(Regnskaber[afkast_cols], function(x) {
  # fjern procenttegn og andet "støj"
  x <- gsub(",", ".", x)         # konverter komma til punktum
  x <- gsub("%", "", x)          # fjern procent
  x <- gsub("[^0-9.+-]", "", x)  # fjern alt der ikke er et tal
  
  as.numeric(x)
})

# 2. Lav ny df med gennemsnit og laane_muligheder
df_clm <- Regnskaber %>%
  mutate(
    gennemsnit_afkast = rowMeans(
      select(., all_of(afkast_cols)), 
      na.rm = TRUE                     # ignorér NA'er i gennemsnit
    )
  ) %>%
  select(laane_muligheder, gennemsnit_afkast)

str(Regnskaber[afkast_cols])

df_clm <- Regnskaber %>%
  mutate(
    gennemsnit_afkast = rowMeans(
      select(., all_of(afkast_cols)),
      na.rm = TRUE
    )
  ) %>%
  select(laane_muligheder, gennemsnit_afkast)

model <- clm(
  laane_muligheder ~ gennemsnit_afkast,
  data = df_clm,
  link = "logit"   # standard – kan udelades
)

summary(model)
