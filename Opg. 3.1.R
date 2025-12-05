library(ordinal)
library(dplyr)
library(ggplot2)

## =========================
## Data
## =========================
df <- as.data.frame(Regnskaber)

## =========================
## Opgave 3.1 – fordeling af svar
## =========================

# 1) Kopiér spørgsmålet over i en ny variabel
df <- df %>%
  mutate(
    laan_raw = `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`
  )

# 2) Slå "Dårlig" og "Dårlige" sammen (hvis begge findes)
df <- df %>%
  mutate(
    laan_raw = ifelse(laan_raw == "Dårlig", "Dårlige", laan_raw)
  )

# 3) Fjern "Ved ikke"
df <- df %>%
  filter(!is.na(laan_raw),
         laan_raw != "Ved ikke")

# 4) Lav ordnet faktor til analyse
df <- df %>%
  mutate(
    laane_muligheder = factor(
      laan_raw,
      levels = c("Dårlige", "Neutrale", "Gode"),
      ordered = TRUE
    )
  )

# 5) Plot fordeling (Opgave 3.1)
plot_data <- df %>%
  count(laane_muligheder, name = "n") %>%
  mutate(procent = n / sum(n) * 100)

ggplot(plot_data, aes(x = laane_muligheder, y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(procent, 1), "%")),
            vjust = -0.5, size = 5) +
  labs(
    title = "Fordeling af svar: Muligheder for at låne penge til virksomheden",
    x     = "Svarmulighed",
    y     = "Antal besvarelser"
  ) +
  theme_minimal()
