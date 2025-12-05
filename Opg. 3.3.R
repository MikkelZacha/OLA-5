## =====================================================
## Opgave 3.3 – Illustration af forklarende variabel
## Balance vs. vurdering af lånemuligheder (3 kategorier)
## =====================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)   # til parse_number

# 1. Start med en kopi af datasættet, så vi ikke rører de andre opgaver
df3 <- as.data.frame(Regnskaber)

# 2. Rens svarvariablen og slå sammen til 3 kategorier:
#    "Dårlige", "Neutrale", "Gode"
df3 <- df3 %>%
  mutate(
    svar = `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`,
    
    svar3 = case_when(
      svar %in% c("Gode", "Meget gode")           ~ "Gode",
      svar %in% c("Neutrale")                    ~ "Neutrale",
      svar %in% c("Dårlige", "Meget dårlige")    ~ "Dårlige",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(svar3)) %>%
  mutate(
    svar3 = factor(svar3,
                   levels = c("Dårlige", "Neutrale", "Gode"),
                   ordered = TRUE)
  )

# 3. Find og rens balance-kolonner (2014–2018/2016–2020 osv.)
balance_cols <- grep("Balance.20", names(df3), value = TRUE)

df3[balance_cols] <- lapply(df3[balance_cols], function(x) {
  x <- as.character(x)
  x <- gsub(",", ".", x)          # komma -> punktum
  x <- gsub("[^0-9.+-]", "", x)   # fjern alt der ikke er tal
  as.numeric(x)
})

# 4. Wide -> long: én række pr. år pr. virksomhed, og log-transformér balance
df3_long <- df3 %>%
  pivot_longer(
    cols      = all_of(balance_cols),
    names_to  = "year_label",
    values_to = "balance"
  ) %>%
  mutate(
    year        = parse_number(year_label),   # fx "Balance.2016.." -> 2016
    log_balance = log(balance + 1)
  ) %>%
  filter(!is.na(log_balance))

# 5. Beregn gennemsnitlig log(balance) pr. år og vurderingsgruppe
df_plot3 <- df3_long %>%
  group_by(year, svar3) %>%
  summarise(
    mean_log_balance = mean(log_balance, na.rm = TRUE),
    .groups = "drop"
  )

# 6. Plot – lignende figuren i artiklen
ggplot(df_plot3,
       aes(x = factor(year),
           y = mean_log_balance,
           fill = svar3)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(
    values = c(
      "Gode"     = "#0099E6",  # blå
      "Neutrale" = "grey40",
      "Dårlige"  = "grey75"
    )
  ) +
  labs(
    title    = "Størrelsen på aktiverne har betydning for lånemulighederne",
    subtitle = "Gennemsnitlig log(balance) efter vurdering af lånemuligheder",
    x        = "",
    y        = "Log(Balance)",
    fill     = "Vurdering",
    caption  = "Kilde: Regnskaber og egne beregninger."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x     = element_text(size = 11),
    plot.caption    = element_text(hjust = 0)
  )
