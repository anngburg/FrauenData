library(tidyverse)

Frauen_data <- read_csv("Frauen_data.csv")


Frauen_data_long_Unt <- Frauen_data %>% select(Case, Unt_offen_AnderePersonPriv:Unt_offen_kA) %>% pivot_longer(cols = c(Unt_offen_AnderePersonPriv:Unt_offen_kA),
                                                                                                           names_to = "Unterstützung_long", values_to = "Value") %>% na.omit(.)

Unt_Zuordnung <- c("Unt_offen_AnderePersonPriv" = "Andere Person (privat)",	
                  "Unt_offen_InstitutionelleProfHilfe" = "Institutionelle/prof. Hilfsangebote",	
                  "Unt_offen_StarkeSymptome" = "Starke Symptome",
                  "Unt_offen_negativeAuswiArbeit" = "Negative Auswirkungen auf Arbeit/Studium/etc.",
                  "Unt_offen_einschneidendeEreignis" = "Einschneidende Lebensereignisse",	
                  "Unt_offen_Zusammenbruch" = "Zusammenbruch",
                  "Unt_offen_Suizidalität" = "Suizidalität",
                  "Unt_offen_Sonstiges" = "Sonstiges",
                  "Unt_offen_Keine" = "Keine Unterstützung",
                  "Unt_offen_kA" = "Keine Angabe")


Frauen_data_long_Unt <- Frauen_data_long_Unt %>%
  mutate(Unterstützung_long = factor(Unterstützung_long, levels = rev(names(Unt_Zuordnung))))

BarPlot_Unterstützung_offen <- ggplot(Frauen_data_long_Unt, aes(y = Unterstützung_long)) +
  geom_bar(color = "skyblue4", fill = "skyblue4") +
  geom_text(stat  = "count",
            aes(label = paste0(round((after_stat(count) / 179) * 100, 1), "%")),
            hjust = 1.1, size = 7, color = "white") +
  geom_text(stat  = "count",
            aes(label = after_stat(count)),
            hjust = -0.25, size = 9, color = "grey30") +
  scale_y_discrete(labels = Unt_Zuordnung) +
  theme_minimal() +
  theme(plot.title   = element_text(size = 34, hjust = 0.5, color = "grey20", face = "bold"),
        axis.text.x  = element_text(size = 30, color = "grey30"),
        axis.text.y  = element_text(size = 30, color = "grey30"),
        axis.title.x = element_text(size = 30, color = "grey30"),
        legend.title = element_text(size = 28, color = "grey30", face = "bold"),
        legend.key.spacing.y = unit(10, "pt"),
        legend.text  = element_text(size = 28, color = "grey30")) +
  labs(title = "Offene Abfrage der erlebten Unterstützung",
       x     = "Absolute Häufigkeit",
       y     = "")

#print(BarPlot_Unterstützung_offen)
ggsave("BarPlot_Unterstützung_offen.svg", BarPlot_Unterstützung_offen, width = 35, height = 13)
