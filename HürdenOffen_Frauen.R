library(tidyverse)

Frauen_data <- read_csv("Frauen_data.csv")


Frauen_data_long <- Frauen_data %>% select(Case, Hü_offen_WartezeitTherapeut:Hü_offen_kA) %>% pivot_longer(cols = c(Hü_offen_WartezeitTherapeut:Hü_offen_kA),
                                                 names_to = "Hürde_long", values_to = "Value") %>% na.omit(.)

Hü_Zuordnung <- c("Hü_offen_WartezeitTherapeut" = "Wartezeit/Therapeut finden",	
               "Hü_offen_Kontaktaufnahme" = "Kontaktaufnahme",	
               "Hü_offen_SchamAngst" = "Scham/Angst",
               "Hü_offen_ErnstGenommenWerden" = "Nicht ernst genommen werden (durch Fachpersonal)",
               "Hü_offen_EingestehenErkennenHilfe" = "Eingestehen/Erkennen, dass man krank ist bzw. Hilfe braucht",	
               "Hü_offen_DepressiveSymptome" = "Depressive Symptome",
               "Hü_offen_schlechteTherapieerf" = "Negative Behandlungserfahrungen",
               "Hü_offen_negativeAuswirkungen" = "Negative Auswirkungen",
               "Hü_offen_Stigma" = "Stigmatisierung",
               "Hü_offen_SorgePlatzwegnehmen" = "Sorge, jemandem den Platz wegzunehmen",
               "Hü_offen_FehlendesWissenInfo" = "Fehlende(s) Wissen/Informationen",
               "Hü_offen_keineZeit" = "Keine Zeit",
               "Hü_offen_KeinVertraueninHilfe" = "Kein Vertrauen in Hilfe",
               "Hü_offen_DiagnoseUrsache" = "Diagnose bekommen",
               "Hü_offen_Sonstiges" = "Sonstiges",
               "Hü_offen_KeineHürde" = "Keine Hürden",
               "Hü_offen_kA" = "Keine Angabe")


Frauen_data_long <- Frauen_data_long %>%
  mutate(Hürde_long = factor(Hürde_long, levels = rev(names(Zuordnung))))

BarPlot_Hürden_offen <- ggplot(Frauen_data_long, aes(y = Hürde_long)) +
  geom_bar(color = "skyblue3", fill = "skyblue3") +
  geom_text(stat  = "count",
            aes(label = paste0(round((after_stat(count) / 179) * 100, 1), "%")),
            hjust = 1.1, size = 7, color = "white") +
  geom_text(stat  = "count",
            aes(label = after_stat(count)),
            hjust = -0.25, size = 9, color = "grey30") +
  scale_y_discrete(labels = Zuordnung) +
  theme_minimal() +
  theme(plot.title   = element_text(size = 34, hjust = 0.5, color = "grey20", face = "bold"),
        axis.text.x  = element_text(size = 30, color = "grey30"),
        axis.text.y  = element_text(size = 30, color = "grey30"),
        axis.title.x = element_text(size = 30, color = "grey30"),
        legend.title = element_text(size = 28, color = "grey30", face = "bold"),
        legend.key.spacing.y = unit(10, "pt"),
        legend.text  = element_text(size = 28, color = "grey30")) +
  labs(title = "Offene Abfrage der erlebten Hürden",
       x     = "Absolute Häufigkeit",
       y     = "")

#print(BarPlot_Hürden_offen)
ggsave("BarPlot_Hürden_offen.svg", BarPlot_Hürden_offen, width = 35, height = 18)
