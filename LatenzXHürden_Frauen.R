library(tidyverse)

Frauen_data <- read_csv("Frauen_data.csv")

HüLZ_data_long <- Frauen_data %>% pivot_longer(cols = c(Hü_offen_WartezeitTherapeut:Hü_offen_kA), names_to = "Hürden", values_to = "Value"
                                               ) %>% select(Hürden, Value, Latenz_Gesamt) %>% na.omit(.) %>% select(-Value)

Hü_Zuordnung <- c("Hü_offen_WartezeitTherapeut" = "Wartezeit/ Therapeut finden",	
                  "Hü_offen_Kontaktaufnahme" = "Kontakt- aufnahme",	
                  "Hü_offen_SchamAngst" = "Scham/ Angst",
                  "Hü_offen_ErnstGenommenWerden" = "Nicht ernst genommen werden (durch Fachpersonal)",
                  "Hü_offen_EingestehenErkennenHilfe" = "Eingestehen/ Erkennen, dass man krank ist bzw. Hilfe braucht",	
                  "Hü_offen_DepressiveSymptome" = "Depressive Symptome",
                  "Hü_offen_schlechteTherapieerf" = "Negative Behandlungs- erfahrungen",
                  "Hü_offen_negativeAuswirkungen" = "Negative Auswir- kungen",
                  "Hü_offen_Stigma" = "Stigma- tisierung",
                  "Hü_offen_SorgePlatzwegnehmen" = "Sorge, jemandem den Platz wegzunehmen",
                  "Hü_offen_FehlendesWissenInfo" = "Fehlende(s) Wissen/ Informa- tionen",
                  "Hü_offen_keineZeit" = "Keine Zeit",
                  "Hü_offen_KeinVertraueninHilfe" = "Kein Vertrauen in Hilfe",
                  "Hü_offen_DiagnoseUrsache" = "Diagnose bekommen",
                  "Hü_offen_Sonstiges" = "Sonstiges",
                  "Hü_offen_KeineHürde" = "Keine Hürden",
                  "Hü_offen_kA" = "Keine Angabe")

HüLZ_data_long <- HüLZ_data_long %>%
  mutate(Hürden = factor(Hürden, levels = names(Hü_Zuordnung)))

#kombinierten Plot erstellen
HürdenxLatenz <- ggplot() +
  geom_hline(yintercept = 166 / 17, color = "skyblue3", linewidth = 1, alpha = 0.5, linetype = 3) +
  geom_hline(yintercept = 44 / 15, color = "skyblue3", linewidth = 1, alpha = 0.5)+
  # Bar graph for counts on primary y-axis
  geom_bar(data = HüLZ_data_long, aes(x = Hürden), stat = "count", fill = "pink3", alpha = 0.5) +
  # Boxplots on secondary y-axis
  geom_boxplot(data = HüLZ_data_long, aes(x = Hürden, y = Latenz_Gesamt / (800/90)), fill = "lightblue", color = "skyblue4", width=0.4, linewidth = 0.8) + #definiert die Boxplots
  stat_boxplot(data = HüLZ_data_long, aes(x = Hürden, y = Latenz_Gesamt / (800/90)), geom = "errorbar", color = "skyblue4", width=0.2, linewidth = 0.8) + #fügt errorbars hinzu
  stat_summary(data = HüLZ_data_long, aes(x = Hürden, y = Latenz_Gesamt / (800/90)), fun.y=mean, geom="point", shape=1, size=3, color="skyblue4") + #fügt einen Kreis beim Mittelwert hinzu
  # Adjust scales
  scale_x_discrete(labels = function(x) str_wrap(Hü_Zuordnung[x], width = 12)) +
  scale_y_continuous(
    name = "Häufigkeit der Nennungen", breaks=seq(0, 100, by = 10),
    limits = c(0, 90),
    sec.axis = sec_axis(~ . * (800/90), name="Gesamtlatenz in Wochen", breaks=seq(0, 800, by = 200)) #definiert die zwei y-Achsen
  ) +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 32, face = "bold"),
        axis.text.x = element_text(size=24, angle = 0, hjust = 0.5,margin=margin(t=-15), color="grey30"),
        axis.text.y.left = element_text(size=26, color="palevioletred4"),
        axis.text.y.right = element_text(size=26, color="skyblue4"),
        axis.title.y.left = element_text(size = 28, color="palevioletred4"),
        axis.title.y.right = element_text(size = 28, color="skyblue4"),
        axis.ticks.x = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.background = element_rect(fill = "white"),  # Background color of entire plot
        panel.background = element_rect(fill = "white"), # Background color of plotting area
        panel.grid.major.y=element_line(color="gray90"), # Major horizontal grid lines
        panel.grid.minor.y=element_line(color="gray90"), # Minor horizontal grid lines
        panel.grid.major.x=element_blank(),              # Remove major vertical grid lines
        panel.grid.minor.x=element_blank()               # Remove minor vertical grid lines
  ) +
  labs(title = "Gesamtlatenz x Hürden", x = "")

HürdenxLatenz
ggsave("HürdenxLatenz.svg", HürdenxLatenz, width = 38, height = 15)
