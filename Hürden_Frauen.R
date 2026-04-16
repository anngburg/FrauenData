library(forcats)
library(tidyverse)
library(psych)
library(summarytools)
library(xlsx)

Frauen_data <- read_csv("Frauen_data.csv")


Frauen_data <- Frauen_data %>% mutate(Hürden_Anzahl = ifelse(Hürden_Anzahl > 0, Hürden_Anzahl, 0))

####### Hürden - Anzahl #######
plot_AnzahlHürden <- ggplot(Frauen_data, aes(x = Hürden_Anzahl)) +
  geom_bar(fill = "steelblue1")+
  geom_text(stat='count', aes(label=paste0(round((..count.. / 178) * 100, 1), "%")), vjust = 1.3, size=11, color = "white")+
  geom_text(stat='count', aes(label=..count..), vjust = -0.4, size=12, color = "grey30")+
  theme_minimal()+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8))+
  theme(plot.title = element_text(size = 40, hjust = 0.5, color = "grey20", face = "bold"),
        axis.text.x = element_text(size = 35, color = "grey30"),
        axis.title.x = element_text(size = 35, color = "grey30"),
        axis.text.y = element_text(size = 35, color = "grey30"),
        axis.title.y = element_text(size = 35, color = "grey30"))+
  labs(title = "Anzahl ausgewählter Hürden", x = "", y = "Absolute Häufigkeit")


ggsave("plot_AnzahlHürden.svg", plot_AnzahlHürden, width = 25, height = 14)

plot_AnzahlHürden_Kontakt <- ggplot(Frauen_data, aes(x = Hürden_Anzahl, fill = as.factor(Kontakt))) +
  geom_bar()+
  scale_fill_discrete(labels=c("1" = "Kontakt aufgenommen\nzu Hilfsangeboten", "2" = "keine Unterstützung\naufgesucht")) +
  theme_minimal()+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8))+
  theme(plot.title = element_text(size = 40, hjust = 0.5, color = "grey20", face = "bold"),
        axis.text.x = element_text(size = 35, color = "grey30"),
        axis.title.x = element_text(size = 35, color = "grey30"),
        axis.text.y = element_text(size = 35, color = "grey30"),
        axis.title.y = element_text(size = 35, color = "grey30"),
        legend.title =element_text(size = 35, color = "grey30", face = "bold"),
        legend.key.spacing.y = unit(10, "pt"),
        legend.text = element_text(size = 35, color = "grey30"))+
  labs(title = "Anzahl ausgewählter Hürden", x = "", y = "Absolute Häufigkeit", fill = "Gruppenzugehörigkeit")


ggsave("plot_AnzahlHürden_Kontakt.svg", plot_AnzahlHürden_Kontakt, width = 30, height = 14)

mean(Frauen_data$Hürden_Anzahl)
sd(Frauen_data$Hürden_Anzahl)
median(Frauen_data$Hürden_Anzahl)

####### Daten vorbereiten für Ranking Auswertung #######

Frauen_data_nurHürden <- Frauen_data %>% select(Hürden_alleineSchaffen, 
                                            Hürden_keineZeit,
                                            Hürden_Antriebslos,
                                            Hürden_AngstKonsequenz,
                                            Hürden_keinVertrauen,
                                            Hürden_finanzielleBarrieren,
                                            Hürden_DruckPrivat,
                                            Hürden_DruckBeruf,
                                            Hürden_Substanzkonsum,
                                            Hürden_geringerSelbstwert,
                                            Hürden_keineUnterstützung,
                                            Hürden_StigmaUmfeld,
                                            Hürden_negErfahrungen,
                                            Hürden_negErwartungen,
                                            Hürden_Unwissenheit,
                                            Hürden_Wartezeiten,
                                            Hürden_keinProblembewusstsein,
                                            Hürden_keinVerständnis,
                                            Hürden_Scham,
                                            Hürden_TradGeschlechterrolle) %>% rename(
                                              `Überzeugung, es allein zu schaffen`                          = Hürden_alleineSchaffen,
                                              `Keine Zeit aufgrund von Verpflichtungen`                     = Hürden_keineZeit,
                                              `Antriebslosigkeit`                                           = Hürden_Antriebslos,
                                              `Angst vor möglichen Konsequenzen`                            = Hürden_AngstKonsequenz,
                                              `Fehlendes Vertrauen in Bezugspersonen`                       = Hürden_keinVertrauen,
                                              `Finanzielle Barrieren`                                       = Hürden_finanzielleBarrieren,
                                              `Druck, nicht ausfallen zu können (privat)`                   = Hürden_DruckPrivat,
                                              `Druck, nicht ausfallen zu können (beruflich)`                = Hürden_DruckBeruf,
                                              `Alkohol- und/oder Drogenmissbrauch`                          = Hürden_Substanzkonsum,
                                              `Geringes Selbstwertgefühl`                                   = Hürden_geringerSelbstwert,
                                              `Fehlende Unterstützung im sozialen Umfeld`                   = Hürden_keineUnterstützung,
                                              `Stigmatisierung im sozialen Umfeld`                          = Hürden_StigmaUmfeld,
                                              `Negative Erfahrungen mit dem Gesundheitssystem`              = Hürden_negErfahrungen,
                                              `Negative Erwartungen gegenüber dem Gesundheitssystem`        = Hürden_negErwartungen,
                                              `Unwissenheit über Anlaufstellen/ Behandlungsmöglichkeiten`   = Hürden_Unwissenheit,
                                              `Lange Wartezeiten für Therapieplätze`                        = Hürden_Wartezeiten,
                                              `Mangelndes Problembewusstsein`                               = Hürden_keinProblembewusstsein,
                                              `Mangelndes Verständnis, was bei mir los ist`                 = Hürden_keinVerständnis,
                                              `Scham, Hilfe zu benötigen`                                   = Hürden_Scham,
                                              `Traditionelle Geschlechterrollen`                            = Hürden_TradGeschlechterrolle
                                            ) %>%  mutate(across(where(is.numeric), ~ . - 1)) 

Frauen_data_nurHürden_t <- as.data.frame(t(Frauen_data_nurHürden))
Frauen_data_nurHürden_t$SUM <- rowSums(Frauen_data_nurHürden_t, na.rm = TRUE)

Frauen_data_nurHürden <- Frauen_data_nurHürden_t %>% select(SUM)

label_Hürden <- c(
  "Überzeugung, es allein zu schaffen",
  "Keine Zeit aufgrund von Verpflichtungen",
  "Antriebslosigkeit",
  "Angst vor möglichen Konsequenzen",
  "Fehlendes Vertrauen in Bezugspersonen",
  "Finanzielle Barrieren",
  "Druck, nicht ausfallen zu können (privat)",
  "Druck, nicht ausfallen zu können (beruflich)",
  "Alkohol- und/oder Drogenmissbrauch",
  "Geringes Selbstwertgefühl",
  "Fehlende Unterstützung im sozialen Umfeld",
  "Stigmatisierung im sozialen Umfeld",
  "Negative Erfahrungen mit dem Gesundheitssystem",
  "Negative Erwartungen gegenüber dem Gesundheitssystem",
  "Unwissenheit über Anlaufstellen/ Behandlungsmöglichkeiten",
  "Lange Wartezeiten für Therapieplätze",
  "Mangelndes Problembewusstsein",
  "Mangelndes Verständnis, was bei mir los ist",
  "Scham, Hilfe zu benötigen",
  "Traditionelle Geschlechterrollen")

####### Hürden - Emotionale Schwierigkeit ######
Frauen_data_schwierigeHürden <- Frauen_data %>% select(
  HüRang_Schwierig_alleineSchaffen, 
  HüRang_Schwierig_keineZeit,
  HüRang_Schwierig_Antriebslos,
  HüRang_Schwierig_AngstKonsequenz,
  HüRang_Schwierig_keinVertrauen,
  HüRang_Schwierig_finanzielleBarrieren,
  HüRang_Schwierig_DruckPrivat,
  HüRang_Schwierig_DruckBeruf,
  HüRang_Schwierig_Substanzkonsum,
  HüRang_Schwierig_geringerSelbstwert,
  HüRang_Schwierig_keineUnterstützung,
  HüRang_Schwierig_StigmaUmfeld,
  HüRang_Schwierig_negErfahrungen,
  HüRang_Schwierig_negErwartungen,
  HüRang_Schwierig_Unwissenheit,
  HüRang_Schwierig_Wartezeiten,
  HüRang_Schwierig_keinProblembewusstsein,
  HüRang_Schwierig_keinVerständnis,
  HüRang_Schwierig_Scham,
  HüRang_Schwierig_TradGeschelchterrolle,
  Hürden_Anzahl) %>% rename(
    Anzahl                                                       = Hürden_Anzahl,
    `Überzeugung, es allein zu schaffen`                          = HüRang_Schwierig_alleineSchaffen,
    `Keine Zeit aufgrund von Verpflichtungen`                     = HüRang_Schwierig_keineZeit,
    `Antriebslosigkeit`                                           = HüRang_Schwierig_Antriebslos,
    `Angst vor möglichen Konsequenzen`                            = HüRang_Schwierig_AngstKonsequenz,
    `Fehlendes Vertrauen in Bezugspersonen`                       = HüRang_Schwierig_keinVertrauen,
    `Finanzielle Barrieren`                                       = HüRang_Schwierig_finanzielleBarrieren,
    `Druck, nicht ausfallen zu können (privat)`                   = HüRang_Schwierig_DruckPrivat,
    `Druck, nicht ausfallen zu können (beruflich)`                = HüRang_Schwierig_DruckBeruf,
    `Alkohol- und/oder Drogenmissbrauch`                          = HüRang_Schwierig_Substanzkonsum,
    `Geringes Selbstwertgefühl`                                   = HüRang_Schwierig_geringerSelbstwert,
    `Fehlende Unterstützung im sozialen Umfeld`                   = HüRang_Schwierig_keineUnterstützung,
    `Stigmatisierung im sozialen Umfeld`                          = HüRang_Schwierig_StigmaUmfeld,
    `Negative Erfahrungen mit dem Gesundheitssystem`              = HüRang_Schwierig_negErfahrungen,
    `Negative Erwartungen gegenüber dem Gesundheitssystem`        = HüRang_Schwierig_negErwartungen,
    `Unwissenheit über Anlaufstellen/ Behandlungsmöglichkeiten`   = HüRang_Schwierig_Unwissenheit,
    `Lange Wartezeiten für Therapieplätze`                        = HüRang_Schwierig_Wartezeiten,
    `Mangelndes Problembewusstsein`                               = HüRang_Schwierig_keinProblembewusstsein,
    `Mangelndes Verständnis, was bei mir los ist`                 = HüRang_Schwierig_keinVerständnis,
    `Scham, Hilfe zu benötigen`                                   = HüRang_Schwierig_Scham,
    `Traditionelle Geschlechterrollen`                            = HüRang_Schwierig_TradGeschelchterrolle
  )


#Frauen_data_schwierigeHürden <- Frauen_data_schwierigeHürden %>% rowwise() %>% mutate(Max = if_else(all(is.na(c_across(everything()))), NA_real_, max(c_across(everything()), na.rm = TRUE))) #maximalen Score ermitteln
Frauen_data_schwierigeHürden <- Frauen_data_schwierigeHürden %>% rowwise() %>% mutate(across(starts_with("Hü"), ~ (Anzahl)+1 - . )) %>% ungroup() #Rankings flippen, sodass hoher Wert = hoher Rang ist

Frauen_data_schwierigeHürden <- Frauen_data_schwierigeHürden %>%
  rowwise() %>%
  mutate(across(-Anzahl, ~ . / Anzahl)) %>%
  ungroup() %>% select(-Anzahl)

Deskriptionen_schwierigeHürden <- data.frame(
  Statistic = character(),
  Value = numeric(),
  row.names = NULL)

for (col_name in names(Frauen_data_schwierigeHürden)) {
  
  # Calculate statistics
  col_mean <- mean(Frauen_data_schwierigeHürden[[col_name]], na.rm = TRUE)
  col_sd <- sd(Frauen_data_schwierigeHürden[[col_name]], na.rm = TRUE)
  col_min <- min(Frauen_data_schwierigeHürden[[col_name]], na.rm = TRUE)
  col_max <- max(Frauen_data_schwierigeHürden[[col_name]], na.rm = TRUE)
  
  # Append to summary dataframe
  Deskriptionen_schwierigeHürden <- rbind(Deskriptionen_schwierigeHürden,
                                          data.frame(Statistic = paste(col_name, "Mean"), Value = col_mean),
                                          data.frame(Statistic = paste(col_name, "SD"), Value = col_sd),
                                          data.frame(Statistic = paste(col_name, "Min"), Value = col_min),
                                          data.frame(Statistic = paste(col_name, "Max"), Value = col_max))
}

MW_schwierigeHürden <- data.frame(
  Statistic = character(),
  Value = numeric(),
  row.names = NULL)

for (col_name in names(Frauen_data_schwierigeHürden)) {
  
  # Calculate statistics
  col_mean <- (mean(Frauen_data_schwierigeHürden[[col_name]], na.rm = TRUE))*100
  
  # Append to summary dataframe
  MW_schwierigeHürden <- rbind(MW_schwierigeHürden,
                               data.frame(Statistic = col_name, Value = col_mean))
}


Frauen_data_schwierigeHürden_t <- as.data.frame(t(Frauen_data_schwierigeHürden))
Frauen_data_schwierigeHürden_t$SUM <- rowSums(Frauen_data_schwierigeHürden_t, na.rm = TRUE)

Frauen_data_schwierigeHürden_t$Relational <- ((Frauen_data_schwierigeHürden_t$SUM) / 178)*100
Frauen_data_schwierigeHürden <- Frauen_data_schwierigeHürden_t %>% select(Relational) %>% rownames_to_column("Hürden")

####### Hürden - Zeitfaktor/Latenz #######
Frauen_data_ZeitHürden <- Frauen_data %>% select(
  HüRang_Zeit_alleineSchaffen, 
  HüRang_Zeit_keineZeit,
  HüRang_Zeit_Antriebslos,
  HüRang_Zeit_AngstKonsequenz,
  HüRang_Zeit_keinVertrauen,
  HüRang_Zeit_finanzielleBarrieren,
  HüRang_Zeit_DruckPrivat,
  HüRang_Zeit_DruckBeruf,
  HüRang_Zeit_Substanzkonsum,
  HüRang_Zeit_geringerSelbstwert,
  HüRang_Zeit_keineUnterstützung,
  HüRang_Zeit_StigmaUmfeld,
  HüRang_Zeit_negErfahrungen,
  HüRang_Zeit_negErwartungen,
  HüRang_Zeit_Unwissenheit,
  HüRang_Zeit_Wartezeiten,
  HüRang_Zeit_keinProblembewusstsein,
  HüRang_Zeit_keinVerständnis,
  HüRang_Zeit_Scham,
  HüRang_Zeit_TradGeschelchterrolle,
  Hürden_Anzahl) %>% rename(
    Anzahl                                                       = Hürden_Anzahl,
    `Überzeugung, es allein zu schaffen`                          = HüRang_Zeit_alleineSchaffen,
    `Keine Zeit aufgrund von Verpflichtungen`                     = HüRang_Zeit_keineZeit,
    `Antriebslosigkeit`                                           = HüRang_Zeit_Antriebslos,
    `Angst vor möglichen Konsequenzen`                            = HüRang_Zeit_AngstKonsequenz,
    `Fehlendes Vertrauen in Bezugspersonen`                       = HüRang_Zeit_keinVertrauen,
    `Finanzielle Barrieren`                                       = HüRang_Zeit_finanzielleBarrieren,
    `Druck, nicht ausfallen zu können (privat)`                   = HüRang_Zeit_DruckPrivat,
    `Druck, nicht ausfallen zu können (beruflich)`                = HüRang_Zeit_DruckBeruf,
    `Alkohol- und/oder Drogenmissbrauch`                          = HüRang_Zeit_Substanzkonsum,
    `Geringes Selbstwertgefühl`                                   = HüRang_Zeit_geringerSelbstwert,
    `Fehlende Unterstützung im sozialen Umfeld`                   = HüRang_Zeit_keineUnterstützung,
    `Stigmatisierung im sozialen Umfeld`                          = HüRang_Zeit_StigmaUmfeld,
    `Negative Erfahrungen mit dem Gesundheitssystem`              = HüRang_Zeit_negErfahrungen,
    `Negative Erwartungen gegenüber dem Gesundheitssystem`        = HüRang_Zeit_negErwartungen,
    `Unwissenheit über Anlaufstellen/ Behandlungsmöglichkeiten`   = HüRang_Zeit_Unwissenheit,
    `Lange Wartezeiten für Therapieplätze`                        = HüRang_Zeit_Wartezeiten,
    `Mangelndes Problembewusstsein`                               = HüRang_Zeit_keinProblembewusstsein,
    `Mangelndes Verständnis, was bei mir los ist`                 = HüRang_Zeit_keinVerständnis,
    `Scham, Hilfe zu benötigen`                                   = HüRang_Zeit_Scham,
    `Traditionelle Geschlechterrollen`                            = HüRang_Zeit_TradGeschelchterrolle
  )



Frauen_data_ZeitHürden <- Frauen_data_ZeitHürden %>% rowwise() %>% mutate(across(starts_with("Hü"), ~ (Anzahl)+1 - . )) %>% ungroup()

Frauen_data_ZeitHürden <- Frauen_data_ZeitHürden %>%
  rowwise() %>%
  mutate(across(-Anzahl, ~ . / Anzahl)) %>%
  ungroup() %>% select(-Anzahl)

MW_ZeitHürden <- data.frame(
  Statistic = character(),
  Value = numeric(),
  row.names = NULL)

for (col_name in names(Frauen_data_ZeitHürden)) {
  
  # Calculate statistics
  col_mean <- (mean(Frauen_data_ZeitHürden[[col_name]], na.rm = TRUE))*100
  
  # Append to summary dataframe
  MW_ZeitHürden <- rbind(MW_ZeitHürden,
                         data.frame(Statistic = col_name, Value = col_mean))
}

Frauen_data_ZeitHürden_t <- as.data.frame(t(Frauen_data_ZeitHürden))
Frauen_data_ZeitHürden_t$SUM <- rowSums(Frauen_data_ZeitHürden_t, na.rm = TRUE)

Frauen_data_ZeitHürden_t$Relational <- ((Frauen_data_ZeitHürden_t$SUM) / 178)*100
Frauen_data_ZeitHürden <- Frauen_data_ZeitHürden_t %>% select(Relational)  %>% rownames_to_column("Hürden")

Frauen_data_Ranking <- left_join(Frauen_data_schwierigeHürden, Frauen_data_ZeitHürden, join_by("Hürden"))

Frauen_data_Ranking <- Frauen_data_Ranking %>% rename(`Emotionale Schwierigkeit` = Relational.x,
                                                  `Zeitliche Relevanz` = Relational.y) %>% mutate(
                                                    across(where(is.numeric), ~ round(., 2)))

Frauen_data_MWRanking <- left_join(MW_schwierigeHürden, MW_ZeitHürden, join_by(Statistic))
Frauen_data_MWRanking <- Frauen_data_MWRanking %>% rename(`Emotionale Schwierigkeit` = Value.x,
                                                      `Zeitliche Relevanz` = Value.y) %>% mutate(
                                                        across(where(is.numeric), ~ round(., 2)))

Reihenfolge <- c("Lange Wartezeiten für Therapieplätze",
                 "Geringes Selbstwertgefühl",
                 "Antriebslosigkeit",
                 "Überzeugung, es allein zu schaffen",
                 "Scham, Hilfe zu benötigen",
                 "Druck, nicht ausfallen zu können (beruflich)",
                 "Angst vor möglichen Konsequenzen",
                 "Mangelndes Verständnis, was bei mir los ist",
                 "Druck, nicht ausfallen zu können (privat)",
                 "Stigmatisierung im sozialen Umfeld",
                 "Keine Zeit aufgrund von Verpflichtungen",
                 "Traditionelle Geschlechterrollen",
                 "Mangelndes Problembewusstsein",
                 "Unwissenheit über Anlaufstellen/ Behandlungsmöglichkeiten",
                 "Fehlendes Vertrauen in Bezugspersonen",
                 "Fehlende Unterstützung im sozialen Umfeld",
                 "Negative Erfahrungen mit dem Gesundheitssystem",
                 "Finanzielle Barrieren",
                 "Negative Erwartungen gegenüber dem Gesundheitssystem",
                 "Alkohol- und/oder Drogenmissbrauch")

Reihenfolge <- rev(Reihenfolge)
Frauen_data_Ranking$Hürden <- factor(Frauen_data_Ranking$Hürden, levels = Reihenfolge)

####### Ranking Plots ######

Frauen_data_Ranking_long <- pivot_longer(Frauen_data_Ranking, c("Emotionale Schwierigkeit", "Zeitliche Relevanz"), names_to = "Ranking", values_to =  "Platzierung")


RankingPlot <- ggplot(Frauen_data_Ranking_long, aes(x = Hürden, y = Platzierung, fill = as.factor(Ranking), color = as.factor(Ranking)))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.85)+
  coord_flip()+
  geom_text(stat= "identity",
            aes(label = Platzierung, group = Ranking), 
            hjust = -0.25,
            position = position_dodge(width = 0.75),
            inherit.aes = TRUE,
            size=6, color = "grey30")+
  theme_minimal()+
  scale_fill_manual(values = c("#98bdd4", "#a2d498")) +    # Apply custom fill colors
  scale_color_manual(values = c("#98bdd4", "#a2d498")) +    # Apply custom border colors (if needed)
  theme(plot.title = element_text(size = 30, hjust = 0.5, color = "grey20", face = "bold"),
        axis.text.x = element_text(size = 24, color = "grey30"),
        axis.title.x = element_text(size = 28, color = "grey30"),
        axis.text.y = element_text(size = 28, color = "grey30"),
        axis.title.y = element_text(size = 28, color = "grey30"),
        legend.title = element_text(size = 28, color = "grey30", face = "bold"),
        legend.text = element_text(size = 28, color = "grey30"))+
  labs(title = "Gesamtes Ranking der verschiedenen Hürden (Häufigkeit x Relevanz)", x = "", y = "Ranking (mögliche Spanne: 0-100)", fill = "Ranking", color = "Ranking")   

RankingPlot

ggsave("RankingPlot.svg", RankingPlot, width = 35, height = 15)

Frauen_data_MWRanking$Statistic <- factor(Frauen_data_MWRanking$Statistic, levels = Reihenfolge)

Frauen_data_MWRanking_long <- pivot_longer(Frauen_data_MWRanking, c("Emotionale Schwierigkeit", "Zeitliche Relevanz"), names_to = "Ranking", values_to =  "Platzierung")

RankingPlot2 <- ggplot(Frauen_data_MWRanking_long, aes(x = Statistic, y = Platzierung, fill = as.factor(Ranking), color = as.factor(Ranking)))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.85)+
  coord_flip()+
  geom_text(stat= "identity",
            aes(label = Platzierung, group = Ranking), 
            hjust = -0.25,
            position = position_dodge(width = 0.75),
            inherit.aes = TRUE,
            size=6, color = "grey30")+
  theme_minimal()+
  scale_fill_manual(values = c("#98bdd4", "#a2d498")) +    # Apply custom fill colors
  scale_color_manual(values = c("#98bdd4", "#a2d498")) +    # Apply custom border colors (if needed)
  theme(plot.title = element_text(size = 30, hjust = 0.5, color = "grey20", face = "bold"),
        axis.text.x = element_text(size = 24, color = "grey30"),
        axis.title.x = element_text(size = 28, color = "grey30"),
        axis.text.y = element_text(size = 28, color = "grey30"),
        axis.title.y = element_text(size = 28, color = "grey30"),
        legend.title = element_text(size = 28, color = "grey30", face = "bold"),
        legend.text = element_text(size = 28, color = "grey30"))+
  labs(title = "Individuelle Relevanz der verschiedenen Hürden", x = "", y = "Ranking (mögliche Spanne: 0-100)", fill = "Ranking", color = "Ranking")   

RankingPlot2

ggsave("RankingPlot2.svg", RankingPlot2, width = 35, height = 15)

####### Scatterplots für MC-Hürden (Ranking x Häufigkeit) #######

Frauen_data_nurHürden <- Frauen_data_nurHürden %>% rownames_to_column("Hürden")
Data_ScatterPlot <- left_join(Frauen_data_nurHürden, Frauen_data_MWRanking, join_by(Hürden == Statistic))

AnzahlxSchwierig_Plot <- ggplot(Data_ScatterPlot, aes(x = SUM, y = `Emotionale Schwierigkeit`))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", aes(x = SUM, y = `Emotionale Schwierigkeit`, color = "black", fill = "red"), inherit.aes = FALSE)+
  theme_minimal()+
  theme(plot.title = element_text(size = 30, hjust = 0.5, color = "grey20", face = "bold"),
        axis.text.x = element_text(size = 24, color = "grey30"),
        axis.title.x = element_text(size = 28, color = "grey30"),
        axis.text.y = element_text(size = 28, color = "grey30"),
        axis.title.y = element_text(size = 28, color = "grey30"),
        legend.position = "none")+
  labs(title = "Zusammemnhang zwischen der Häufigkeit der gewählten Hürden\nund der individuellen Bewertung der emotionalen Schwierigkeit",
       x = "Absolute Häufigkeit der ausgewählten Hürden",
       y = "Individuelle Bewertung der emotionalen Schwierigkeit der Hürden")

AnzahlxSchwierig_Plot

ggsave("AnzahlxSchwierig_Plot.svg", AnzahlxSchwierig_Plot, width = 20, height = 12)

AnzahlxZeit_Plot <- ggplot(Data_ScatterPlot, aes(x = SUM, y = `Zeitliche Relevanz`))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", aes(x = SUM, y = `Zeitliche Relevanz`, color = "black", fill = "pink"), inherit.aes = FALSE)+
  theme_minimal()+
  theme(plot.title = element_text(size = 30, hjust = 0.5, color = "grey20", face = "bold"),
        axis.text.x = element_text(size = 24, color = "grey30"),
        axis.title.x = element_text(size = 28, color = "grey30"),
        axis.text.y = element_text(size = 28, color = "grey30"),
        axis.title.y = element_text(size = 28, color = "grey30"),
        legend.position = "none")+
  labs(title = "Zusammemnhang zwischen der Häufigkeit der gewählten Hürden\nund der individuellen Bewertung der zeitlichen Relevanz",
       x = "Absolute Häufigkeit der ausgewählten Hürden",
       y = "Individuelle Bewertung der zeitlichen Relevanz der Hürden")

AnzahlxZeit_Plot

ggsave("AnzahlxZeit_Plot.svg", AnzahlxZeit_Plot, width = 20, height = 12)


####### MC-Plots #######
Frauen_data_MC <- Frauen_data %>% select(Case,
                                         Hürden_Anzahl,
                                         Hürden_alleineSchaffen,
                                         Hürden_keineZeit,
                                         Hürden_Antriebslos,
                                         Hürden_AngstKonsequenz,
                                         Hürden_keinVertrauen,
                                         Hürden_finanzielleBarrieren,
                                         Hürden_DruckPrivat,
                                         Hürden_DruckBeruf,
                                         Hürden_Substanzkonsum,
                                         Hürden_geringerSelbstwert,
                                         Hürden_keineUnterstützung,
                                         Hürden_StigmaUmfeld,
                                         Hürden_negErfahrungen,
                                         Hürden_negErwartungen,
                                         Hürden_Unwissenheit,
                                         Hürden_Wartezeiten,
                                         Hürden_keinProblembewusstsein,
                                         Hürden_keinVerständnis,
                                         Hürden_Scham,
                                         Hürden_TradGeschlechterrolle
                                         ) %>% mutate(
                                               Hürden_Anzahl = ifelse(Hürden_Anzahl > 0, NA_real_, 2),
                                               across(where(is.numeric) & !any_of("Case"), ~ . - 1)
                                         ) %>% rename(
                                               `Keine Hürden`                                              = Hürden_Anzahl,
                                               `Überzeugung, es allein zu schaffen`                        = Hürden_alleineSchaffen,
                                               `Keine Zeit aufgrund von Verpflichtungen`                   = Hürden_keineZeit,
                                               `Antriebslosigkeit`                                         = Hürden_Antriebslos,
                                               `Angst vor möglichen Konsequenzen`                          = Hürden_AngstKonsequenz,
                                               `Fehlendes Vertrauen in Bezugspersonen`                     = Hürden_keinVertrauen,
                                               `Finanzielle Barrieren`                                     = Hürden_finanzielleBarrieren,
                                               `Druck, nicht ausfallen zu können (privat)`                 = Hürden_DruckPrivat,
                                               `Druck, nicht ausfallen zu können (beruflich)`              = Hürden_DruckBeruf,
                                               `Alkohol- und/oder Drogenmissbrauch`                        = Hürden_Substanzkonsum,
                                               `Geringes Selbstwertgefühl`                                 = Hürden_geringerSelbstwert,
                                               `Fehlende Unterstützung im sozialen Umfeld`                 = Hürden_keineUnterstützung,
                                               `Stigmatisierung im sozialen Umfeld`                        = Hürden_StigmaUmfeld,
                                               `Negative Erfahrungen mit dem Gesundheitssystem`            = Hürden_negErfahrungen,
                                               `Negative Erwartungen gegenüber dem Gesundheitssystem`      = Hürden_negErwartungen,
                                               `Unwissenheit über Anlaufstellen/Behandlungsmöglichkeiten`  = Hürden_Unwissenheit,
                                               `Lange Wartezeiten für Therapieplätze`                      = Hürden_Wartezeiten,
                                               `Mangelndes Problembewusstsein`                             = Hürden_keinProblembewusstsein,
                                               `Mangelndes Verständnis, was bei mir los ist`               = Hürden_keinVerständnis,
                                               `Scham, Hilfe zu benötigen`                                 = Hürden_Scham,
                                               `Traditionelle Geschlechterrollen`                          = Hürden_TradGeschlechterrolle
                                               )

Frauen_data_MC[Frauen_data_MC == 0] <- NA

Frauen_data_MC_long <- Frauen_data_MC %>% pivot_longer(cols      = -Case,          # alle Spalten außer Case
                                                       names_to  = "Hürden",
                                                       values_to = "Anzahl",
                                                       values_drop_na = TRUE
                                                       )

Reihenfolge <- c("Lange Wartezeiten für Therapieplätze",
                 "Geringes Selbstwertgefühl",
                 "Antriebslosigkeit",
                 "Überzeugung, es allein zu schaffen",
                 "Scham, Hilfe zu benötigen",
                 "Druck, nicht ausfallen zu können (beruflich)",
                 "Angst vor möglichen Konsequenzen",
                 "Mangelndes Verständnis, was bei mir los ist",
                 "Druck, nicht ausfallen zu können (privat)",
                 "Stigmatisierung im sozialen Umfeld",
                 "Keine Zeit aufgrund von Verpflichtungen",
                 "Traditionelle Geschlechterrollen",
                 "Mangelndes Problembewusstsein",
                 "Unwissenheit über Anlaufstellen/ Behandlungsmöglichkeiten",
                 "Fehlendes Vertrauen in Bezugspersonen",
                 "Fehlende Unterstützung im sozialen Umfeld",
                 "Negative Erfahrungen mit dem Gesundheitssystem",
                 "Finanzielle Barrieren",
                 "Negative Erwartungen gegenüber dem Gesundheitssystem",
                 "Alkohol- und/oder Drogenmissbrauch",
                 "Keine Hürden") %>% rev()

Frauen_data_MC_long$Hürden <- factor(Frauen_data_MC_long$Hürden, levels = Reihenfolge)

HürdenPlotMC_rel <- ggplot(Frauen_data_MC_long, aes(x = Hürden)) +
  geom_bar(width = 0.8, color = "#a1d3ed", fill = "#a1d3ed") +
  geom_text(stat  = "count",
            aes(label = paste0(round((after_stat(count) / 179) * 100, 1), "%")),
            hjust = 1.1, size = 8, color = "white") +
  geom_text(stat  = "count",
            aes(label = after_stat(count)),
            hjust = -0.25, size = 9, color = "grey30") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title   = element_text(size = 34, hjust = 0.5, color = "grey20", face = "bold"),
        axis.text.x  = element_text(size = 30, color = "grey30"),
        axis.text.y  = element_text(size = 30, color = "grey30"),
        axis.title.x = element_text(size = 30, color = "grey30"),
        legend.title = element_text(size = 28, color = "grey30", face = "bold"),
        legend.key.spacing.y = unit(10, "pt"),
        legend.text  = element_text(size = 28, color = "grey30")) +
  labs(title = "Multiple Choice Abfrage der erlebten Hürden",
       x     = "",
       y     = "Absolute Häufigkeit")

ggsave("HürdenPlotMC_rel.svg", HürdenPlotMC_rel, width = 35, height = 18)

###########
Frauen_data_Ranking$Hürden <- factor(Frauen_data_Ranking$Hürden, levels = Reihenfolge)
Frauen_data_Ranking <- Frauen_data_Ranking %>% arrange(Hürden)

Frauen_data_MWRanking$Statistic <- factor(Frauen_data_MWRanking$Statistic, levels = Reihenfolge)
Frauen_data_MWRanking <- Frauen_data_MWRanking %>% arrange(Statistic)

write.xlsx(Frauen_data_Ranking, "Ranking_Hürden.xlsx", "Absolute Relevanz")
  write.xlsx(Frauen_data_MWRanking, "Ranking_Hürden.xlsx", "Individuelle Relevanz", append = TRUE)
