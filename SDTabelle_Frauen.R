library(tidyverse)
library(xlsx)

Frauen_data <- read_csv("Frauen_data.csv")

##### SOZIODEMOGRAPHIE - TABELLEN #####

Sozio_Tabelle_Ka <- Frauen_data %>% filter(Kontakt == 1)

Sozio_Tabelle_Ka <- select(Sozio_Tabelle_Ka, c("profHilfe", "Diagnose", "PostPartDep", "PostPartDep_dich", "SD_Bildung", "SD_Berufsfeld", "SD_Beziehung", "SD_Kinder", "SD_Wohnsituation", "SD_Tätigkeit"))

Sozio_Tabelle_Ka <- Sozio_Tabelle_Ka %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value"
               ) %>% filter(!is.na(value)
               ) %>% count(variable, value
               ) %>%
  pivot_wider(id_expand = TRUE,
              names_from = "variable",
              values_from = n,
              values_fill = list(n = 0)) %>%
  select(!"value")

view(Sozio_Tabelle_Ka)

#vorbereitende Soziodemographie Tabelle ohne Kontakt

Sozio_Tabelle_oK <- Frauen_data %>% filter(Kontakt == 2)

Sozio_Tabelle_oK <- select(Sozio_Tabelle_oK, c("profHilfe", "Diagnose", "PostPartDep_dich", "SD_Bildung", "SD_Berufsfeld", "SD_Beziehung", "SD_Kinder", "SD_Wohnsituation", "SD_Tätigkeit"))

Sozio_Tabelle_oK <- Sozio_Tabelle_oK %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value" 
               ) %>% filter(!is.na(value)
               ) %>% count(variable, value
               ) %>%
  pivot_wider(id_expand = TRUE,
              names_from = "variable",
              values_from = n,
              values_fill = list(n = 0)) %>%
  select(!"value")

view(Sozio_Tabelle_oK)

Sozio_Tabelle_oK[nrow(Sozio_Tabelle_oK) + 1 , ] <- NA #leere Reihe hinzufügen, damit die Tabellen beider Gruppen gleich lang sind
Sozio_Tabelle_oK[nrow(Sozio_Tabelle_oK) + 1 , ] <- NA 

#Labels erstellen
psychHilfe_label <- c("Ja, ich habe in der Vergangenheit eine solche Behandlung in Anspruch genommen.", "Ja, ich bin aktuell in einer solchen Behandlung.", 
                      "Ja, ich hatte ein Erst- bzw. Beratungsgespräch.", "Nein, aber ich habe mich aufgrund meiner Gemütslage an meine Hausarztpraxis gewandt.",
                      "Nein, ich habe keine solche Hilfe in Anspruch genommen.")

Diagnose_label <- c("Ja, ich habe eine Diagnose durch Ärzt/innen und/oder Therapeut/innen erhalten.", "Ja, mir wurde durch Ärzt/innen und/oder Therapeut/innen der Verdacht geäußert.",
                    "Mir wurde der Verdacht in meinem privaten Umfeld geäußert.", "Ich habe selber diesen Verdacht.", "Ich bin mir nicht sicher.", "Nein, definitiv nicht.")

PostPartDep_label <- c("Ja", "Nein")

Erkenntnis_label <- c("Ich hatte eigenständig diese Erkenntnis", "Mich hat eine Bezugsperson darauf gebracht", "Ich wurde von einem Arzt/einer Ärztin darauf gebracht", "Sonstiges")

Erstkontakt_label <- c("Hausarztpraxis", "kassenärztliche Auskunft (116-117) o.a. Beratungsstelle", "psychotherapeutische/psychiatrische Praxis", 
                       "Sonstige Facharztpraxis", "Zwangseinweisung", "nicht zuordbar")

Bildung_label <- c("kein Abschluss", "Hauptschulabschluss", "Mittlere Reife", "Fachhochschulreife", "Abitur", "Bachelor/Vordiplom", "Master/Diplom/Magister/Staatsexamen", 
                   "Promotion o. höher", "Meister/Techniker")

Berufsfeld_label <- c("praktisch-technisch", "wissenschaftlich-forschend", "künstlerisch-sprachlich", "sozial-erziehend-pflegend", "wirtschaftlich", 
                      "verwaltend", "rechtlich", "keine Berufstätigkeit")

Tätigkeit_label <- c("keine Tätigkeit (arbeitsunfähig)", "keine Tätigkeit (arbeitssuchend)", "Schule", "Ausbildung", "Studium", "Angestellten-/Beamtentätigkeit", 
                     "Selbstständigkeit", "Rente", "Care-Arbeit")

Beziehung_label <- c("Single/Ledig", "In fester Partnerschaft", "Verheiratet", "In Trennung/Geschieden", "Verwitwet", "Sonstiges")

Kinder_label <- c("Nein, ich habe keine Kinder", "Ja, bei mir wohnend", "Ja, anteilig bei mir wohnend", "Ja, aber nicht bei mir wohnend")

Wohnsituation_label <- c("Mit Partner:in zusammen lebend", "In einer WG/mit Freund:innen zusammen lebend", "Bei/mit sonstiger Familie zusammen lebend", 
                         "Alleine mit meinen Kindern zusammen lebend", "Alleine lebend", "Betreutes Wohnen, Wohnprojekt o.ä.")

#einzelne Tabellen daraus printen
psychHilfe_Ka <- (Sozio_Tabelle_Ka$profHilfe) #Spalte für Kontaktaufnahme extrahieren
  psychHilfe_oK <- (Sozio_Tabelle_oK$profHilfe) #Spalte für ohne Kontakt extrahieren
  psychHilfe_Tabelle <- cbind(psychHilfe_Ka, psychHilfe_oK) #neue Tabelle erstellen
  psychHilfe_Tabelle <- psychHilfe_Tabelle[-6:-10,] #leere Zeilen entfernen
  row.names(psychHilfe_Tabelle) <- psychHilfe_label #Zeilen beschriften

Diagnose_Ka <- (Sozio_Tabelle_Ka$Diagnose)
  Diagnose_oK <- (Sozio_Tabelle_oK$Diagnose)
  Diagnose_Tabelle <- cbind(Diagnose_Ka, Diagnose_oK) 
  Diagnose_Tabelle <- Diagnose_Tabelle[-7:-10,]
  row.names(Diagnose_Tabelle) <- Diagnose_label

PostPartDep_Ka <- (Sozio_Tabelle_Ka$PostPartDep_dich)
  PostPartDep_oK <- (Sozio_Tabelle_oK$PostPartDep_dich)
  PostPartDep_Tabelle <- cbind(PostPartDep_Ka, PostPartDep_oK) 
  PostPartDep_Tabelle <- PostPartDep_Tabelle[-3:-10,]
  row.names(PostPartDep_Tabelle) <- PostPartDep_label

Diagnose_oK <- (Sozio_Tabelle_oK$Diagnose)
  Diagnose_Tabelle <- cbind(Diagnose_Ka, Diagnose_oK) 
  Diagnose_Tabelle <- Diagnose_Tabelle[-7:-10,]
  row.names(Diagnose_Tabelle) <- Diagnose_label

Bildung_Ka <- (Sozio_Tabelle_Ka$SD_Bildung)
  Bildung_oK <- (Sozio_Tabelle_oK$SD_Bildung)
  Bildung_Tabelle <- cbind(Bildung_Ka, Bildung_oK) 
  Bildung_Tabelle <- Bildung_Tabelle[-10,]
  row.names(Bildung_Tabelle) <- Bildung_label

Berufsfeld_Ka <- (Sozio_Tabelle_Ka$SD_Berufsfeld)
  Berufsfeld_oK <- (Sozio_Tabelle_oK$SD_Berufsfeld)
  Berufsfeld_Tabelle <- cbind(Berufsfeld_Ka, Berufsfeld_oK) 
  Berufsfeld_Tabelle <- Berufsfeld_Tabelle[-9:-10,]
  row.names(Berufsfeld_Tabelle) <- Berufsfeld_label

Tätigkeit_Ka <- (Sozio_Tabelle_Ka$SD_Tätigkeit)
  Tätigkeit_oK <- (Sozio_Tabelle_oK$SD_Tätigkeit)
  Tätigkeit_Tabelle <- cbind(Tätigkeit_Ka, Tätigkeit_oK) 
  row.names(Tätigkeit_Tabelle) <- Tätigkeit_label

Beziehung_Ka <- (Sozio_Tabelle_Ka$SD_Beziehung)
  Beziehung_oK <- (Sozio_Tabelle_oK$SD_Beziehung)
  Beziehung_Tabelle <- cbind(Beziehung_Ka, Beziehung_oK)
  Beziehung_Tabelle <- Beziehung_Tabelle[-7:-10,]
  row.names(Beziehung_Tabelle) <- Beziehung_label

Kinder_Ka <- (Sozio_Tabelle_Ka$SD_Kinder)
  Kinder_oK <- (Sozio_Tabelle_oK$SD_Kinder)
  Kinder_Tabelle <- cbind(Kinder_Ka, Kinder_oK)
  Kinder_Tabelle <- Kinder_Tabelle[-5:-10,]
  row.names(Kinder_Tabelle) <- Kinder_label

Wohnsituation_Ka <- (Sozio_Tabelle_Ka$SD_Wohnsituation)
  Wohnsituation_oK <- (Sozio_Tabelle_oK$SD_Wohnsituation)
  Wohnsituation_Tabelle <- cbind(Wohnsituation_Ka, Wohnsituation_oK)
  Wohnsituation_Tabelle <- Wohnsituation_Tabelle[-7:-10,]
  row.names(Wohnsituation_Tabelle) <- Wohnsituation_label

#Alle Tabellen als einzelne Arbeitsblätter in eine Excel-Datei printen
write.xlsx(psychHilfe_Tabelle, file = "SD_Tabelle_Frauen.xlsx", sheetName = "psychHilfe")
  write.xlsx(Diagnose_Tabelle, file = "SD_Tabelle_Frauen.xlsx", sheetName = "Diagnose", append = TRUE)
  write.xlsx(PostPartDep_Tabelle, file = "SD_Tabelle_Frauen.xlsx", sheetName = "PostPartDep", append = TRUE)
  write.xlsx(Bildung_Tabelle, file = "SD_Tabelle_Frauen.xlsx", sheetName = "Bildung", append = TRUE)
  write.xlsx(Berufsfeld_Tabelle, file = "SD_Tabelle_Frauen.xlsx", sheetName = "Berufsfeld", append = TRUE)
  write.xlsx(Tätigkeit_Tabelle, file = "SD_Tabelle_Frauen.xlsx", sheetName = "Tätigkeit", append = TRUE)
  write.xlsx(Beziehung_Tabelle, file = "SD_Tabelle_Frauen.xlsx", sheetName = "Beziehung", append = TRUE)
  write.xlsx(Kinder_Tabelle, file = "SD_Tabelle_Frauen.xlsx", sheetName = "Kinder", append = TRUE)
  write.xlsx(Wohnsituation_Tabelle, file = "SD_Tabelle_Frauen.xlsx", sheetName = "Wohnsituation", append = TRUE)

##SO
  
table(Frauen_data$SO_spez_dich, Frauen_data$Kontakt)

      