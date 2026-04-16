library(tidyverse)
library(xlsx)
library(summarytools)

Frauen_data <- read_csv("Frauen_data.csv")

#Deskriptionen in Tabelle formatieren
Deskriptionen_Tabelle_Ka <- Frauen_data %>% filter(Kontakt == 1)

Deskriptionen_Tabelle_Ka <- select(Deskriptionen_Tabelle_Ka, c(SD_Alter, PHQ2_Sum, IDQ_DenialNeg_Sum, IDQ_ResistCh_Sum, IDQ_ConscAv_Sum, MHSAS_MW, DepSter_Lit_MW, 
                                                               DepSter_Mis_MW, UBogen_KA_MW, SIAS_MW, KÜ_ext_MW, KÜ_int_MW, SELSA_Soz_MW, SELSA_Fam_MW,
                                                               SELSA_Rom_MW, Alexithymie_MW, PAREMO_MW, Veränderung_P_MW, Veränderung_S_MW, 
                                                               NEOffi_Neur_MW, NEOffi_Ext_MW, Optimismus, Pessimismus, BSSS_need_MW, BSSS_seek_MW, BSSS_receive_MW, SozU_MW,
                                                               ProzEinschränkung, VergleichAndere, SO_spez_kont, SO_allgemein)) 

Deskriptionen_Tabelle_Ka <- data.frame(t(Deskriptionen_Tabelle_Ka)) 
Deskriptionen_Tabelle_Ka <- Deskriptionen_Tabelle_Ka %>% rowwise() %>% 
  mutate(
    N = sum(!is.na(c_across(X1:X152))),
    Min = min(c_across(X1:X152), na.rm = TRUE),
    Max = max(c_across(X1:X152), na.rm = TRUE),
    AM = mean(c_across(X1:X152), na.rm = TRUE),
    SD = sd(c_across(X1:X152), na.rm = TRUE)
  ) %>% mutate_if(is.numeric, round, digits = 2) 

Deskriptionen_Tabelle_Ka <- Deskriptionen_Tabelle_Ka %>% mutate(Spanne = paste0("[", Min, "-", Max, "]")) %>% select(-Min, -Max)

Deskriptionen_Tabelle_Ka <- as.matrix(Deskriptionen_Tabelle_Ka)

Zeilenbeschriftung <- c("Alter", "Depressivität (PHQ-2)", "Denial of Negativity (IDQ)","Resistance to Change (IDQ)", "Bewusste Vermeidung (IDQ)", "Einstellung ggü. Psychotherapie (MHSAS)", 
                        "Depression Literacy (DepSter)", "Depression Misconceptions (DepSter)", "Kontaktangst (UBogen)", "Interaktionsangst (SIAS)",
                        "externale Kontrollüberzeugungen (IE-4)", "internale Konrollüberzeugungen (IE-4)", "Soziale Einsamkeit (SELSA)", "Familiäre Einsamkeit (SELSA)", "Romantische Einsamkeit (SELSA)", 
                        "Alexithymie (TAS)", "Therapiemotivation (PAREMO)", "Veränderungsbereitschaft - Preperation (FEVER)", "Veränderungsbereitschaft - Self Help (FEVER)", 
                        "Neurotizismus (NEO-FFI)", "Extraversion (NEO-FFI)", "Optimismus", "Pessimismus", "Need for Support (BSSS)", "Support Seeking (BSSS)", "Received Support (BSSS)", 
                        "Soziale Unterstützung (F_Soz-U-K6)", "Einschränkung durch Symptome", "Vergleich mit Anderen", "Depressionsbezogene Selbstoffenbarung", "Allgemeine Selbstoffenbarung")                       

row.names(Deskriptionen_Tabelle_Ka) <- Zeilenbeschriftung

Deskriptionen_Tabelle_Ka <- Deskriptionen_Tabelle_Ka [ , c("N", "AM", "SD", "Spanne")]

#view(Deskriptionen_Tabelle_Ka)
#write.xlsx(Deskriptionen_Tabelle_Ka, "Deskriptionen_Ka.xlsx")

#Deskriptionen in Tabelle formatieren
Deskriptionen_Tabelle_oK <- Frauen_data %>% filter(Kontakt == 2)

Deskriptionen_Tabelle_oK <- select(Deskriptionen_Tabelle_oK, c(SD_Alter, PHQ2_Sum, IDQ_DenialNeg_Sum, IDQ_ResistCh_Sum, IDQ_ConscAv_Sum, MHSAS_MW, DepSter_Lit_MW, 
                                                               DepSter_Mis_MW, UBogen_KA_MW, SIAS_MW, KÜ_ext_MW, KÜ_int_MW, SELSA_Soz_MW, SELSA_Fam_MW,
                                                               SELSA_Rom_MW, Alexithymie_MW, PAREMO_MW, Veränderung_P_MW, Veränderung_S_MW, 
                                                               NEOffi_Neur_MW, NEOffi_Ext_MW, Optimismus, Pessimismus, BSSS_need_MW, BSSS_seek_MW, BSSS_receive_MW, SozU_MW,
                                                               ProzEinschränkung, VergleichAndere, SO_spez_kont, SO_allgemein)) 

Deskriptionen_Tabelle_oK <- data.frame(t(Deskriptionen_Tabelle_oK)) 
Deskriptionen_Tabelle_oK <- Deskriptionen_Tabelle_oK %>% rowwise() %>% 
  mutate(
    N = sum(!is.na(c_across(X1:X27))),
    Min = min(c_across(X1:X27), na.rm = TRUE),
    Max = max(c_across(X1:X27), na.rm = TRUE),
    AM = mean(c_across(X1:X27), na.rm = TRUE),
    SD = sd(c_across(X1:X27), na.rm = TRUE)
  ) %>% mutate_if(is.numeric, round, digits = 2) 

Deskriptionen_Tabelle_oK <- Deskriptionen_Tabelle_oK %>% mutate(Spanne = paste0("[", Min, "-", Max, "]")) %>% select(-Min, -Max)

Deskriptionen_Tabelle_oK <- as.matrix(Deskriptionen_Tabelle_oK)

row.names(Deskriptionen_Tabelle_oK) <- Zeilenbeschriftung

Deskriptionen_Tabelle_oK <- Deskriptionen_Tabelle_oK [ , c("N", "AM", "SD", "Spanne")]

#view(Deskriptionen_Tabelle_oK)
#write.xlsx(Deskriptionen_Tabelle_oK, "Deskriptionen_oK.xlsx")

Reliabilitäten_berechnen <- function(data) {
  
  #leeren Vektor erstellen  
  Reliabilitäten_alle <- c()
  
  Reliabilitäten_alle["Rel_Alter"] <- NA
  #Reliabilitäten berechnen und in den Vektor einfügen
  Rel_PHQ <- cor.test(data$PHQ2_01, data$PHQ2_02, method = 'spearman', use = "complete.obs")
  Reliabilitäten_alle["RelRaw_PHQ"] <- Rel_PHQ$estimate
  
  Rel_IDQ_DenialNeg <- psych::alpha(data[, c("IDQ_DenialNeg_01", "IDQ_DenialNeg_02", "IDQ_DenialNeg_03", "IDQ_DenialNeg_04", "IDQ_DenialNeg_05", "IDQ_DenialNeg_06", "IDQ_DenialNeg_07")])
  Reliabilitäten_alle["RelRaw_IDQ_DenialNeg"] <- Rel_IDQ_DenialNeg$total$raw_alpha
  
  Rel_IDQ_ResistCh <- psych::alpha(data[, c("IDQ_ResistCh_01", "IDQ_ResistCh_02", "IDQ_ResistCh_03", "IDQ_ResistCh_04", "IDQ_ResistCh_05", "IDQ_ResistCh_06", "IDQ_ResistCh_07")])
  Reliabilitäten_alle["RelRaw_IDQ_ResistCh"] <- Rel_IDQ_ResistCh$total$raw_alpha
  
  Rel_IDQ_ConscAv <- psych::alpha(data[, c("IDQ_ConscAv_01", "IDQ_ConscAv_02", "IDQ_ConscAv_03", "IDQ_ConscAv_04", "IDQ_ConscAv_05", "IDQ_ConscAv_06", "IDQ_ConscAv_07", "IDQ_ConscAv_08")])
  Reliabilitäten_alle["RelRaw_IDQ_ConscAv"] <- Rel_IDQ_ConscAv$total$raw_alpha
  
  Rel_MHSAS <- psych::alpha(data[, c("MHSAS_01", "MHSAS_02", "MHSAS_03", "MHSAS_04", "MHSAS_05", "MHSAS_06", "MHSAS_07", "MHSAS_08", "MHSAS_09")])
  Reliabilitäten_alle["RelRaw_MHSAS"] <- Rel_MHSAS$total$raw_alpha
  
  Rel_DepSter_Lit <- psych::alpha(data[, c("DepSter_Lit_01", "DepSter_Lit_02", "DepSter_Lit_03", "DepSter_Lit_04", "DepSter_Lit_05", "DepSter_Lit_06")])
  Reliabilitäten_alle["RelRaw_DepSter_Lit"] <- Rel_DepSter_Lit$total$raw_alpha
  
  Rel_DepSter_Mis <- psych::alpha(data[, c("DepSter_Mis_01", "DepSter_Mis_02", "DepSter_Mis_03", "DepSter_Mis_04", "DepSter_Mis_05", "DepSter_Mis_06", "DepSter_Mis_07", "DepSter_Mis_08")])
  Reliabilitäten_alle["RelRaw_DepSter_Mis"] <- Rel_DepSter_Mis$total$raw_alpha
  
  Rel_UBogen_KA <- psych::alpha(data[, c("UBogen_KA_01", "UBogen_KA_02", "UBogen_KA_03", "UBogen_KA_04", "UBogen_KA_05", "UBogen_KA_06")])
  Reliabilitäten_alle["RelRaw_UBogen_KA"] <- Rel_UBogen_KA$total$raw_alpha
  
  Rel_SIAS <- psych::alpha(data[, c("SIAS_01", "SIAS_02", "SIAS_03", "SIAS_04", "SIAS_05", "SIAS_06")])
  Reliabilitäten_alle["RelRaw_SIAS"] <- Rel_SIAS$total$raw_alpha
  
  Rel_KÜ_ext <- cor.test(data$KÜ_ext_01, data$KÜ_ext_02, method = 'spearman', use = "complete.obs")
  Reliabilitäten_alle["RelRaw_KÜ_ext"] <- Rel_KÜ_ext$estimate
  
  Rel_KÜ_int <- cor.test(data$KÜ_int_01, data$KÜ_int_02, method = 'spearman', use = "complete.obs")
  Reliabilitäten_alle["RelRaw_KÜ_int"] <- Rel_KÜ_int$estimate 
  
  Rel_SELSA_Soz <- psych::alpha(data[, c("SELSA_Soz_01", "SELSA_Soz_02", "SELSA_Soz_03", "SELSA_Soz_04", "SELSA_Soz_05")])
  Reliabilitäten_alle["RelRaw_SELSA_Soz"] <- Rel_SELSA_Soz$total$raw_alpha
  
  Rel_SELSA_Fam <- psych::alpha(data[, c("SELSA_Fam_01", "SELSA_Fam_02", "SELSA_Fam_03", "SELSA_Fam_04", "SELSA_Fam_05")])
  Reliabilitäten_alle["RelRaw_SELSA_Fam"] <- Rel_SELSA_Fam$total$raw_alpha
  
  Rel_SELSA_Rom <- psych::alpha(data[, c("SELSA_Rom_01", "SELSA_Rom_02", "SELSA_Rom_03", "SELSA_Rom_04", "SELSA_Rom_05")])
  Reliabilitäten_alle["RelRaw_SELSA_Rom"] <- Rel_SELSA_Rom$total$raw_alpha
  
  Rel_Alexithymie <- psych::alpha(data[, c("Alexithymie_01", "Alexithymie_02", "Alexithymie_03", "Alexithymie_04", "Alexithymie_05", "Alexithymie_06", "Alexithymie_07", "Alexithymie_08", "Alexithymie_09", "Alexithymie_10")])
  Reliabilitäten_alle["RelRaw_Alexithymie"] <- Rel_Alexithymie$total$raw_alpha
  
  Rel_PAREMO <- psych::alpha(data[, c("PAREMO_01", "PAREMO_02", "PAREMO_03", "PAREMO_04", "PAREMO_05")])
  Reliabilitäten_alle["RelRaw_PAREMO"] <- Rel_PAREMO$total$raw_alpha
  
  Rel_Veränderung_P <- psych::alpha(data[, c("Veränderung_01", "Veränderung_02", "Veränderung_03", "Veränderung_04", "Veränderung_05", "Veränderung_06", "Veränderung_07", "Veränderung_08")])
  Reliabilitäten_alle["RelRaw_Veränderung_P"] <- Rel_Veränderung_P$total$raw_alpha
  
  Rel_Veränderung_S <- psych::alpha(data[, c("Veränderung_09", "Veränderung_10", "Veränderung_11", "Veränderung_12", "Veränderung_13", "Veränderung_14", "Veränderung_15", "Veränderung_16")])
  Reliabilitäten_alle["RelRaw_Veränderung_S"] <- Rel_Veränderung_S$total$raw_alpha
  
  Rel_NEOffi_Neur <- psych::alpha(data[, c("NEOffi_Neur_01", "NEOffi_Neur_02", "NEOffi_Neur_03", "NEOffi_Neur_04", "NEOffi_Neur_05", "NEOffi_Neur_06")])
  Reliabilitäten_alle["RelRaw_NEOffi_Neur"] <- Rel_NEOffi_Neur$total$raw_alpha
  
  Rel_NEOffi_Ext <- psych::alpha(data[, c("NEOffi_Ext_01", "NEOffi_Ext_02", "NEOffi_Ext_03", "NEOffi_Ext_04", "NEOffi_Ext_05", "NEOffi_Ext_06")])
  Reliabilitäten_alle["RelRaw_NEOffi_Ext"] <- Rel_NEOffi_Ext$total$raw_alpha
  
  Reliabilitäten_alle["Optimismus"] <- c(NA)
  Reliabilitäten_alle["Pessimismus"] <- c(NA)
  
  Rel_BSSS_need <- psych::alpha(data[, c("BSSS_need_01", "BSSS_need_02","BSSS_need_03", "BSSS_need_04")])
  Reliabilitäten_alle["BSSS_need"] <- Rel_BSSS_need$total$raw_alpha
  
  Rel_BSSS_seek <- psych::alpha(data[, c("BSSS_seek_01", "BSSS_seek_02","BSSS_seek_03", "BSSS_seek_04", "BSSS_seek_05")])
  Reliabilitäten_alle["BSSS_seek"] <- Rel_BSSS_seek$total$raw_alpha
  
  Rel_BSSS_receive <- psych::alpha(data[, c("BSSS_receive_emo_01", "BSSS_receive_emo_02","BSSS_receive_emo_03", "BSSS_receive_emo_04", "BSSS_receive_emo_05", 
                                            "BSSS_receive_emo_06", "BSSS_receive_inst_01", "BSSS_receive_inst_02","BSSS_receive_inst_03", "BSSS_receive_info_01", 
                                            "BSSS_receive_info_02", "BSSS_receive_sat_01")])
  Reliabilitäten_alle["BSSS_receive"] <- Rel_BSSS_receive$total$raw_alpha
  
  Rel_SozU <- psych::alpha(data[, c("SozU_01", "SozU_02","SozU_03", "SozU_04", "SozU_05", "SozU_06")])
  Reliabilitäten_alle["SozU"] <- Rel_SozU$total$raw_alpha
  
  Reliabilitäten_alle["Einschränkungen"] <- c(NA)
  Reliabilitäten_alle["Vergleich"] <- c(NA)
  Reliabilitäten_alle["Private Selbstoffenbarung D"] <- c(NA)
  Reliabilitäten_alle["Private Selbstoffenbarung A"] <- c(NA)
  
  Reliabilitäten_alle <- as.matrix(Reliabilitäten_alle)
  return(Reliabilitäten_alle)
  
}

Frauen_Ka <- Frauen_data %>% filter(Kontakt == 1)  
Frauen_oK <- Frauen_data %>% filter(Kontakt == 2)

Reliabilitäten_Ka <- Reliabilitäten_berechnen(Frauen_Ka)
#Reliabilitäten_Ka <- ifelse(!is.na(Frauen_Ka),
                            #round(Frauen_Ka, 2), NA)

Reliabilitäten_oK <- Reliabilitäten_berechnen(Frauen_oK)
#Reliabilitäten_oK <- ifelse(!is.na(Frauen_oK),
                            #round(Frauen_oK, 2), NA)
Reliabilitäten_Ka <- round(Reliabilitäten_Ka, 2)
Reliabilitäten_oK <- round(Reliabilitäten_oK, 2)

#Tabelle für Kontaktaufnahme
Deskriptionen_Tabelle_Ka <- cbind(Deskriptionen_Tabelle_Ka [, "N", drop=FALSE], Reliabilitäten_Ka, Deskriptionen_Tabelle_Ka[, -1])
colnames(Deskriptionen_Tabelle_Ka) [2] = "Reliabilität"

#Tabelle für ohne Kontakt
Deskriptionen_Tabelle_oK <- cbind(Deskriptionen_Tabelle_oK [, "N", drop=FALSE], Reliabilitäten_oK, Deskriptionen_Tabelle_oK[, -1])
colnames(Deskriptionen_Tabelle_oK) [2] = "Reliabilität"

Deskriptionen_Tabelle_all <- cbind(Deskriptionen_Tabelle_Ka, Deskriptionen_Tabelle_oK)

write.csv(Deskriptionen_Tabelle_all, "Deskriptionen & Reliabilitäten_Frauen.csv", row.names = TRUE)

saveRDS(Deskriptionen_Tabelle_all, file = "Deskriptionen_Tabelle_all.rds")
write.xlsx(Deskriptionen_Tabelle_all, "Deskriptionen & Reliabilitäten_Frauen.xlsx", sheetName = "alle", showNA = FALSE)
write.xlsx(Deskriptionen_Tabelle_Ka, "Deskriptionen & Reliabilitäten_Frauen.xlsx", sheetName = "Kontaktaufnahme", append = TRUE)
write.xlsx(Deskriptionen_Tabelle_oK, "Deskriptionen & Reliabilitäten_Frauen.xlsx", sheetName = "ohne Kontakt", append = TRUE)

#Reliabilitäten-Tabelle
Reliabilitäten_Tabelle <- cbind(Reliabilitäten_Ka, Reliabilitäten_oK)
colnames(Reliabilitäten_Tabelle) <- c("Kontaktaufnahme", "ohne Kontakt")
rownames(Reliabilitäten_Tabelle) <- Zeilenbeschriftung
Reliabilitäten_Tabelle[,] <- ifelse(!is.na(Reliabilitäten_Tabelle[,]),
                                    round(Reliabilitäten_Tabelle[,], 2), NA)
