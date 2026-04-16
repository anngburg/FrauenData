library(tidyverse)

Frauen_data <- read_csv("Frauen_data.csv")

Frauen_data_subset <- Frauen_data %>% select(Kontakt, PHQ2_Sum, IDQ_DenialNeg_Sum, IDQ_ResistCh_Sum, IDQ_ConscAv_Sum, MHSAS_MW, DepSter_Lit_MW, 
                                             DepSter_Mis_MW, UBogen_KA_MW, SIAS_MW, KÜ_ext_MW, KÜ_int_MW, SELSA_Soz_MW, SELSA_Fam_MW,
                                             SELSA_Rom_MW, Alexithymie_MW, PAREMO_MW, Veränderung_P_MW, Veränderung_S_MW, 
                                             NEOffi_Neur_MW, NEOffi_Ext_MW, Optimismus, Pessimismus, BSSS_need_MW, BSSS_seek_MW, BSSS_receive_MW, SozU_MW,
                                             ProzEinschränkung, VergleichAndere, SO_spez_kont, SO_allgemein) %>% na.omit(.)

corr_Frauen_data <- abs(cor(Frauen_data_subset))
dist_Frauen_data <- as.dist(1 - corr_Frauen_data)

hierCluster_Frauen_data <- hclust(dist_Frauen_data, method = "ward.D2")

Variablenbeschriftung <- c("Depressivität (PHQ-2)", "Einstellung ggü. Psychotherapie (MHSAS)", "Denial of Negativity (IDQ)","Resistance to Change (IDQ)", 
                           "Bewusste Vermeidung (IDQ)", "Depression Literacy (DepSter)", "Depression Misconceptions (DepSter)", "Kontaktangst (UBogen)", "Interaktionsangst (SIAS)",
                           "Eingeschränkte Emotionalität (EE)", "externale Kontrollüberzeugungen (IE-4)", "internale Konrollüberzeugungen (IE-4)", "Soziale Einsamkeit (SELSA)",
                           "Familiäre Einsamkeit (SELSA)", "Romantische Einsamkeit (SELSA)", "Alexithymie (TAS)", "Therapiemotivation (PAREMO)", "Veränderungsbereitschaft - Preperation (FEVER)", 
                           "Veränderungsbereitschaft - Self Help (FEVER)", "Einschränkung durch Symptome", "Vergleich mit Anderen", "Private Selbstoffenbarung", "Hilfestatus")

svg("VariableClustering.svg", width = 16, height = 9)
plot(hierCluster_Frauen_data, labels = colnames(Frauen_data_subset), main = "Clustering der erhobenen Variablen (basierend auf Interkorrelationen)")
dev.off()

