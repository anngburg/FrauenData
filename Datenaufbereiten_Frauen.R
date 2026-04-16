library(tidyverse)
library(xlsx)

Frauen_data <- read_csv("data_MaeFraDep25.csv")

view(Frauen_data)

Frauen_data_rename <- Frauen_data %>% rename(
                                      Datenschutz = DS01,
                                      Volljährig = DS02,
                                      Geschlecht = EK04,
                                      
                                      ProzEinschränkung = AG17,
                                      VergleichAndere = AG18_01,
                                      ZeitraumSymptome = DP03_01,
                                      PostPartDep = DP04,
                                      PostPartDep_Sonstiges = DP04_03,
                                      
                                      profHilfe = EK01,
                                      profHilfe_anderes = EK01_06,
                                      Diagnose = EK09,
                                      Diagnose_anderes = EK09_07,
                                      Erkenntnis = LZ02,
                                      Erkenntnis_anderes = LZ02_04,
                                      Erstkontakt = LZ03,
                                      Erstkontakt_anderes = LZ03_04,
                                      
                                      Latenz_Erkenntnis = LZ04_01,
                                      Latenz_Wahrnehmung = LZ08_01,
                                      Latenz_Problembewusst = LZ12_01,
                                      Latenz_Öffnung = LZ10_01,
                                      Latenz_Gesamt = LZ05_01,
                                      
                                      SO_allgemein = PS01_01,
                                      SO_spez_dich = PS02,
                                      SO_spez_kont = PS03_01,
                                      SO_Initiative = PS07_01,
                                      
                                      PHQ2_01 = DP01_01,
                                      PHQ2_02 = DP01_02,
                                      
                                      DepSter_Lit_01 = AG04_01,
                                      DepSter_Lit_02 = AG04_02,
                                      DepSter_Lit_03 = AG04_03,
                                      DepSter_Lit_04 = AG04_04,
                                      DepSter_Lit_05 = AG04_05,
                                      DepSter_Lit_06 = AG04_06,
                                      DepSter_Mis_01 = AG04_07,
                                      DepSter_Mis_02 = AG04_08,
                                      DepSter_Mis_03 = AG04_09,
                                      DepSter_Mis_04 = AG04_10,
                                      DepSter_Mis_05 = AG04_11,
                                      DepSter_Mis_06 = AG04_12,
                                      DepSter_Mis_07 = AG04_13,
                                      DepSter_Mis_08 = AG04_14,
                                      
                                      PAREMO_01 = AG13_01,
                                      PAREMO_02 = AG13_02,
                                      PAREMO_03 = AG13_03,
                                      PAREMO_04 = AG13_04,
                                      PAREMO_05 = AG13_05,
                                      PAREMO_06 = AG13_06,
                                      
                                      Veränderung_01 = AG13_07,
                                      Veränderung_02 = AG13_08,
                                      Veränderung_03 = AG13_09,
                                      Veränderung_04 = AG13_10,
                                      Veränderung_05 = AG13_11,
                                      Veränderung_06 = AG13_12,
                                      Veränderung_07 = AG13_13,
                                      Veränderung_08 = AG13_14,
                                      Veränderung_09 = AG13_15,
                                      Veränderung_10 = AG13_16,
                                      Veränderung_11 = AG13_17,
                                      Veränderung_12 = AG13_18,
                                      Veränderung_13 = AG13_19,
                                      Veränderung_14 = AG13_20,
                                      Veränderung_15 = AG13_21,
                                      Veränderung_16 = AG13_22,
  
                                      Alexithymie_01 = AG40_01, 
                                      Alexithymie_02 = AG40_02,
                                      Alexithymie_03 = AG40_03, 
                                      Alexithymie_04 = AG40_04, 
                                      Alexithymie_05 = AG40_05, 
                                      Alexithymie_06 = AG40_06, 
                                      Alexithymie_07 = AG40_07,
                                      Alexithymie_08 = AG40_08, 
                                      Alexithymie_09 = AG40_09, 
                                      Alexithymie_10 = AG40_10, 

                                      SELSA_Soz_01 = EI01_01,
                                      SELSA_Soz_02 = EI01_02,
                                      SELSA_Soz_03 = EI01_03,
                                      SELSA_Soz_04 = EI01_04,
                                      SELSA_Soz_05 = EI01_05,
  
                                      SELSA_Rom_01 = EI02_01,
                                      SELSA_Rom_02 = EI02_02,
                                      SELSA_Rom_03 = EI02_03,
                                      SELSA_Rom_04 = EI02_04,
                                      SELSA_Rom_05 = EI02_05,

                                      SELSA_Fam_01 = EI03_01,
                                      SELSA_Fam_02 = EI03_02,
                                      SELSA_Fam_03 = EI03_03,
                                      SELSA_Fam_04 = EI03_04,
                                      SELSA_Fam_05 = EI03_05,

                                      MHSAS_01 = DV02_01,
                                      MHSAS_02 = DV02_02,
                                      MHSAS_03 = DV02_03,
                                      MHSAS_04 = DV02_04,
                                      MHSAS_05 = DV02_05,
                                      MHSAS_06 = DV02_06,
                                      MHSAS_07 = DV02_07,
                                      MHSAS_08 = DV02_08,
                                      MHSAS_09 = DV02_09,
                                      
                                      KÜ_ext_01 = KO01_01,
                                      KÜ_ext_02 = KO01_02,
                                      KÜ_int_01 = KO01_03,
                                      KÜ_int_02 = KO01_04,
                                      
                                      UBogen_KA_01 = SK01_07,
                                      UBogen_KA_02 = SK01_08,
                                      UBogen_KA_03 = SK01_09,
                                      UBogen_KA_04 = SK01_10,
                                      UBogen_KA_05 = SK01_11,
                                      UBogen_KA_06 = SK01_12,
                                      
                                      SIAS_01 = SK03_02,
                                      SIAS_02 = SK03_04,
                                      SIAS_03 = SK03_06,
                                      SIAS_04 = SK03_08,
                                      SIAS_05 = SK03_10,
                                      SIAS_06 = SK03_13,
                                      
                                      IDQ_ConscAv_01 = SV01_15, #Illness Denial - Conscious Avoidance
                                      IDQ_ConscAv_02 = SV01_16,
                                      IDQ_ConscAv_03 = SV01_17,
                                      IDQ_ConscAv_04 = SV01_18,
                                      IDQ_ConscAv_05 = SV01_19,
                                      IDQ_ConscAv_06 = SV01_20,
                                      IDQ_ConscAv_07 = SV01_21,
                                      IDQ_ConscAv_08 = SV01_22,
                                      
                                      IDQ_DenialNeg_01 = SV01_01, #Illness Denial - Denial of Negative Emotions
                                      IDQ_DenialNeg_02 = SV01_02,
                                      IDQ_DenialNeg_03 = SV01_03,
                                      IDQ_DenialNeg_04 = SV01_04, 
                                      IDQ_DenialNeg_05 = SV01_05,
                                      IDQ_DenialNeg_06 = SV01_06,
                                      IDQ_DenialNeg_07 = SV01_07,
                                      
                                      IDQ_ResistCh_01 = SV01_08, #Illness Denial - Resistance to Change 
                                      IDQ_ResistCh_02 = SV01_09,
                                      IDQ_ResistCh_03 = SV01_10, 
                                      IDQ_ResistCh_04 = SV01_11, 
                                      IDQ_ResistCh_05 = SV01_12,
                                      IDQ_ResistCh_06 = SV01_13,
                                      IDQ_ResistCh_07 = SV01_14,
                                      
                                      NEOffi_Ext_01 = PK01_07, #Big-5 Extraversion
                                      NEOffi_Ext_02 = PK01_08,
                                      NEOffi_Ext_03 = PK01_09,
                                      NEOffi_Ext_04 = PK01_10,
                                      NEOffi_Ext_05 = PK01_11,
                                      NEOffi_Ext_06 = PK01_12,
                                      
                                      NEOffi_Neur_01 = PK01_01, #Big-5 Neurotizismus
                                      NEOffi_Neur_02 = PK01_02,
                                      NEOffi_Neur_03 = PK01_03,
                                      NEOffi_Neur_04 = PK01_04,
                                      NEOffi_Neur_05 = PK01_05,
                                      NEOffi_Neur_06 = PK01_06,
                                      
                                      Optimismus = SO01_01, #dispositionaler Optimismus
                                      Pessimismus = SO02_01, #dispositionaler Pessimismus
                                      
                                      BSSS_need_01 = SU01_01, #Need for Support
                                      BSSS_need_02 = SU01_02,
                                      BSSS_need_03 = SU01_03,
                                      BSSS_need_04 = SU01_04,
                                      
                                      BSSS_seek_01 = SU01_05, #Support Seeking
                                      BSSS_seek_02 = SU01_06,
                                      BSSS_seek_03 = SU01_07,
                                      BSSS_seek_04 = SU01_08,
                                      BSSS_seek_05 = SU01_09,
                                      
                                      BSSS_receive_emo_01 = SU03_01, #actually received Support - Emotional
                                      BSSS_receive_emo_02 = SU03_02,
                                      BSSS_receive_emo_03 = SU03_03,
                                      BSSS_receive_emo_04 = SU03_04,
                                      BSSS_receive_emo_05 = SU03_05,
                                      BSSS_receive_emo_06 = SU03_06,
                                      
                                      BSSS_receive_inst_01 = SU03_07, #actually received Support - Instrumentell
                                      BSSS_receive_inst_02 = SU03_08,
                                      BSSS_receive_inst_03 = SU03_09,
                                      
                                      BSSS_receive_info_01 = SU03_10, #actually received Support - Informational
                                      BSSS_receive_info_02 = SU03_11,
                                      
                                      BSSS_receive_sat_01 = SU03_12, #actually received Support - Satisfaction
                                      
                                      SozU_01 = SU04_01, #F-Soz-U-K6 - Soziale Unterstützung
                                      SozU_02 = SU04_02,
                                      SozU_03 = SU04_03,
                                      SozU_04 = SU04_04,
                                      SozU_05 = SU04_05,
                                      SozU_06 = SU04_06,
                                      
                                      SD_Alter = SD02_01,
                                      SD_Bildung = SD03,
                                      SD_Bildung_anderes = SD03_09,
                                      SD_Tätigkeit = SD04,
                                      SD_Tätigkeit_anderes = SD04_09,
                                      SD_Berufsfeld = SD05,
                                      SD_Berufsfeld_anderes = SD05_08,
                                      SD_Beziehung = SD06,
                                      SD_Beziehung_anderes = SD06_06,
                                      SD_Kinder = SD07,
                                      SD_Kinder_Anzahl = SD10_01,
                                      SD_Wohnsituation = SD08,
                                      SD_Wohnsituation_anderes = SD08_06,
                                      
                                      Unterstützung_offen = LZ13_01,
                                      Hürden_offen = HU01_01, #allgemeine Hürdenabfragen
                                      Hürden_Anzahl = HU02,
                                      
                                      Hürden_alleineSchaffen = HU02_01, #Hürden Variablen in der ersten "einfachen" MC-Auswahl
                                      Hürden_keineZeit = HU02_02,
                                      Hürden_Antriebslos = HU02_03,
                                      Hürden_AngstKonsequenz = HU02_04,
                                      Hürden_keinVertrauen = HU02_05,
                                      Hürden_finanzielleBarrieren = HU02_06,
                                      Hürden_DruckPrivat = HU02_07,
                                      Hürden_DruckBeruf = HU02_08,
                                      Hürden_Substanzkonsum = HU02_09,
                                      Hürden_geringerSelbstwert = HU02_10,
                                      Hürden_keineUnterstützung = HU02_11,
                                      Hürden_StigmaUmfeld = HU02_12,
                                      Hürden_negErfahrungen = HU02_13,
                                      Hürden_negErwartungen = HU02_14,
                                      Hürden_Unwissenheit = HU02_15,
                                      Hürden_Wartezeiten = HU02_16,
                                      Hürden_keinProblembewusstsein = HU02_17,
                                      Hürden_keinVerständnis = HU02_18,
                                      Hürden_Scham = HU02_19,
                                      Hürden_TradGeschlechterrolle = HU02_20,
                                      
                                      HüRang_Schwierig_alleineSchaffen = HU03_01, #Rankingposition der einzelnen Hürden hinsichtlich der Schwierigkeit
                                      HüRang_Schwierig_keineZeit = HU03_02,
                                      HüRang_Schwierig_Antriebslos = HU03_03,
                                      HüRang_Schwierig_AngstKonsequenz = HU03_04,
                                      HüRang_Schwierig_keinVertrauen = HU03_05,
                                      HüRang_Schwierig_finanzielleBarrieren = HU03_06,
                                      HüRang_Schwierig_DruckPrivat = HU03_07,
                                      HüRang_Schwierig_DruckBeruf = HU03_08,
                                      HüRang_Schwierig_Substanzkonsum = HU03_09,
                                      HüRang_Schwierig_geringerSelbstwert = HU03_10,
                                      HüRang_Schwierig_keineUnterstützung = HU03_11,
                                      HüRang_Schwierig_StigmaUmfeld = HU03_12,
                                      HüRang_Schwierig_negErfahrungen = HU03_13,
                                      HüRang_Schwierig_negErwartungen = HU03_14,
                                      HüRang_Schwierig_Unwissenheit = HU03_15,
                                      HüRang_Schwierig_Wartezeiten = HU03_16,
                                      HüRang_Schwierig_keinProblembewusstsein = HU03_17,
                                      HüRang_Schwierig_keinVerständnis = HU03_18,
                                      HüRang_Schwierig_Scham = HU03_19,
                                      HüRang_Schwierig_TradGeschelchterrolle = HU03_20,
                                      
                                      HüRang_Zeit_alleineSchaffen = HU04_01, #Rankingposition der einzelnen Hürden hinsichtlich der Schwierigkeit
                                      HüRang_Zeit_keineZeit = HU04_02,
                                      HüRang_Zeit_Antriebslos = HU04_03,
                                      HüRang_Zeit_AngstKonsequenz = HU04_04,
                                      HüRang_Zeit_keinVertrauen = HU04_05,
                                      HüRang_Zeit_finanzielleBarrieren = HU04_06,
                                      HüRang_Zeit_DruckPrivat = HU04_07,
                                      HüRang_Zeit_DruckBeruf = HU04_08,
                                      HüRang_Zeit_Substanzkonsum = HU04_09,
                                      HüRang_Zeit_geringerSelbstwert = HU04_10,
                                      HüRang_Zeit_keineUnterstützung = HU04_11,
                                      HüRang_Zeit_StigmaUmfeld = HU04_12,
                                      HüRang_Zeit_negErfahrungen = HU04_13,
                                      HüRang_Zeit_negErwartungen = HU04_14,
                                      HüRang_Zeit_Unwissenheit = HU04_15,
                                      HüRang_Zeit_Wartezeiten = HU04_16,
                                      HüRang_Zeit_keinProblembewusstsein = HU04_17,
                                      HüRang_Zeit_keinVerständnis = HU04_18,
                                      HüRang_Zeit_Scham = HU04_19,
                                      HüRang_Zeit_TradGeschelchterrolle = HU04_20,
                                      
                                      Case = CASE,
                                      Datum = STARTED,
                                      Dauer = TIME_SUM)

Frauen_data_reduced <- Frauen_data_rename %>% select(-SERIAL, -REF, -QUESTNNR, -MODE, -TIME001, -TIME002, -TIME003, -TIME004, -TIME005, -TIME006, -TIME007, -TIME008,
                                                           -TIME009, -TIME010, -TIME011, -TIME012, -TIME013, -TIME014, -TIME015, -TIME016, -TIME017, -TIME018, -TIME019,
                                                           -TIME020, -TIME021, -TIME022, -TIME023, -TIME024, -TIME025, 
                                                           -MAILSENT, -LASTDATA, -STATUS, -FINISHED, -Q_VIEWER, -LASTPAGE, -MAXPAGE, -MISSING, -MISSREL, -TIME_RSI)

Frauen_data_recoded <- Frauen_data_reduced %>% mutate(
                                               across(PHQ2_01:PHQ2_02, ~ . - 1),
  
                                                      MHSAS_02 = 8 - MHSAS_02, #inverse Items umcodieren  
                                                      MHSAS_05 = 8 - MHSAS_05,  
                                                      MHSAS_06 = 8 - MHSAS_06,  
                                                      MHSAS_08 = 8 - MHSAS_08,  
                                                      MHSAS_09 = 8 - MHSAS_09,
                                                      
                                                      PAREMO_04 = 7 - PAREMO_04,
                                                      PAREMO_06 = 7 - PAREMO_06,
  
  
                                               across(IDQ_DenialNeg_01:IDQ_DenialNeg_07, ~ . - 1), #IDQ auf 0 & 1 umcodieren
                                               across(IDQ_ResistCh_01:IDQ_ResistCh_07, ~ . - 1),
                                               
                                               across(IDQ_ConscAv_01:IDQ_ConscAv_08, ~ . - 1),
                                               
                                               across(c(IDQ_DenialNeg_04, IDQ_ResistCh_03, IDQ_ResistCh_04, IDQ_ResistCh_07), ~ 1 - .),
  
                                                      IDQ_DenialNeg_R_01 = 1 - IDQ_DenialNeg_01,
                                                      IDQ_DenialNeg_R_02 = 1 - IDQ_DenialNeg_02,
                                                      IDQ_DenialNeg_R_03 = 1 - IDQ_DenialNeg_03,
                                                      IDQ_DenialNeg_R_04 = 1 - IDQ_DenialNeg_04,
                                                      IDQ_DenialNeg_R_05 = 1 - IDQ_DenialNeg_05,
                                                      IDQ_DenialNeg_R_06 = 1 - IDQ_DenialNeg_06,
                                                      IDQ_DenialNeg_R_07 = 1 - IDQ_DenialNeg_07,
                                               
                                                      IDQ_ConscAv_R_01 = 1 - IDQ_ConscAv_01,
                                                      IDQ_ConscAv_R_02 = 1 - IDQ_ConscAv_02,
                                                      IDQ_ConscAv_R_03 = 1 - IDQ_ConscAv_03,
                                                      IDQ_ConscAv_R_04 = 1 - IDQ_ConscAv_04,
                                                      IDQ_ConscAv_R_05 = 1 - IDQ_ConscAv_05,
                                                      IDQ_ConscAv_R_06 = 1 - IDQ_ConscAv_06,
                                                      IDQ_ConscAv_R_07 = 1 - IDQ_ConscAv_07,
                                                      IDQ_ConscAv_R_08 = 1 - IDQ_ConscAv_08,
  
                                                      DepSter_Mis_R_01 = 6 - DepSter_Mis_01,
                                                      DepSter_Mis_R_02 = 6 - DepSter_Mis_02,
                                                      DepSter_Mis_R_03 = 6 - DepSter_Mis_03,
                                                      DepSter_Mis_R_04 = 6 - DepSter_Mis_04,
                                                      DepSter_Mis_R_05 = 6 - DepSter_Mis_05,
                                                      DepSter_Mis_R_06 = 6 - DepSter_Mis_06,
                                                      DepSter_Mis_R_07 = 6 - DepSter_Mis_07,
                                                      DepSter_Mis_R_08 = 6 - DepSter_Mis_08,
                                               
                                                      PAREMO_R_01 = 7 - PAREMO_01,
                                                      PAREMO_R_02 = 7 - PAREMO_02,
                                                      PAREMO_R_03 = 7 - PAREMO_03,
                                                      PAREMO_R_04 = 7 - PAREMO_04,
                                                      PAREMO_R_05 = 7 - PAREMO_05,
                                                      PAREMO_R_06 = 7 - PAREMO_06,
                                                      
                                                      SELSA_Soz_01 = 8 - SELSA_Soz_01,
                                                      SELSA_Soz_02 = 8 - SELSA_Soz_02,
                                                      SELSA_Soz_04 = 8 - SELSA_Soz_04,
  
                                                      SELSA_Rom_01 = 8 - SELSA_Rom_01,
                                                      SELSA_Rom_02 = 8 - SELSA_Rom_02,
                                                      SELSA_Rom_04 = 8 - SELSA_Rom_04,
                                               
                                                      SELSA_Fam_03 = 8 - SELSA_Fam_03,
                                                      SELSA_Fam_04 = 8 - SELSA_Fam_04,
                                                      SELSA_Fam_05 = 8 - SELSA_Fam_05,

                                                      KÜ_int_R_01 = 6 - KÜ_int_01,
                                                      KÜ_int_R_02 = 6 - KÜ_int_02,
  
                                                      Alexithymie_05 = 7 - Alexithymie_05, 
                                                      Alexithymie_06 = 7 - Alexithymie_06,
                                                      Alexithymie_07 = 7 - Alexithymie_07,
                                               
                                                      BSSS_need_04 = 5 - BSSS_need_04, #inverses Item
  
                                                      ProzEinschränkung = (ProzEinschränkung - 1)*10,
  
                                                      VergleichAndere = 9 - VergleichAndere, #recodieren sodass Hohe Ausprägung = Schlechter
                                                      
                                                      NAs = rowSums(is.na(.))
                                                      ) 

#summary(Frauen_data_recoded$NAs) 
Frauen_data_noNAs <- Frauen_data_recoded %>% filter(NAs < 200)

Frauen_data_extended <- Frauen_data_noNAs %>% select(order(colnames(.))
                                        ) %>% mutate(PHQ2_Sum = rowSums(select(., PHQ2_01:PHQ2_02), na.rm = TRUE),
                                                     IDQ_DenialNeg_Sum = rowSums(select(., IDQ_DenialNeg_01:IDQ_DenialNeg_07), na.rm = TRUE),
                                                     IDQ_DenialNeg_Sum_R = rowSums(select(., IDQ_DenialNeg_R_01:IDQ_DenialNeg_R_07), na.rm = TRUE),
                                                     IDQ_ResistCh_Sum = rowSums(select(., IDQ_ResistCh_01:IDQ_ResistCh_07), na.rm = TRUE),
                                                     IDQ_ConscAv_Sum = rowSums(select(., IDQ_ConscAv_01:IDQ_ConscAv_08), na.rm = TRUE),
                                                     IDQ_ConscAv_Sum_R = rowSums(select(., IDQ_ConscAv_R_01:IDQ_ConscAv_R_08), na.rm = TRUE),
                                                     MHSAS_MW = rowMeans(select(., MHSAS_01:MHSAS_09), na.rm = TRUE),
                                                     DepSter_Lit_MW = rowMeans(select(., DepSter_Lit_01:DepSter_Lit_06), na.rm = TRUE),
                                                     DepSter_Mis_MW = rowMeans(select(., DepSter_Mis_01:DepSter_Mis_08), na.rm = TRUE),
                                                     DepSter_Mis_MW_R = rowMeans(select(., DepSter_Mis_R_01:DepSter_Mis_R_08), na.rm = TRUE),
                                                     UBogen_KA_MW = rowMeans(select(., UBogen_KA_01:UBogen_KA_06), na.rm = TRUE),
                                                     SIAS_MW = rowMeans(select(., SIAS_01:SIAS_06), na.rm = TRUE), 
                                                     KÜ_ext_MW = rowMeans(select(., KÜ_ext_01:KÜ_ext_02), na.rm = TRUE),
                                                     KÜ_int_MW = rowMeans(select(., KÜ_int_01:KÜ_int_02), na.rm = TRUE),
                                                     KÜ_int_MW_R = rowMeans(select(., KÜ_int_R_01:KÜ_int_R_02), na.rm = TRUE),
                                                     SELSA_Soz_MW = rowMeans(select(., SELSA_Soz_01:SELSA_Soz_05), na.rm = TRUE),
                                                     SELSA_Fam_MW = rowMeans(select(., SELSA_Fam_01:SELSA_Fam_05), na.rm = TRUE),
                                                     SELSA_Rom_MW = rowMeans(select(., SELSA_Rom_01:SELSA_Rom_05), na.rm = TRUE),
                                                     Alexithymie_MW = rowMeans(select(., Alexithymie_01:Alexithymie_10), na.rm = TRUE),
                                                     PAREMO_MW = rowMeans(select(., PAREMO_01:PAREMO_06), na.rm = TRUE),
                                                     PAREMO_MW_R = rowMeans(select(., PAREMO_R_01:PAREMO_R_06), na.rm = TRUE),
                                                     Veränderung_P_MW = rowMeans(select(., Veränderung_01:Veränderung_08), na.rm = TRUE),
                                                     Veränderung_S_MW = rowMeans(select(., Veränderung_09:Veränderung_16), na.rm = TRUE),
                                                     NEOffi_Neur_MW = rowMeans(select(., NEOffi_Neur_01:NEOffi_Neur_06), na.rm = TRUE),
                                                     NEOffi_Ext_MW = rowMeans(select(., NEOffi_Ext_01:NEOffi_Ext_06), na.rm = TRUE),
                                                     BSSS_need_MW = rowMeans(select(., BSSS_need_01:BSSS_need_04), na.rm = TRUE),
                                                     BSSS_seek_MW = rowMeans(select(., BSSS_seek_01:BSSS_seek_05), na.rm = TRUE),
                                                     BSSS_receive_MW = rowMeans(select(., BSSS_receive_emo_01:BSSS_receive_sat_01), na.rm = TRUE),
                                                     SozU_MW = rowMeans(select(., SozU_01:SozU_06), na.rm = TRUE),
                                                     ) %>% filter(PHQ2_Sum >= 2 & Case != 347)  

interim_data <- Frauen_data_extended %>% select(Case, Latenz_Erkenntnis, Latenz_Wahrnehmung, Latenz_Problembewusst, Latenz_Öffnung, Latenz_Gesamt
                                      ) %>% filter(!is.na(Latenz_Erkenntnis) | !is.na(Latenz_Wahrnehmung) | !is.na(Latenz_Problembewusst) | !is.na(Latenz_Öffnung) | !is.na(Latenz_Gesamt)
                                      ) %>% filter(is.na(as.numeric(Latenz_Erkenntnis)) | is.na(as.numeric(Latenz_Wahrnehmung)) | is.na(as.numeric(Latenz_Problembewusst)) | is.na(as.numeric(Latenz_Öffnung)) | is.na(as.numeric(Latenz_Gesamt)))          

Frauen_data_typed <- Frauen_data_extended %>% mutate(profHilfe = case_when(
                                 Case == 227 ~ 5,
                                 Case == 904 ~ 5,
                                 Case == 1063 ~ 2,
                                 TRUE ~ profHilfe)
                           ) %>% mutate(Kontakt = case_when(
                                 profHilfe %in% c(1, 2, 3, 4) ~ 1,
                                 profHilfe %in% c(5) ~ 2)
                           ) %>% mutate(Diagnose = case_when(
                                 Case == 976 ~ 2,
                                 TRUE ~ Diagnose)
                           ) %>% mutate(Erkenntnis = case_when(
                                 Case %in% c(298, 357, 1049) ~ 1,
                                 Case %in% c(592, 976) ~ 2,
                                 Case %in% c(918, 225, 275) ~ 3,
                                 TRUE ~ Erkenntnis)
                           ) %>% mutate(Erstkontakt = case_when(
                                 Case %in% c() ~ 1,
                                 Case %in% c(279, 209, 929, 917, 878, 252, 505) ~ 2, #116-117 oder Beratunsgstelle
                                 Case %in% c(861, 225, 351) ~ 3,
                                 Case %in% c(884, 210, 961, 973, 357, 921, 1047) ~ 4, #anderer Facharzt
                                 Case %in% c(998, 238, 275) ~ 5, #Zwansgeinweisung
                                 Case %in% c(904, 570, 869) ~ 6, #nicht zuordbar
                                 TRUE ~ Erstkontakt)
                           ) %>% mutate(SD_Bildung = case_when(
                                 Case == 238 ~ 1, 
                                 Case == 1013 ~ 3,
                                 Case == 144 ~ 4,
                                 Case %in% c(901, 1025) ~ 7,
                                 Case == 987 ~ 9,
                                 TRUE ~ SD_Bildung)
                           ) %>% mutate(SD_Tätigkeit = case_when(
                                 Case %in% c() ~ 1,
                                 Case %in% c(225) ~ 2,
                                 Case %in% c() ~ 3,
                                 Case %in% c() ~ 4,
                                 Case %in% c(986) ~ 5,
                                 Case %in% c(238, 748, 907, 968, 1017, 1051) ~ 6,
                                 Case %in% c(570) ~ 7,
                                 Case %in% c() ~ 8,
                                 Case %in% c(968) ~ 9,
                                 TRUE ~ SD_Tätigkeit) 
                           ) %>% mutate(SD_Wohnsituation = case_when(
                                 Case %in% c(226, 423) ~ 5,
                                 Case %in% c(238) ~ 6,
                                 TRUE ~ SD_Wohnsituation) 
                           ) %>% mutate(SD_Beziehung = case_when(
                                 Case %in% c(226, 975, 1049) ~ 1,
                                 TRUE ~ SD_Beziehung)
                           ) %>% mutate(SD_Berufsfeld = case_when(
                                 Case %in% c(570, 929) ~ 1,
                                 Case %in% c(331,911, 972) ~ 2,
                                 Case %in% c(213, 249) ~ 3,
                                 Case %in% c(592, 868) ~ 4,
                                 Case %in% c(114, 238, 279, 734, 740, 833, 835, 918) ~ 5,
                                 Case %in% c() ~ 6,
                                 Case %in% c() ~ 7,
                                 Case %in% c(299) ~ 8,
                                 TRUE ~ SD_Berufsfeld)
                           ) %>% mutate(PostPartDep_dich = case_when(
                                 PostPartDep == 1 ~ 1,
                                 PostPartDep %in% c(2, 3) ~ 2,
                                 TRUE ~ PostPartDep)
                           ) %>% mutate(Latenz_Erkenntnis = as.numeric(Latenz_Erkenntnis),
                                 Latenz_Erkenntnis = case_when(
                                 Case %in% c(275, 357, 545, 861, 869) ~ NA_real_,
                                 Case == 729 ~ 520,
                                 Case == 895 ~ 7.5,
                                 Case == 904 ~ 12.5,
                                 Case == 990 ~ 8.5,
                                 TRUE ~ Latenz_Erkenntnis)
                           ) %>% mutate(Latenz_Wahrnehmung = as.numeric(Latenz_Wahrnehmung),
                                 Latenz_Wahrnehmung = case_when(
                                 Case %in% c(357, 802) ~ NA,
                                 Case %in% c(740, 1075) ~ 0,
                                 Case == 209 ~ 0.5,
                                 Case == 423 ~ 2.5,
                                 Case == 895 ~ 3.5,
                                 Case == 904 ~ 9,
                                 Case == 990 ~ 8.5,
                                 Case == 911 ~ 312,
                                 TRUE ~ Latenz_Wahrnehmung)
                           ) %>% mutate(Latenz_Problembewusst = as.numeric(Latenz_Problembewusst),
                                 Latenz_Problembewusst = case_when(
                                 Case %in% c(357, 740, 869) ~ NA,
                                 Case == 729 ~ 0,
                                 Case == 895 ~ 5.5,
                                 Case == 990 ~ 8.5,
                                 Case == 911 ~ 312,
                                 TRUE ~ Latenz_Problembewusst) 
                           ) %>% mutate(Latenz_Öffnung = as.numeric(Latenz_Öffnung),
                                 Latenz_Öffnung = case_when(
                                 Case %in% c(318, 357, 545, 784, 861, 869) ~ NA,
                                 Case %in% c(290, 740) ~ 0,
                                 Case == 209 ~ 0.5,
                                 Case == 301 ~ 3.5,
                                 Case == 518 ~ 156,
                                 Case == 729 ~ 78,
                                 Case == 895 ~ 7.5,
                                 Case == 911 ~ 780,
                                 TRUE ~ Latenz_Öffnung) 
                           ) %>% mutate(Latenz_Gesamt = as.numeric(Latenz_Gesamt),
                                 Latenz_Gesamt = case_when(
                                 Case %in% c(357, 545, 570, 780, 861, 869) ~ NA,
                                 Case == 301 ~ 8.5,
                                 Case == 895 ~ 9.5,
                                 Case == 911 ~ 260,
                                 Case == 729 ~ 676,
                                 TRUE ~ Latenz_Gesamt 
                                 ))

Frauen_data_filter <- Frauen_data_typed %>% filter(!is.na(Kontakt))

Hürden_offen_data <- read_csv2("Hürden_offen_ausgewertet.csv")
Unterstützung_offen_data <- read_csv2("Unterstützung_offen_ausgewertet.csv")

Frauen_data_final <- full_join(Frauen_data_filter, Hürden_offen_data, by = "Case")
Frauen_data_final <- full_join(Frauen_data_final, Unterstützung_offen_data, by = "Case")

write_csv(Frauen_data_final, "Frauen_data.csv")
write_csv(Frauen_data_final, "C:/Users/ann-k/sciebo - Grotenburg, Ann-Kathrin (94U1XE@rwth-aachen.de)@rwth-aachen.sciebo.de/Arbeit/08 Promotion/08_Datensätze/MännerxFrauen_Rproject/Frauen_data.csv")
write.xlsx(Frauen_data_final, "Frauen_data.xlsx", showNA = FALSE)

summary(Frauen_data_final$NAs)

