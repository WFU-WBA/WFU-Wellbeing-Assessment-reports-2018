
#This script creates tables and charts for the wellbeing report

## @knitr chartstables

#Libraries and source documents ------------------------------------------
library(kableExtra)
library(knitr)

#From utilities
library(tidyverse)
library(ggrepel)

#From import_and_clean syntax
library(purrr)
library(tidyverse)
#devtools::install_github("medewitt/datademon")
devtools::install_github("WFU-WBA/wellbeing_tools")
library(datademon)
library(qualtRics)
library(rlang)

datademon::update_geom_font_defaults()

source("likerts.R")
source("import_and_clean_wb_report.R")
source("utilities.R")

#creating column names
table_columns <- c('Response', ' School Freq', ' School %', ' School Valid %', 'Ag Freq', 'Ag %', 'Ag Valid %')


# Create school-specific data set ------------------------------------------------
#make school-specific data frame
school_of_interest <- "insert name"

oneschool_complete <- df_survey_complete %>% 
  filter(school_name == school_of_interest) %>% 
  mutate(response = as.numeric(response)) %>% 
  arrange(question, response)
####################################################################


# Call school-sepcific additional items -------------------
#Medical care additional items=======
# tmedcare_1_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MED_CARE_MED_CARE_1",
#                                    scale_label = yn)
# 
# cmedcare_1_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MED_CARE_MED_CARE_1",
# 
#                                    scale_label = yn, graph_label = "yn")
# 
# tmedcare_2_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MED_CARE_MED_CARE_2",
#                                    scale_label = yn)
# 
# cmedcare_2_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MED_CARE_MED_CARE_2",
# 
#                                    scale_label = yn, graph_label = "yn")
# 
# tmedcare_3_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MED_CARE_MED_CARE_3",
#                                    scale_label = yn)
# 
# cmedcare_3_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MED_CARE_MED_CARE_3",
# 
#                                    scale_label = yn, graph_label = "yn")

# # #Mental health care additional items =================
# tmentcare_1_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MENTAL_HEALTH_MENTAL_HEALTH_1",
#                                     scale_label = yn)
# 
# cmentcare_1_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MENTAL_HEALTH_MENTAL_HEALTH_1",
# 
#                                     scale_label = yn, graph_label = "yn")
# 
# tmentcare_2_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MENTAL_HEALTH_MENTAL_HEALTH_2",
#                                     scale_label = yn)
# 
# cmentcare_2_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MENTAL_HEALTH_MENTAL_HEALTH_2",
# 
#                                     scale_label = yn, graph_label = "yn")
# 
# tmentcare_3_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MENTAL_HEALTH_MENTAL_HEALTH_3",
#                                     scale_label = yn)
# 
# cmentcare_3_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MENTAL_HEALTH_MENTAL_HEALTH_3",
# 
#                                     scale_label = yn, graph_label = "yn")
# 
# tmentcare_4_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MENTAL_HEALTH_MENTAL_HEALTH_4",
#                                     scale_label = yn)
# 
# cmentcare_4_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MENTAL_HEALTH_MENTAL_HEALTH_4",
# 
#                                     scale_label = yn, graph_label = "yn")


#MONEY_MONEY additional items ===========================
# tmoney_add_1_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MONEY_MONEY_1",
#                                      scale_label = often)
# 
# cmoney_add_1_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MONEY_MONEY_1",
# 
#                                      scale_label = often, graph_label = "often")
# 
#  tmoney_add_2_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MONEY_MONEY_2",
#                                      scale_label = often)
# 
# cmoney_add_2_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MONEY_MONEY_2",
# 
#                                      scale_label = often, graph_label = "often")
# 
# tmoney_add_3_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MONEY_MONEY_3",
#                                      scale_label = often)
# 
# cmoney_add_3_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "MONEY_MONEY_3",
# 
#                                     scale_label = often, graph_label = "often")
# 
# 
#Campus safety additional items =================
# tsafe_1_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "CAMP_SAFE_CAMP_SAFE_1",
#                                 scale_label = agreement)
# 
# csafe_1_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "CAMP_SAFE_CAMP_SAFE_1",
# 
#                                 scale_label = agreement, graph_label = "agreement")
# 
# tsafe_2_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "CAMP_SAFE_CAMP_SAFE_2",
#                                 scale_label = agreement)
# 
# csafe_2_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "CAMP_SAFE_CAMP_SAFE_2",
# 
#                                 scale_label = agreement, graph_label = "agreement")
# 
# tsafe_3_noag <- make_table_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "CAMP_SAFE_CAMP_SAFE_3",
#                                 scale_label = agreement)
# 
# csafe_3_noag <- make_graph_noag(data =df_survey_complete, school_name = school_of_interest, question_name = "CAMP_SAFE_CAMP_SAFE_3",
# 
#                                 scale_label = agreement, graph_label = "agreement")

#################################################################

# Call school-specific custom items -------------------
#Comment out and fold other schools' items. Unfold and un-comment 
#target school's items.
# Duke's items ============================================================
# t_duke_1_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "DukeSp18_Custom_1_29",
#                       scale_label = dukeagree)
# 
# c_duke_1_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "DukeSp18_Custom_1_29",
# 
#                       scale_label = dukeagree, graph_label = "dukeagree")
# 
# t_duke_2_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "DukeSp18_Custom_1_30",
#                       scale_label = dukeagree)
# 
# c_duke_2_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "DukeSp18_Custom_1_30",
# 
#                       scale_label = dukeagree, graph_label = "dukeagree")
# 
# t_duke_3_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "DukeSp18_Custom_1_31",
#                       scale_label = dukeagree)
# 
# c_duke_3_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "DukeSp18_Custom_1_31",
# 
#                       scale_label = dukeagree, graph_label = "dukeagree")

# CCA's items ============================================================
# t_CCA_1_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "CA_CotASp18_Custom_1",
#                       scale_label = yn)
# 
# c_CCA_1_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "CA_CotASp18_Custom_1",
# 
#                       scale_label = yn, graph_label = "yn")
# 
# t_CCA_2_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "CA_CotASp18_Custom_2",
#                     scale_label = often)
# 
# c_CCA_2_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "CA_CotASp18_Custom_2",
# 
#                     scale_label = often, graph_label = "often")
# 
# t_CCA_3_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "CA_CotASp18_Custom_3",
#                     scale_label = agreementNA)
# 
# c_CCA_3_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "CA_CotASp18_Custom_3",
# 
#                     scale_label = agreementNA, graph_label = "agreementNA")
# 

# Chatham's items =========================================================
# t_chatham_1_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "ChathamSp18_Custom_1",
#                       scale_label = yn)
# 
# c_chatham_1_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "ChathamSp18_Custom_1",
# 
#                       scale_label = yn, graph_label = "yn")
# 
# t_chatham_2_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "ChathamSp18_Custom_2",
#                       scale_label = yn)
# 
# c_chatham_2_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "ChathamSp18_Custom_2",
# 
#                       scale_label = yn, graph_label = "yn")
# 
# t_chatham_3_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "ChathamSp18_Custom_3",
#                       scale_label = yn)
# 
# c_chatham_3_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "ChathamSp18_Custom_3",
# 
#                       scale_label = yn, graph_label = "yn")

# Doane's items ===========================================================
# t_doane_1_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "DoaneSp18_Custom_1",
#                       scale_label = yn)
# 
# c_doane_1_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "DoaneSp18_Custom_1",
# 
#                       scale_label = yn, graph_label = "yn")
# 
# t_doane_2_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "DoaneSp18_Custom_2",
#                       scale_label = agreementNA)
# 
# c_doane_2_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "DoaneSp18_Custom_2",
# 
#                       scale_label = agreementNA, graph_label = "agreementNA")
# 
# t_doane_3_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "DoaneSp18_Custom_3",
#                       scale_label = yn)
# 
# c_doane_3_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "DoaneSp18_Custom_3",
# 
#                       scale_label = yn, graph_label = "yn")


# ECU's items =============================================================
# t_ecu_1_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "ECUSp18_Custom_1",
#                       scale_label = yn)
# 
# c_ecu_1_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "ECUSp18_Custom_1",
# 
#                       scale_label = yn, graph_label = "yn")
# 
# t_ecu_2_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "ECUSp18_Custom_2",
#                       scale_label = often)
# 
# c_ecu_2_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "ECUSp18_Custom_2",
# 
#                       scale_label = often, graph_label = "often")
# 
# t_ecu_3_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "ECUSp18_Custom_3",
#                       scale_label = helpful)
# 
# c_ecu_3_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "ECUSp18_Custom_3",
# 
#                       scale_label = helpful, graph_label = "helpful")


# GA Tech's items =========================================================
# t_gt_1_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "GTSp18_Custom_1",
#                       scale_label = yn)
# 
# c_gt_1_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "GTSp18_Custom_1",
# 
#                       scale_label = yn, graph_label = "yn")
# 
# t_gt_2_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "GTSp18_Custom_2",
#                       scale_label = yn)
# 
# c_gt_2_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "GTSp18_Custom_2",
# 
#                       scale_label = yn, graph_label = "yn")
# 
# t_gt_3_noag <- make_table(data =oneschool_complete, school_name = school_of_interest, question_name = "GTSp18_Custom_3",
#                       scale_label = yn)
# 
# c_gt_3_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "GTSp18_Custom_3",
# 
#                       scale_label = yn, graph_label = "yn")
# 
# 

# Juniata's items =========================================================
# t_juniata_1_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "JuniataSp18_Custom_1",
#                                    scale_label = yn)
# 
# c_juniata_1_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "JuniataSp18_Custom_1",
# 
#                                scale_label = yn, graph_label = "yn")
# 
# 
# 
# t_juniata_2_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "JuniataSp18_Custom_2",
#                          scale_label = agreementNA)
# 
# c_juniata_2_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "JuniataSp18_Custom_2",
# 
#                          scale_label = agreementNA, graph_label = "agreementNA")
# 
# t_juniata_3_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "JuniataSp18_Custom_3",
#                          scale_label = yn)
# 
# c_juniata_3_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "JuniataSp18_Custom_3",
# 
#                          scale_label = yn, graph_label = "yn")

# Trinity's items =========================================================
# t_trinity_1_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "TrinitySp18_Custom_1",
#                       scale_label = yn)
# 
# c_trinity_1_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "TrinitySp18_Custom_1",
# 
#                       scale_label = yn, graph_label = "yn")
# 
# t_trinity_2_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "TrinitySp18_Custom_2",
#                       scale_label = often)
# 
# c_trinity_2_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "TrinitySp18_Custom_2",
# 
#                       scale_label = often, graph_label = "often")
# 
# t_trinity_3_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "TrinitySp18_Custom_3",
#                       scale_label = agreementNA)
# 
# c_trinity_3_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "TrinitySp18_Custom_3",
# 
#                       scale_label = agreementNA, graph_label = "agreementNA")


# U of Richmond's items ===================================================
# t_ur_1_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "URSp18_Custom_1",
#                       scale_label = yn)
# 
# c_ur_1_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "URSp18_Custom_1",
# 
#                       scale_label = yn, graph_label = "yn")
# 
# t_ur_2_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "URSp18_Custom_2",
#                       scale_label = often)
# 
# c_ur_2_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "URSp18_Custom_2",
# 
#                       scale_label = often, graph_label = "often")
# 
# t_ur_3_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "URSp18_Custom_3",
#                       scale_label = yn)
# 
# c_ur_3_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "URSp18_Custom_3",
# 
#                       scale_label = yn, graph_label = "yn")


# UNC Chapel Hill's items =================================================
# t_unc_1_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "UNCSp18_Custom_1",
#                       scale_label = yn)
# 
# c_unc_1_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "UNCSp18_Custom_1",
# 
#                       scale_label = yn, graph_label = "yn")
# 
# t_unc_2_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "UNCSp18_Custom_2",
#                       scale_label = often)
# 
# c_unc_2_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "UNCSp18_Custom_2",
# 
#                       scale_label = often, graph_label = "often")
# 
# t_unc_3_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "UNCSp18_Custom_3",
#                       scale_label = agreementNA)
# 
# c_unc_3_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "UNCSp18_Custom_3",
# 
#                       scale_label = agreementNA, graph_label = "agreementNA")
# 

# UNC Pembroke's items ====================================================
# t_pembroke_1_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "UNCPSp18_Custom_1",
#                       scale_label = often)
# 
# c_pembroke_1_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "UNCPSp18_Custom_1",
# 
#                       scale_label = often, graph_label = "often")
# 
# t_pembroke_2_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "UNCPSp18_Custom_2",
#                       scale_label = often)
# 
# c_pembroke_2_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "UNCPSp18_Custom_2",
# 
#                       scale_label = often, graph_label = "often")
# 
# t_pembroke_3_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "UNCPSp18_Custom_3",
#                       scale_label = often)
# 
# c_pembroke_3_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "UNCPSp18_Custom_3",
# 
#                       scale_label = often, graph_label = "often")


# Wake's items ============================================================
# t_wake_1_noag <- make_table_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "WFUSp18_Custom_1",
#                       scale_label = yn)
# 
# c_wake_1_noag <- make_graph_noag(data =oneschool_complete, school_name = school_of_interest, question_name = "WFUSp18_Custom_1",
# 
#                       scale_label = yn, graph_label = "yn")





##############################################################

# Demographics & participation rates ----------------------------------------------------
#calculate N per school
select_id <- oneschool_complete %>% 
  filter(question == "stuid") 
#filter(!is.na(response)) %>% 

school_n <- sum(select_id$freq)  

#calculate N of aggregate
select_id_ag <- df_survey_complete %>%
  filter(question == "stuid")

ag_n <- sum(select_id_ag$freq)

#Create means for individual schools & aggregate, and
#report number of missing cases on age for individual school.
#Start by creating mini data frame for the rotated age variable.
select_age <- oneschool_complete %>% 
  filter(question == "AGE") 

#Create a new df to add response * freq,
#which we then use to calculate the average.
meanage_df <- select_age %>% 
  filter(!is.na(response)) %>% 
  mutate(response = as.integer(response)) %>% 
  mutate(age_freq = (response * freq))

meanage <- round((sum(meanage_df$age_freq)/sum(meanage_df$freq)),digits = 2)

#Use our mini df to identify number of missing cases.
#meanage_missing_df <- oneschool_complete %>% 
  #filter(is.na(response))
  
#meanage_missing <- meanage_missing$freq

#calculate mean age for aggregate data set
select_age_ag<- df_survey_complete %>% 
  filter(question == "AGE") 
  
#Create a new df to add response * freq,
  #which we then use to calculate the average.
meanage_df_ag <- select_age_ag %>% 
  filter(!is.na(response)) %>% 
  mutate(response = as.integer(response)) %>% 
  mutate(age_freq = (response * freq))
  
meanage_ag <- round((sum(meanage_df_ag$age_freq, na.rm=TRUE)/sum(meanage_df_ag$freq, na.rm=TRUE)), digits = 2)

#Identify number of missing cases on age for aggregate.
meanage_missing_df_ag <- select_age_ag %>% 
  filter(is.na(response))

#meanage_missing_ag <- sum(meanage_missing_df_ag$freq)

###Creating objects for valid participation by gender
####First for the school
femaleschooldf <- oneschool_complete %>% 
  filter(question == "GENDER") %>% 
  filter(response == 2) 

femaleschool <- round(femaleschooldf$perc_no_na, digits = 2)

maleschooldf <- oneschool_complete %>% 
  filter(question == "GENDER") %>% 
  filter(response == 1)

maleschool <- round(maleschooldf$perc_no_na, digits = 2)

otherschooldf <- oneschool_complete %>% 
  filter(question == "GENDER") %>% 
  filter(response == 3)

otherschool <- round(otherschooldf$perc_no_na, digits = 2)

####for the aggregate

femaleschool_ag <- round(femaleschooldf$perc_ag_no_na, digits = 2)

maleschool_ag <- round(maleschooldf$perc_ag_no_na, digits = 2)

otherschool_ag <- round(otherschooldf$perc_ag_no_na, digits = 2)

###creating objects for valid percent participation by class
##School
freshschool <- oneschool_complete%>% 
  filter(question == "CLASS")%>% 
  filter(response == 1)

sophschool <- oneschool_complete %>% 
  filter(question == "CLASS")%>% 
  filter(response == 2)

junschool <- oneschool_complete %>% 
  filter(question == "CLASS")%>% 
  filter(response == 3)

senschool <- oneschool_complete %>% 
  filter(question == "CLASS")%>% 
  filter(response == 4)


schfreshvalid <- round(freshschool$perc_no_na, digits = 2)

schsophvalid <- round(sophschool$perc_no_na, digits = 2)

schjunvalid <- round(junschool$perc_no_na, digits = 2)

schsenvalid <- round(senschool$perc_no_na, digits = 2)

##aggregate

agfreshvalid <- round(freshschool$perc_ag_no_na, digits = 2)

agsophvalid <- round(sophschool$perc_ag_no_na, digits = 2)

agjunvalid <- round(junschool$perc_ag_no_na, digits = 2)

agsenvalid <- round(senschool$perc_ag_no_na, digits = 2)


# Start of charts and tables ----------------------------------------------
#Listed in alphabetical order, not in order of survey.

#Abroad========================================
tabroad <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ABROAD",
                        scale_label = ynplan)

cabroad <- make_graph_tall(data =df_survey_complete, school_name = school_of_interest, question_name = "ABROAD",
                      
                      scale_label = ynplan, graph_label = "ynplan")

#Academic engagement===========================

tacaeng_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACAENG_ACAENG_1",
                      scale_label = agreement)

cacaeng_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACAENG_ACAENG_1",
                      
                      scale_label = agreement, graph_label = "agreement")

tacaeng_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACAENG_ACAENG_2",
                        scale_label = agreement)

cacaeng_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACAENG_ACAENG_2",
                        
                        scale_label = agreement, graph_label = "agreement")

tacaeng_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACAENG_ACAENG_3",
                        scale_label = agreement)

cacaeng_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACAENG_ACAENG_3",
                        
                        scale_label = agreement, graph_label = "agreement")

#Activities==============================
#ACT1####

tact1_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_1",
                        scale_label = yn)

#cact1_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_1",
                        
                        #scale_label = yn, graph_label = "yn")

tact1_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_2",
                      scale_label = yn)

#cact1_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_2",
                      
                      #scale_label = yn, graph_label = "yn")

tact1_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_3",
                      scale_label = yn)

#cact1_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_3",
                      
                      #scale_label = yn, graph_label = "yn")

tact1_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_4",
                      scale_label = yn)

#cact1_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_4",
                      
                      #scale_label = yn, graph_label = "yn")

tact1_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_5",
                      scale_label = yn)

#cact1_5 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_5",
                      
                      #scale_label = yn, graph_label = "yn")

tact1_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_6",
                      scale_label = yn)

#cact1_6 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_6",
                    
                      #scale_label = yn, graph_label = "yn")

tact1_7 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_7",
                      scale_label = yn)

#cact1_7 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_7",
                      
                      #scale_label = yn, graph_label = "yn")

tact1_8 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_8",
                      scale_label = yn)

#cact1_8 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_8",
                      
                      #scale_label = yn, graph_label = "yn")

tact1_9 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_9",
                      scale_label = yn)

#cact1_9 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_9",
                      
                      #scale_label = yn, graph_label = "yn")

tact1_10 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_10",
                      scale_label = yn)

#cact1_10 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_10",
                      
                      #scale_label = yn, graph_label = "yn")

tact1_11 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_11",
                      scale_label = yn)

#cact1_11 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_11",
                      
                      #scale_label = yn, graph_label = "yn")

tact1_12 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_12",
                      scale_label = yn)

#cact1_12 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_12",
                      
                      #scale_label = yn, graph_label = "yn")

tact1_13 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_13",
                      scale_label = yn)

#cact1_13 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT1_ACT1_13",
                      
                     #scale_label = yn, graph_label = "yn")

##ACT2##########

tact2_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_1",
                       scale_label = agreement)

cact2_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_1",
                       
                       scale_label = agreement, graph_label = "agreement")

tact2_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_2",
                      scale_label = agreement)

cact2_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_2",
                      
                      scale_label = agreement, graph_label = "agreement")

tact2_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_3",
                      scale_label = agreement)

cact2_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_3",
                      
                      scale_label = agreement, graph_label = "agreement")

tact2_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_4",
                      scale_label = agreement)

cact2_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_4",
                      
                      scale_label = agreement, graph_label = "agreement")

tact2_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_5",
                      scale_label = agreement)

cact2_5 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_5",
                      
                      scale_label = agreement, graph_label = "agreement")

tact2_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_6",
                      scale_label = agreement)

cact2_6 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_6",
                      
                      scale_label = agreement, graph_label = "agreement")

tact2_7 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_7",
                      scale_label = agreement)

cact2_7 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_7",
                      
                      scale_label = agreement, graph_label = "agreement")

tact2_8 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_8",
                      scale_label = agreement)

cact2_8 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_8",
                      
                      scale_label = agreement, graph_label = "agreement")

tact2_9 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_9",
                      scale_label = agreement)

cact2_9 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_9",
                      
                      scale_label = agreement, graph_label = "agreement")

tact2_10 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_10",
                      scale_label = agreement)

cact2_10 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ACT2_ACT2_10",
                      
                      scale_label = agreement, graph_label = "agreement")

##ACT3##########
#The object is tact3
act_1 <- oneschool_complete %>% 
  filter(question == "Act3_1") %>%
  filter(response == 1)

act_2 <- oneschool_complete %>% 
  filter(question == "Act3_2") %>%
  filter(response == 1)

act_3 <- oneschool_complete %>% 
  filter(question == "Act3_3") %>%
  filter(response == 1)

act_4 <- oneschool_complete %>% 
  filter(question == "Act3_4") %>%
  filter(response == 1)

act_5 <- oneschool_complete %>% 
  filter(question == "Act3_5") %>%
  filter(response == 1)

act_6 <- oneschool_complete %>% 
  filter(question == "Act3_6") %>%
  filter(response == 1) %>% 
  mutate(formatted_title = "Other extracurricular activities")

act_7 <- oneschool_complete %>% 
  filter(question == "Act3_7") %>%
  filter(response == 1)

act_data <- bind_rows(act_1, act_2, act_3,
                      act_4, act_5, act_6, act_7)

tact3 <- act_data %>%
  select(question, formatted_title, freq, perc, freq_ag, perc_ag) %>%
  arrange(question) %>%
  ungroup() %>% 
  knitr::kable(col.names = c("Item number", "Item text", "School freq", 
                             "School perc", "Ag freq", "Ag perc"), digits = c(0,2, 2, 2, 2, 2))

#ANXIETY=================================

tanx_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_Negative_ANX_1",
                       scale_label = frequency)

canx_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_Negative_ANX_1",
                       
                       scale_label = frequency, graph_label = "frequency")

tanx_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_Negative_ANX_2",
                     scale_label = frequency)

canx_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_Negative_ANX_2",
                     
                     scale_label = frequency, graph_label = "frequency")

tanx_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_Negative_ANX_3",
                     scale_label = frequency)

canx_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_Negative_ANX_3",
                     
                     scale_label = frequency, graph_label = "frequency")
#B Discrim=======================================
tbdisc_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "B_DISCRIM_B_DISCRIM_1",
                       scale_label = yn)

tbdisc_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "B_DISCRIM_B_DISCRIM_2",
                       scale_label = yn)

tbdisc_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "B_DISCRIM_B_DISCRIM_3",
                       scale_label = yn)

tbdisc_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "B_DISCRIM_B_DISCRIM_4",
                       scale_label = yn)

tbdisc_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "B_DISCRIM_B_DISCRIM_5",
                       scale_label = yn)

tbdisc_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "B_DISCRIM_B_DISCRIM_6",
                       scale_label = yn)

tbdisc_7 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "B_DISCRIM_B_DISCRIM_7",
                       scale_label = yn)

#BELONGING==========================

tbelong_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_1",
                        scale_label = agreement)

cbelong_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_1",
                        
                        scale_label = agreement, graph_label = "agreement")

tbelong_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_2",
                        scale_label = agreement)

cbelong_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_2",
                        
                        scale_label = agreement, graph_label = "agreement")

tbelong_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_3",
                        scale_label = agreement)

cbelong_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_3",
                        
                        scale_label = agreement, graph_label = "agreement")

tbelong_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_4",
                        scale_label = agreement)

cbelong_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_4",
                        
                        scale_label = agreement, graph_label = "agreement")

tbelong_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_5",
                        scale_label = agreement)

cbelong_5 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_5",
                        
                        scale_label = agreement, graph_label = "agreement")

tbelong_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_6",
                        scale_label = agreement)

cbelong_6 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_6",
                        
                        scale_label = agreement, graph_label = "agreement")

tbelong_7 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_7",
                        scale_label = agreement)

cbelong_7 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_7",
                        
                        scale_label = agreement, graph_label = "agreement")

tbelong_8 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_8",
                        scale_label = agreement)

cbelong_8 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_8",
                        
                        scale_label = agreement, graph_label = "agreement")

tbelong_9 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_9",
                        scale_label = agreement)

cbelong_9 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_9",
                        
                        scale_label = agreement, graph_label = "agreement")

tbelong_10 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_10",
                         scale_label = agreement)

cbelong_10 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_10",
                         
                         scale_label = agreement, graph_label = "agreement")

tbelong_11 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_11",
                         scale_label = agreement)

cbelong_11 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_11",
                         
                         scale_label = agreement, graph_label = "agreement")

tbelong_12 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_12",
                         scale_label = agreement)

cbelong_12 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_12",
                         
                         scale_label = agreement, graph_label = "agreement")

tbelong_13 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_13",
                         scale_label = agreement)

cbelong_13 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "BELONG_BELONG_13",
                         
                         scale_label = agreement, graph_label = "agreement")


#CLASS============
#The code did not work properly for this item for some unknown reason, so we had to manually create this chart
tclass <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "CLASS",
                     scale_label = class)

cclass_test_df <- df_survey_complete %>% 
  filter(school_name == school_of_interest) %>% 
  filter(question == "CLASS") %>%
  mutate(response = as.numeric(response)) %>% 
  arrange(response) %>%
  mutate(response = factor(response,  labels =c("FY/Freshman", "Sophomore", "Junior", "Senior"))) 

cclass <- ggplot(data = subset(cclass_test_df, !is.na(response)), 
         aes(response, perc_no_na, fill = response))+
  geom_col()+
  theme_wellbeing()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_fill_wellness()+
  ggtitle("What class are you in?")+
  scale_x_discrete(labels = c("FY/Freshman", "Sophomore", "Junior", "Senior"))+
  geom_point(aes(response, perc_ag_no_na), shape =95, size = 10)+ylab("")+xlab("")


#DEPRESSION==============

tdep_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_Negative_DEP_1",
                     scale_label = frequency)

cdep_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_Negative_DEP_1",
                     
                     scale_label = frequency, graph_label = "frequency")

tdep_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Q118_ANX_1",
                     scale_label = frequency)

cdep_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Q118_ANX_1",
                     
                     scale_label = frequency, graph_label = "frequency")

tdep_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Q118_ANX_2",
                     scale_label = frequency)

cdep_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Q118_ANX_2",
                     
                     scale_label = frequency, graph_label = "frequency")
#D Friends========================

tdfriends_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DFRIENDS_DFRIENDS_1",
                          scale_label = agreement)

cdfriends_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "DFRIENDS_DFRIENDS_1",
                          
                          scale_label = agreement, graph_label = "agreement")

tdfriends_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DFRIENDS_DFRIENDS_2",
                          scale_label = agreement)

cdfriends_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "DFRIENDS_DFRIENDS_2",
                          
                          scale_label = agreement, graph_label = "agreement")

tdfriends_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DFRIENDS_DFRIENDS_3",
                          scale_label = agreement)

cdfriends_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "DFRIENDS_DFRIENDS_3",
                          
                          scale_label = agreement, graph_label = "agreement")

tdfriends_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DFRIENDS_DFRIENDS_4",
                          scale_label = agreement)

cdfriends_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "DFRIENDS_DFRIENDS_4",
                          
                          scale_label = agreement, graph_label = "agreement")
#Disability=================

tdisabl_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DISABL_DISABL_1",
                        scale_label = yn)

tdisabl_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DISABL_DISABL_2",
                        scale_label = yn)

tdisabl_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DISABL_DISABL_3",
                        scale_label = yn)

tdisabl_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DISABL_DISABL_4",
                        scale_label = yn)

tdisabl_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DISABL_DISABL_5",
                        scale_label = yn)

tdisabl_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DISABL_DISABL_6",
                        scale_label = yn)
#DISCRIMINATION=========
tdiscrim_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DISCRIM_DISCRIM_1",
                         scale_label = agreement)

cdiscrim_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "DISCRIM_DISCRIM_1",
                         
                         scale_label = agreement, graph_label = "agreement")

tdiscrim_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DISCRIM_DISCRIM_2",
                         scale_label = agreement)

cdiscrim_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "DISCRIM_DISCRIM_2",
                         
                         scale_label = agreement, graph_label = "agreement")

tdiscrim_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DISCRIM_DISCRIM_3",
                         scale_label = agreement)

cdiscrim_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "DISCRIM_DISCRIM_3",
                         
                         scale_label = agreement, graph_label = "agreement")

tdiscrim_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DISCRIM_DISCRIM_4",
                         scale_label = agreement)

cdiscrim_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "DISCRIM_DISCRIM_4",
                         
                         scale_label = agreement, graph_label = "agreement")

tdiscrim_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "DISCRIM_DISCRIM_5",
                         scale_label = agreement)

cdiscrim_5 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "DISCRIM_DISCRIM_5",
                         
                         scale_label = agreement, graph_label = "agreement")
#FRIENDS==============
tfriends_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_1",
                         scale_label = agreement)

cfriends_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_1",
                         
                         scale_label = agreement, graph_label = "agreement")

tfriends_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_2",
                         scale_label = agreement)

cfriends_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_2",
                         
                         scale_label = agreement, graph_label = "agreement")

tfriends_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_3",
                         scale_label = agreement)

cfriends_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_3",
                         
                         scale_label = agreement, graph_label = "agreement")

tfriends_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_4",
                         scale_label = agreement)

cfriends_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_4",
                         
                         scale_label = agreement, graph_label = "agreement")

tfriends_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_5",
                         scale_label = agreement)

cfriends_5 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_5",
                         
                         scale_label = agreement, graph_label = "agreement")

tfriends_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_6",
                         scale_label = agreement)

cfriends_6 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_6",
                         
                         scale_label = agreement, graph_label = "agreement")

tfriends_7 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_7",
                         scale_label = agreement)

cfriends_7 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_7",
                         
                         scale_label = agreement, graph_label = "agreement")

tfriends_8 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_8",
                         scale_label = agreement)

cfriends_8 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_8",
                         
                         scale_label = agreement, graph_label = "agreement")

tfriends_9 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_9",
                         scale_label = agreement)

cfriends_9 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_9",
                         
                         scale_label = agreement, graph_label = "agreement")

tfriends_10 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_10",
                          scale_label = agreement)

cfriends_10 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "FRIENDS_FRIENDS_10",
                          
                          scale_label = agreement, graph_label = "agreement")
#Foreign School===========
tfrn <- oneschool_complete %>% 
  ungroup() %>% 
  filter(question == "FRNSCHL") %>% 
  mutate(response = as.numeric(response)) %>% 
  select(response, freq, perc, freq_ag, perc_ag) %>% 
  arrange(response) %>% 
  knitr::kable(row.names = FALSE,
               col.names = c("Years", "School freq", "School perc", 
                             "Ag freq", "Ag perc"),
               digits = c(0,0,2,0,2))

#GENDER=====================
tgender <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "GENDER",
                         scale_label = gender)

cgender <- make_graph_tall(data =df_survey_complete, school_name = school_of_interest, question_name = "GENDER",
                         
                         scale_label = gender, graph_label = "gender")
#HAPPY============================
thappy_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_happy_HAPPY_1",
                     scale_label = frequency)

chappy_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_happy_HAPPY_1",
                     
                     scale_label = frequency, graph_label = "frequency")

thappy_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_happy_HAPPY_2",
                       scale_label = frequency)

chappy_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_happy_HAPPY_2",
                       
                       scale_label = frequency, graph_label = "frequency")

thappy_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_happy_HAPPY_3",
                       scale_label = frequency)

chappy_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Mood_happy_HAPPY_3",
                       
                       scale_label = frequency, graph_label = "frequency")
#HEALTH=================
thealth_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_1",
                         scale_label = agreement)

chealth_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_1",
                         
                         scale_label = agreement, graph_label = "agreement")

thealth_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_2",
                        scale_label = agreement)

chealth_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_2",
                        
                        scale_label = agreement, graph_label = "agreement")

thealth_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_3",
                        scale_label = agreement)

chealth_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_3",
                        
                        scale_label = agreement, graph_label = "agreement")

thealth_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_4",
                        scale_label = agreement)

chealth_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_4",
                        
                        scale_label = agreement, graph_label = "agreement")

thealth_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_5",
                        scale_label = agreement)

chealth_5 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_5",
                        
                        scale_label = agreement, graph_label = "agreement")

thealth_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_6",
                        scale_label = agreement)

chealth_6 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_6",
                        
                        scale_label = agreement, graph_label = "agreement")

thealth_7 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_7",
                        scale_label = agreement)

chealth_7 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "HEALTH_HEALTH_7",
                        
                        scale_label = agreement, graph_label = "agreement")
#Housing =================================
thousing <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Housing",
                        scale_label = housing)

chousing <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Housing",
                        
                        scale_label = housing, graph_label = "housing")

#HS_GPA ========================================
thsgpa <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "HS_GPA",
                       scale_label = gpa)

chsgpa <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "HS_GPA",
                       
                       scale_label = gpa, graph_label = "gpa")
#Life Satisfaction===========================================================
tlifesat_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LIFESAT_LIFESAT_1",
                        scale_label = agreement)

clifesat_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LIFESAT_LIFESAT_1",
                        
                        scale_label = agreement, graph_label = "agreement")

tlifesat_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LIFESAT_LIFESAT_2",
                         scale_label = agreement)

clifesat_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LIFESAT_LIFESAT_2",
                         
                         scale_label = agreement, graph_label = "agreement")

tlifesat_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LIFESAT_LIFESAT_3",
                         scale_label = agreement)

clifesat_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LIFESAT_LIFESAT_3",
                         
                         scale_label = agreement, graph_label = "agreement")

tlifesat_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LIFESAT_LIFESAT_4",
                         scale_label = agreement)

clifesat_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LIFESAT_LIFESAT_4",
                         
                         scale_label = agreement, graph_label = "agreement")

tlifesat_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LIFESAT_LIFESAT_5",
                         scale_label = agreement)

clifesat_5 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LIFESAT_LIFESAT_5",
                         
                         scale_label = agreement, graph_label = "agreement")
#LLS======================
tlls_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_1",
                        scale_label = agreement)

clls_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_1",
                        
                        scale_label = agreement, graph_label = "agreement")

tlls_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_2",
                     scale_label = agreement)

clls_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_2",
                     
                     scale_label = agreement, graph_label = "agreement")

tlls_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_3",
                     scale_label = agreement)

clls_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_3",
                     
                     scale_label = agreement, graph_label = "agreement")

tlls_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_4",
                     scale_label = agreement)

clls_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_4",
                     
                     scale_label = agreement, graph_label = "agreement")

tlls_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_5",
                     scale_label = agreement)

clls_5 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_5",
                     
                     scale_label = agreement, graph_label = "agreement")

tlls_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_6",
                     scale_label = agreement)

clls_6 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_6",
                     
                     scale_label = agreement, graph_label = "agreement")

tlls_7 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_7",
                     scale_label = agreement)

clls_7 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_7",
                     
                     scale_label = agreement, graph_label = "agreement")

tlls_8 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_8",
                     scale_label = agreement)

clls_8 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LLS_LLS_8",
                     
                     scale_label = agreement, graph_label = "agreement")

#Loans=========================
tloans <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "LOANS",
                     scale_label = loans)

cloans <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "LOANS",
                     
                     scale_label = loans, graph_label = "loans")

#Loneliness===================================
tlone_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Q118_ANX_3",
                     scale_label = frequency)

clone_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Q118_ANX_3",
                     
                     scale_label = frequency, graph_label = "frequency")

tlone_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Q118_Q118_16",
                      scale_label = frequency)

clone_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Q118_Q118_16",
                      
                      scale_label = frequency, graph_label = "frequency")

tlone_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Q119_ANX_1",
                      scale_label = frequency)

clone_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Q119_ANX_1",
                      
                      scale_label = frequency, graph_label = "frequency")

#MAJOR =============================
#the object is tmajor
maj1 <- oneschool_complete %>% 
  filter(question == "MAJOR_1") %>%
  filter(response == 1)

maj2 <- oneschool_complete %>% 
  filter(question == "MAJOR_2") %>%
  filter(response == 1)

maj3 <- oneschool_complete %>% 
  filter(question == "MAJOR_3") %>%
  filter(response == 1)

maj4 <- oneschool_complete %>% 
  filter(question == "MAJOR_4") %>%
  filter(response == 1)

maj5 <- oneschool_complete %>% 
  filter(question == "MAJOR_5") %>%
  filter(response == 1)

maj6 <- oneschool_complete %>% 
  filter(question == "MAJOR_6") %>%
  filter(response == 1)

maj7 <- oneschool_complete %>% 
  filter(question == "MAJOR_7") %>%
  filter(response == 1)

maj8 <- oneschool_complete %>% 
  filter(question == "MAJOR_8") %>%
  filter(response == 1)

maj9 <- oneschool_complete %>% 
  filter(question == "MAJOR_9") %>%
  filter(response == 1)

maj10 <- oneschool_complete %>% 
  filter(question == "MAJOR_10") %>%
  filter(response == 1)

maj11 <- oneschool_complete %>% 
  filter(question == "MAJOR_11") %>%
  filter(response == 1)

maj12 <- oneschool_complete %>% 
  filter(question == "MAJOR_12") %>%
  filter(response == 1)

maj13 <- oneschool_complete %>% 
  filter(question == "MAJOR_13") %>%
  filter(response == 1)

maj14 <- oneschool_complete %>% 
  filter(question == "MAJOR_14") %>%
  filter(response == 1)

maj15 <- oneschool_complete %>% 
  filter(question == "MAJOR_15") %>%
  filter(response == 1)

maj16 <- oneschool_complete %>% 
  filter(question == "MAJOR_16") %>%
  filter(response == 1)

maj17 <- oneschool_complete %>% 
  filter(question == "MAJOR_17") %>%
  filter(response == 1)

maj18 <- oneschool_complete %>% 
  filter(question == "MAJOR_18") %>%
  filter(response == 1)

maj19 <- oneschool_complete %>% 
  filter(question == "MAJOR_19") %>%
  filter(response == 1)

maj20 <- oneschool_complete %>% 
  filter(question == "MAJOR_20") %>%
  filter(response == 1)

maj21 <- oneschool_complete %>% 
  filter(question == "MAJOR_21") %>%
  filter(response == 1)

maj22 <- oneschool_complete %>% 
  filter(question == "MAJOR_22") %>%
  filter(response == 1)

maj23 <- oneschool_complete %>% 
  filter(question == "MAJOR_23") %>%
  filter(response == 1)

maj_data <- bind_rows(maj1, maj2, maj3, maj4, maj5, maj6,
                      maj7, maj8, maj9, maj10, maj11, maj12,
                      maj13, maj14, maj15, maj16, maj17, maj18,
                      maj19, maj20, maj21, maj22, maj23)

maj_labels <- tribble(
  ~question, ~short_title,
  "MAJOR_1", "Not yet selected a major",
  "MAJOR_2", "Agriculture",
  "MAJOR_3", "Biological/life sciences",
  "MAJOR_4", "Business",
  "MAJOR_5", "Communication",
  "MAJOR_6", "Computer & information sciences",
  "MAJOR_7", "Education",
  "MAJOR_8", "Engineering",
  "MAJOR_9", "Ethnic, cultural, and area studies",
  "MAJOR_10", "Foreign languages & literature",
  "MAJOR_11", "Health-related fields",
  "MAJOR_12", "History",
  "MAJOR_13", "Humanities",
  "MAJOR_14", "Liberal/general studies",
  "MAJOR_15", "Mathematics",
  "MAJOR_16", "Multi/interdisciplinary studies",
  "MAJOR_17", "Parks, recreation, leisure, sports",
  "MAJOR_18", "Physical sciences",
  "MAJOR_19", "Pre-professional",
  "MAJOR_20", "Public administration",
  "MAJOR_21", "Social sciences",
  "MAJOR_22", "Visual and performing arts",
  "MAJOR_23", "Other"
  )


maj_data <- maj_data %>% left_join(., maj_labels, by = "question") 

#Leaving out the "arrange" command should print the variables in the 
#order they're pasted into maj_data above. Otherwise, they get sorted 
#alphabetically, which means they're out of order numerically.
tmajor <- maj_data %>%
  select(question, short_title, freq, perc, freq_ag, perc_ag) %>%
  ungroup() %>% 
  knitr::kable(col.names = c("Item number", "Item text", "School freq", 
                             "School perc", "Ag freq", "Ag perc"), digits = c(0,2, 2, 2, 2, 2))

#MEANING=========================================================
tmeaning_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_1",
                        scale_label = agreement)

cmeaning_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_1",
                        
                        scale_label = agreement, graph_label = "agreement")

tmeaning_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_2",
                         scale_label = agreement)

cmeaning_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_2",
                         
                         scale_label = agreement, graph_label = "agreement")

tmeaning_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_3",
                         scale_label = agreement)

cmeaning_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_3",
                         
                         scale_label = agreement, graph_label = "agreement")

tmeaning_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_4",
                         scale_label = agreement)

cmeaning_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_4",
                         
                         scale_label = agreement, graph_label = "agreement")

tmeaning_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_5",
                         scale_label = agreement)

cmeaning_5 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_5",
                         
                         scale_label = agreement, graph_label = "agreement")

tmeaning_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_6",
                         scale_label = agreement)

cmeaning_6 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_6",
                         
                         scale_label = agreement, graph_label = "agreement")

tmeaning_7 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_7",
                         scale_label = agreement)

cmeaning_7 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_7",
                         
                         scale_label = agreement, graph_label = "agreement")

tmeaning_8 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_8",
                         scale_label = agreement)

cmeaning_8 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_8",
                         
                         scale_label = agreement, graph_label = "agreement")

tmeaning_9 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_9",
                         scale_label = agreement)

cmeaning_9 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_9",
                         
                         scale_label = agreement, graph_label = "agreement")

tmeaning_10 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_10",
                         scale_label = agreement)

cmeaning_10 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_10",
                         
                         scale_label = agreement, graph_label = "agreement")

tmeaning_11 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_11",
                         scale_label = agreement)

cmeaning_11 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_11",
                         
                         scale_label = agreement, graph_label = "agreement")
tmeaning_12 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_12",
                         scale_label = agreement)

cmeaning_12 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MEANING_MEANING_12",
                         
                         scale_label = agreement, graph_label = "agreement")

#Military================================
tmilitary <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "MILITARY",
                          scale_label = military)

cmilitary <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "MILITARY",
                          
                          scale_label = military, graph_label = "military")

#MONEY in demographics =================================
#the object is tmoney
money1 <- oneschool_complete %>% 
  filter(question == "MONEY_1") %>%
  filter(response == 1)

money2 <- oneschool_complete %>% 
  filter(question == "MONEY_2") %>%
  filter(response == 1)

money3 <- oneschool_complete %>% 
  filter(question == "MONEY_3") %>%
  filter(response == 1)

money4 <- oneschool_complete %>% 
  filter(question == "MONEY_4") %>%
  filter(response == 1)

money5 <- oneschool_complete %>% 
  filter(question == "MONEY_5") %>%
  filter(response == 1)

money_data <- bind_rows(money1, money2, money3, money4, money5)

tmoney <- money_data %>%
  select(question, formatted_title, freq, perc, freq_ag, perc_ag) %>%
  ungroup() %>% 
  knitr::kable(col.names = c("Item number", "Item text", "School freq", 
                             "School perc", "Ag freq", "Ag perc"), digits = c(0,2, 2, 2, 2, 2))

#OPTIMISM==================================
topt_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_OPT_1",
                          scale_label = agreement)

copt_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_OPT_1",
                          
                          scale_label = agreement, graph_label = "agreement")

topt_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_OPT_2",
                     scale_label = agreement)

copt_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_OPT_2",
                     
                     scale_label = agreement, graph_label = "agreement")

topt_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_OPT_3",
                     scale_label = agreement)

copt_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_OPT_3",
                     
                     scale_label = agreement, graph_label = "agreement")

#Outcomes====================
toutcomes_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Outcomes_Outcomes_1",
                     scale_label = likely)

coutcomes_1 <- make_graph_tall(data =df_survey_complete, school_name = school_of_interest, question_name = "Outcomes_Outcomes_1",
                     
                     scale_label = likely, graph_label = "likely")

toutcomes_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Outcomes_Outcomes_2",
                          scale_label = likely)

coutcomes_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Outcomes_Outcomes_2",
                          
                          scale_label = likely, graph_label = "likely")

toutcomes_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Outcomes_Outcomes_3",
                          scale_label = likely)

coutcomes_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Outcomes_Outcomes_3",
                          
                          scale_label = likely, graph_label = "likely")
#Parent edu ========================
tparedu <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PAREDU",
                          scale_label = ynd)

cparedu <- make_graph_tall(data =df_survey_complete, school_name = school_of_interest, question_name = "PAREDU",
                          
                          scale_label = ynd, graph_label = "ynd")

#PAY=================================
tpay_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PAY_PAY_1",
                      scale_label = yn)

tpay_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PAY_PAY_2",
                     scale_label = yn)

tpay_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PAY_PAY_3",
                     scale_label = yn)

tpay_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PAY_PAY_4",
                     scale_label = yn)

tpay_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PAY_PAY_5",
                     scale_label = yn)

tpay_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PAY_PAY_6",
                     scale_label = yn)

#Perseverance=======================
tpers_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_PERS_1",
                     scale_label = agreement)

cpers_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_PERS_1",
                     
                     scale_label = agreement, graph_label = "agreement")

tpers_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_PERS_2",
                      scale_label = agreement)

cpers_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_PERS_2",
                      
                      scale_label = agreement, graph_label = "agreement")

tpers_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_PERS_3",
                      scale_label = agreement)

cpers_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_PERS_3",
                      
                      scale_label = agreement, graph_label = "agreement")

#PROGRAMS====================
tprograms_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PROGRAMS_PROGRAMS_1",
                      scale_label = ynns)

tprograms_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PROGRAMS_PROGRAMS_2",
                          scale_label = ynns)

tprograms_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PROGRAMS_PROGRAMS_3",
                          scale_label = ynns)

tprograms_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PROGRAMS_PROGRAMS_4",
                          scale_label = ynns)

tprograms_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PROGRAMS_PROGRAMS_5",
                          scale_label = ynns)

tprograms_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PROGRAMS_PROGRAMS_6",
                          scale_label = ynns)

tprograms_7 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PROGRAMS_PROGRAMS_7",
                          scale_label = ynns)

#PURPOSE1========================
tpurp1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP1",
                      scale_label = purpose)

cpurp1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP1",
                      
                      scale_label = purpose, graph_label = "purpose")

#PURPOSE2_*======================

tpurp2_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_1",
                       scale_label = agreement)

cpurp2_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_1",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp2_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_2",
                     scale_label = agreement)

cpurp2_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_2",
                     
                     scale_label = agreement, graph_label = "agreement")

tpurp2_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_3",
                       scale_label = agreement)

cpurp2_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_3",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp2_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_4",
                       scale_label = agreement)

cpurp2_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_4",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp2_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_5",
                       scale_label = agreement)

cpurp2_5 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_5",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp2_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_6",
                       scale_label = agreement)

cpurp2_6 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_6",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp2_7 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_7",
                       scale_label = agreement)

cpurp2_7 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_7",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp2_8 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_8",
                       scale_label = agreement)

cpurp2_8 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_8",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp2_9 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_9",
                       scale_label = agreement)

cpurp2_9 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_9",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp2_10 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_10",
                       scale_label = agreement)

cpurp2_10 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_10",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp2_11 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_11",
                       scale_label = agreement)

cpurp2_11 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_11",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp2_12 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_12",
                       scale_label = agreement)

cpurp2_12 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURPOSE_12",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp2_13 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURP2_13",
                       scale_label = agreement)

cpurp2_13 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURP2_13",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp2_14 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURP2_14",
                       scale_label = agreement)

cpurp2_14 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP2_PURP2_14",
                       
                       scale_label = agreement, graph_label = "agreement")


#PURPOSE3_*====================
tpurp3_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_1",
                       scale_label = agreement)

cpurp3_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_1",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp3_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_2",
                       scale_label = agreement)

cpurp3_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_2",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp3_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_3",
                       scale_label = agreement)

cpurp3_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_3",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp3_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_4",
                       scale_label = agreement)

cpurp3_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_4",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp3_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_5",
                       scale_label = agreement)

cpurp3_5 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_5",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp3_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_6",
                       scale_label = agreement)

cpurp3_6 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_6",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp3_7 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_7",
                       scale_label = agreement)

cpurp3_7 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_7",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp3_8 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_8",
                       scale_label = agreement)

cpurp3_8 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_8",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp3_9 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_9",
                       scale_label = agreement)

cpurp3_9 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_9",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp3_10 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_10",
                       scale_label = agreement)

cpurp3_10 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_10",
                       
                       scale_label = agreement, graph_label = "agreement")

tpurp3_11 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_11",
                       scale_label = agreement)

cpurp3_11 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "PURP3_PURP3_11",
                       
                       scale_label = agreement, graph_label = "agreement")

#RA=========================
tra <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "RA",
                       scale_label = ra)

cra <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "RA",
                       
                       scale_label = ra, graph_label = "ra")

#RACETHN============================
tracethn <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "racethn",
                      scale_label = race)

cracethn <- make_graph_tall(data =df_survey_complete, school_name = school_of_interest, question_name = "racethn",
                      
                      scale_label = race, graph_label = "race")

#
#ROMAN ==========================
troman_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN_1",
                      scale_label = romantic)

croman_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN_1",
                      
                      scale_label = romantic, graph_label = "romantic")

troman_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN_2",
                      scale_label = yn)

croman_2 <- make_graph_tall(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN_2",
                      
                      scale_label = yn, graph_label = "yn")

troman3_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN3_ROMAN3_1",
                      scale_label = agreement)

croman3_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN3_ROMAN3_1",
                      
                      scale_label = agreement, graph_label = "agreement")

troman3_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN3_ROMAN3_2",
                        scale_label = agreement)

croman3_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN3_ROMAN3_2",
                        
                        scale_label = agreement, graph_label = "agreement")

troman3_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN3_ROMAN3_3",
                        scale_label = agreement)

croman3_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN3_ROMAN3_3",
                        
                        scale_label = agreement, graph_label = "agreement")

troman3_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN3_ROMAN3_4",
                        scale_label = agreement)

croman3_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN3_ROMAN3_4",
                        
                        scale_label = agreement, graph_label = "agreement")

troman3_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN3_ROMAN3_5",
                        scale_label = agreement)

croman3_5 <- make_graph_tall(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN3_ROMAN3_5",
                        
                        scale_label = agreement, graph_label = "agreement")

troman4_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN4_ROMAN4_1",
                        scale_label = agreement)

croman4_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN4_ROMAN4_1",
                        
                        scale_label = agreement, graph_label = "agreement")

troman4_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN4_ROMAN4_2",
                        scale_label = agreement)

croman4_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "ROMAN4_ROMAN4_2",
                        
                        scale_label = agreement, graph_label = "agreement")

#Self-esteem =============================
tselfest_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_SELFEST_1",
                        scale_label = agreement)

cselfest_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_SELFEST_1",
                        
                        scale_label = agreement, graph_label = "agreement")

tselfest_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_SELFEST_2",
                         scale_label = agreement)

cselfest_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_SELFEST_2",
                         
                         scale_label = agreement, graph_label = "agreement")

tselfest_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_SELFEST_3",
                         scale_label = agreement)

cselfest_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_SELFEST_3",
                         
                         scale_label = agreement, graph_label = "agreement")

tselfest_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_SELFEST_4",
                         scale_label = agreement)

cselfest_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Att_SELFEST_4",
                         
                         scale_label = agreement, graph_label = "agreement")


#Sexual orientation ======================
tsexor <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "SEX_ORIENT",
                         scale_label = sexor)

csexor <- make_graph_tall(data =df_survey_complete, school_name = school_of_interest, question_name = "SEX_ORIENT",
                         
                         scale_label = sexor, graph_label = "sexor")

#Social Anxiety ==========================
tsocanx_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Q119_ANX_2",
                         scale_label = frequency)

csocanx_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Q119_ANX_2",
                         
                         scale_label = frequency, graph_label = "frequency")

tsocanx_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Q119_ANX_3",
                        scale_label = frequency)

csocanx_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Q119_ANX_3",
                        
                        scale_label = frequency, graph_label = "frequency")

tsocanx_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "Q119_DEP_1",
                        scale_label = frequency)

csocanx_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "Q119_DEP_1",
                        
                        scale_label = frequency, graph_label = "frequency")

#Spirit==========================
tspirit <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "SPIRIT",
                     scale_label = spirituality)

#cspirit <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "SPIRIT",
                     
                     #scale_label = spirituality, graph_label = "spirituality")


#Substance ===============================
tsubst1_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST1_SUBST1_1",
                        scale_label = agreement)

csubst1_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST1_SUBST1_1",
                        
                        scale_label = agreement, graph_label = "agreement")

tsubst1_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST1_SUBST1_2",
                        scale_label = agreement)

csubst1_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST1_SUBST1_2",
                        
                        scale_label = agreement, graph_label = "agreement")

tsubst1_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST1_SUBST1_3",
                        scale_label = agreement)

csubst1_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST1_SUBST1_3",
                        
                        scale_label = agreement, graph_label = "agreement")

tsubst2_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST2_SUBST_1",
                        scale_label = agreementNA)

csubst2_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST2_SUBST_1",
                        
                        scale_label = agreementNA, graph_label = "agreementNA")

tsubst2_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST2_SUBST_2",
                        scale_label = agreementNA)

csubst2_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST2_SUBST_2",
                        
                        scale_label = agreementNA, graph_label = "agreementNA")

tsubst2_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST2_SUBST_3",
                        scale_label = agreementNA)

csubst2_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST2_SUBST_3",
                        
                        scale_label = agreementNA, graph_label = "agreementNA")

tsubst2_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST2_SUBST_4",
                        scale_label = agreementNA)

csubst2_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST2_SUBST_4",
                        
                        scale_label = agreementNA, graph_label = "agreementNA")

tsubst3_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST3_1",
                        scale_label = substance)

csubst3_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST3_1",
                        
                        scale_label = substance, graph_label = "substance")


tsubst3_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST3_2",
                        scale_label = substance)

csubst3_2 <- make_graph_tall(data =df_survey_complete, school_name = school_of_interest, question_name = "SUBST3_2",
                        
                        scale_label = substance, graph_label = "substance")

#Trans ===================================

ttrans <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "TRANS",
                        scale_label = yn)

ctrans <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "TRANS",
                        
                        scale_label = yn, graph_label = "yn")

#Volunteer ===============================

tvolunt_1 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_1",
                        scale_label = agreement)

cvolunt_1 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_1",
                        
                        scale_label = agreement, graph_label = "agreement")

tvolunt_2 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_2",
                        scale_label = agreement)

cvolunt_2 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_2",
                        
                        scale_label = agreement, graph_label = "agreement")

tvolunt_3 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_3",
                        scale_label = agreement)

cvolunt_3 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_3",
                        
                        scale_label = agreement, graph_label = "agreement")

tvolunt_4 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_4",
                        scale_label = agreement)

cvolunt_4 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_4",
                        
                        scale_label = agreement, graph_label = "agreement")

tvolunt_5 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_5",
                        scale_label = agreement)

cvolunt_5 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_5",
                        
                        scale_label = agreement, graph_label = "agreement")

tvolunt_6 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_6",
                        scale_label = agreement)

cvolunt_6 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_6",
                        
                        scale_label = agreement, graph_label = "agreement")

tvolunt_7 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_7",
                        scale_label = agreement)

cvolunt_7 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_7",
                        
                        scale_label = agreement, graph_label = "agreement")

tvolunt_8 <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_8",
                        scale_label = agreement)

cvolunt_8 <- make_graph(data =df_survey_complete, school_name = school_of_interest, question_name = "VOLUNT_VOLUNT_8",
                        
                        scale_label = agreement, graph_label = "agreement")

#Work ====================================
twork <- make_table(data =df_survey_complete, school_name = school_of_interest, question_name = "WORK",
                        scale_label = work)

cwork <- make_graph_tall(data =df_survey_complete, school_name = school_of_interest, question_name = "WORK",
                        
                        scale_label = work, graph_label = "work")

