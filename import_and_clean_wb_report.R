#This script imports survey data from qualtrics, creates a data frame with data from selected surveys
  #including question text and response options and cleans the data


#This is Nicole's token.
#deleted for security reasons

#See what surveys are available.
surveys <- getSurveys() 
surveys

#enter the names of the colleges whose surveys you wish to pull.
colleges <- c("insert names here")

year <- "2018 pilot"

survey_of_interest <- tibble(school_name = surveys$name, 
                             college =grepl(paste(colleges,collapse = "|"), 
                                            surveys$name, ignore.case = TRUE),
                             year = grepl(paste(year,collapse = "|"), 
                                          surveys$name, ignore.case = TRUE),
                             keep = college & year) %>% filter(keep==TRUE)

surveys_to_analyze <- inner_join(survey_of_interest, surveys, by = c("school_name"= "name"))


#Pull the surveys, clean, then append-------------------------------
combined_surveys <-list()
for (i in 1:nrow(survey_of_interest)){
  message("Retrieving: ", surveys_to_analyze$school_name[i])
  
  #Get the survey
  mysurvey <- getSurvey(surveyID = surveys_to_analyze$id[i], 
                        verbose = TRUE, force_request = TRUE)
  #Make they survey key
  my_survey_key <- datademon::make_survey_key(mysurvey) %>% 
    unique()
  
  #Add to survey key the new survey items generated during cleaning
  new_items <- tribble(
    ~survey_id,  ~question_details ,
    "Student ID", "stuid",
    "Ethnicity/Race", "racethn")
  
  my_survey_key <- bind_rows(my_survey_key, new_items)
  
  #print the key to double-check that the key is right
 # names(my_survey_key)
  
  #Perform the cleaning activities===========================
  
  #There is no need to assign missing values.
  #The package qualtRics does it automatically.
  
  #Create the concatenated student id
  mysurvey <- mysurvey %>% mutate(stuid = 
                                    str_c(ID_1_TEXT, ID_2_TEXT, ID_3_TEXT, 
                                          ID_4_TEXT, ID_5_TEXT,ID_6_TEXT)) 
  
  #Make the race variables numeric to create
  #intermediary variables for logic in creation of racethn.
  #For unknown reasons, the sum statements did not work well in 
  #ifelse and case_when statements, so we included them in
  #our creation of dummy variables.
  mysurvey <- mysurvey %>% 
    mutate(ethnicity = ifelse(is.na(ETHNICITY), 0, 
                              ifelse(ETHNICITY == 1, 1, 0))) %>%
    mutate(race1 = ifelse(is.na(RACE_1), 0, 1)) %>%
    mutate(race2 = ifelse(is.na(RACE_2), 0, 1)) %>%
    mutate(race3 = ifelse(is.na(RACE_3), 0, 1)) %>%
    mutate(race4 = ifelse(is.na(RACE_4), 0, 1)) %>%
    mutate(race5 = ifelse(is.na(RACE_5), 0, 1)) %>%
    mutate(missrace = (ethnicity + race1 + race2 + race3 + race4 + race5)) %>%
    mutate(twoplus = (race1 + race2 + race3 + race4 + race5))
  
  #case_when wants everything on the right-hand-side (RHS)
  #to be the same, so we had to break Mike's case_when
  #into two parts: one for the recoding without NAs, 
  #one for the coding with NAs. 
  #This is the code for adding the numeric values.
  mysurvey <- mysurvey %>%
    mutate (racethn = case_when(
      missrace == 0 ~ 0,
      ethnicity == 1 ~ 1,
      twoplus >1 ~ 7,
      race1 == 1 ~ 2,
      race2 == 1 ~ 3,
      race3 == 1 ~ 4,
      race4 == 1 ~ 5,
      race5 == 1 ~ 6
    ))
  #Now assign NAs back into the variable.
  mysurvey <- mysurvey %>%
    mutate(racethn = ifelse(racethn == 0, NA, racethn))
  
  #Take out the purpos2_13 for the people who did not report a major
  #or who said they were undecided. Take out purpos2_14 for the people
  #who reported a major; keep it for people who were undecided. 
  #Start by recoding the individual variables into 0s and 1s 
  #so the NAs don't break the formula
  
  mysurvey <- mysurvey %>%
    mutate(major1 = ifelse(is.na(MAJOR_1), 0, 1)) %>%
    mutate(major2 = ifelse(is.na(MAJOR_2), 0, 1)) %>%
    mutate(major3 = ifelse(is.na(MAJOR_3), 0, 1)) %>%
    mutate(major4 = ifelse(is.na(MAJOR_4), 0, 1)) %>%
    mutate(major5 = ifelse(is.na(MAJOR_5), 0, 1)) %>%
    mutate(major6 = ifelse(is.na(MAJOR_6), 0, 1)) %>%
    mutate(major7 = ifelse(is.na(MAJOR_7), 0, 1)) %>%
    mutate(major8 = ifelse(is.na(MAJOR_8), 0, 1)) %>%
    mutate(major9 = ifelse(is.na(MAJOR_9), 0, 1)) %>%
    mutate(major10 = ifelse(is.na(MAJOR_10), 0, 1)) %>%
    mutate(major11 = ifelse(is.na(MAJOR_11), 0, 1)) %>%
    mutate(major12 = ifelse(is.na(MAJOR_12), 0, 1)) %>%
    mutate(major13 = ifelse(is.na(MAJOR_13), 0, 1)) %>%
    mutate(major14 = ifelse(is.na(MAJOR_14), 0, 1)) %>%
    mutate(major15 = ifelse(is.na(MAJOR_15), 0, 1)) %>%
    mutate(major16 = ifelse(is.na(MAJOR_16), 0, 1)) %>%
    mutate(major17 = ifelse(is.na(MAJOR_17), 0, 1)) %>%
    mutate(major18 = ifelse(is.na(MAJOR_18), 0, 1)) %>%
    mutate(major19 = ifelse(is.na(MAJOR_19), 0, 1)) %>%
    mutate(major20 = ifelse(is.na(MAJOR_20), 0, 1)) %>%
    mutate(major21 = ifelse(is.na(MAJOR_21), 0, 1)) %>%
    mutate(major22 = ifelse(is.na(MAJOR_22), 0, 1)) %>%
    mutate(major23 = ifelse(is.na(MAJOR_23), 0, 1)) %>%
    mutate(majorsum = major2 + major3 + major4 + major5 +
             major6 + major7+ major8 + major9 + major10 +
             major11 + major12 + major13 + major14 + major15 +
             major16 + major17 + major18 + major19 + major20 +
             major21 + major22 + major23)
  
  #Fix the coding for the SPIRIT response options
  #so that we can later arrange them numerically to
  #apply level labels.
  mysurvey$SPIRIT[mysurvey$SPIRIT == 10] <- 11
  mysurvey$SPIRIT[mysurvey$SPIRIT == 9] <- 10
  mysurvey$SPIRIT[mysurvey$SPIRIT == 8] <- 9
  mysurvey$SPIRIT[mysurvey$SPIRIT == 7] <- 8
  mysurvey$SPIRIT[mysurvey$SPIRIT == 6] <- 7
  mysurvey$SPIRIT[mysurvey$SPIRIT == 5] <- 6
  mysurvey$SPIRIT[mysurvey$SPIRIT == 4] <- 5
  mysurvey$SPIRIT[mysurvey$SPIRIT == 18] <- 4
  
    #Now remove purp2_13 for the people who did not report a major
  #or who said they were undecided.
  mysurvey <- mysurvey %>%
    mutate (PURP2_PURP2_13 = ifelse(majorsum == 0, NA,
                                    ifelse(major1 == 1, NA, PURP2_PURP2_13)))
  
  #Now remove purp2_14 for people who said major was undecided.
  #It's counter-intuitive:
  #if MAJOR_1 (which is "undecided") was selected and is therefore
  #NOT blank, insert the original PURP2_13/14 answer. 
  #If MAJOR_1 was not selected and is therefore blank,
  #substitute an NA for that person's answer to PURPO2_13/14
  mysurvey <- mysurvey %>% 
    mutate(PURP2_PURP2_14 = ifelse(!is.na(MAJOR_1), PURP2_PURP2_14, NA))
  
  #Select everything together in the right order.
  #I was trying to write one set of code to create our final data
  #set by selecting just the variables we wanted, but the long
  #variable names were causing a lot of typos and missed variables.
  #As such, in this chunk I select all the central variables we want in the right order,
  #which has the effect of un-selecting beginning and end variables we don't want.
  #I then take out unncessary variables in the chunk after this one.
  mysurvey <- mysurvey %>% select (c(stuid,
                                     Form,
                                     Consent,
                                     Mood_happy_HAPPY_1:SEX_ORIENT,
                                     racethn,
                                     ETHNICITY:FB_PROJECTSdis))
  
  #Un-select unnecessary variables.
  #Text variables are removed because they aren't
  #analyzed in the reports and they mess up the later
  #left_join with the list of questions and responses.
  mysurvey <- mysurvey %>% select(-c(Outc_intro,
                                     Domsat_intro,
                                     Att_intro,
                                     ACT_INTRO,
                                     Acaeng_intro,
                                     BELONG_INTRO,
                                     DISCRIM_INTRO,
                                     LLS_INTRO,
                                     VOLUNT_INTRO,
                                     FRIENDS_INTRO,
                                     ROMAN_INTRO,
                                     MEANING_INTRO,
                                     PURPOSE_INTRO,
                                     HEALTHS_INTRO,
                                     FB_Inst,
                                     MAJOR_23_TEXT,
                                     DISABL_DISABL_6_TEXT,
                                     ACT1_ACT1_13_TEXT,
                                     PROGRAMS_PROGRAMS_7_TEXT,
                                     MONEY_5_TEXT,
                                     PAY_PAY_6_TEXT))
  
  #Filter out non-consents, missing consents, and people
  #who do not have data for at least the following 3:
  #id, form, and consent.
  #Have to write it for 4 variables because answered_per_row
  #gets tacked on and counts itself.
  #In final line, remove the answered_per_row variable.
  mysurvey <- mysurvey %>%
    filter(Consent == 1) %>%
    filter(!is.na(Consent)) %>%
    mutate(answered_per_row = rowSums(!is.na(.))) %>%
    filter(answered_per_row > 4) %>% 
    select(stuid:FB_PROJECTSdis)
  
########Finished with cleaning.

  #Rotate the data====================================
  
  df_key <- read.csv("df_key.csv", stringsAsFactors = FALSE)
  
  #Make the long data format for easy filtering.
  #In the full_join, add in responses from df_key that
  #no one chose. In the left_join, add the question metadata.
  #In the last two mutates, turn missings into 0s
  #for the responses added in the full_join.
  #Same comments for df_long_1_na
  df_long_1 <-  mysurvey%>%
    select(stuid:FB_PROJECTSdis) %>%
    gather("question", "response", na.rm = FALSE) %>% 
    mutate(response = ifelse(response == "", NA, response)) %>% 
    group_by(question, response) %>% 
    summarise( freq = n()) %>% 
    mutate(perc = freq/sum(freq)*100) %>% 
    full_join(., df_key, by = c("question" = "question", "response" = "response"))  %>% 
    left_join(., my_survey_key, by = c("question" = "question_details")) %>% 
    mutate(freq = ifelse(is.na(freq), 0, freq)) %>% 
    mutate(perc = ifelse(is.na(perc), 0, perc))
  
  df_long_1_na <-  mysurvey %>%
    select(stuid:FB_PROJECTSdis) %>%
    gather("question", "response", na.rm = FALSE) %>% 
    mutate(response = ifelse(response == "", NA, response)) %>% 
    group_by(question, response) %>% 
    summarise( freq_no_na = n()) %>% 
    filter(!is.na(response)) %>% 
    mutate(perc_no_na = freq_no_na/sum(freq_no_na)*100) %>% 
    full_join(., df_key, by = c("question" = "question", "response" = "response"))  %>% 
    left_join(., my_survey_key, by = c("question" = "question_details")) %>% 
    mutate(freq_no_na = ifelse(is.na(freq_no_na), 0, freq_no_na)) %>% 
    mutate(perc_no_na = ifelse(is.na(perc_no_na), 0, perc_no_na))
  
  #Join and add formatted survey name
  df_complete <- df_long_1 %>% 
    left_join(. , df_long_1_na) %>%
    mutate(formatted_title = datademon::make_formatted_title(survey_id, 60))
  df_complete <- add_column(df_complete, school_name = surveys_to_analyze$school_name[i])
  
  combined_surveys[[i]] <- df_complete
}

df_survey <- bind_rows(combined_surveys)


#Aggregation of all schools ------------------------------------
#I didn't join with df_key or add the mutates for nas
#because they seem to be automatically hanlded through the
#left joins that make the final df_survey_complete.

df_survey_ag <- df_survey %>% 
  group_by(question, response) %>% 
  summarise(freq_ag = sum(freq)) %>% 
  mutate(perc_ag = freq_ag/ sum(freq_ag)*100)

df_survey_ag_no_na <- df_survey %>% 
  filter(!is.na(response)) %>% 
  group_by(question, response) %>% 
  summarise(freq_ag_no_na = sum(freq)) %>% 
  mutate(perc_ag_no_na = freq_ag_no_na/ sum(freq_ag_no_na)*100)

#Put all the information together
df_survey_complete <- df_survey %>% 
  left_join(., df_survey_ag) %>% 
  left_join(., df_survey_ag_no_na) %>% 
  arrange(question, response)
