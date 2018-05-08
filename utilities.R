#Purpose MAke Functions that generate tables and charts

####################################################
##################  IMPORTANT ######################
# This table and chart code is fragile. The code   #
# relies on sorting for the factor lables to be    #
# correctly applied, which is not the best method. #
# If you decide to fool with it, please check it   #
# using an item that has 10+ response options,     #
# such as housing or spirituality. I think these   #
# sorting changes also need to be reflected in     #
# make_wellbeing_charts, but I'm not completely    #
# sure about that. I made similar changes there,   #
# but don't know if they were actually effective.  #
####################################################

# make_tables -------------------------------------------------------------

make_table <- function(data, school_name, question_name, scale_label){
  
  if(!school_name %in% data$school_name){
    stop("Please ensure that the school name provided is in the data frame.")
  }
  if(!question_name %in% data$question){
    stop("Pleasure ensure that you have specified a valid question name.")
  }
  
  
  data %>% 
    ungroup() %>% 
    dplyr::filter(school_name == !!school_name) %>% 
    dplyr::filter(question == question_name) %>% 
    mutate(response = as.numeric(response)) %>% 
    arrange(response) %>% 
    mutate(response = factor(response,  labels = scale_label) ) %>%
    ungroup() %>% 
    select(response, freq, perc, perc_no_na, freq_ag, perc_ag, perc_ag_no_na) %>% 
    knitr::kable(col.names = table_columns, digits = c(0,2, 2, 2, 2, 2, 2)) #%>% 
    #add_header_above(., c(" ", "School" = 3, "Aggregate" = 3))%>% 
    #kable_styling(bootstrap_options = "striped")#%>% 
    #kable_as_image()  ##We'll add these three lines back in for pdf/html
}


# make_graph --------------------------------------------------------------

make_graph <- function(data, school_name, question_name, scale_label, graph_label){
  
  if(!school_name %in% data$school_name){
    stop("Please ensure that the school name provided is in the data frame.")
  }
  if(!question_name %in% data$question){
    stop("Pleasure ensure that you have specified a valid question name.")
  }

  p <- data %>% 
    filter(school_name == !!school_name) %>% 
    filter(question == question_name) %>%
    mutate(response = as.numeric(response)) %>% 
    arrange(response) %>%
    mutate(response = factor(response,  labels = scale_label) ) %>% 
    datademon::make_wellbeing_charts(., question_name, likert_scale = graph_label)+
    geom_point(aes(response, perc_ag_no_na), shape =95, size = 10)+ylim(0,72)+ylab("")+xlab("")
  
  p
}


# Make tables without aggregate data --------------------------------------
make_table_noag <- function(data, school_name, question_name, scale_label){
  
  if(!school_name %in% data$school_name){
    stop("Please ensure that the school name provided is in the data frame.")
  }
  if(!question_name %in% data$question){
    stop("Pleasure ensure that you have specified a valid question name.")
  }
  
  
  data %>% 
    ungroup() %>% 
    dplyr::filter(school_name == !!school_name) %>% 
    dplyr::filter(question == question_name) %>% 
    mutate(response = as.numeric(response)) %>% 
    arrange(response) %>% 
    mutate(response = factor(response,  labels = scale_label) ) %>%
    ungroup() %>% 
    select(response, freq, perc, perc_no_na) %>% 
    knitr::kable(col.names = c("Response", "School Freq", "School %", "School Valid %")
                 , digits = c(0,2, 2, 2, 2, 2, 2)) #%>% 
}

# make charts without aggregate data --------------------------------------------------------

make_graph_noag <- function(data, school_name, question_name, scale_label, graph_label){
  
  if(!school_name %in% data$school_name){
    stop("Please ensure that the school name provided is in the data frame.")
  }
  if(!question_name %in% data$question){
    stop("Pleasure ensure that you have specified a valid question name.")
  }
  
  p <- data %>% 
    filter(school_name == !!school_name) %>% 
    filter(question == question_name) %>%
    mutate(response = as.numeric(response)) %>% 
    arrange(response) %>%
    mutate(response = factor(response,  labels = scale_label) ) %>% 
    datademon::make_wellbeing_charts(., question_name, likert_scale = graph_label)+
    ylab("")+xlab("")
  
  p
}

# make_graph_tall --------------------------------------------------------------

make_graph_tall <- function(data, school_name, question_name, scale_label, graph_label){
  
  if(!school_name %in% data$school_name){
    stop("Please ensure that the school name provided is in the data frame.")
  }
  if(!question_name %in% data$question){
    stop("Pleasure ensure that you have specified a valid question name.")
  }
  
  p <- data %>% 
    filter(school_name == !!school_name) %>% 
    filter(question == question_name) %>%
    mutate(response = as.numeric(response)) %>% 
    arrange(response) %>%
    mutate(response = factor(response,  labels = scale_label) ) %>% 
    datademon::make_wellbeing_charts(., question_name, likert_scale = graph_label)+
    geom_point(aes(response, perc_ag_no_na), shape =95, size = 10)+ylab("")+xlab("")
  
  p
}
