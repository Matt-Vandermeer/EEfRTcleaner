#Clean Physical_EEfRT Data Function
Clean_Physical_EEfRT <- function(file_path, output_directory) {
  
  data <- read.csv(file_path)
  
  maximum_calibration <- data %>% 
    select(hard_cal_max) %>% 
    filter(!is.na(.)) %>% 
    max(.)
  
  easy_calibration <- round(maximum_calibration * 0.5)
  hard_calibration <- round(maximum_calibration * 0.8)
  
  
  
  data_select <- data %>%
    mutate(easy_calibration = easy_calibration,
           hard_calibration = hard_calibration) %>%
    select(participant,
           session,
           handedness,
           time,
           date,
           easy_calibration,
           hard_calibration,
           Trial,
           Reward,
           Probability,
           decision_outcome,
           key_resp_decision.rt,
           button_count,
           trial_outcome,
           reward_won) %>% 
    filter(!is.na(Probability)) %>% 
    rename(Reward_Magnitude = Reward,
           Trial_Type = decision_outcome,
           Trial_Success = trial_outcome,
           EEfRT_Version = time,
           Trial_Type_Decision_RT = key_resp_decision.rt,
           Reward_Won = reward_won,
           Button_Count = button_count) %>% 
    mutate(Expected_Value = (Reward_Magnitude*(Probability*0.01)))
  
  file_name <- paste0(data_select$participant[1],"_00",data_select$session[1],"_Physical_EEfRT_Clean.csv")
  
  write.csv(x = data_select, 
            file = paste(output_directory, file_name))
}

#Clean Cognitive EEfRT Data Function
Clean_Cognitive_EEfRT <- function(file_path, output_directory) {
  data <- read.csv(file_path)
  
  maximum_calibration <- data %>% 
    select(hard_cal) %>% 
    filter(!is.na(.)) %>% 
    max(.)
  
  easy_calibration <- round(maximum_calibration * 0.5)
  hard_calibration <- round(maximum_calibration * 0.8)
  
  
  
  data_select <- data %>%
    mutate(easy_calibration = easy_calibration,
           hard_calibration = hard_calibration,
           Round_Number = trials_4.thisN+1,
           n_back_choice = n_back_choice_alt.keys,
           n_back_choice_rt = n_back_choice_alt.rt,
           Expected_Value = NA) %>%
    filter(choice_instructions=="") %>% 
    select(participant:date,
           Round_Number,
           easy_calibration,
           hard_calibration,
           Hard_Reward,
           Probability,
           stimuli,
           target,
           n_back_choice,
           n_back_choice_rt,
           cond_list,
           n_back_key_resp_alt.keys,
           n_back_key_resp_alt.corr,
           n_back_key_resp_alt.rt,
           trial_outcome,
           reward_value,
           Expected_Value) %>% 
    filter(!is.na(Probability)) %>% 
    rename(n_back_response = n_back_key_resp_alt.keys, n_back_response_rt = n_back_key_resp_alt.rt, correct_response = n_back_key_resp_alt.corr)
  
  data_select$n_back_choice<-gsub("\\[\\'left\\'\\]","1-Back",as.character(data_select$n_back_choice)) 
  data_select$n_back_choice<-gsub("\\[\\'right\\'\\]","2-Back",as.character(data_select$n_back_choice))
  
  data_select$n_back_choice_rt <- gsub("\\[","",as.character(data_select$n_back_choice_rt))
  data_select$n_back_choice_rt <- gsub("\\]","",as.character(data_select$n_back_choice_rt)) %>% 
    as.numeric()
  
  data_select$reward_value <- gsub("\\$","",as.character(data_select$reward_value)) %>%
    as.numeric()
  
  data_select$Hard_Reward <- gsub("\\$","",as.character(data_select$Hard_Reward)) %>%
    as.numeric()
  
  
  for (i in 1:nrow(data_select)) {
    
    if ((i-1)%%17 == 0) {
      n_back_value = data_select$n_back_choice[i]
      data_select$Expected_Value[i-1] = (data_select$Hard_Reward[i-1]*data_select$Probability[i-1])
    }
    else {
      data_select$n_back_choice[i] = n_back_value
      
    }
    
    if (is.na(data_select$n_back_choice_rt[i])) {
      data_select$n_back_choice_rt[i] = data_select$n_back_choice_rt[i-1]
    } else {
      TRUE
    }
  }
  
  
  
  cog_eefrt_summary <- data_select %>% 
    filter(is.na(target)) %>% 
    select(-stimuli, -target, -c(cond_list:n_back_response_rt)) %>% 
    mutate(Expected_Value = Hard_Reward*Probability)
  
  file_name_all_data <- paste0(data_select$participant[1],"_00",data_select$session[1],"_All_Clean.csv")
  file_name_summary_data <- paste0(data_select$participant[1],"_00",data_select$session[1],"_Summary_Clean.csv")
  
  print(paste("Saving -", file_name_all_data, "to", as.character(output_directory)))
  write.csv(x = data_select, file = paste(output_directory, file_name_all_data))
  
  print(paste("Saving -", file_name_summary_data, "to", as.character(output_directory)))
  write.csv(x = cog_eefrt_summary, file = paste(output_directory, file_name_summary_data))
}