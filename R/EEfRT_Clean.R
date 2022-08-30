#Clean Physical_EEfRT Data Function
Clean_Physical_EEfRT <- function(file_path, output_directory) {

  data <- read.csv(file_path)

  maximum_calibration <- data %>%
    dplyr::select(hard_cal_max) %>%
    dplyr::filter(!is.na(.)) %>%
    max(.)

  easy_calibration <- round(maximum_calibration * 0.5)
  hard_calibration <- round(maximum_calibration * 0.8)



  data_select <- data %>%
    dplyr::mutate(easy_calibration = easy_calibration,
           hard_calibration = hard_calibration) %>%
    dplyr::select(participant,
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
    dplyr::filter(!is.na(Probability)) %>%
    dplyr::rename(Reward_Magnitude = Reward,
           Trial_Type = decision_outcome,
           Trial_Success = trial_outcome,
           EEfRT_Version = time,
           Trial_Type_Decision_RT = key_resp_decision.rt,
           Reward_Won = reward_won,
           Button_Count = button_count) %>%
    dplyr::mutate(Expected_Value = (Reward_Magnitude*(Probability*0.01)))

  file_name <- paste0(data_select$participant[1],"_00",data_select$session[1],"_Physical_EEfRT_Clean.csv")

  write.csv(x = data_select,
            file = paste(output_directory, file_name))
}

#Clean Cognitive EEfRT Data Function
Clean_Cognitive_EEfRT <- function(file_path, output_directory) {
  data <- read.csv(file_path)

  maximum_calibration <- data %>%
    dplyr::select(hard_cal) %>%
    dplyr::filter(!is.na(.)) %>%
    max(.)

  easy_calibration <- round(maximum_calibration * 0.5)
  hard_calibration <- round(maximum_calibration * 0.8)



  data_select <- data %>%
    dplyr::mutate(easy_calibration = easy_calibration,
           hard_calibration = hard_calibration,
           Round_Number = trials_4.thisN+1,
           n_back_choice = n_back_choice_alt.keys,
           n_back_choice_rt = n_back_choice_alt.rt,
           Expected_Value = NA) %>%
    dplyr::filter(choice_instructions=="") %>%
    dplyr::select(participant:date,
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
    dplyr::filter(!is.na(Probability)) %>%
    dplyr::rename(n_back_response = n_back_key_resp_alt.keys, n_back_response_rt = n_back_key_resp_alt.rt, correct_response = n_back_key_resp_alt.corr)

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
    dplyr::filter(is.na(target)) %>%
    dplyr::select(-stimuli, -target, -c(cond_list:n_back_response_rt)) %>%
    dplyr::mutate(Expected_Value = Hard_Reward*Probability)

  file_name_all_data <- paste0(data_select$participant[1],"_00",data_select$session[1],"_All_Clean.csv")
  file_name_summary_data <- paste0(data_select$participant[1],"_00",data_select$session[1],"_Summary_Clean.csv")

  print(paste("Saving -", file_name_all_data, "to", as.character(output_directory)))
  write.csv(x = data_select, file = paste(output_directory, file_name_all_data))

  print(paste("Saving -", file_name_summary_data, "to", as.character(output_directory)))
  write.csv(x = cog_eefrt_summary, file = paste(output_directory, file_name_summary_data))
}

#loop through multiple csv files using EEfRTcleaner functions
EEfRTcleanerLoop <- function(input, output, EEfRT_Type) {
  #create a list "file_list" of all ".csv" files in the directory "input"
  file_list = list.files(path = input,
                         pattern="ACED.*.csv")

  #if (length(file_list == 0)) {
  #
  #  print(paste0("There are no .csv files in the specified directory: \n\n ",input, "\n\nPlease choose another directory."))
  #
  #} else {
  if (EEfRT_Type == "physical") {

    for (i in file_list) {

      writeLines(paste0("Cleaning and processing physical EEfRT behavioural data:", i))
      EEfRTcleaner::Clean_Physical_EEfRT(file_path = file.path(input, i), output_directory = output)

    }
  } else if (EEfRT_Type == "cognitive") {

    for (i in file_list) {

      writeLines(paste("Cleaning and processing cognitive EEfRT behavioural data:", i))
      EEfRTcleaner::Clean_Cognitive_EEfRT(file_path = file.path(input, i), output_directory = output)

    }
  } else {

    writeLines(paste0("ERROR! \n\n You did not set a valid EEfRT_Type.\n\nPlease enter 'cognitive' if you are processing cognitive EEfRT behavioural data.\n\nPlease enter 'physical' if you are processing physical EEfRT behavioural data."))

  }

}
