#importing libraries ---------------
library(readr)
library(tidyr)
library(dplyr)
library(openxlsx)


#selecting working directiory
setwd('D:/Projects/tubitak_2209')

#importing and cleaning the data --------------
data_path <- 'D:/Projects/tubitak_2209/data'

#take each datafile path based on the first three numbers
file_list <- list.files(data_path, pattern = "\\d{2}.*\\.csv$", full.names = TRUE)

#create an list to save each participant's dataframes
dataframes <- list()

# upload each data file and remove practice trials
for (file in file_list) {
  df <- read_csv(file) %>%
    mutate(asama_1_block = asama_1_loop_name.thisRepN + 1,
          asama_2_block = asama_2_loop_name.thisRepN + 6) %>%
    select(asama_1_block, asama_2_block, Bakiye, Para, keyTepki.keys, keyTepki_2.keys, participant, expName) %>%
    pivot_longer(cols = c(asama_1_block, asama_2_block), values_drop_na = TRUE, names_to = "Source", values_to = "block_number") %>%
    pivot_longer(cols = c(keyTepki.keys, keyTepki_2.keys), values_drop_na = TRUE, names_to = "Source_key", values_to = "response_key") %>%
    mutate(ExpCond = case_when(
      expName == "mvezdenemesi2" ~ "punishment",
      expName == "deneygrubuters" ~ "punishment",
      expName == "Grup_3" ~ "control",
      expName == "Grup_4" ~ "control"
    ),
    counterbalanced_groups = case_when(
      expName == "mvezdenemesi2" ~ "z_money",
      expName == "deneygrubuters" ~ "z_food",
      expName == "Grup_3" ~ "z_money",
      expName == "Grup_4" ~ "z_food"
    ),
    response = case_when(
      expName == "mvezdenemesi2" & response_key == "z" ~ "money",
      expName == "mvezdenemesi2" & response_key == "m" ~ "food",
      expName == "deneygrubuters" & response_key == "z" ~ "food",
      expName == "deneygrubuters" & response_key == "m" ~ "money",
      expName == "Grup_3" & response_key == "z" ~ "money",
      expName == "Grup_3" & response_key == "m" ~ "food",
      expName == "Grup_4" & response_key == "z" ~ "food",
      expName == "Grup_4" & response_key == "m" ~ "money",
      TRUE ~ "wrong_key"
    )) %>%
    select(ExpCond, block_number, response, Bakiye, Para, counterbalanced_groups)
  code <- substr(basename(file), 1, 3)
  dataframes[[code]] <- df
}

combined_df <- bind_rows(dataframes, .id = "participant") %>%
  filter(block_number <= 13)

write.xlsx(combined_df, "df.xlsx")




