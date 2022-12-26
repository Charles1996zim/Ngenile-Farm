library(lubridate)
library(dplyr)
library(tidyverse)
library(readxl)
library(janitor)
library(tidyr)
library(rmsfuns)
library(openxlsx)




lapply(excel_sheets("C:/Users/charlesr/Downloads/MILK RECORDING SHEETS COMBINED.xlsx"), read_excel, 
       path = "")


path <- "C:/Users/charlesr/Downloads/MILK RECORDING SHEETS COMBINED.xlsx"
excel_sheets(path = path)


list <-
path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_xlsx, path = path)

sheets <-
list %>% names %>% as.numeric() %>% na.omit() %>% as_vector() %>% as.character() %>% as.numeric()


# DATES OF SPREADHSEET SORTED

out <- c()

for(i in sheets){
  
  for(j in 1:length(sheets)){
    
    out <- c(out,
             
             dmy(
               paste0(
                 list[[paste0(i)]] %>% row_to_names(1) %>% names() %>%
                   as.numeric() %>% na.omit() %>% .[(1+j):(3+j)], collapse = "-")))
  }
  
}


out1 <- out[seq(3, length(out), 3)]

out2 <-
  lubridate::as_date(out1)



final <- list()
finallist <- list()

for(i in sheets){
  
  for(j in 0:9){

list[[paste0(i)]] %>% row_to_names(1) %>% clean_names() %>% select(1:31) %>%
      select(1, c((2+3*j):(3+3*j))) %>%
      rename(M = 2, A = 3) %>% drop_na(cow_date) %>% write.xlsx(paste0("output/",i, j, ".xlsx"))
  }
}


file.list <-
  list.files(path="output/",  pattern = '*.xlsx')

file <-
  read.xlsx(paste0('output/', file.list[1])) %>% as_tibble %>% 
  mutate(date = dateconverter(as.Date("2018-07-20"), as.Date("2023-02-03"), "weekdayEOW")[1])


for(i in 2:length(file.list)) {
  
  file <-
    file %>%
    bind_rows(read.xlsx(paste0('output/', file.list[i])) %>%
                mutate(across(1:3, as.character)) %>%
                mutate(date = dateconverter(as.Date("2018-07-13"), as.Date("2023-02-10"), "weekdayEOW")[i]))
  
}



file %>% mutate(cow_date = gsub("D", "", cow_date)) %>%
  mutate(cow_date = as.numeric(cow_date)) %>% drop_na(cow_date) %>%
  mutate(status = ifelse(grepl("C", A), "C", ifelse(grepl("M", A), "M", ifelse(grepl("D", M), grepl("D", A), NA)))) %>%
  filter(!is.na(status)) %>% view


file %>% mutate(cow_date = gsub("D", "", cow_date)) %>% 
  mutate(cow_date = as.numeric(cow_date)) %>% drop_na(cow_date) %>% gather(type, value, -date, -cow_date) %>%
  mutate(status = ifelse(grepl("^[[:digit:]]+$", value), NA, value)) %>%
  mutate(value = ifelse(grepl("^[[:digit:]]+$", value), value, NA)) %>%  hablar::retype() %>% 
  mutate(total = ifelse(is.na(value), 0, value*7)) %>% arrange(date) %>% rmsfuns::ViewXL()
  group_by(cow_date) %>% summarise(total = sum(total, na.rm = T)) %>% arrange(desc(total))




