
## doc data
released_with_addresses <- readRDS("./temp/hills_with_ads.rds") %>% 
  mutate(DCNUM = as.character(row_number())) %>% 
  select(DCNUM, last_name, first_name,
         middle_name, dob = date_of_birth) %>% 
  mutate(dob = as.Date(dob, "%m/%d/%Y")) %>% 
  mutate_at(vars(last_name, first_name, middle_name),
            ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, toupper(.))))

released_with_addresses <- as.data.table(released_with_addresses)
## voter data
fl_file <- dbGetQuery(db, "select LALVOTERID, Voters_FirstName,
                           Voters_MiddleName, Voters_LastName,
                           Voters_BirthDate from fl  where Voters_FIPS == 57") %>% 
  rename(last_name = Voters_LastName,
         first_name = Voters_FirstName,
         middle_name = Voters_MiddleName) %>% 
  mutate(dob = as.Date(Voters_BirthDate, "%m/%d/%Y")) %>% 
  mutate_at(vars(last_name, first_name, middle_name),
            ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, toupper(.)))) %>% 
  select(-Voters_BirthDate)
fl_file <- as.data.table(fl_file)
  
## match
merge_list <- match_rolls_to_doc(released_with_addresses, "DCNUM", fl_file, "LALVOTERID")

small <- merge_list[[1]] %>% 
  select(LALVOTERID, DCNUM) %>% 
  filter(!is.na(LALVOTERID), !is.na(DCNUM))

saveRDS(small, "./temp/matched_doc_file_hills.rds")
