
## doc data
released_with_addresses <- readRDS("./temp/released_with_addresses.rds") %>% 
  select(DCNumber, last_name = LastName, first_name = FirstName,
         middle_name = MiddleName, dob = BirthDate) %>% 
  mutate(dob = as.Date(substring(dob, 1, 10), "%m/%d/%Y")) %>% 
  mutate_at(vars(last_name, first_name, middle_name),
            ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, toupper(.))))

## voter data
fl_file <- dbGetQuery(db, "select LALVOTERID, Voters_FirstName,
                           Voters_MiddleName, Voters_LastName,
                           Voters_BirthDate from fl") %>% 
  rename(last_name = Voters_LastName,
         first_name = Voters_FirstName,
         middle_name = Voters_MiddleName) %>% 
  mutate(dob = as.Date(Voters_BirthDate, "%m/%d/%Y")) %>% 
  mutate_at(vars(last_name, first_name, middle_name),
            ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, toupper(.)))) %>% 
  select(-Voters_BirthDate)
  
## match
merge_list <- match_rolls_to_doc(released_with_addresses, DCNumber, fl_file, LALVOTERID)

small <- merge_list[[1]] %>% 
  select(LALVOTERID, DCNumber) %>% 
  filter(!is.na(LALVOTERID), !is.na(DCNumber))

saveRDS(small, "./temp/matched_doc_file.rds")
