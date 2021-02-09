stargazer::stargazer(models1, models2,
          column.labels = c("Lives with Formerly Incarcerated", "Lives with Disenfranchised"),
          column.separate = c(4, 4),
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("2018", "Treated", "Years Since Latest Incarceration",
                               "2018 $\\times$ Treated",
                               "2018 $\\times$ Years Since",
                               "Treated $\\times$ Years Since",
                               "2018 $\\times$ Treated $\\times$ Years Since"),
          dep.var.labels.include = FALSE,
          title = "\\label{tab:ap-hills-2} General Election Turnout, 2010 {--} 2018",
          table.placement = "H",
          omit.stat = c("f", "ser"),
          table.layout = "-cm#-t-a-s-n",
          out = "./temp/bigreg_hills_av.tex",
          out.header = F,
          omit = c("white", "black", "latino", "asian", "female", "male",
                   "reg_date", "age", "dem", "rep", "median_income", "some_college",
                   "US_Congressional_District"),
          notes = "TO REPLACE",
          se = ses_cl,
          add.lines=list(c("Includes covariates from matching" , "", "X", "", "X", "", "X", "", "X"),
                         c("Congressional District fixed effects" , "", "X", "", "X", "", "X", "", "X")))

j <- fread("./temp/bigreg_hills_av.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{9}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors (clustered at level of match) in parentheses.}}}"

j <- j %>% 
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
  filter(!grepl("Note:", V1))

insert1 <- "\\resizebox{1\\textwidth}{!}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(3.1, nrow(j) + 1 - 0.01))) %>% 
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
  arrange(n) %>% 
  select(-n)


write.table(j, "./temp/dind_reg_hills_av.tex", quote = F, col.names = F,
            row.names = F)

