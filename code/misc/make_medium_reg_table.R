stargazer::stargazer(m1c, m2c,
                     header = F,
                     type = "latex", notes.align = "l",
                     covariate.labels = c("2018", "Treated",
                                          "2018 $\\times$ Treated"),
                     dep.var.labels.include = FALSE,
                     title = "\\label{tab:oldies} General Election Turnout, 2010 {--} 2018",
                     table.placement = "H",
                     omit.stat = c("f", "ser"),
                     table.layout = "-cm#-t-a-s-n",
                     out = "./temp/medreg.tex",
                     out.header = F,
                     omit = c("white", "black", "latino", "asian", "female", "male",
                              "reg_date", "age", "dem", "rep", "median_income", "some_college",
                              "US_Congressional_District"),
                     notes = "TO REPLACE",
                     se = list(m1c_ses, m2c_ses),
                     add.lines=list(c("Includes covariates from matching" , "", "X"),
                                    c("Congressional District fixed effects" , "", "X")))

j <- fread("./temp/medreg.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{3}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors (clustered at level of match) in parentheses.}}}"

j <- j %>% 
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
  filter(!grepl("Note:", V1))

insert1 <- "\\small"

j <- bind_rows(j, data.frame(V1 = c(insert1), n = c(3.1))) %>% 
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
  arrange(n) %>% 
  select(-n)


write.table(j, "./temp/dind_reg_medium.tex", quote = F, col.names = F,
            row.names = F)
