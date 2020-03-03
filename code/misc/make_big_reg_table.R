load("./temp/full_match_reg.rdata")
stargazer::stargazer(m1, m2, m3, m4, m1b, m2b, m3b, m4b,
          column.labels = c("All Matched Observations", "Registration Date prior to Discharge"),
          column.separate = c(4, 4),
          header = F,
          type = "latex", notes.align = "l",
          covariate.labels = c("D(2018)", "D(Treated)", "Years Since Latest Incarceration",
                               "D(2018) $\\times$ D(Treated)",
                               "D(2018) $\\times$ Years Since",
                               "D(Treated) $\\times$ Years Since",
                               "D(2018) $\\times$ D(Treated) $\\times$ Years Since"),
          dep.var.labels.include = FALSE,
          title = "\\label{tab:tab-dind} General Election Turnout, 2010 {--} 2018",
          table.placement = "H",
          omit.stat = c("f", "ser"),
          table.layout = "-cm#-t-a-s-n",
          out = "./temp/bigreg.tex",
          out.header = F,
          omit = c("white", "black", "latino", "asian", "female", "male",
                   "reg_date", "age", "dem", "rep", "median_income", "some_college",
                   "US_Congressional_District"),
          notes = "TO REPLACE",
          se = list(coef(summary(m1,cluster = c("match_group")))[, 2],
                    coef(summary(m2,cluster = c("match_group")))[, 2],
                    coef(summary(m3,cluster = c("match_group")))[, 2],
                    coef(summary(m4,cluster = c("match_group")))[, 2],
                    coef(summary(m1b,cluster = c("match_group")))[, 2],
                    coef(summary(m2b,cluster = c("match_group")))[, 2],
                    coef(summary(m3b,cluster = c("match_group")))[, 2],
                    coef(summary(m4b,cluster = c("match_group")))[, 2]),
          add.lines=list(c("Includes covariates from matching" , "", "X", "", "X", "", "X", "", "X"),
                         c("Congressional District fixed effects" , "", "X", "", "X", "", "X", "", "X")))

j <- fread("./temp/bigreg.tex", header = F, sep = "+")

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


write.table(j, "./temp/dind_reg.tex", quote = F, col.names = F,
            row.names = F)
