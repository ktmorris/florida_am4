t <- readRDS("./temp/balance_table.rds")

colnames(t) <- clean_names(t)


t <- dplyr::select(t, X, treated, control) %>% 
  mutate_at(vars(treated, control), ~ as.numeric(gsub("%|[$]|,", "", .))) %>% 
  mutate(X = ifelse(X == "%White", "% White", X))


t <- pivot_longer(t, c("treated", "control")) %>% 
  mutate(name = str_to_sentence(name))

j <- filter(t, X %in% c("% White", "% Black","% Democrat",
                        "% with Some College",
                        "% Female",
                        "Median Income")) %>% 
  mutate(lab = ifelse(X == "Median Income", dollar(value, 1), paste0(value, "%")),
         value = ifelse(X == "Median Income", value, value / 100 * 89992.86))

j$X <- factor(j$X, levels = c("Median Income",
                              "% with Some College", "% Female", "% Democrat", "% White", "% Black"))
j$name <- factor(j$name, levels = c("Treated", "Control"))



ggplot(j, aes(fill = name, x = value, y = X)) + 
  geom_bar(position = position_dodge(-.6), stat = "identity", width = .5, color = "black") +
  theme_bc(base_size = 15, face = "bold") +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_x_continuous(labels = scales::dollar,
                    
                     sec.axis = sec_axis(~./89992.86,labels = scales::percent, name = NULL),
                     limits = c(0, 75000)) +
  scale_fill_manual(values = c("#F4B41A", "#143D59")) +
  geom_text(aes(x = value, y = X, label = lab),
            position = position_dodge(-.6),
            hjust = -0.15, family = "BentonSans") +
  ggtitle("Comparisons for Select Matching Measures")

ggsave("temp/comp_bars.png", width = 9, height = 11*(7/16), units = "in")  
