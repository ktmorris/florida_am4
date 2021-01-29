library(jtools)
c1 <- as.data.frame(confint(models1[[1]])) %>%
  add_rownames("vars") %>%
  mutate(model = "Without Matched Covariates")

c2 <- as.data.frame(confint(models1[[2]])) %>%
  add_rownames("vars") %>%
  mutate(model = "With Matched Covariates")


tidies <- bind_rows(c1, c2) %>% 
  mutate(vars = gsub("scale|[(]|[)]", "", vars)) %>% 
  filter(!grepl("Cong|Intercept|female|rep|reg_date", vars))
colnames(tidies) <- c("term", "conf.low", "conf.high", "model")

tidies <- tidies %>%
  mutate(estimate = (conf.low + conf.high) / 2,
         term = ifelse(term == "d18", "D(2018)", term),
         term = ifelse(term == "treated", "Treated", term),
         term = ifelse(term == "d18:treated", "D(2018) * Treated", term),
         term = ifelse(term == "white", "White", term),
         term = ifelse(term == "black", "Black", term),
         term = ifelse(term == "latino", "Latino", term),
         term = ifelse(term == "asian", "Asian", term),
         term = ifelse(term == "male", "Male", term),
         term = ifelse(term == "age", "Age", term),
         term = ifelse(term == "median_income", "Median Income", term),
         term = ifelse(term == "some_college", "% with Some College", term),
         term = ifelse(term == "dem", "Democrat", term)) %>%
  filter(!grepl("locality", term),
         term != "Intercept")

######################
tidies$model <- factor(tidies$model, levels = rev(c("Without Matched Covariates",
                                                    "With Matched Covariates")))

tidies$term <- factor(tidies$term, levels = (rev(c("Treated",
                                               "D(2018)",
                                               "D(2018) * Treated",
                                               "Black",
                                               "Asian",
                                               "Latino",
                                               "White",
                                               "Male",
                                               "Democrat",
                                               "Age",
                                               "Median Income",
                                               "% with Some College"))))

ci_level = 0.95
legend.title = "Model"
facet.label.pos = "top"
n_models <- length(unique(tidies$model))
oshapes <- c(21:25, 15:18, 3, 4, 8)
shapes <- oshapes[seq_len(n_models)]
exp <- F

################

p <- ggplot(data = tidies) +
  ggstance::geom_pointrangeh(aes(y = term, x = estimate, 
                                 xmin = conf.low, xmax = conf.high, colour = model, 
                                 shape = model), position = ggstance::position_dodgev(height = -.5), 
                             fill = "white", fatten = 3, size = 0.8, show.legend = T)+
  geom_vline(xintercept = 1 - !exp, linetype = 2, 
             size = 0.25) + scale_colour_manual(values = c("red", "blue"), name = legend.title) + 
  theme_nice(legend.pos = "right", base_family = "BentonSans") +
  scale_shape_manual(values = shapes, name = legend.title) +
  drop_y_gridlines() + theme(axis.title.y = element_blank(), 
                             axis.text.y = element_text(size = 12),
                             panel.grid.major.x = element_line(linetype = "solid"),
                             text = element_text(family = "BentonSans", face = "bold")) + 
  xlab("Estimate") +
  scale_x_continuous(labels = percent, ) +
  labs(caption = "Registration date and congressional district fixed effects not shown.") +
  theme(plot.caption = element_text(hjust = 0),
        text = element_text(size = 12))
p
ggsave("./temp/coef1.png", width = 9, height = 11*(7/16), units = "in")
p + geom_rect(ymin = 11.5, ymax = 12.5, xmin = -.07, xmax = .11,
              fill = NA,
              color = "hot pink",
              size = 2)
ggsave("./temp/coef2.png", width = 9, height = 11*(7/16), units = "in")
p + geom_rect(ymin = 9.5, ymax = 10.5, xmin = -.07, xmax = .11,
              fill = NA,
              color = "hot pink",
              size = 2)
ggsave("./temp/coef3.png", width = 9, height = 11*(7/16), units = "in")
