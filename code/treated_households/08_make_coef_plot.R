

load("./temp/confints.rdata")


overall <- bind_rows(
  as.data.frame(c1) %>% 
    mutate(model = "No Controls",
           term = "Overall") %>% 
    filter(row_number() == n()),
  as.data.frame(c2) %>% 
    mutate(model = "With Controls",
           term = "Overall") %>% 
    filter(row_number() == n())
)

black <- bind_rows(
  as.data.frame(c1_b) %>% 
    mutate(model = "No Controls",
           term = "Black Voters") %>% 
    filter(row_number() == n()),
  as.data.frame(c2_b) %>% 
    mutate(model = "With Controls",
           term = "Black Voters") %>% 
    filter(row_number() == n())
)

white <- bind_rows(
  as.data.frame(c1_w) %>% 
    mutate(model = "No Controls",
           term = "White Voters") %>% 
    filter(row_number() == n()),
  as.data.frame(c2_w) %>% 
    mutate(model = "With Controls",
           term = "White Voters") %>% 
    filter(row_number() == n())
)

latino <- bind_rows(
  as.data.frame(c1_l) %>% 
    mutate(model = "No Controls",
           term = "Latino Voters") %>% 
    filter(row_number() == n()),
  as.data.frame(c2_l) %>% 
    mutate(model = "With Controls",
           term = "Latino Voters") %>% 
    filter(row_number() == n())
)

female <- bind_rows(
  as.data.frame(c1_f) %>% 
    mutate(model = "No Controls",
           term = "Female Voters") %>% 
    filter(row_number() == n()),
  as.data.frame(c2_f) %>% 
    mutate(model = "With Controls",
           term = "Female Voters") %>% 
    filter(row_number() == n())
)

men <- bind_rows(
  as.data.frame(c1_m) %>% 
    mutate(model = "No Controls",
           term = "Male Voters") %>% 
    filter(row_number() == n()),
  as.data.frame(c2_m) %>% 
    mutate(model = "With Controls",
           term = "Male Voters") %>% 
    filter(row_number() == n())
)


tidies <- rbind(overall, black, white, latino, female, men)
colnames(tidies) <- c("conf.low", "conf.high", "model", "term")
tidies <- tidies %>% 
  mutate(estimate = (conf.low + conf.high) / 2)

tidies$term <- factor(tidies$term,
                     levels = rev(c("Overall",
                                    "Black Voters",
                                    "Latino Voters",
                                    "White Voters",
                                    "Female Voters",
                                    "Male Voters"
                     )))

#############
ci_level = 0.95
legend.title = "Model"
facet.label.pos = "top"
n_models <- length(unique(tidies$model))
oshapes <- c(21:25, 15:18, 3, 4, 8)
shapes <- oshapes[seq_len(n_models)]
colors <- get_colors("CUD Bright", n_models)
exp <- T
################

p <- ggplot(data = tidies)

p <- p + ggstance::geom_pointrangeh(aes(y = term, x = estimate, 
                                       xmin = conf.low, xmax = conf.high, linetype = model, 
                                       shape = model), position = ggstance::position_dodgev(height = -.5), 
                                   fill = "white", fatten = 3, size = 0.8, show.legend = T)

p <- p + geom_vline(xintercept = 1 - !exp, linetype = 2, 
                   size = 0.25) + 
  labs(linetype = "Model", shape = "Model") +
 theme(legend.pos = "right") +
 drop_y_gridlines() + theme(axis.title.y = element_blank(), 
                            axis.text.y = element_text(size = 10),
                            panel.grid.major.x = element_line(linetype = "solid")) + 
 xlab("exp(Estimate)") +
 theme(text = element_text(family = "LM Roman 10"))

save(p, file = "./temp/coef_plot.rdata")
         


