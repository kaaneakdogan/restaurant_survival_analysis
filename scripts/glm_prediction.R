library(MASS)
#Predicting relationship between reviews and business to the survival chance
rpbglm <- glm(
  cbind(survived, did_not_survive) ~ reviewperbusiness,
  data = data,
  family = binomial)

summary(rpbglm)


quantileglm <- glm(
  cbind(survived, did_not_survive) ~ rpb_bin,
  family = binomial,
  data = data
)
summary(quantileglm)

 # Create prediction grid
pred_grid <- expand.grid(
  reviewperbusiness = seq(
    min(data$reviewperbusiness, na.rm = TRUE),
    max(data$reviewperbusiness, na.rm = TRUE),
    length.out = 100
  ),
  rpb_bin = levels(data$rpb_bin)
)

# Generate predictions
pred_grid$pred_prob_quantile <- predict(
  quantileglm,
  newdata = pred_grid,
  type = "response"
)

pred_grid$pred_prob_rpb <- predict(
  rpbglm,
  newdata = pred_grid,
  type = "response"
)

ggplot(pred_grid,
       aes(x = rpb_bin,
           y = pred_prob_quantile
       )) +
  geom_point() +
  labs(
    x = "Reviews Per Business Quantile in a Grid",
    y = "Survival Probability"
  ) 

ggplot(pred_grid,
       aes(x = reviewperbusiness,
           y = pred_prob_rpb
       )) +
  geom_point() +
  labs(
    x = "Reviews Per Business Quantile in a Grid",
    y = "Survival Probability"
  ) 
## Improved Visual here
ggplot(pred_grid,
       aes(x = rpb_bin,
           y = pred_prob_quantile,
           group = 1)) +
  geom_line(color = "#2C7BB6", linewidth = 0.8) +
  geom_point(color = "#2C7BB6", size = 4) +
  labs(
    title = "Restaurant Survival Probability by Review Density",
    subtitle = "Restaurants in grids with 7–15 reviews per business show highest survival",
    x = "Reviews Per Business Quantile in a Grid",
    y = "Survival Probability"
  ) +
  scale_y_continuous(limits = c(0.55, 0.68),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )
