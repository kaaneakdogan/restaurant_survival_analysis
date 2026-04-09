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
