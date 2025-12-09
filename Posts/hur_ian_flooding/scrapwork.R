### Log Transformation

# Create new columns for the quadratic and log distance 
join_df <- join_df %>% 
  mutate(dist_quad = (dist_hur)^2 / 1e6, # Sqaure and then scale it 
         dist_log = log(dist_hur + 0.001))

# Quadratic distance model 
model_quad <- lm(totals_inches ~ p_peopcolorpct * p_lowincpct + dist_quad, 
                 data = join_df)

# Log distance mdeo 
model_log <- lm(totals_inches ~ p_peopcolorpct * p_lowincpct + dist_log, 
                data = join_df)

#### Bayesian information Criterion

#BIC principle: - The lower the BIC value, the better model. - A difference of at least 10, meaningful difference between models

BIC(model_quad, model_log)

#Both models have 6 degrees of freedom. Because complexity is equal, the Bayesian information Criterion is comparing the models on fit alone.

#The BIC values have about a 22 difference with model_log having the lower BIC value. Therefore, the quadratic model fits the data better. Moving forward, we will make predictions using the log transformation model.


summary(model_log)

# expand grid 
grid <- expand_grid(p_peopcolorpct = seq(min(join_df$p_peopcolorpct), max(join_df$p_peopcolorpct), length.out = 50), 
                    p_lowincpct = seq(min(join_df$p_lowincpct), max(join_df$p_lowincpct), length.out = 50), 
                    dist_log = seq(min(join_df$dist_log), max(join_df$dist_log), length.out = 50))

## Predictions 
prediction_grid <- grid %>% 
  mutate(flood_predictions = predict(object = model_log,
                                     newdata = grid,
                                     type = 'response'
  ))

# CI 
prediction_grid_se <- predict(object = model_log, 
                              newdata = prediction_grid, 
                              type = 'response', 
                              se.fit = TRUE)

prediction_grid <- prediction_grid %>% 
  mutate(
    prediction_se = prediction_grid_se$se.fit,
    ci_lower = qnorm(0.025, mean = flood_predictions, sd = prediction_se),
    ci_upper = qnorm(0.975, mean = flood_predictions, sd = prediction_se)
  )

## Fit model

The response variable (what we are predicting) is the amount of flooding (inches). The predictors are the people of color percentage, low income percentages, and if location is in the hurricanes path.



sum(join_df$totals_inches < 0) # No negatives 
sum(join_df$totals_inches == 0) # Lots of 0 

# Add a small constant 
# If zeros occur because some locations recorded no rainfall:
#join_df$totals_inches <- join_df$totals_inches + 0.001


model <- glmmTMB(totals_inches ~ p_peopcolorpct * p_lowincpct + hur_path, 
        data = join_df, 
        ziformula = ~ hur_path
        )

mod_summary <- summary(model)


dim(join_df)


### Model coefficients


# Data frame of coefficients from CONDITIONAL models  
mod_summary_cond_table <- data.frame(
  'Esimates' = mod_summary$coefficients$cond[ ,1], 
  'StdError' = mod_summary$coefficients$cond[,2], 
  'P-Value' = mod_summary$coefficients$cond[,4], 
  row.names = c("Intercept",
  "People of Color (%)",
  "Low Income (%)",
  "PoC × Low Income Interaction",
  "Hurricane Path"))


# Data frame of coefficients from ZERO INFLATED models  
mod_summary_zi_table <- data.frame(
    'Esimates' = mod_summary$coefficients$zi[ ,1], 
  'StdError' = mod_summary$coefficients$zi[,2], 
  'P-Value' = mod_summary$coefficients$zi[,4], 
  row.names = c("Intercept", "Hurricane Path"))


kableExtra::kable(mod_summary_zi_table, 
                  digits = 4, 
                  align = 'c',
                  caption = "Generalized Linear Mixed Model Zero-Inflated Coeffients")

kableExtra::kable(mod_summary_cond_table, 
                  digits = 4, 
                  align = 'c',
                  caption = "Generalized Linear Mixed Model Conditional Coeffients")


### Create an Expand Grid


grid <- expand_grid(p_peopcolorpct = seq(min(join_df$p_peopcolorpct), max(join_df$p_peopcolorpct)), 
            p_lowincpct = seq(min(join_df$p_lowincpct), max(join_df$p_lowincpct)), 
            hur_path = 0:1) 

### Making Predictions


prediction_grid <- grid %>% 
  mutate(
    flood_prediction = predict(object = model, 
                               newdata = grid,
                               type = 'response')
  )



### Confidence Intervals

prediction_grid_se <- predict(object = model, 
                              newdata = prediction_grid, 
                              type = 'response', 
                              se.fit = TRUE)

prediction_grid <- prediction_grid %>% 
  mutate(
    prediction_se = prediction_grid_se$se.fit,
    ci_lower = qnorm(0.025, mean = flood_prediction, sd = prediction_se),
    ci_upper = qnorm(0.975, mean = flood_prediction, sd = prediction_se)
  )



### Plots

#### The Interaction between Low Income and Minority Communities Influences on Flooding



prediction_grid %>% 
  filter(hur_path == 1) %>% 
  ggplot(aes(x = p_peopcolorpct, y = p_lowincpct, fill = flood_prediction)) + 
  geom_raster() +
  scale_fill_steps2(low = "#ffad76",        #- THIS CREATES COLOR BINS 
                     mid = "#ffe896",
                     high = "#488f30", 
                     midpoint = median(prediction_grid$flood_prediction), 
                     n.breaks = 8, 
                     limits = c(0, 8)) +
  #scale_fill_gradient(low = "#466DA7", high = "#D9252A") + # Produces the continuous color scale 
  theme_classic() + 
  labs(title = "Flood Predictions for Areas Inside the Storm’s Path", 
       x = "People of Color (%)", 
       y = "Low Income (%)", 
       fill = "Flood Predictions\n(inches)", 
       caption = "Figure 5"
       )


prediction_grid %>% 
  filter(hur_path == 0) %>% 
  ggplot(aes(x = p_peopcolorpct, y = p_lowincpct, fill = flood_prediction)) + 
  geom_raster() +
  scale_fill_steps2(low = "#ffad76",        #- THIS CREATES COLOR BINS 
                     mid = "#ffe896",
                     high = "#488f30", 
                     midpoint = median(prediction_grid$flood_prediction), 
                     n.breaks = 8, 
                    limits = c(0, 8)) +
  #scale_fill_gradient(low = "#466DA7", high = "#D9252A") + # Produces the continuous color scale 
  theme_classic() +
   labs(title = "Flood Predictions for Areas Outside the Storm’s Path", 
       x = "People of Color (%)", 
       y = "Low Income (%)", 
       fill = "Flood Predictions\n(inches)", 
       caption = "Figure 6"
       )



#### Flooding in Minority Populations


#| code-fold: true
ggplot(prediction_grid, 
       aes(x = p_peopcolorpct, 
           y = flood_prediction, 
           group = hur_path)) + 
  geom_line(aes(color = factor(hur_path))) + 
  geom_ribbon(
    aes(ymin = ci_lower, ymax = ci_upper),
    fill = "#ffad76",
    alpha = 0.5,
    color = NA
  ) + 
  scale_color_manual(
    values = c("0" = "#488f30","1" = "#fc7082"),
    labels = c("Inside", "Outside"),
    name = "Hurricane Path"
  ) +
  labs(
    title = "Flood Exposure in Communities of Color in FL During Hurricane Ian", 
    x = "People of Color (%)", 
    y = "Flood Predictions (in)", 
    caption = "Figure 7"
  ) + 
  theme_bw()


#### Flooding in Low Income Populations


ggplot(prediction_grid, 
       aes(x = p_lowincpct, 
           y = flood_prediction, 
           group = hur_path)) + 
  geom_line(aes(color = factor(hur_path))) + 
  geom_ribbon(
    aes(ymin = ci_lower, ymax = ci_upper),
    fill = "#ffad76",
    alpha = 0.5,
    color = NA
  ) + 
  scale_color_manual(
    values = c("0" = "#488f30","1" = "#fc7082"),
    labels = c("Inside", "Outside"),
    name = "Hurricane Path"
  ) +
  labs(
    title = "Flood Exposure in Low Income Communities in FL During Hurricane Ian", 
    x = "Low Income (%)", 
    y = "Flood Predictions (in)", 
    caption = "Figure 8"
  ) + 
  theme_bw()

