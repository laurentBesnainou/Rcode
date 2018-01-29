library(timetk)
library(tidyquant)

#on va regarder le CA par mois
data_win <- pilotage_data %>% 
  filter(STEP=="4 - Gagnée",WEEK==max(WEEK),! is.na(CA_BT__N__KE))  %>% 
  
data_win %>% 
  ggplot(aes(D_GAIN,CA_BT__N__KE))+
  geom_line()+
  geom_point()+
  geom_ma(ma_fun= SMA, n=12)

data_win %>% as_xts(date_col = D_GAIN) %>% tk_index() %>% tk_get_timeseries_summary() %>% glimpse()

#Augmentation des donnees
data_AUG <- data_win %>% select(D_GAIN,CA_BT__N__KE) %>% tk_augment_timeseries_signature()

#Modelisation
fit_lm <- lm(CA_BT__N__KE~., data = select(data_AUG,-c(D_GAIN,diff)))
summary(fit_lm)

#previsions
data_idx <- data_win %>% as_xts(date_col = D_GAIN) %>%tk_index()
tail(data_idx)
#données futures
futur_idx <- data_idx %>% tk_make_future_timeseries(n_future = 80)

new_data <- futur_idx %>% tk_get_timeseries_signature() 

#faire les prédictions
pred <-  predict(fit_lm,newdata = select(new_data,-c(index,diff)))
predictions <- tibble(date=futur_idx, value=pred)


data_win %>% 
  
  ggplot(aes(D_GAIN,CA_BT__N__KE))+

  # Training data
  geom_line(color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]]) +
  # Predictions
  geom_line(aes(x=date, y = value), color = palette_light()[[2]], data = predictions) +
  geom_point(aes(x=date, y = value), color = palette_light()[[2]], data = predictions) +
  # Actuals
  geom_line(color = palette_light()[[1]], data = data_win) +
  geom_point(color = palette_light()[[1]], data = data_win) +
  # Aesthetics
  theme_tq() + coord_trans(y="log2") +
  labs(title = "CA BT dans le temps",
       subtitle = "Using basic multivariate linear regression can yield accurate results")