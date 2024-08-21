


alch_ret_daily <- Alchian_Data %>% 
  filter(!is.na(`DJIA`)) %>% 
  # calculate price returns
  mutate(ret_bc = 100*(`Beryllium Corp`/dplyr::lag(`Beryllium Corp`) -1),
         ret_lith = 100*(`Lithium Corp of America`/dplyr::lag(`Lithium Corp of America`) -1),
         ret_ASRC = 100*(`American Smelting & Refining Co.`/dplyr::lag(`American Smelting & Refining Co.`) -1),
         ret_MHI = 100*(`Metal Hydrides Inc`/dplyr::lag(`Metal Hydrides Inc`) -1),
         ret_DJIA = 100*(`DJIA`/dplyr::lag(`DJIA`) -1),
         # de-annualize yield
         Rf = `Market yield on 3 month bills`/365) 


alch_ret_weekly <- Alchian_Data %>% 
  # linearly interpolate missing risk free rates; this covers some cases where bond markets closed but stock markets didn't
  mutate(Rf_interp = zoo::na.approx(`Market yield on 3 month bills`, na.rm = FALSE)) %>% 
  filter(!is.na(`Metal Hydrides Inc`)) %>% 
  mutate(ret_bc = 100*(`Beryllium Corp`/dplyr::lag(`Beryllium Corp`) -1),
         ret_lith = 100*(`Lithium Corp of America`/dplyr::lag(`Lithium Corp of America`) -1),
         ret_ASRC = 100*(`American Smelting & Refining Co.`/dplyr::lag(`American Smelting & Refining Co.`) -1),
         ret_MHI = 100*(`Metal Hydrides Inc`/dplyr::lag(`Metal Hydrides Inc`) -1),
         ret_DJIA = 100*(`DJIA`/dplyr::lag(`DJIA`) -1),
         Rf = Rf_interp/52)


# set a particular day for BC as missing because of a stock split
alch_ret_daily$ret_bc[alch_ret_daily$Date == mdy("4/28/1953")] <- NA
alch_ret_weekly$ret_bc[alch_ret_weekly$Date == mdy("5/1/1953")] <- NA

## windows
# T0 to T1: presample
# T1 to T2: event window
# T2 to T3: post-sample

T0 <- ymd("1953-01-01")
T1 <- ymd("1953-07-31")
T2 <- ymd("1953-12-31")
T3 <- ymd("1954-12-31")



market_daily <- list("Beryllium Corp" = lm(ret_bc ~ ret_DJIA, data = alch_ret_daily, subset = (Date >= T0 & Date <= T1)),
                    "Lithium Corp.of Am."=lm(ret_lith ~ ret_DJIA, data = alch_ret_daily, subset = (Date >= T0 & Date <= T1)),
                    "ASRC" = lm(ret_ASRC ~ ret_DJIA, data = alch_ret_daily, subset = (Date >= T0 & Date <= T1)))
                    
                    
market_weekly <- list("Beryllium Corp" = lm(ret_bc ~ ret_DJIA, data = alch_ret_weekly, subset = (Date >= T0 & Date <= T1)),
                     "Lithium Corp.of Am."=lm(ret_lith ~ ret_DJIA, data = alch_ret_weekly, subset = (Date >= T0 & Date <= T1)),
                     "ASRC" = lm(ret_ASRC ~ ret_DJIA, data = alch_ret_weekly, subset = (Date >= T0 & Date <= T1)),
                     "MHI" =  lm(ret_MHI ~  ret_DJIA, data = alch_ret_weekly, subset = (Date >= T0 & Date <= T1)))

### Table formatting for modelsummary

starlist <- c('*' = .1, '**' = 0.05, '***' = 0.01)


gm <- gof_map
gm$omit <- FALSE
cm <- c("(Intercept)" = "alpha",
        "ret_DJIA" = "Market beta")

modelsummary(market_daily, coef_map = cm, stars = starlist, fmt = 2, gof_map = c("nobs", "r.squared","adj.r.squared", "F","rmse","vcov.type"), escape = FALSE, vcov ="NeweyWest", output = paste0(table_path, "/market_daily.docx"))
modelsummary(market_weekly, coef_map = cm, stars = starlist, fmt = 2, gof_map = c("nobs", "r.squared","adj.r.squared", "F","rmse","vcov.type"), escape = FALSE, vcov ="NeweyWest", output = paste0(table_path, "/market_weekly.docx"))

### Abnormal returns


alch_market_daily_AR <- alch_ret_daily %>% 
  mutate(market_bc =   predict(market_daily[[1]],alch_ret_daily, interval = "none"),
         market_lith=  predict(market_daily[[2]],alch_ret_daily, interval = "none"),
         market_ASRC = predict(market_daily[[3]],alch_ret_daily, interval = "none")) %>% 
  mutate(AR_bc_market_ret =   ret_bc - market_bc,
         AR_lith_market_ret= ret_lith  - market_lith,
         AR_ASRC_market_ret= ret_ASRC  - market_ASRC) %>% 
  # select(`Date`,ex_DJI, ex_bc:ex_ASRC, CAPM_bc:AR_ASRC_CAPM) %>% 
  mutate(inwindow = Date %within% interval(start = T1, end =T2)) 

### Weekly abnormal returns


alch_market_weekly_AR <- alch_ret_weekly %>% 
  mutate(market_bc = predict(market_weekly[[1]],  alch_ret_weekly, interval = "none"),
         market_lith= predict(market_weekly[[2]], alch_ret_weekly, interval = "none"),
         market_ASRC = predict(market_weekly[[3]],alch_ret_weekly,interval = "none") ,
         market_MHI = predict(market_weekly[[4]], alch_ret_weekly,interval = "none") ) %>% 
  
  mutate(AR_bc_market_ret = ret_bc  - market_bc,
         AR_lith_market_ret = ret_lith - market_lith,
         AR_ASRC_market_ret = ret_ASRC - market_ASRC,
         AR_MHI_market_ret = ret_MHI   - market_MHI) %>% 
  # select(`Date`,ex_DJI, ex_bc:ex_MHI, CAPM_bc:AR_MHI_CAPM) %>% 
  mutate(inwindow = Date %within% interval(start = T1, end = T2))


n_daily_bc <-   nrow(market_daily$`Beryllium Corp`$model)
n_daily_lith <- nrow(market_daily$`Lithium Corp.of Am.`$model)
n_daily_ASRC <- nrow(market_daily$`ASRC`$model)


bc_df  <- market_daily$`Beryllium Corp`$df.residual
lith_df<- market_daily$`Lithium Corp.of Am.`$df.residual
ASRC_df<- market_daily$`ASRC`$df.residual


rse_bc  <-  sqrt(sum((market_daily[[1]]$residuals)^2 )/(market_daily[[1]]$df.residual))
rse_lith <- sqrt(sum((market_daily[[2]]$residuals)^2 )/(market_daily[[2]]$df.residual))
rse_ASRC <- sqrt(sum((market_daily[[3]]$residuals)^2 )/(market_daily[[3]]$df.residual))


# weekly

n_weekly_bc <-   nrow(market_weekly$`Beryllium Corp`$model)
n_weekly_lith <- nrow(market_weekly$`Lithium Corp.of Am.`$model)
n_weekly_lith <- nrow(market_weekly$`Lithium Corp.of Am.`$model)
n_weekly_MHI <- nrow(market_weekly$`MHI`$model)


bc_weekly_df  <-  market_weekly$`Beryllium Corp`$df.residual
lith_weekly_df<-  market_weekly$`Lithium Corp.of Am.`$df.residual
ASRC_weekly_df<-  market_weekly$`ASRC`$df.residual
MHI_weekly_df<-  market_weekly$`MHI`$df.residual

rse_weekly_bc  <-  sqrt(sum((market_weekly[[1]]$residuals)^2 )/(market_weekly[[1]]$df.residual))
rse_weekly_lith <- sqrt(sum((market_weekly[[2]]$residuals)^2 )/(market_weekly[[2]]$df.residual))
rse_weekly_ASRC <- sqrt(sum((market_weekly[[3]]$residuals)^2 )/(market_weekly[[3]]$df.residual))
rse_weekly_MHI <-  sqrt(sum((market_weekly[[4]]$residuals)^2 )/(market_weekly[[4]]$df.residual))



alch_plot_std_AR <- alch_market_daily_AR %>% 
  select(Date,AR_bc_market_ret:AR_ASRC_market_ret) %>% 
  mutate(`Beryllium Corp.` = AR_bc_market_ret/rse_bc,
         `Lithium Corp. of America`  = AR_lith_market_ret/rse_lith,
         `ASRC` = AR_ASRC_market_ret/rse_ASRC) %>% 
  select(Date, `Beryllium Corp.`:`ASRC`) %>% 
  pivot_longer(cols = -Date)

#

ggplot(data = filter(alch_plot_std_AR, Date >= T1 & Date <= T2), aes(x = Date, y = value, color = name))+
  geom_point(alpha = 0.25, size = 2, color = "black") +
  geom_point(data = filter(alch_plot_std_AR, (Date >= T1 & Date <= T2& abs(value)>=1.96)), aes(x = Date, y = value), color = "red", size = 2, shape = "square")+
    theme_carleton()+   scale_color_paletteer_d("ggthemes::calc")+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(aes(yintercept = 1.96),  linetype = 2)+
  geom_hline(aes(yintercept =- 1.96),  linetype = 2)+
  guides(color = "none")+
  ylab('Standardized abnormal return')+  facet_wrap(facets = vars(as_factor(name)))

ggsave("figures/AR_market_daily.png", device = "png", dpi = "retina", scale = 1, bg = "white",
       width=320*.9, height =180*.9, units = "mm")

alch_plot_std_w_AR <- alch_market_weekly_AR %>% 
  select(Date,AR_bc_market_ret:AR_MHI_market_ret) %>% 
  mutate(`Beryllium Corp.` = AR_bc_market_ret/rse_weekly_bc,
         `Lithium Corp. of America`  = AR_lith_market_ret/rse_weekly_lith,
         `ASRC` = AR_ASRC_market_ret/rse_weekly_ASRC,
         `Metal Hydrides, Inc` = AR_MHI_market_ret/rse_weekly_MHI) %>% 
  select(Date, `Beryllium Corp.`:`Metal Hydrides, Inc`) %>% 
  pivot_longer(cols = -Date)

#

ggplot(data = filter(alch_plot_std_w_AR, Date >= T1 & Date <= T2), aes(x = Date, y = value, color = name))+
  geom_point(alpha = 0.25, size = 2, color = "black") +
  geom_point(data = filter(alch_plot_std_w_AR, (Date >= T1 & Date <= T2& abs(value)>=1.96)), aes(x = Date, y = value), color = "red", size = 2, shape = "square")+
    theme_carleton()+   scale_color_paletteer_d("ggthemes::calc")+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(aes(yintercept = 1.96),  linetype = 2)+
  geom_hline(aes(yintercept =- 1.96),  linetype = 2)+
  guides(color = "none")+
  ylab('Standardized abnormal return')+  facet_wrap(facets = vars(as_factor(name)))

ggsave("figures/AR_market_weekly.png", device = "png", dpi = "retina", scale = 1, bg = "white",
       width=320*.9, height =180*.9, units = "mm")




tauwstart = 0
tauwend = 26

row_event <- which(alch_market_weekly_AR$Date == T1)


alch_market_weekly_CAR<-alch_market_weekly_AR %>% 
  filter(Date >= T1 & Date <=T2) %>% 
  #filter(!is.na(ex_MHI)) %>% 
  # mutate(tauw = row_number() - row_test) %>% 
  # filter(tauw >= tauwstart & tauw <= tauwend) %>% 
  mutate(CAR_MHI  = cumsum(AR_MHI_market_ret),
         CAR_BC   = cumsum(AR_bc_market_ret),
         CAR_ASRC = cumsum(AR_ASRC_market_ret),
         CAR_LC   = cumsum(AR_lith_market_ret)) %>% 
  mutate(CAR_var_MHI = row_number()*rse_weekly_MHI^2,
         CAR_var_BC  = row_number()*rse_weekly_bc^2,
         CAR_var_ASRC= row_number()*rse_weekly_ASRC^2,
         CAR_var_LC  = row_number()*rse_weekly_lith^2) %>% 
  mutate(CAR_MHI_std  = CAR_MHI /sqrt( CAR_var_MHI ),
         CAR_BC_std   = CAR_BC  /sqrt( CAR_var_BC  ),
         CAR_ASRC_std = CAR_ASRC/sqrt( CAR_var_ASRC),
         CAR_LC_std   = CAR_LC  /sqrt( CAR_var_LC  ))

ggplot(alch_market_weekly_CAR, aes(x = as.Date(Date)))+
  geom_line(aes(y = CAR_BC_std, color = "Beryllium Corp")) + geom_point(aes(y = CAR_BC_std, color = "Beryllium Corp"))+
  geom_line(aes(y = CAR_LC_std, color = "Lithium Corp. of America"))+ geom_point(aes(y = CAR_LC_std, color = "Lithium Corp. of America"))+
  geom_line(aes(y = CAR_ASRC_std, color = "ASRC"))+ geom_point(aes(y = CAR_ASRC_std, color = "ASRC")) +
  geom_line(aes(y = CAR_MHI_std, color = "MHI"))+ geom_point(aes(y = CAR_MHI_std, color = "MHI")) +
    theme_carleton()+   scale_color_paletteer_d("ggthemes::calc")+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(aes(yintercept = 1.96),  linetype = 2)+
  geom_hline(aes(yintercept =- 1.96),  linetype = 2)+
  ylab('Standardized cumulative excess return')+
  labs(color = element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  xlab("Date")

ggsave("figures/CAR_market_weekly.png", device = "png", dpi = "retina", scale = 1, bg = "white",
       width=320*.9, height =180*.9, units = "mm")


alch_market_daily_CAR<-alch_market_daily_AR %>% 
  filter(Date >= T1 & Date <=T2) %>% 
  filter(!is.na(ret_bc)) %>% 
  # mutate(tauw = row_number() - row_test) %>% 
  # filter(tauw >= tauwstart & tauw <= tauwend) %>% 
  mutate(#CAR_MHI  = cumsum(AR_MHI_market_ret),
    CAR_BC   = cumsum(AR_bc_market_ret),
    CAR_ASRC = cumsum(AR_ASRC_market_ret),
    CAR_LC   = cumsum(AR_lith_market_ret)) %>% 
  mutate(#CAR_var_MHI = row_number()*rse_MHI^2,
    CAR_var_BC  = row_number()*rse_bc^2,
    CAR_var_ASRC= row_number()*rse_ASRC^2,
    CAR_var_LC  = row_number()*rse_lith^2) %>% 
  mutate(#CAR_MHI_std  = CAR_MHI /sqrt( CAR_var_MHI ),
    CAR_BC_std   = CAR_BC  /sqrt( CAR_var_BC  ),
    CAR_ASRC_std = CAR_ASRC/sqrt( CAR_var_ASRC),
    CAR_LC_std   = CAR_LC  /sqrt( CAR_var_LC  ))


ggplot(alch_market_daily_CAR, aes(x = as.Date(Date)))+
  geom_line(aes(y = CAR_BC_std, color = "Beryllium Corp")) + geom_point(aes(y = CAR_BC_std, color = "Beryllium Corp"))+
  geom_line(aes(y = CAR_LC_std, color = "Lithium Corp. of America"))+ geom_point(aes(y = CAR_LC_std, color = "Lithium Corp. of America"))+
  geom_line(aes(y = CAR_ASRC_std, color = "ASRC"))+ geom_point(aes(y = CAR_ASRC_std, color = "ASRC")) +
    theme_carleton()+   scale_color_paletteer_d("ggthemes::calc")+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(aes(yintercept = 1.96),  linetype = 2)+
  geom_hline(aes(yintercept =- 1.96),  linetype = 2)+
  ylab('Standardized cumulative excess return')+
  labs(color = element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  xlab("Date")

ggsave("figures/CAR_market_daily.png", device = "png", dpi = "retina", scale = 1, bg = "white",
       width=320*.9, height =180*.9, units = "mm")
