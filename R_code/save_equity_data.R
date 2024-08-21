
alch_ret_daily <- Alchian_Data %>% 
  filter(!is.na(`DJIA`)) %>% 
  # calculate price returns
  mutate(ret_bc = 100*(`Beryllium Corp`/dplyr::lag(`Beryllium Corp`) -1),
         ret_lith = 100*(`Lithium Corp of America`/dplyr::lag(`Lithium Corp of America`) -1),
         ret_ASRC = 100*(`American Smelting & Refining Co.`/dplyr::lag(`American Smelting & Refining Co.`) -1),
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

wb <- createWorkbook("Alchian formatted")

addWorksheet(wb, "raw")
addWorksheet(wb, "Daily returns")
addWorksheet(wb, "Weekly returns")

writeData(wb, "raw", x = `Alchian_Data`)

alch_ret_daily <- alch_ret_daily %>% 
  select(Date, ret_bc:Rf) %>% 
  rename("Beryllium Corp" = ret_bc, "Lithium Corp of America" = ret_lith, "American Smelting & Refining Co." = ret_ASRC,  "Dow Jones Industrial Average" = ret_DJIA, "3 month T-bill (de-annualized)" = Rf)


writeData(wb, "Daily returns", x = `alch_ret_daily`)

alch_ret_weekly <- alch_ret_weekly %>% 
  select(Date, ret_bc:Rf) %>% 
  rename("Beryllium Corp" = ret_bc, "Lithium Corp of America" = ret_lith, "American Smelting & Refining Co." = ret_ASRC,"Metal Hydrides, Inc." = ret_MHI,  "Dow Jones Industrial Average" = ret_DJIA, "3 month T-bill (de-annualized)" = Rf)


writeData(wb, "Weekly returns", x = `alch_ret_weekly`)

saveWorkbook(wb, "Alchian formatted.xlsx", overwrite = TRUE)
