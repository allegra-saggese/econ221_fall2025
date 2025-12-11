# author: Allegra Saggese
# last updated: 11 Dec 2025
# purpose: final analysis - graphics only 

# execute start file with data, etc. 
source("00-startup.R")
source("01-prelim-data-analysis.R")



# -----------------------------------------------------------------------
###### ###### ###### ###### DESCRIPTIVE DATA PLOTS  ###### ###### ###### ###### 
# -----------------------------------------------------------------------


## PLOT FRED FED DATA ON CA TAXES (total, sales, income) 
ggplot() +
  geom_line(data = inc_tax_coll_df,
            aes(x = year, y = amount_mills_usd, color = "Income Tax"),
            size = 1) +
  geom_line(data = tax_coll_df,
            aes(x = year, y = amount_mills_usd, color = "Sales Tax"),
            size = 1) +
  geom_line(data = tax_q_df_collapse,
            aes(x = year, y = total_tax_year, color = "Total Tax"),
            size = 1) +
  labs(
    x = "Year",
    y = "Tax collected (millions USD)",
    color = "FRED FED series"
  ) +
  theme_minimal()


## simple FRED FED plot of sales tax over time 
ggplot(tax_q_df, aes(x = observation_date, y = total_tax_collected_CA)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Tax collected (US millions $)") +
  theme_minimal()



## plot CA GDP over time (pct change, and total - aggregate)
ggplot(gdp_df_v2, aes(x = year, y = gdp_nominal_dollars)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "GDP (millions USD)") +
  theme_minimal()



## plot PCE raw data over time 
ggplot(pce_df, aes(x = year, y = CAPCE)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "CA PCE (millions US dollars)") +
  theme_minimal()



## plot PCE and GDP over time 
ggplot() +
  
  # ---- PCE ----
geom_line(data = pce_df,
          aes(x = year, y = CAPCE, color = "PCE"),
          size = 1) +
  geom_point(data = pce_df,
             aes(x = year, y = CAPCE, color = "PCE"),
             size = 2) +
  
  # ---- GDP ----
geom_line(data = gdp_df_v2,
          aes(x = year, y = gdp_nominal_dollars, color = "GDP"),
          size = 1) +
  geom_point(data = gdp_df_v2,
             aes(x = year, y = gdp_nominal_dollars, color = "GDP"),
             size = 2) +
  
  # ---- Axes ----
scale_y_continuous(
  name = "PCE & GDP (millions USD)")
 # ---- Colors ----
scale_color_manual(values = c(
  "PCE" = "blue",
  "GDP" = "red",
  "Unemployment Rate" = "darkgreen"
)) +
  
  # ---- Labels ----
labs(
  x = "Year",
  color = "Series"
) +
  
  theme_minimal()

## PLOT - unemployment vs change in GDP / PCE 
scale_factor <- max(df_merge$diff_pce_sales, na.rm = TRUE) / 
  max(ur_yearly$urate_avg, na.rm = TRUE) # scale unemployment so it fits on left axis

ggplot(df_merge, aes(x = year, y = diff_pce_sales)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "purple", size = 2) +
  labs(
    x = "Year",
    y = "PCE – Sales Tax (millions USD)",
    title = "Difference Between PCE and Sales Tax"
  ) +
  theme_minimal()



## Plot PCE - sales tax (?) vs. average unemployment rate to see if the gap shrinks during downturns 
ggplot() +
  # ----- Difference Line (PCE – Sales Tax) -----
geom_line(data = df_merge,
          aes(x = year, y = diff_pce_sales, color = "PCE – Sales Tax"),
          size = 1.2) +
  geom_point(data = df_merge,
             aes(x = year, y = diff_pce_sales, color = "PCE – Sales Tax"),
             size = 2) +
  
  # ----- Unemployment Rate (scaled) -----
geom_line(data = ur_filtered,
          aes(x = year, y = urate_avg * scale_factor, color = "Avg Unemployment"),
          size = 1, linetype = "dashed") +
  
  # ----- Axes -----
scale_y_continuous(
  name = "PCE – Sales Tax (millions USD)",
  sec.axis = sec_axis(~ . / scale_factor,
                      name = "Unemployment Rate (%)")
) +
  
  # ----- Legend Colors -----
scale_color_manual(values = c(
  "PCE – Sales Tax" = "purple",
  "Avg Unemployment" = "darkgreen"
)) +
  
  labs(
    x = "Year",
    color = "Series",
    title = "PCE - collected sales vs. unemployment rate"
  ) +
  theme_minimal() # shows no trend between spending and unemployment - as proxied

# -----------------------------------------------------------------------
###### ###### ###### ###### PLOTS OF ESTIMATED VARIABLES  ###### ###### ###### ###### 
# -----------------------------------------------------------------------


## plot comparisons of tax revenue estimates (four sources - 2 actual, 2 estimates)
merged_long_v1 <- merged_long %>%
  filter(year >= 2009)

ggplot(merged_long_v1, aes(x = year, y = value / 1e6, color = source)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2009, 2023, 2), labels = as.character) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                     name = "Revenue (Millions of USD)") +
  scale_color_discrete(
    name = "",
    labels = c("Estimated Revenue (PCE)", "Estimated Revenue (Trans)", "Revenue (FRED)", "Revenue (CATFA)")
  ) +
  theme_minimal()

# add another filter 
nocovid_data <- merged_long_v1 %>%
  filter(year <= 2019)

ggplot(nocovid_data , aes(x = year, y = value / 1e6, color = source)) + # check the PCE inflation process - want to see if this is due to inflation 
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2009, 2023, 2), labels = as.character) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                     name = "Revenue (Millions of USD)") +
  scale_color_discrete(
    name = "",
    labels = c("Estimated Revenue (PCE)", "Estimated Revenue (Trans)", "Revenue (FRED)", "Revenue (CATFA)")
  ) +
  theme_minimal()


## maps of county level data 
ca_counties <- counties(state = "CA", cb = TRUE) %>%
  st_as_sf() %>%
  select(GEOID, NAME, geometry)

map_df <- ca_counties %>%
  left_join(
    merged_df %>% filter(year == 2015),
    by = "GEOID"
  )

ggplot(map_df) +
  geom_sf(aes(fill = cty_rate), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::comma,
    name = "Effective county tax rate (2015)"
  ) +
  labs(
    title = "California Taxable Sales by County",
    caption = "Source: CDTFA / ACS"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


ggplot(map_df) +
  geom_sf(aes(fill = est_tax_rev), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::comma,
    name = "Taxable Revenue (USD)"
  ) +
  labs(
    title = "California Taxable Sales by County",
    caption = "Source: CDTFA / ACS"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

## plot the final additional line - estimated state level CA PCE 
state_level_est_plot <- state_level_est %>%
  filter(year <= 2019)

ggplot(state_level_est_plot, aes(x = year)) +
  geom_line(aes(y = total_est_rev, color = "Aggregate state PCE"), linewidth = 1.1) +
  geom_line(aes(y = est_tax_rev, color = "ACS quintile PCE"), linewidth = 1.1) +

  labs(
    x = "Year",
    y = "Revenue (nominal USD)",
    color = "",
    title = "Divergent estimate methods: quintile vs. state-level approach"
  ) +
  theme_minimal()

## plot simple state-level CAPCE approach from FRED FED data 
# note this depends heavily on calculated incidence (aggregate) of what share of spending we think is taxable
ggplot(state_level_est, aes(x = year, y = total_est_rev)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Estimated tax revenue (USD)",
    title = "Revenue estimate (based on CAPCE and estimated tax rate)"
  ) +
  theme_minimal()

# plot difference (i.e. magnitudes)
state_level_est1 <- state_level_est %>%
  filter(year >= 2009)

ggplot(state_level_est1, aes(x = year, y = (taxable_trans_total - revenue_FRED_source) / 1e6)) +
  geom_line(color = "#D55E00", linewidth = 1.1) +
  geom_point(color = "#D55E00", size = 2) +
  labs(
    x = "Year",
    y = "Difference (Millions of USD)",
    title = "Total taxable transaction - Revenue collection"
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_continuous(breaks = seq(2009, 2023, 2), labels = as.character) +
  theme_minimal()







