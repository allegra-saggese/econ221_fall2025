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
    color = "FRED FED series",
    title = "California tax collection by source"
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
  name = "Aggregate measure (millions USD)") + 
 # ---- Colors ----
scale_color_manual(values = c(
  "PCE" = "purple",
  "GDP" = "hotpink",
  "Unemployment Rate" = "darkgreen"
)) +
  
  # ---- Labels ----
labs(
  x = "Year",
  color = "Series",
  title = "Descriptive Comparison: GDP vs. Aggregate PCE"
) +
  theme_minimal()
  

## PLOT - unemployment vs change in GDP / PCE 
scale_factor <- max(df_merge$diff_pce_sales, na.rm = TRUE) / 
  max(ur_yearly$urate_avg, na.rm = TRUE) + # scale unemployment so it fits on left axis

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
    labels = str_wrap(c("Estimated (county level PCE)", "Estimate (Transaction approach)",
                        "Observed Revenue", "Observed Revenue (select consumer sectors)"), width = 15)
  ) +
  theme(legend.key.width = unit(1, "cm"),
        legend.spacing.y = unit(0.7, "cm"),
        legend.key.height = unit(1, "cm")) +
  labs(title = "Comparison of revenue estimates") +
        
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

# est tax spend
ggplot(map_df) +
  geom_sf(aes(fill = est_taxable_spend), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::comma,
    name = "Estimated tax base"
  ) +
  labs(
    title = "Taxable spend (2023)",
    caption = "Source: CDTFA / ACS"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

map_df <- ca_counties %>%
  left_join(
    merged_df %>% filter(year == 2023),
    by = "GEOID"
  )

# for PCE 
ca_mappce <- map_df %>%
  mutate(est_PCE_cty_agg_m = est_PCE_cty_agg / 1000000)

ggplot(ca_mappce) +
  geom_sf(aes(fill = est_PCE_cty_agg_m), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::comma,
    name = "County PCE (millions USD)"
  ) +
  labs(
    title = "Quintile-based personal consumption expenditure (2023)",
    caption = "Source: CDTFA / ACS"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

map_df <- ca_counties %>%
  left_join(
    merged_df %>% filter(year == 2023),
    by = "GEOID"
  )


## effective tax rate
ggplot(map_df) +
  geom_sf(aes(fill = cty_rate), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::comma,
    name = "Effective county tax rate (2023)"
  ) +
  labs(
    title = "California Taxable Sales by County",
    caption = "Source: CDTFA / ACS"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# median household income 
ggplot(map_df) +
  geom_sf(aes(fill = estimate), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::comma,
    name = "Median come (USD)"
  ) +
  labs(
    title = "County-level median household income (2023)",
    caption = "Source: Census ACS, 1-year and 5-year"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# est-tax-revenue
map_df_rev <- map_df %>%
  mutate(est_tax_rev = est_tax_rev/1000000)

ggplot(map_df_rev) +
  geom_sf(aes(fill = est_tax_rev), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::comma,
    name = "Tax revenue (millions USD)"
  ) +
  labs(
    title = "County-level tax revenue estimate (2023)",
    caption = "Source: Census ACS, FRED FED, CAFTD"
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
ggplot(state_level_est, aes(x = year, y = est_tax_rev)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Estimated tax revenue (USD)",
    title = "Revenue estimate (based on CAPCE and estimated tax rate)"
  ) +
  theme_minimal()

# plot difference (i.e. magnitudes) between actually recorded transaction and federal estimated taxes 
state_level_est1 <- state_level_est %>%
  filter(year >= 2009)

# create the shades for the recessionary periods 
state_level_est1$year <- as.numeric(state_level_est1$year)
# define shading periods explicitly for recessions - can use later in graphs 
shades <- data.frame(
  xmin = c(2009, 2020),
  xmax = c(2010, 2022),
  ymin = -Inf,
  ymax = Inf
)

# PLOT the differences 
ggplot() +
  # shaded blocks first
  geom_rect(
    data = shades,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey80", alpha = 0.4
  ) +
  # main data line + points
  geom_line(
    data = state_level_est1,
    aes(x = year, y = (est_tax_rev - revenue_FRED_source) / 1e6),
    color = "#D55E00", linewidth = 1.1
  ) +
  geom_point(
    data = state_level_est1,
    aes(x = year, y = (est_tax_rev - revenue_FRED_source) / 1e6),
    color = "#D55E00", size = 2
  ) +
  geom_hline(yintercept = 0, color = "lavender", linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_continuous(breaks = seq(2009, 2023, 2)) +
  labs(
    x = "Year",
    y = "Difference (millions USD)",
    title = "Estimated CPE-based revenue - actual revenue collection"
  ) +
  theme_minimal()


# PLOT for difference in millions 
# base plot
pce_max  <- max(merged_yearly$pce_estimate, na.rm = TRUE)
rate_max <- max(merged_yearly$cty_rate, na.rm = TRUE)

shadesx <- data.frame(
  xmin = c(2008, 2020),
  xmax = c(2010, 2021),
  ymin = -Inf,
  ymax = Inf
)

ggplot() +
  # shaded background blocks
  geom_rect(
    data = shadesx,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey90", alpha = 0.4
  ) +
  # PCE line (left axis)
  geom_line(
    data = merged_yearly,
    aes(x = year, y = pce_estimate, color = "pce estimate"),
    linewidth = 1.1
  ) +
  # effective rate line (right axis, rescaled)
  geom_line(
    data = merged_yearly,
    aes(x = year, y = (cty_rate / rate_max) * pce_max, color = "effective rate"),
    linewidth = 1.1
  ) +
  scale_y_continuous(
    name = "pce estimate",
    sec.axis = sec_axis(~ . / pce_max * rate_max, name = "effective rate")
  ) +
  scale_color_manual(
    values = c("pce estimate" = "hotpink", "effective rate" = "royalblue"),
    name = "series"
  ) +
  labs(
    x = "year",
    title = "Taxable share of PCE vs. Effective sales tax rate "
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y.left  = element_text(color = "hotpink"),
    axis.title.y.right = element_text(color = "royalblue")
  )


## plot gamma estimates 
state_level_est_filtered <- state_level_est %>%
  filter(year >= 2008) %>%
  filter(year <= 2019)
  
gamma_long <-state_level_est_filtered %>%
  select(year, gamma_t, gamma_tot_trans) %>%
  pivot_longer(cols = starts_with("gamma"),
               names_to = "version",
               values_to = "gamma")

# plot with line at 100% for full compliance
ggplot(gamma_long, aes(x = year, y = gamma, color = version)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.8) +  # 100% reference line
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = unique(gamma_long$year),
                     labels = as.character(unique(gamma_long$year))) +
  labs(
    title = "Sales Tax Compliance Rate",
    x = "Year",
    y = "Compliance Rate",
    color = expression("Estimated " * gamma)
  ) +
  theme_minimal()

### plot the unemployment rate against other vals 
df_norm <-  bigdf %>%
  mutate(
    capce_n = as.numeric(scale(percap_taxable_total)),
    gpd_n = as.numeric(scale(gdp_nominal_dollars)),
    gva_n = as.numeric(scale(gva_t)),
    gamma_trans_n = as.numeric(scale(gamma_tot_trans)), 
    gamma_n = as.numeric(scale(gamma_t))
  )


df_norm_long <- pivot_longer(
  df_norm,
  cols = c(capce_n, gpd_n, gva_n, gamma_trans_n, gamma_n),
  names_to = "series",
  values_to = "value"
)

df_norm2 <- bigdf %>%
  mutate(
    capce_n = (percap_taxable_total - min(percap_taxable_total, na.rm = TRUE)) /
      (max(percap_taxable_total, na.rm = TRUE) - min(percap_taxable_total, na.rm = TRUE)),
    
    gpd_n   = (gdp_nominal_dollars - min(gdp_nominal_dollars, na.rm = TRUE)) /
      (max(gdp_nominal_dollars, na.rm = TRUE) - min(gdp_nominal_dollars, na.rm = TRUE)),
    
    gva_n   = (gva_t - min(gva_t, na.rm = TRUE)) /
      (max(gva_t, na.rm = TRUE) - min(gva_t, na.rm = TRUE)),
    
    gamma_trans_n =
      (gamma_tot_trans - min(gamma_tot_trans, na.rm = TRUE)) /
      (max(gamma_tot_trans, na.rm = TRUE) - min(gamma_tot_trans, na.rm = TRUE)),
    
    gamma_n =
      (gamma_t - min(gamma_t, na.rm = TRUE)) /
      (max(gamma_t, na.rm = TRUE) - min(gamma_t, na.rm = TRUE)),
    
    urate_n =
      (urate_avg - min(urate_avg, na.rm = TRUE)) /
      (max(urate_avg, na.rm = TRUE) - min(urate_avg, na.rm = TRUE))
  )

df_norm_long2 <- df_norm2 %>%
  pivot_longer(
    cols = c(capce_n, gpd_n, gva_n, gamma_n, urate_n),
    names_to = "series",
    values_to = "value"
  )

# RAW PLOT - normalized outcome vars 
ggplot(df_norm_long2, aes(x = year, y = value, color = series)) +
  geom_line(size = 1) +
  labs(
    x = "Year",
    y = "Normalized value (min–max)",
    color = ""
  ) +
  theme_minimal()


# VARPLOT w/ recessions and emphasis on GAMMA and UNEMP RATE
ggplot(df_norm_long2, aes(x = year, y = value)) +
  
  # recession / shock bands
  annotate(
    "rect",
    xmin = 2008, xmax = 2010,
    ymin = -Inf, ymax = Inf,
    fill = "grey90", alpha = 0.25
  ) +
  annotate(
    "rect",
    xmin = 2020, xmax = 2022,
    ymin = -Inf, ymax = Inf,
    fill = "grey90", alpha = 0.25
  ) +
  
  # time series
  geom_line(
    aes(
      color = series,
      linetype = series,
      size = series
    ),
    na.rm = TRUE
  ) +
  
  # linetypes: default solid, urate dotted
  scale_linetype_manual(
    values = c(
      "capce_n" = "solid",
      "gpd_n"   = "solid",
      "gva_n"   = "solid",
      "gamma_n" = "solid",
      "urate_n" = "dotted"
    ),
    guide = "none"
  ) +
  
  # sizes: default 1, gamma emphasized, urate slightly thicker
  scale_size_manual(
    values = c(
      "capce_n" = 1,
      "gpd_n"   = 1,
      "gva_n"   = 1,
      "gamma_n" = 2.2,
      "urate_n" = 1.4
    ),
    guide = "none"
  ) +
  
  labs(
    x = "Year",
    y = "Normalized value (min–max)",
    color = ""
  ) +
  theme_minimal()