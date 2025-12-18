# author: Allegra Saggese
# last updated: 04 Dec 2025
# purpose: final analysis for macro measurement 

library(tigris)
library(sf)

owd <- getwd()
# if current folder is not 'code_data', move into it
if (basename(owd) != "code_data") {
  target <- file.path(owd, "code_data")
  if (!dir.exists(target)) stop("Missing 'code_data' inside: ", owd)
  setwd(target)
}

# execute start file with data, etc. 
source("00-startup.R")
# inspect data sets 
names(datasets)

# -----------------------------------------------------------------------
###### ###### ###### ######  PRE-ANALYSIS: SUFFICIENT DATA UPLOAD   ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# data review --- check to ensure sheets all loaded - note manual edits made to the data 
stopifnot(is.list(datasets), length(names(datasets)) == length(datasets))

is_intlike <- function(x) is.numeric(x) && all(is.na(x) | x == floor(x))
likely_year_colnames <- function(nm) grepl("(^|_)(year|yr)(_|$)", nm, ignore.case = TRUE)

# scan for relevant col names
scan_year_cols <- function(df) {
  by_name <- names(df)[likely_year_colnames(names(df))]
  by_vals <- names(df)[sapply(df, function(x)
    is_intlike(x) && mean(x >= 1900 & x <= 2100, na.rm = TRUE) > 0.8)]
  unique(c(by_name, by_vals))
}

# try to ID relevant cols across cols - although we're going to use a manual approach
key_candidates <- function(df) {
  keys <- c("year","Year","YR","FIPS","STATEFP","STATE","state",
            "county","County","county_name","NAICS","naics",
            "zip","ZIP","ZIPCODE","zip_code","geoid","GEOID")
  intersect(keys, names(df))
}

# function to review cols quickly and their names 
peek_columns <- function(df, n_show = 40) {
  cls <- sapply(df, function(x) paste(class(x), collapse = "|"))
  nunq <- sapply(df, function(x) length(unique(x[!is.na(x)])))
  nna  <- sapply(df, function(x) sum(is.na(x)))
  ex   <- sapply(df, function(x) {
    v <- unique(x[!is.na(x)])
    paste(utils::head(v, 3), collapse = " | ")
  })
  out <- data.frame(column = names(df),
                    class = unname(cls),
                    n_unique = unname(nunq),
                    n_na = unname(nna),
                    example = unname(ex),
                    stringsAsFactors = FALSE)
  out[seq_len(min(nrow(out), n_show)), , drop = FALSE]
}

# check years on data 
year_ranges <- function(df, ycols) {
  if (!length(ycols)) return(NULL)
  do.call(rbind, lapply(ycols, function(yc) {
    v <- as.integer(df[[yc]])
    v <- v[!is.na(v)]
    if (!length(v)) return(NULL)
    data.frame(column = yc, min_year = min(v), max_year = max(v))
  }))
}

small_cats <- function(df, max_levels = 30) {
  idx <- which(sapply(df, function(x) is.character(x) || is.factor(x)))
  idx <- idx[sapply(df[idx], function(x) length(unique(x[!is.na(x)])) <= max_levels)]
  if (!length(idx)) return(NULL)
  out <- lapply(names(idx), function(nm) {
    vals <- sort(unique(df[[nm]][!is.na(df[[nm]])]))
    data.frame(column = nm, level = vals, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# big review of datasets 
explore_dataset <- function(nm, df, show_rows = 3) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("DATASET: ", nm, "\n", sep = "")
  cat("dim: ", nrow(df), " x ", ncol(df), "\n", sep = "")
  cat("colnames[1:10]: ", paste(utils::head(colnames(df), 10), collapse = ", "), "\n", sep = "")
  
  keys <- key_candidates(df)
  if (length(keys)) cat("key candidates: ", paste(keys, collapse = ", "), "\n", sep = "")
  
  ycols <- scan_year_cols(df)
  if (length(ycols)) {
    yr <- year_ranges(df, ycols)
    if (!is.null(yr)) {
      cat("year ranges:\n")
      print(yr, row.names = FALSE)
    }
  }
  
  cat("\ncolumn summary (first 40):\n")
  print(peek_columns(df, n_show = 40), row.names = FALSE)
  
  cat("\nhead():\n")
  suppressWarnings(print(utils::head(df, show_rows)))
  
  cats <- small_cats(df, max_levels = 15)
  if (!is.null(cats)) {
    cat("\nsmall categorical previews (<=15 levels):\n")
    print(cats, row.names = FALSE)
  }
}

# run on all datasets -- load in the datasets to review ranges of years 
invisible(lapply(names(datasets), function(nm) explore_dataset(nm, datasets[[nm]])))

# consolidate the dictionaries 
make_dictionary <- function(datasets) {
  do.call(rbind, lapply(names(datasets), function(nm) {
    df <- datasets[[nm]]
    data.frame(
      dataset = nm,
      column = names(df),
      class = sapply(df, function(x) paste(class(x), collapse="|")),
      n = nrow(df),
      n_na = sapply(df, function(x) sum(is.na(x))),
      n_unique = sapply(df, function(x) length(unique(x[!is.na(x)]))),
      stringsAsFactors = FALSE
    )
  }))
}

# browse universe of variables --- although its not so standard - this doesn't work 
dict <- make_dictionary(datasets)
dict$na_pct <- round(100 * dict$n_na / pmax(dict$n, 1), 2)
utils::write.csv(dict, "00_column_dictionary.csv", row.names = FALSE)

#  save year coverage
year_cov <- do.call(rbind, lapply(names(datasets), function(nm) {
  df <- datasets[[nm]]
  ycols <- scan_year_cols(df)
  yr <- year_ranges(df, ycols)
  if (is.null(yr)) return(NULL)
  cbind(data.frame(dataset = nm, stringsAsFactors = FALSE), yr)
}))
if (!is.null(year_cov)) utils::write.csv(year_cov, "00_year_ranges.csv", row.names = FALSE)



# -----------------------------------------------------------------------
###### ###### ###### ######  DATA CLEANING  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# DF 1a ------ TAX COLLECTION DATA (TOTAL) FROM FRED FED (including income and corporate tax)
nm <- "2023-FRED-CA-tax-collected"  
tax_q_df <- datasets[[nm]]

# convert to date format so we parse out month, yr, and fiscal quarter
tax_q_df$observation_date <- as.Date(tax_q_df$observation_date, format = "%m/%d/%y")
d <- tax_q_df$observation_date

# calendar month and year
m <- ifelse(is.na(d), NA_integer_, as.integer(format(d, "%m")))
y <- ifelse(is.na(d), NA_integer_, as.integer(format(d, "%Y")))
q <- ifelse(is.na(d), NA_character_, paste0("Q", ((m - 1L - 6L) %% 12L) %/% 3L + 1L))
fy <- ifelse(is.na(d), NA_integer_, ifelse(m >= 7L, y + 1L, y))

# attach columns back
tax_q_df$month          <- m
tax_q_df$year           <- y
tax_q_df$fiscal_quarter <- q
tax_q_df$fiscal_year    <- fy

# save back into the list
datasets[[nm]] <- tax_q_df

# quick check
str(datasets[[nm]][, c("observation_date","month","year","fiscal_quarter","fiscal_year")])
head(datasets[[nm]][, c("observation_date","month","year","fiscal_quarter","fiscal_year")])

tax_q_df_collapse <- tax_q_df %>%
  group_by(year) %>%
  summarise(total_tax_year = sum(total_tax_collected_CA, na.rm = TRUE))



# DF 1b.1 ----- CA total sales and use tax collected (FRED FED)
collected_tax <- "CA-FED-taxable_sales_collected"
tax_coll_df <- datasets[[collected_tax]]
names(tax_coll_df) <- tolower(names(tax_coll_df))
tax_coll_df$amount_mills_usd <- tax_coll_df$amount_thousands_usd/1000 # create mil col for comparison
tax_coll_df <- tax_coll_df %>%
  mutate(revenue_FRED_source = amount_mills_usd * 1e6)

# DF 1b.2 ----- CA total sales and use tax collected (CA tax revenue board)
CA_collect <- "SUTStateCollNoPermits"
ca_sales_tax_survey <- datasets[[CA_collect]]

ca_salestax_survey_clean <- ca_sales_tax_survey %>%
  {names(.) <- as.character(unlist(.[1,])); .[-1,]} %>%
  rename(year = `Fiscal Year To`) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year >= 2005) %>%
  select(year, tax_collected = `Collections: Taxes`)


# DF 1c ----- CA aggregate income tax 
collected_inc_tax <- "CAINCTAX"
inc_tax_coll_df <- datasets[[collected_inc_tax]]
names(inc_tax_coll_df) <- tolower(names(inc_tax_coll_df)) 

# convert to date format so we parse out month, yr, and fiscal quarter
inc_tax_coll_df$observation_date <- as.Date(inc_tax_coll_df$observation_date, format = "%m/%d/%y")
d <- inc_tax_coll_df$observation_date

# calendar month and year
m <- ifelse(is.na(d), NA_integer_, as.integer(format(d, "%m")))
y <- ifelse(is.na(d), NA_integer_, as.integer(format(d, "%Y")))
q <- ifelse(is.na(d), NA_character_, paste0("Q", ((m - 1L - 6L) %% 12L) %/% 3L + 1L))
fy <- ifelse(is.na(d), NA_integer_, ifelse(m >= 7L, y + 1L, y))

# attach columns back
inc_tax_coll_df$month          <- m
inc_tax_coll_df$year           <- y
inc_tax_coll_df$fiscal_quarter <- q
inc_tax_coll_df$fiscal_year    <- fy

# save back into the list
datasets[[collected_inc_tax]] <- inc_tax_coll_df

# quick check
str(datasets[[collected_inc_tax]][, c("observation_date","month","year","fiscal_quarter","fiscal_year")])
head(datasets[[collected_inc_tax]][, c("observation_date","month","year","fiscal_quarter","fiscal_year")])

inc_tax_coll_df <- inc_tax_coll_df[-(1:58), ] # start in 2000 (earlier years not useful)
inc_tax_coll_df$amount_mills_usd <- inc_tax_coll_df$ca_income_tax_thousands_of_dollars/1000 # create mil col for comparison


# DF 1d ----- UR unemployment rate in CA for biz cycle graph
UR <- "CAUR"
ur_df <- datasets[[UR]]
names(ur_df) <- tolower(names(ur_df))

ur_df$date <- as.Date(ur_df$observation_date, format = "%m/%d/%y")
ur_df$year <- format(ur_df$date, "%Y")

# create annual average for exploratory stats 
ur_yearly <- ur_df %>%
  group_by(year) %>%
  summarise(urate_avg = mean(caur, na.rm = TRUE))

ur_yearly$year <- as.numeric(ur_yearly$year) # make numeric for plot 



# DF 2 ------ AGGREGATE GDP DATA 
gdp <- "2024-CA-GDP-TOTAL"
gdp_df <- datasets[[gdp]]

# make numeric 
gdp_df$year <- as.numeric(gdp_df$year)

# drop NA cols in year
gdp_df_v2 <- gdp_df[!is.na(gdp_df$year), ]




# DF 3a ------ Personal Consumption Expenditure (PCE) for CA

pce <- "CA-PCE-annual-FRED-FED" # total for CA (not per capita)
pce_df <- datasets[[pce]]

pce_df$observation_date <- as.Date(pce_df$observation_date, format = "%Y-%d-%m")
e <- pce_df$observation_date

# calendar month and year
m <- ifelse(is.na(e), NA_integer_, as.integer(format(e, "%m"))) # STILL NOT WORKING 
y <- ifelse(is.na(e), NA_integer_, as.integer(format(e, "%Y")))
q <- ifelse(is.na(e), NA_character_, paste0("Q", ((m - 1L - 6L) %% 12L) %/% 3L + 1L))
fy <- ifelse(is.na(e), NA_integer_, ifelse(m >= 7L, y + 1L, y))

# attach columns back
pce_df$month          <- m # assumption - will ignore as we go aggregate
pce_df$year           <- y
pce_df$fiscal_quarter <- q
pce_df$fiscal_year    <- fy

# save back into the list
datasets[[pce]] <- pce_df
class(pce_df$CAPCE)



# DF 3b ------ Personal Consumption Expenditure - Sales tax (novel variable)
df_merge <- merge(
  inc_tax_coll_df |> dplyr::rename(pce = amount_mills_usd),
  tax_coll_df     |> dplyr::rename(salestax = amount_mills_usd),
  by = "year"
)
df_merge$diff_pce_sales <- df_merge$pce - df_merge$salestax # spending less sales tax - saved in a line

# restrict sample for unemployment data to match the merged data
ur_filtered <- ur_yearly %>% 
  dplyr::filter(year %in% df_merge$year)



# DF 4a  ------ LOCAL (COUNTY) TAX RATES IN CA 

sales_tax_rates <- "SalesTaxRates"
sales_tax_rates <- datasets[[sales_tax_rates]]
colnames(sales_tax_rates) <- as.character(unlist(sales_tax_rates[1, ]))
sales_tax_rates <- sales_tax_rates[-1, ]
names(sales_tax_rates) <- tolower(names(sales_tax_rates))

sales_tax_rates <- sales_tax_rates %>%
  mutate(
    rate = as.numeric(rate),
    rate = round(rate, 3)
  )

county_missing_later_QA <- c("Alpine", "Amador", "Calaveras", "Colusa", "Del Norte",
                             "Glenn", "Inyo", "Lassen", "Mariposa", "Modoc", "Mono", 
                             "Plumas", "Sierra", "Siskiyou", "Trinity", "Tuolumne")

#setdiff(county_missing_later_QA, unique(sales_tax_rates$county))

sales_tax_avg <- sales_tax_rates %>%
  group_by(county) %>%
  summarise(avg_rate = mean(rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = 2025)

#setdiff(county_missing_later_QA, unique(sales_tax_avg$county))


state_avg_2025 <- mean(sales_tax_avg$avg_rate, na.rm = TRUE) # get effective rate for 2025
# create county adjusters to apply to historical data 
county_adjusters <- sales_tax_avg %>%
  mutate(adj_factor = avg_rate / state_avg_2025) %>%
  select(county, adj_factor)

# take CA average for the previous years 
sales_tax_hist <- data.frame(
  year = 2005:2024,
  eff_rate = NA_real_
)

## taken from https://cdtfa.ca.gov/taxes-and-fees/sales-use-tax-rates-history.htm#note (manual data input)
sales_tax_hist$eff_rate <- c(
  .0725, .0725, .0725, .0725, .0725, .0725, .0725, .0725,
  .0750, .0750, .0750, .0750, .0725, .0725, .0825, .0825, 
  .0725, .0725, .0725, .0725
)

# create estimated county-level weights based on current differences in county-level taxation (given data limitations)
sales_tax_avg_full <- county_adjusters %>%
  crossing(year = 2005:2025) %>%
  left_join(sales_tax_hist, by = "year") %>%
  mutate(
    avg_rate = ifelse(year == 2025, adj_factor * state_avg_2025, eff_rate * adj_factor)
  ) %>%
  select(county, year, avg_rate) %>%
  arrange(county, year)

#setdiff(county_missing_later_QA, unique(sales_tax_avg_full$county))

# now merge in eff_rate for 2005–2024
sales_tax_avg_full <- left_join(sales_tax_avg_full, sales_tax_hist, by = "year")
head(sales_tax_avg_full)

# combine cols 
sales_tax_avg_full <- sales_tax_avg_full %>%
  mutate(eff_rate = if_else(is.na(eff_rate), avg_rate, eff_rate)) %>% 
  rename(cty_rate = avg_rate)


## DF 4b - QUINTILE LEVEL TAX INCIDENCE OVER CONSUMPTION BUNDLES
# from BLS data - estimate quintile tax incidence over bundles
incidence_avg <- c(
  q1 = 0.293,
  q2 = 0.180,
  q3 = 0.200,
  q4 = 0.259,
  q5 = 0.342
)


# DF 5 ------- MEDIAN INCOME OVER TIME 
years <- setdiff(2005:2023, 2020)   # skip 2020

# NOTE THE API-CALLED DATA IS NOT IN DATAFRAMES -- need to just load in here (could move to separate data script in future for clean-ish look at manual / API data uploads )
acs_income <- map_dfr(years, ~
                        get_acs(
                          geography = "county",
                          state = "CA",
                          variables = "B19013_001",
                          year = .x,
                          survey = "acs1"
                        ) %>%
                        mutate(source = "ACS", year = .x)
)

acs_income_5yr <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "CA",
  year = 2023,        # or any year you want
  survey = "acs5"
)

acs_income_5yr <- acs_income_5yr%>%
  mutate(year = 2023)

acs_x1 <- acs_income_5yr %>%
  rowwise() %>%
  mutate(year_full = list((year - 4):year)) %>%
  unnest(year_full) %>%
  rename(acs_release = year, year = year_full)

acs_income_5yr_2 <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "CA",
  year = 2018,        # or any year you want
  survey = "acs5"
)

acs_income_5yr_2 <- acs_income_5yr_2 %>%
  mutate(year = 2018)

acs_x2 <- acs_income_5yr_2 %>%
  rowwise() %>%
  mutate(year_full = list((year - 4):year)) %>%
  unnest(year_full) %>%
  rename(acs_release = year, year = year_full)

acs_income_5yr_3 <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "CA",
  year = 2013,        # or any year you want
  survey = "acs5"
)

acs_income_5yr_3 <- acs_income_5yr_3 %>%
  mutate(year = 2013)

acs_x3 <- acs_income_5yr_3 %>%
  rowwise() %>%
  mutate(year_full = list((year - 4):year)) %>%
  unnest(year_full) %>%
  rename(acs_release = year, year = year_full)

acs_income_5yr_4 <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "CA",
  year = 2009,        # or any year you want
  survey = "acs5"
)

acs_income_5yr_4 <- acs_income_5yr_4 %>%
  mutate(year = 2009)

acs_x4 <- acs_income_5yr_4 %>%
  rowwise() %>%
  mutate(year_full = list((year - 4):year)) %>%
  unnest(year_full) %>%
  rename(acs_release = year, year = year_full)

# row bind the five year survey - this is to get data on the dropped counties
acs_5yr_all <- bind_rows(
  acs_x1,
  acs_x2,
  acs_x3,
  acs_x4
)


add_quintiles <- function(df, value_col = "estimate", year_col = "year") {
  df %>%
    group_by(.data[[year_col]]) %>%
    mutate(quintile = ntile(.data[[value_col]], 5)) %>%
    ungroup()
}

acs_income_q <- add_quintiles(acs_income)
acs_5yr_all_q <-add_quintiles(acs_5yr_all)

# remove California and any other padding for match
acs_income_q <- acs_income_q %>%
  mutate(
    county = trimws(sub(", California$", "", NAME))
  )

acs_income_q <- acs_income_q %>%
  mutate(
    county_short = trimws(sub(" County$", "", county))
  )

acs_5yr_all_q <- acs_5yr_all_q %>%
  mutate(
    county = trimws(sub(", California$", "", NAME))
  )

acs_5yr_all_q <- acs_5yr_all_q %>%
  mutate(
    county_short = trimws(sub(" County$", "", county))
  )


# check for missing counties 
setdiff(county_missing_later_QA, unique(acs_income_q$county_short)) #ACS is missing these data 
setdiff(county_missing_later_QA, unique(acs_5yr_all_q$county_short))

toadd_ctys <- setdiff(unique(acs_5yr_all_q$GEOID), unique(acs_income_q$GEOID)) # find where pop is too small for year 1 survey 
acs_5yr_missing <- acs_5yr_all_q %>%
  filter(GEOID %in% toadd_ctys)

acs_5yr_covid <- acs_5yr_all_q %>% # find where covid data doesn't exist in year 1 survey 
  filter(year == 2020)

acs_income_q <- bind_rows(acs_income_q, acs_5yr_missing, acs_5yr_covid)


# DF 6 ------- BLS CE by QUINTILE for CA 
path_bls <- "BLS_CES_tables_per_yr"
bls_df <- datasets[[path_bls]]

bls_df <- bls_df %>%
  mutate(year = ...1,
         year = as.numeric(year)) %>% 
  select(-1) %>%
  mutate(year = if_else(is.na(year), 2023, year))
  

# take growth rate from CA FED of PCE to interpolate missing years (2005-2017)
pce_growth <- pce_df %>%
  arrange(year) %>%
  mutate(pce_growth = CAPCE / lag(CAPCE))

# backfill
pce_g <- pce_growth %>% filter(year < 2017) %>% arrange(desc(year))
pce_g <- pce_g[order(pce_g$year), ]

# for each quintile, backcast CE
years_to_fill <- 2005:2016
base_vals <- as.numeric(bls_df[bls_df$year == 2017, c("q1","q2","q3","q4","q5")])
g <- pce_g$pce_growth[pce_g$year %in% years_to_fill]
cum_factor <- rev(cumprod(rev(g)))   # inverse growth

backcast <- data.frame(
  year = years_to_fill,
  q1 = base_vals[1] / cum_factor,
  q2 = base_vals[2] / cum_factor,
  q3 = base_vals[3] / cum_factor,
  q4 = base_vals[4] / cum_factor,
  q5 = base_vals[5] / cum_factor
)

bls_df_full <- bind_rows(backcast, bls_df) # complete backcast of share of spending that is taxable

# merge in with the ACS data on quintiles
acs_income_q2 <- acs_income_q %>%
  mutate(quintile = paste0("q", quintile)) %>%      
  left_join(
    bls_df_full %>%
      select(year, q1, q2, q3, q4, q5) %>%
      tidyr::pivot_longer(
        cols = starts_with("q"),
        names_to = "quintile",
        values_to = "pce_estimate"
      ),
    by = c("year", "quintile")
  )


# DF 6 --------- POPULATION ESTIMATES 

# agg data 
pop <- "CAPOP"
capop <- datasets[[pop]]
capop <- capop  %>%
  rename(pop = CAPOP) %>%          
  mutate(pop = tolower(pop),       
         pop = as.numeric(pop) * 1000,
         year = as.integer(format(as.Date(observation_date), "%Y")))  # convert values to numeric and multiply by 1000

# county level data
countypop <- "CA-county-pop-estimates-final"
cty_pop <- datasets[[countypop]]
names(cty_pop)[2:ncol(cty_pop)] <- paste0("yr_", 2000:2025)
names(cty_pop) <- tolower(names(cty_pop))

cty_pop_long <- cty_pop %>% # pivot longer 
  pivot_longer(
    cols = starts_with("yr_"),
    names_to = "year",
    values_to = "pop_est"
  ) %>%
  mutate(year = as.integer(sub("yr_", "", year)))

# DF 7 --------- ACTUAL TAX RECEIPTS (CA) REPORTED TRANSACTIONS - TRANSACTIONS
trans <- "CA-taxable-sales-base"
taxable_trans <- datasets[[trans]]

taxable_trans_annual <- taxable_trans %>%
  {names(.) <- as.character(unlist(.[1,])); .[-1,]} %>%
  filter(Quarter == "A") %>%
  mutate(
    `Taxable Transactions Amount` = as.numeric(gsub(",", "", `Taxable Transactions Amount`)),
    `Current Year Per Capita Taxable Transactions Amount` = as.numeric(gsub(",", "", `Current Year Per Capita Taxable Transactions Amount`)),
    `Calendar Year` = as.numeric(`Calendar Year`)
  )

# manually drop categories where its wholesale / firm-to-firm (dealers) \ totals so is not really consumption spend
todrop <- c("Educational Services", "Construction", "Administrative and Support and Waste Management and Remediation Services",
            "Building Material and Supplies Dealers", "Finance and Insurance", "Health Care and Social Assistance",
            "Information", "Management of Companies and Enterprises", "Manufacturing", "Mining, Quarrying, and Oil and Gas Extraction",
            "Motor Vehicle and Parts Dealers", "New Car Dealers", "Other Motor Vehicle Dealers", "Professional, Scientific, and Technical Services",
            "Public Administration", "Real Estate and Rental and Leasing", "Transportation and Warehousing",
            "Used Car Dealers", "Wholesale Trade", "Utilities", "Total Retail and Food Services",
            "Total All Outlets", "Total All Other Outlets", "Supermarkets and Other Grocery Stores")

taxable_trans_annual <- taxable_trans_annual %>%
  filter(!`Business Type` %in% todrop)%>%
  group_by(`Calendar Year`) %>%
  summarise(
    taxable_trans_total = sum(`Taxable Transactions Amount`, na.rm = TRUE),
    percap_taxable_total = sum(`Current Year Per Capita Taxable Transactions Amount`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(year = `Calendar Year`)

# DF 8a --------- CALIFORNIA LEVEL PRODUCTION
# upload GVA (GSP) data - note the nuance is necessary here 
naics <- "2025-BEA-GDP-CA-by-NAICS"
sagdp2_df <- datasets[[naics]]
names(sagdp2_df) <- names(sagdp2_df) |> tolower() |> gsub(" ", "_", x = _)

gva_ca <- sagdp2_df %>%
  filter(description == "All industry total") %>%
  select(where(is.numeric) | starts_with("x")) %>%  # keep numeric year columns
  pivot_longer(
    cols = everything(),
    names_to = "year",
    values_to = "gva_t"
  ) %>%
  mutate(
    year = as.numeric(gsub("[^0-9]", "", year)),
    gva_t = as.numeric(gva_t)
  ) %>%
  arrange(year)

head(gva_ca) # here calling in GVA but it should be equivalent to GDP figures 

# DF 8b --------- CALIFORNIA LEVEL EXPORTS / IMPORTS
exps <- "CA_Exports_Imports-onetable"
exp_imp_df <- datasets[[exps]]

share_cons <- "NSAIMP-census-share-consumption-imports"
cons_share_imports <- datasets[[share_cons]]
cons_share_imports <- cons_share_imports %>% 
  rename(year = Year)


# -----------------------------------------------------------------------
###### ###### ###### ######  DATA MERGING  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# create GDP level data frame for use in EVADE measure
df_list <- list(cons_share_imports, exp_imp_df, gdp_df, gva_ca)
prod_df <- reduce(df_list, left_join, by = "year")
prod_df <- left_join(prod_df, ur_yearly, by = "year")

# combine ACS quintile data w/ effective tax rates + estimate for tax incidence on PCE 
acs_income_q3 <- acs_income_q2 %>%
  mutate(county = str_remove(county, " County$"),
         county = str_trim(county))

# sales tax (eff rate and estimated county level rate) in with the quintile<>county data
merged_df <- acs_income_q3 %>%
  left_join(sales_tax_avg_full, by = c("county", "year"))

merged_df <- merged_df %>%
  left_join(cty_pop_long, by = c("county", "year")) # add in population

# check merge col 
merged_df$quintile <- as.character(merged_df$quintile)
unique(merged_df$quintile)

merged_df$incidence_avg <- incidence_avg[merged_df$quintile] #average incidence, but we'll use quintile disagg instead 
merged_df$est_PCE_cty_agg <- merged_df$estimate * merged_df$pop_est
merged_df$est_taxable_spend <- (merged_df$est_PCE_cty_agg * merged_df$pce_estimate) # total PCE * share of taxable PCE 
merged_df$est_tax_rev <- merged_df$est_taxable_spend * merged_df$cty_rate # new col for taxable PCE * local rate 

# merge in the CA transactions data --- get a second estimate for the tax base (based on per capita transactions)
merged_df <- merged_df %>%
  left_join(taxable_trans_annual, by = c("year")) # add in aggregates - create new cols 

# create col for estimated total taxable spending (based on CA transaction data)
merged_df$est_tax_spend_trans <-  merged_df$percap_taxable_total * merged_df$pop_est
merged_df$est_tax_rev_trans <- merged_df$est_tax_spend_trans * merged_df$eff_rate # new col for taxable trans * state rate 


# EXPORT THE MERGED dataset so data cleaning process isn't necessary next time 
output_path <- file.path(getwd(), "output-tables")

write.csv(merged_df,
          file = file.path(output_path, "cty-level-estimated-taxbase.csv"),
          row.names = FALSE)

# -----------------------------------------------------------------------
###### ###### ###### ######  Estimate tax collection  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# CREATE YR LEVEL DF from estimated tax base
merged_yearly <- merged_df %>%
  group_by(year) %>%
  summarise(
    across(c(pop_est, est_PCE_cty_agg, est_taxable_spend, est_tax_rev, taxable_trans_total, est_tax_spend_trans, est_tax_rev_trans), sum, na.rm = TRUE),
    across(c(estimate, incidence_avg, pce_estimate, cty_rate, eff_rate, percap_taxable_total), mean, na.rm = TRUE), # where estimate = median household income
    .groups = "drop"
  ) %>% 
  select(year, estimate, pop_est, est_PCE_cty_agg, est_taxable_spend, 
         est_tax_rev, taxable_trans_total, est_tax_spend_trans, est_tax_rev_trans,
         incidence_avg, pce_estimate, cty_rate, eff_rate, percap_taxable_total)

merged_yearly <- merged_yearly %>%
  rename(median_hh_income = estimate)


# merge in collected tax data from both CA (local + state) and FRED FED (state gross only)
merged_all <- merged_yearly %>%
  left_join(tax_coll_df, by = "year") %>%
  left_join(ca_salestax_survey_clean, by = "year")

merged_all <- merged_all %>%
  mutate(
    tax_collected = as.numeric(tax_collected) # making them all numeric 
  )

merged_long <- merged_all %>%
  pivot_longer(
    cols = c(est_tax_rev, est_tax_rev_trans, revenue_FRED_source, tax_collected),      
    names_to = "source",
    values_to = "value"
  )

# -----------------------------------------------------------------------
###### ###### ###### ######  Compare county w/ agg vals  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# test for measurement error 
annual_vals_for_state <- merged_all

state_level_est <- left_join(annual_vals_for_state, pce_df %>% # note CAPCE is in millions USD 
                          select(year, CAPCE), by = "year")

# write estimate var
state_level_est$total_est_rev_mills <- (state_level_est$incidence_avg*state_level_est$CAPCE)*state_level_est$eff_rate
state_level_est$total_est_rev <- state_level_est$total_est_rev_mills*1000000

# estimate taxable base at county level over time 
merged_df %>%
  group_by(year, quintile) %>%
  summarise(est_tax_rev = (sum(est_tax_rev, na.rm = TRUE))/1000000, .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = est_tax_rev, fill = quintile)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    x = "Year",
    y = "Estimated tax revenue (millions USD)",
    title = "Stacked county-quintile estimated tax revenue by year",
    fill = "Quintile bin"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

bars_df <- merged_df %>%
  group_by(year, quintile) %>%
  summarise(est_tax_rev = (sum(est_tax_rev, na.rm = TRUE))/1000000, .groups = "drop")

line_df <- merged_df %>%
  group_by(year) %>%
  summarise(avg_rate = mean(cty_rate, na.rm = TRUE), .groups = "drop")

# scaling constant so both fit visually
scale_factor <- max(bars_df$est_tax_rev, na.rm = TRUE) / max(line_df$avg_rate, na.rm = TRUE)

ggplot() +
  # stacked bars (left axis)
  geom_bar(
    data = bars_df,
    aes(x = factor(year), y = est_tax_rev, fill = quintile),
    stat = "identity"
  ) +
  # line (right axis, rescaled)
  geom_line(
    data = line_df,
    aes(x = factor(year), y = avg_rate * scale_factor, group = 1),
    color = "grey", linewidth = 1
  ) +
  geom_point(
    data = line_df,
    aes(x = factor(year), y = avg_rate * scale_factor),
    color = "black", size = 2
  ) +
  # axes and labels
  scale_y_continuous(
    name = "Estimated tax revenue",
    labels = scales::comma_format(),
    sec.axis = sec_axis(~ . / scale_factor, name = "Average tax rate")
  ) +
  labs(
    x = "Year",
    title = "Quintile tax revenue (bars) and statewide average tax rate (line)",
    fill = "Quintile"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


# -----------------------------------------------------------------------
###### ###### ###### ######  CALC FORMULA VALS  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# ratio of collection based on [actual rev collection] / [projected revenue] - note we adopt an approximate to reduce business related taxes in line with literature 
state_level_est$gamma_t = (state_level_est$revenue_FRED_source)*.75 / state_level_est$est_tax_rev
state_level_est$gamma_tot_trans =  (state_level_est$revenue_FRED_source)*.75 / state_level_est$est_tax_rev_trans

# calculate GDP less trade deficit 
prod_df$gva_net = prod_df$gva_t - (prod_df$exports_mill_dollars + prod_df$imports_mill_dollars)
prod_df$est_spend_imports = prod_df$imports_mill_dollars*prod_df$share_imports_consumption # assume full compliance here

# estimate deltas now where we are only estimating non-compliance on non-imported goods
bigdf <- left_join(state_level_est, prod_df, by="year")
bigdf$est_spend_imports_dollars <- bigdf$est_spend_imports*1000000
bigdf$sum_tax_imports <- (bigdf$est_spend_imports_dollars * bigdf$incidence_avg * bigdf$eff_rate)
bigdf$sum_tax_ALT <- (bigdf$share_imports_consumption* bigdf$CAPCE * bigdf$pop_est* bigdf$incidence_avg * bigdf$eff_rate)
bigdf$sum_tax_dom <- ((1-bigdf$share_imports_consumption)* bigdf$CAPCE * bigdf$pop_est* bigdf$incidence_avg * bigdf$eff_rate)


bigdf$delta_t = ((bigdf$revenue_FRED_source/100000) - bigdf$sum_tax_ALT) / bigdf$sum_tax_dom

# filter table for relevant years only
bigdf <- bigdf %>%
  filter(year >= 2005)

# compute x_t --- export share 
bigdf <- bigdf %>%
  mutate(x_t = exports_mill_dollars / gva_t) # reasonable assessment of share of exports

# calculate EVADE measures first, ignoring NT_prod adaptation and looking just at GDP adjustment
bigdf2 <- bigdf %>%
  mutate(
    gdp_full = (gva_t - exports_mill_dollars) + (1-delta_t)*(1-x_t)*(gva_t - exports_mill_dollars) + ((1/gamma_t)-1)*(revenue_FRED_source/1000000),
    evade_t = (gdp_full - gva_t)/gva_t # note this measure is skewed bc we cannot adjust for value add given CA taxation is not like VAT
  )

# EVADE measure incorrect given our units of analysis
bigdf <- bigdf %>%
  left_join(
    state_level_est %>% select(year, gamma_t, gamma_tot_trans),
    by = "year"
  )

