#install.packages(c("jsonlite", "tidyverse", "reticulate", "patchwork", "plotly", "brglm2")) 
library(jsonlite)
library(tidyverse)
library(lubridate)
library(patchwork)
library(reticulate)
#py_install("pandas")

#using jsonlite package to read the dataset of all businesses
business <- stream_in(file("/Users/kaan/Desktop/yelp_wd/data_raw/yelp_academic_dataset_business.json"))

#selecting only columns that we need to do our analysis to increase compuatational efficiency
all_business <- business |>
  select(business_id, name, categories, latitude, longitude, stars, review_count)

#make sure to run python code first before moving below!

library(data.table)
#loading all reviews in r after processing them in python
all_review <- fread(
  "all_reviews.csv",
  select = c("business_id", "stars", "date"),
  header = TRUE,       # make sure header is used
  sep = ",",           # match actual delimiter
  quote = "\"",        # handles commas inside text
  showProgress = TRUE
)

#it is better to join the data like this, so only businesses with reviews are used.
global <- all_review |>
  inner_join(all_business, by = "business_id") |>
  rename(review_stars = stars.x, mean_stars = stars.y) |>
  select(-mean_stars) |>
  relocate(name)

global_restaurants <- global |>
  filter(grepl("Restaurants", categories, ignore.case = TRUE)) |>
  mutate(
    year = year(as.Date(date)),
    month = month(as.Date(date)),
    lat_bin = round(latitude, 2),
    lon_bin = round(longitude, 2),
    location_bin = paste(lat_bin, lon_bin, sep = ", ")
  )

#distinct_categories_cleaning <- global_restaurants |>
  #separate_rows(categories, sep = ",") |>
  #mutate(categor = str_trim(categories)) |> 
  #distinct(categor) |> 
  #pull(categor) |> # Extract as a vector
  #sort()

#distinct_categories_cleaning <- data.frame(distinct_categories_cleaning)
# a sample of categories that signal a business is not a restaurant. 
unnecessary_categories <- c("Active Life", "Adult", "Amateur Sports Teams", "Antiques", "Apartments", 
                            "Art Galleries", "Arts & Crafts", "Arts & Entertainment", 
                            "Auto Customization", "Auto Customization", "Auto Parts & Supplies", "Automotive", 
                            "Beauty & Spas", "Books", "Bookstores", "Cards & Stationery", "Chiropractors", "Day Spas", 
                            "Diagnostic Imaging", "Diagnostic Services", "Doctors", "Drugstores", "Eyelash Service", 
                            "Financial Services", "Fitness & Instructio", "Flowers & Gifts", "Gas Stations", "Golf",
                            "Golf Cart Rentals", "Golf Lessons", "Gun/Rifle Ranges", "Gyms", "Hair Removal", "Home Services",
                            "Health & Medical", "Horseback Riding", "Hotels", "Hotels & Travel", "Internal Medicine",
                            "Jewelry", "Leather Goods", "Mags", "Makeup Artists", "Massage", "Massage Therapy", "Movers", "Museums",
                            "Music & Video", "Music Venues", "Outlet Stores", "Party Equipment Rentals", "Party Supplies", "Pet Services",
                            "Pets", "Pet Stores", "Piercing", "Pool Halls", "Radiologists", "Real Estate", "Reflexology", "Religious Organizations",
                            "Resorts", "Septic Services", "Skin Care", "Soccer", "Tattoo", "Tax Services", "Trainers", "Travel Services",
                            "Vitamins & Supplements", "Waxing")

unnecessary_categories <- paste(unnecessary_categories, collapse = "|")

cln_global_res <- global_restaurants |>
  filter(!str_detect(global_restaurants$categories, unnecessary_categories))



#this lists the earliest reviews of businesses started in january and february
#months of years 2013, 14, 15, 16, 17, 18. We assume earliest reviews
#signals start date.
first_reviews_2013to2018januaryfebruary <- cln_global_res |>
  mutate(date = as.Date(date)) |>
  group_by(business_id) |>
  slice_min(date, n = 1, with_ties = FALSE) |>
  filter(year == 2013 | year == 2014 | year == 2015 | year == 2016 | year == 2017 | year == 2018) |>
  filter(month %in% 1:2) |>
  ungroup()

#if we assume normal distribution of last reviews of each business opened around a mean of month june, then
#our data would be testing survival of approximately 4.5 years for a given restaurant
survival_data_2014_2021 <- cln_global_res |>
  filter(business_id %in% first_reviews_2013to2018januaryfebruary$business_id) |>
  distinct(business_id, year, lat_bin, lon_bin) |>
  group_by(business_id) |>
  summarise(
    first_year = min(year),
    
    opened_2013 = as.integer(first_year == 2013),
    survived_2013 = as.integer(first_year == 2013 & all(2014:2016 %in% year)),
    
    opened_2014 = as.integer(first_year == 2014),
    survived_2014 = as.integer(first_year == 2014 & all(2015:2017 %in% year)),
    
    opened_2015 = as.integer(first_year == 2015),
    survived_2015 = as.integer(first_year == 2015 & all(2016:2018 %in% year)),
    
    opened_2016 = as.integer(first_year == 2016),
    survived_2016 = as.integer(first_year == 2016 & all(2017:2019 %in% year)),
    
    opened_2017 = as.integer(first_year == 2017),
    survived_2017 = as.integer(first_year == 2017 & all(2018:2020 %in% year)),
    
    opened_2018 = as.integer(first_year == 2018),
    survived_2018 = as.integer(first_year == 2018 & all(2019:2021 %in% year)),
    
    lat_bin = first(lat_bin),
    lon_bin = first(lon_bin),
    .groups = "drop"
  ) |>
  filter(first_year %in% 2013:2018)

  

#sanity check: This tests whether any business is classified as opened in two different years. 
#if the results of this exceeds observations in survival_data, then it is flawed.
sum(survival_data_2014_2021$opened_2013 == TRUE) + sum(survival_data_2014_2021$opened_2014 == TRUE) + sum(survival_data_2014_2021$opened_2015 == TRUE) + sum(survival_data_2014_2021$opened_2016 == TRUE) + sum(survival_data_2014_2021$opened_2017 == TRUE) + sum(survival_data_2014_2021$opened_2018 == TRUE)
#they are both equal, so it should be valid. 
sum(survival_data_2014_2021$survived_2013 == TRUE) + sum(survival_data_2014_2021$survived_2014 == TRUE) + sum(survival_data_2014_2021$survived_2015 == TRUE) + sum(survival_data_2014_2021$survived_2016 == TRUE) + sum(survival_data_2014_2021$survived_2017 == TRUE) + sum(survival_data_2014_2021$survived_2018 == TRUE)
#number of survived businesses is 1961. let's keep this number so we can measure it against our final dataset to 
#ensure validity


open_ids_2013 <- survival_data_2014_2021 %>%
  filter(opened_2013 == TRUE) %>%
  distinct(business_id)
surv_ids_2013 <- survival_data_2014_2021 %>%
  filter(survived_2013 == 1) %>%
  distinct(business_id)
open_ids_2014 <- survival_data_2014_2021 %>%
  filter(opened_2014 == TRUE) %>%
  distinct(business_id)
surv_ids_2014 <- survival_data_2014_2021 %>%
  filter(survived_2014 == 1) %>%
  distinct(business_id)
open_ids_2015 <- survival_data_2014_2021 %>%
  filter(opened_2015 == TRUE) %>%
  distinct(business_id)
surv_ids_2015 <- survival_data_2014_2021 %>%
  filter(survived_2015 == 1) %>%
  distinct(business_id)
open_ids_2016 <- survival_data_2014_2021 %>%
  filter(opened_2016 == TRUE) %>%
  distinct(business_id)
surv_ids_2016 <- survival_data_2014_2021 %>%
  filter(survived_2016 == 1) %>%
  distinct(business_id)
open_ids_2017 <- survival_data_2014_2021 %>%
  filter(opened_2017 == TRUE) %>%
  distinct(business_id)
surv_ids_2017 <- survival_data_2014_2021 %>%
  filter(survived_2017 == 1) %>%
  distinct(business_id)
open_ids_2018 <- survival_data_2014_2021 %>%
  filter(opened_2018 == TRUE) %>%
  distinct(business_id)
surv_ids_2018 <- survival_data_2014_2021 %>%
  filter(survived_2018 == 1) %>%
  distinct(business_id)



survival_analysis <- cln_global_res |>
  mutate(
    opened_2013 = business_id %in% open_ids_2013$business_id == TRUE,
    survived_2013 = business_id %in% surv_ids_2013$business_id == 1,
    opened_2014 = business_id %in% open_ids_2014$business_id == TRUE,
    survived_2014 = business_id %in% surv_ids_2014$business_id == 1,
    opened_2015 = business_id %in% open_ids_2015$business_id == TRUE,
    survived_2015 = business_id %in% surv_ids_2015$business_id == 1,
    opened_2016 = business_id %in% open_ids_2016$business_id == TRUE,
    survived_2016 = business_id %in% surv_ids_2016$business_id == 1,
    opened_2017 = business_id %in% open_ids_2017$business_id == TRUE,
    survived_2017 = business_id %in% surv_ids_2017$business_id == 1,
    opened_2018 = business_id %in% open_ids_2018$business_id == TRUE,
    survived_2018 = business_id %in% surv_ids_2018$business_id == 1
  )

survival_analysis1 <- survival_analysis |>
  mutate(
    not_survived_2013 = if_else(opened_2013 == 1 & survived_2013 == 0, 1L, 0L,),
    not_survived_2014 = if_else(opened_2014 == 1 & survived_2014 == 0, 1L, 0L,),
    not_survived_2015 = if_else(opened_2015 == 1 & survived_2015 == 0, 1L, 0L,),
    not_survived_2016 = if_else(opened_2016 == 1 & survived_2016 == 0, 1L, 0L,),
    not_survived_2017 = if_else(opened_2017 == 1 & survived_2017 == 0, 1L, 0L,),
    not_survived_2018 = if_else(opened_2018 == 1 & survived_2018 == 0, 1L, 0L,)
  ) 


business_summary <- survival_analysis1 |>
  group_by(lon_bin, lat_bin, business_id, year) |>
  summarise(
    business_avg_stars = mean(review_stars, na.rm = TRUE),
    n_reviews_business = n(),
    survived = max(survived_2013 + survived_2014 + survived_2015 + survived_2016 + survived_2017 + survived_2018, na.rm = TRUE),
    did_not_survive = max(not_survived_2013 + not_survived_2014 + not_survived_2015 + not_survived_2016 + not_survived_2017 + not_survived_2018, na.rm = TRUE),
    .groups = "drop" 
  )


business_summary_survived <- business_summary |>
  filter(survived == 1 | did_not_survive == 1) |>
  group_by(lon_bin, lat_bin, business_id)|>
  slice_min(year, n = 1, with_ties = FALSE)

#sanity check
sum(business_summary_survived$survived) == 1961
sum(business_summary_survived$did_not_survive) == 1277
sum(business_summary_survived$survived) + sum(business_summary_survived$did_not_survive) == 3238


business_summary_survived <- business_summary_survived |>
  group_by(lon_bin, lat_bin, year) |>
  summarise(
    survived = sum(survived == 1, na.rm = TRUE),
    did_not_survive = sum(did_not_survive == 1, na.rm = TRUE),
    .groups = "drop"
  )

#sanity check
sum(business_summary_survived$survived) == 1961
sum(business_summary_survived$did_not_survive) == 1277
sum(business_summary_survived$survived) + sum(business_summary_survived$did_not_survive) == 3238


grid_summary_test <- business_summary |>
  group_by(lon_bin, lat_bin, year) |>
  summarise(
    grid_avg_stars = mean(business_avg_stars),
    grid_n_reviews = sum(n_reviews_business),
    grid_n_businesses = n(),
    .groups = "drop"
  ) |>
  mutate(year_lag_1 = year + 1)

#sanity check
sum(grid_summary_test$grid_n_reviews) == 4460847

business_summary |> 
  summarise(n_unique = n_distinct(business_id))
survival_analysis1 |>
  summarise(n_unique = n_distinct(business_id))
sum(grid_summary_test$grid_n_businesses)

grid_summary_matched <- grid_summary_test |>
  left_join(
    business_summary_survived,
    by = c(
      "lon_bin" = "lon_bin",
      "lat_bin" = "lat_bin",
      "year_lag_1"   = "year"
    )
  )

surv_bus_sum_lagged <- grid_summary_matched |>
  filter(survived >= 1 | did_not_survive >= 1)


rm(list = setdiff(ls(), "surv_bus_sum_lagged")) #clean environment for clarity
data <- surv_bus_sum_lagged
data1 <- data |>
  mutate(reviewperbusiness = grid_n_reviews/grid_n_businesses)
data <- data1
data <- data |>
  mutate(exposure = survived + did_not_survive)
rm(surv_bus_sum_lagged)
rm(data1)
sum(data$survived)
sum(data$did_not_survive)


###saveRDS(data, file = "data.rds")
##data1 <- readRDS("data.rds")
##data <- data1


#I found that businesses started i 2019 cant be reliably included
#because reviews are cutoff sometime in 2022. modifying the cleaning process.
yearly_survivals <- data %>%
  group_by(year_lag_1) %>%
  summarise(total_survived = sum(survived),
            total_not_survived = sum(did_not_survive),
            .groups = "drop")

data$rpb_bin <- cut(
  data$reviewperbusiness,
  breaks = quantile(data$reviewperbusiness, probs = seq(0,1,0.25)),
  include.lowest = TRUE
)
data <- data |>
  mutate(
    survival_ratio = survived/exposure
  )
