# Load packages
library(ggplot2)  # Normalraphs
library(tidyverse) # Data prepartion
library(lubridate) # Date manipulation
library(data.table) # Fast data import and preparation
library(gbm) #  Machine Learning (ML): classification in this case
library(rsample) # Sampling data
library(vip) # Plotting variable importance"graphs after ML
library(pdp) # Plotting "variables importance" graphs
library(corrplot) # Plotting correlation matrix

# Set working directory
set.seed(123) # for reproducibility
setwd("~/Desktop/Brandeis/Career/Airbnb Data Challenge")

# Import data -------------------------------------------------------------

# Contacts: user inquiry 
df_contacts <- fread("Contacts - Data.csv")

# Listings   Market listings 
df_listings <- fread("Listings - Data.csv")

# Users: user data
df_users <- fread("Users - Data.csv")

# Data Preparation -----------------------------------------------------
df_contacts_listings <- df_contacts %>% 
# Convert to data variables & create new variables
  mutate(ts_interaction_first = ymd_hms(ts_interaction_first),
         ts_reply_at_first    = ymd_hms(ts_reply_at_first),
         ts_accepted_at_first = ymd_hms(ts_accepted_at_first),
         ts_booking_at        = ymd_hms(ts_booking_at),
         ds_checkin_first     = ymd(ds_checkin_first),
         ds_checkout_first    = ymd(ds_checkout_first),
         inquiry_day          = weekdays(ts_interaction_first, abbreviate = TRUE),
         inquiry_week         = week(ts_interaction_first),
         inquiry_month        = months(ts_interaction_first, abbreviate = TRUE),
         inquiry_year         = year(ts_interaction_first),
         booking_day          = weekdays(ts_booking_at, abbreviate = TRUE),
         booking_week         = week(ts_booking_at),
         booking_month        = months(ts_booking_at,abbreviate = TRUE),
         booking_year         = year(ts_booking_at),
# Duration of stay at the listing
         booking_duration     =  (ds_checkout_first-ds_checkin_first)/ddays(),
# Reorder factors
inquiry_day                   = factor(inquiry_day, 
                                       levels = c("Mon", "Tue", "Wed", "Thu", "Fri","Sat", "Sun")),
inquiry_month                 = factor(inquiry_month, 
                                       levels  = c("Jan", "Feb", "Mar", "Apr", "May",
                                 "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
# Mark successful bookings
        successful_booking    = ifelse(!is.na(ts_booking_at),1,0)) %>% 
# Merge user contact data and listing data
  left_join(df_listings, by  = "id_listing_anon") %>% 
# Recode negative total reviews as zero: logically a listing cannot have negative
# total reveiws
  mutate(total_reviews = ifelse(total_reviews < 0, 0, total_reviews))

# Summary Statistics ------------------------------------------------------

summary(df_contacts_listings)

# Success rate per contact channel
df_sum_stats <- df_contacts_listings %>% 
  group_by(contact_channel_first) %>% 
  summarise(percent = round(mean(successful_booking)*100,2))
df_sum_stats

# User actions on platform
  df_sum_stats_2 <- df_contacts_listings %>% 
  group_by(successful_booking, contact_channel_first ) %>% 
  summarise(total = n(),
            share = round((total/nrow(df_contacts_listings))*100,2)) %>% 
  select(contact_channel_first,successful_booking,share)
  df_sum_stats_2
  
 # Booking duration by month
  df_sum_stats_3 <- df_contacts_listings %>% 
    filter(successful_booking == 1) %>% 
    group_by(booking_month) %>% 
    summarise(mean_duration = round(mean(booking_duration),2))
  df_sum_stats_3
    
# Metric 1 ---------------------------------------------------------------------
# Metric 1: booking conversion rate per contact_channel_first - contact_me
  metric_1_contact_me <- df_contacts_listings %>% 
    filter(contact_channel_first == "contact_me") %>% 
    group_by(inquiry_month, room_type) %>% 
    summarise(total                    = n(),
              booking_conversion_rate  = round(mean(successful_booking)*100,1)) 
 
  # Plot 1
  metric_1_plot <- ggplot(data = metric_1_contact_me, aes(x = inquiry_month,  y = booking_conversion_rate, group =1)) +
    geom_line(linetype = 2, colour = 'indianred2', size=0.4) + 
    geom_point(shape=18, size =3, color ="indianred2") +
    geom_text(aes(label=booking_conversion_rate,hjust= 0.5, vjust=-1), size=2) +
    ylab("Booking conversion rate (%)") +
    xlab("Month") +
    coord_cartesian(ylim = c(0, 10))+
    scale_x_discrete(expand = c(0, 0.1)) +
    theme_bw()+
    theme(text = element_text(size=14),
          legend.position = "none",
          panel.border = element_blank(),  
          # Remove panel grid lines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))+
    facet_wrap(~room_type)
  metric_1_plot
  
# Metric 2 ---------------------------------------------------------------------
# Metric 2: booking conversion rate per contact_channel_first - book_it
  metric_2_book_it <- df_contacts_listings %>% 
    filter(contact_channel_first == "book_it") %>% 
    group_by(inquiry_month, room_type) %>% 
    summarise(total                    = n(),
              booking_conversion_rate  = round(mean(successful_booking)*100,1)) 
  
  metric_2_plot <- ggplot(data = metric_2_book_it, aes(x = inquiry_month,  y = booking_conversion_rate, group =1)) +
    geom_line(linetype = 2, colour = 'indianred2', size=0.4) + 
    geom_point(shape=18, size =3, color ="indianred2") +
    ylab("Booking conversion rate (%)") +
    xlab("Month") +
    coord_cartesian(ylim = c(30, 60))+
    scale_x_discrete(expand = c(0, 0.1)) +
    geom_text(aes(label=booking_conversion_rate,hjust= 0.5, vjust=-1), size=2) +
    theme_bw()+
    theme(text = element_text(size=14),
          legend.position = "none",
          panel.border = element_blank(),  
          # Remove panel grid lines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))+
    facet_wrap(~room_type)
  metric_2_plot
  
# remove used data sets and object
  rm(list = ls(pattern = "^metric"), df_sum_stats, df_sum_stats_2, df_sum_stats_3,
     df_contacts, df_listings)

# Business Improvements ---------------------------------------------------
  # Feature engineering 
  # I used one-hot encoding  for some variables since 
  # gbm could not handle variables with categorical data
df_model_rio <-  df_contacts_listings %>% 
  # In this merge, I assume that the guest uses their user_id in making 
  # an inquiry hence guest_id and user_id are the same for each individual
    left_join(df_users, by = c("id_guest_anon" = "id_user_anon")) %>%
  # How long the guest takes to respond to an inquiry
    mutate(host_response_duration = (ts_reply_at_first - ts_interaction_first)/dhours(),
  # Inquiry contact channel
           contact_me_ind  = ifelse(contact_channel_first == "contact_me", 1,0),
  # Inquiry contact channel
           book_it_ind     = ifelse(contact_channel_first == "book_it", 1,0),
  # Inquiry contact channel
           instant_book_ind  = ifelse(contact_channel_first == "instant_book", 1,0),
  # Seasonality: Test if probability of booking depends on weekday/weekend
           weekend_ind     = ifelse(inquiry_day == "Fri"|inquiry_day =="Sat", 1,0),
  # Demographics: Test if probability of booking depends on nationality: Here I assume the
  # "BR" is the country abbreviation on Brazil
           tourist_ind     = ifelse(!country == "BR", 1,0),
  # Listing type: test if probability on room type
           home_ind = ifelse(room_type == "Entire home/apt", 1,0),
  # Listing type: test if probability on room type
           pvt_room_ind = ifelse(room_type == "Private room", 1,0),
  # Listing type: test if probability on room type
           shared_room_ind = ifelse(room_type == "Shared room", 1,0),
  # Seasonality: Test if probability of booking depends on seasons of the year in Rio de Janeiro
  # http://trip-n-travel.com/listicle/21050/
           summer_ind = ifelse(inquiry_month %in% c("Dec", "Jan", "Feb", "Mar"), 1,0),
  # Seasonality: Test if probability of booking depends on seasons of the year in Rio de Janeiro
           fall_ind = ifelse(inquiry_month %in% c("Apr", "May"), 1,0),
  # Seasonality: Test if probability of booking depends on seasons of the year in Rio de Janeiro
           winter_ind = ifelse(inquiry_month %in% c("Jun", "Jul"), 1,0),
  # Seasonality: Test if probability of booking depends on seasons of the year in Rio de Janeiro
           spring_ind = ifelse(inquiry_month %in% c("Sep", "Oct"), 1,0),
  # User history: Test if probability of booking depends user's past preferences
           past_booker_ind = ifelse(guest_user_stage_first == "past_booker",1,0),
  # User history: Test if probability of booking depends user's past preferences
           new_booker_ind = ifelse(guest_user_stage_first == "new",1,0),
  # User history: Test if probability of booking depends user's past preferences
           unknown_booker_ind = ifelse(guest_user_stage_first == "unknown",1,0)) %>% 
  # Select features to use in machine learning model
  select(successful_booking,m_guests,m_interactions, m_first_message_length_in_characters,
         total_reviews,words_in_user_profile,host_response_duration, 
         ends_with("ind"), -instant_book_ind)
  
# Plot to correlation to visualize variable relationships
  cor      <-  cor(df_model_rio,use="complete.obs")
  cor_plot <- corrplot(cor,method = "circle",  tl.col ="red", tl.pos='n')
  
# Sampling: 75% Training data and 25% Testing data
df_model_rio_split <- initial_split(df_model_rio, prop = .75)
df_model_rio_train <- training(df_model_rio_split)
df_model_rio_split_test  <- testing(df_model_rio_split)

# Machine Learning: Rationale for Gradient Boosting Machines (GBM)
# The features provided may not necessarry be the best learners:
# with GBM, these week learners are converted to strong learners
# GBM offers best prodictive accauracy
# GBM Handles missing data - imputation not required
# GBM allows for optimization on different loss functions 

# Run machine learning model
gbm_model_1 <- gbm(successful_booking ~ ., data = df_model_rio_train,
                      distribution      = "gaussian",
                      n.trees           = 10000,
                      shrinkage         = 0.01, 
                      interaction.depth = 4,
                      cv.folds          = 5)
# print results
print(gbm_model_1)

# Model perfomamce
# RMSE
RMSE_gbm_model_1 <- sqrt(min(gbm_model_1$cv.error))
RMSE_gbm_model_1

# Loss function 
gbm.perf(gbm_model_1, method = "cv")
  
# Plot model importance
# removed instant_book from the machine leaning model  since
# if "instant_book" = 1, success_booking =1 
plot_gbm_model_1 <- vip(gbm_model_1, fill = "indianred2") +
  ggtitle("Variable importance: Succceful bookings") +
  theme(
    legend.position = "none",
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0))
plot_gbm_model_1

# Tune Model
# Run machine learning model
gbm_model_2 <- gbm(successful_booking ~ ., data = df_model_rio_train,
                   distribution      = "gaussian",
                   n.trees           = 5000,
                   shrinkage         = 0.01, 
                   interaction.depth = 3,
                   cv.folds          = 5)
# print results
print(gbm_model_2)
summary(gbm_model_2)

# Model perfomamce
# RMSE
RMSE_gbm_model_2 <-  sqrt(min(gbm_model_2$cv.error))
RMSE_gbm_model_2

# Plot loss function 
gbm.perf(gbm_model_2, method = "cv")

# Plot model importance
plot_gbm_model_2 <- vip(gbm_model_2, fill = "indianred2") +
  ggtitle("Variable importance: Succceful bookings") +
  theme(
    text = element_text(size=16),
    legend.position = "none",
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")) +
    scale_y_continuous(expand = c(0, 0)) +
  ylab("Importance (%)")

plot_gbm_model_2

# Prediction
#predict values for test data
pred <- predict(gbm_model_2, n.trees = gbm_model_2$n.trees, df_model_rio_split_test)

# RMSE
caret::RMSE(pred, df_model_rio_split_test$successful_booking)

# Number of interactions partial plot
partial_plot_1 <- partial(gbm_model_2, pred.var = "m_interactions", 
                        n.trees = gbm_model_2$n.trees, 
                        plot = TRUE, plot.engine = "ggplot2") + 
                        theme(
                        text = element_text(size=16),
                        legend.position = "none",
                        panel.border = element_blank(),  
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black")) +
                        ggtitle("Probability of successful booking") +
                        ylab("Probability of successful booking") +
                        xlab("Number of interactions") + 
                        geom_line(col ="indianred2")
partial_plot_1
  
# Reviews partial plot
partial_plot_2 <- partial(gbm_model_2, pred.var = "total_reviews", 
                          n.trees = gbm_model_2$n.trees, 
                          plot = TRUE, plot.engine = "ggplot2") + 
  theme(
    text = element_text(size=16),
    legend.position = "none",
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")) +
  ggtitle("Probability of successful booking") +
  ylab("Probability of successful booking") +
  xlab("Number of reviews") + 
  geom_line(col ="indianred2")
partial_plot_2

# Foreign nationals partial plot
partial_plot_4 <- partial(gbm_model_2, pred.var = "tourist_ind", 
                          n.trees = gbm_model_2$n.trees, 
                          plot = TRUE, plot.engine = "ggplot2") + 
  theme(
    text = element_text(size=16),
    legend.position = "none",
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")) +
  ggtitle("Probability of successful booking") +
  ylab("Probability of successful booking") +
  xlab("Foreign nationality") + 
  geom_line(col ="indianred2")
partial_plot_3
  


  
