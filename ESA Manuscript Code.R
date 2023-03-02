# Purpose: Data Visualizations and Analysis of Flow and Pulse
#          pulse subsidy impacts on fish foraging and growth

# Date: 3/1/23


# Set Up ------------------------------------------------------------------
# install.packages('ggpubr')
# install.packages('ggh4x')
# install.packages('ggtext')
# install.packages("paletteer")
# install.packages('cartography')
# install.packages('RColorBrewer')
# install.packages('viridis')
# install.packages('streamgraph')

library(here)
library(readxl)
library(lubridate)
library(tidyverse)
library(ggsci)
library(mgcv)
library(ggthemes)
library(dplyr)
library(ggpubr)
library(ggh4x)
library(ggeasy)
library(cowplot)
library(ggtext)
library(paletteer)
library(cartography)
library(RColorBrewer)
library(viridis)

# library(streamgraph)

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# Read in Data ------------------------------------------------------------

# Read in coho diets
coho.d <- read.csv('Raw Coho Diet Data Final.csv') %>% 
  mutate(Sample.code = as.numeric(substr(Sample.code, 2, 20))) # Remove weird sample code stuff

# Read in dolly diets
dolly.d <- read.csv('Raw Dolly Diet Data Final.csv') 

# Length, weight & species data from capture events
capture.data <- read_csv( "Upper_MT_FishData_Final_Analysis.csv")

# MT Creek 2021 Flow Data
flow <- read.csv('Montana abv. McGinnis 2021 15 min.csv')

# 15 min temperature data from main stem
temp <- read_xlsx( 'MT Creek Pressure Trans Temp Data.xlsx')

#temp$Date <- as.Date(temp$Date)
#temp.mean.daily <- temp %>% group_by(Date) %>% 
#                            summarise(MD.Temp = mean(Temperature, na.rm = TRUE))

#write.csv(temp.mean.daily, "2021_Mean_Daily_Temps.csv")

# Hourly temperature and turbidity data from main stem
turb <- read.csv( 'MT Creek 2021 Hourly Temp and Turbidity Data.csv')

# Drift Data 
drift <- read_xlsx('2021 MT Creek Drift Data Final.xlsx', sheet = 'Long output')

# Pink Salmon Spawning Density

Spawn.density <- read.csv('Salmon spawning density estimates.csv')

# Bioenergetic predictions of fish size

Bio.e.age1 <- read_xlsx('Consumption_Model_Outputs.xlsx', sheet = 'Age1 BioE Outputs')
Bio.e.age0 <- read_xlsx('Consumption_Model_Outputs.xlsx', sheet = 'Age0 BioE Outputs')

# General data munging ------------------------------------------------------------


# Add biomass and energy consumption values to Coho and Dolly data frames
coho.cap.dat <- capture.data %>% 
  filter(Diet !='N', Species == "Coho",!is.na(TL),TL > 28) %>% 
  mutate(Date = mdy(Date), Fish_ID_day = as.numeric(Fish_ID_day))

# Filter dolly capture data
dolly.cap.dat <- capture.data %>% 
  filter(Diet !='N', Species == "Dolly Varden",!is.na(TL),TL > 28) %>% 
  mutate(Date = mdy(Date), Fish_ID_day = as.numeric(Fish_ID_day))

# Sum biomass values by individual to find mass of stomach contents...
# for cohos
coho.diets <- coho.d %>% mutate(Date = ymd(Date)) %>% 
  group_by(Sample.code, Date) %>% summarise(Biomass = sum(Biomass),
                                            energy = sum(energy)) 

# for dollies
dolly.diets <- dolly.d %>% mutate(Date = ymd(Date)) %>% 
  group_by(Sample.code, Date) %>% summarise(Biomass = sum(Biomass),
                                            energy = sum(energy))

# Join data frames of capture data and biomass diet data 
coho.df <- left_join(coho.diets, coho.cap.dat, by = c('Date', 'Sample.code' = "Fish_ID_day" ))
dolly.df <- left_join(dolly.diets, dolly.cap.dat, by = c('Date', 'Sample.code' = "Fish_ID_day" ))

# convert biomass values to stomach fullness values
coho.df <- coho.df %>%  mutate(stomach.fullness = (Biomass/1000) / Weight,
                               Date = ymd(Date),
                               doy = yday(Date))
dolly.df <- dolly.df %>%  mutate(stomach.fullness = (Biomass/1000) / Weight,
                                 Date = ymd(Date))

coho.df <- coho.df %>%  mutate(energy.cons.g = (energy) / Weight,
                               Date = ymd(Date),
                               doy = yday(Date))
dolly.df <- dolly.df %>%  mutate(energy.cons.g = (energy) / Weight,
                                 Date = ymd(Date))


# aggregate biomass by day
drift <- drift %>% mutate(Date = ymd(Date)) %>% 
                   group_by(Date) %>% 
                   summarise(Biomass.drift = sum(Biomass))

# Joining physical data to diet dataframe ---------------------------------

# Add values of mean flow, temperature, and turbidity for all fish of each unique 
# sample day

# create a vector of all time-stamped sample points
samp.points <- data.frame(date = ymd_hm(c( "2021-05-05 14:30",
                                           "2021-05-11 10:30",
                                           "2021-05-14 13:00",
                                           "2021-05-18 13:15",
                                           "2021-05-21 12:00",
                                           "2021-05-26 14:00",
                                           "2021-05-28 13:15",
                                           "2021-05-31 09:45",
                                           "2021-06-01 10:00",
                                           "2021-06-04 10:15",
                                           "2021-06-09 12:45",
                                           "2021-06-10 10:45",
                                           "2021-06-15 12:00",
                                           "2021-06-17 10:30",
                                           "2021-06-21 11:15",
                                           "2021-06-24 12:45",
                                           "2021-06-30 12:45",
                                           "2021-07-02 11:45",
                                           "2021-07-06 11:00",
                                           "2021-07-13 11:45", 
                                           "2021-07-15 12:00",
                                           "2021-07-20 11:15",
                                           "2021-07-29 11:15",
                                           "2021-08-03 13:15",
                                           "2021-08-04 12:45",
                                           "2021-08-09 12:45",
                                           "2021-08-12 13:15",
                                           "2021-08-13 12:00",
                                           "2021-08-14 11:00",
                                           "2021-08-18 12:45",
                                           "2021-08-25 12:00",
                                           "2021-08-28 10:30",
                                           "2021-09-02 12:00", 
                                           "2021-09-03 21:00",
                                           "2021-09-10 20:00",
                                           "2021-09-14 17:45",
                                           "2021-09-21 12:00",
                                           "2021-09-23 11:15",
                                           "2021-09-24 08:00",
                                           "2021-10-01 11:45",
                                           "2021-10-02 13:00",
                                           "2021-10-07 13:00",
                                           "2021-10-12 12:00",
                                           "2021-10-18 12:45",
                                           "2021-10-28 04:00")), 
                                            q = NA, temp = NA, 
                                            turb = NA)

# Now munge the flow dataframe to make it a date format
flow <- flow[-c(1:2), ] # Remove first two weird rows

flow <- flow %>% 
  mutate(X = mdy_hm(X),
         q = as.numeric(Discharge.Montana.abv.McGinnis.near.Juneau)) %>% 
  rename(Date = X)

# Cleaning for the temp dataframe
temp <- temp %>% 
  mutate(`Date Time` = ymd_hms(`Date Time`),
         Temperature = as.numeric(Temperature)) %>% 
  rename(Date = `Date Time`)

# mdt <- temp %>%  mutate(date = floor_date(Date)) %>%
#                 group_by(ymd(Date)) %>% 
#                 summarise(mean.temp = mean(Temperature))
              

# Cleaning for the turbidity dataframe
turb <- turb %>% 
  mutate(Date_Time = mdy_hms(paste(Date, Time)))

turb$Date_Time <- substr(turb$Date_Time, 1, 16)

# Now remake this into a date again
turb <- turb %>% 
  mutate(Date_Time = ymd_hm(Date_Time))


# Loop through to correct dates and times ---------------------------------

# Now create a loop to add 12-hour average flow values.

for(i in 1:nrow(samp.points)) {
  
  # Get time 12 hours prior to the sampling point
  twlve_hr_pt <- samp.points$date[i] - hours(12)
  
  # Get flow points 12 hours prior and up to the
  # sampling point
  flow_12hr_pts <- flow %>% 
    filter(Date >= twlve_hr_pt ,
           Date <= samp.points$date[i])
  
  # Now, take the average flow across the 12 hour range
  mean_12hr_flow <- mean(flow_12hr_pts$q, na.rm = TRUE)
  
  # Now, stick the mean 12 hour flow into the sampling point dataframe
  samp.points$q[i] <- mean_12hr_flow
  
  # Now repeat the above but for temp
  temp_12hr_pts <- temp %>% 
    filter(Date >= twlve_hr_pt,
           Date <= samp.points$date[i])
  
  # Now, take the average temp across the 12 hr range
  mean_12hr_temp <- mean(temp_12hr_pts$Temperature, na.rm = TRUE)
  
  # Stick this into the sampling point df
  samp.points$temp[i] <- mean_12hr_temp
  
  # Do the above but for turbidity
  turb_12hr_pts <- turb %>% 
    filter(Date_Time >= twlve_hr_pt,
           Date_Time <= samp.points$date[i])
  
  # Take the mean
  mean_12hr_turb <- mean(turb_12hr_pts$Turbidity.FNU, na.rm =TRUE)
  
  # Stick this into sampling point df
  samp.points$turb[i] <- mean_12hr_turb
} # end i

# Now that these values are in the 'samp.points' data frame, join them to the 
# coho.df and dolly.df

# First get the date into the correct format
samp.points$nohms_date <- ymd(substr(samp.points$date, 1, 10))

# Join drift data to sample points
samp.points <- samp.points %>% 
  left_join(drift, by = c('nohms_date' = "Date"))

# Finally join the physical data to the diets of coho and dollies
coho.df <- coho.df %>% 
  left_join(samp.points, by = c("Date" = "nohms_date")) 

dolly.df <- dolly.df %>% 
  left_join(samp.points, by = c("Date" = "nohms_date")) %>% 
  mutate(doy = yday(Date)) 

# Combine these two dataframes together
allsp_df <- rbind(coho.df, dolly.df)

# Modelling data munging -------------------------------------------------------

# For the diets, remove the high q flow point
allsp_df <- allsp_df %>% 
  filter(q < 6,
         Biomass < 400) %>% 
  mutate(Species = factor(Species),
         Habitat = ifelse(Habitat == "Seep", "Main_Seep", Habitat))

# Next, create two separate dataframes which reflect: 1) when pinks are absent
# and 2) when pinks are present

# Pinks absent
allsp_pinks_abs <- allsp_df %>% 
  filter(doy < 210,
         TL != is.na(TL),
         Biomass < 400,
         Biomass.drift < 1000)

# Pinks present
allsp_pinks_pres <- allsp_df %>% 
  filter(doy > 210,
         TL != is.na(TL),
         Biomass < 250)


# Set up models here -----------------------------------------------------

# Now, lets create a saturated model so that we can do model selection on.

# The following are variables we want to consider
# Biomass ~
# q (flow, by = Species)
# Habitat (independent factor)

# Control Variables
# Total Length
# Temperature


### Function for model selection --------------------------------------------
#' @param possible_variables A vector of variables we want to consider
#' @param control_variables A formula in character form that specifies the response, as a function
#' @param df: A dataframe of the data you want to use
#' @param error_dist: The error distribution you want to use for your GAM model

# Create a generalized function to do model selection
model_selex <- function(possible_variables, control_variables,
                        df, error_dist) {
  
  # Get all possible combinations of possible variables
  variables <- unlist(lapply(1:length(possible_variables), combn, 
                             x = possible_variables, simplify = FALSE), 
                      recursive = FALSE)
  
  # Create a list object to store models in
  all_models <- list(length(variables))
  
  # Create a dataframe to store model summary statistics
  mod_statistics <- data.frame(AIC = NA, BIC = NA, R2 = NA,
                               Dev_Exp = NA, model = rep(NA, length(variables))) 
  
  for(i in 1:length(variables)) {
    
    # Setting up loop for model formula
    # Model formula set up, and going through all combinations here
    formula <- as.formula(paste(control_variables, paste(variables[[i]], collapse = "+")))
    
    # Pipe this formula into our model formula for BAM
    model_output <- gam(formula, data = df, 
                        family = error_dist)
    
    # Stick model outputs into list
    all_models[[i]] <- model_output
    
    # Name these model to differentiate them
    names(all_models)[[i]] <- paste("model", i)
    
 print(paste("done w/ model", i)) } # End for loop
  
  # Next evaluate AIC, BIC, deviance explained, and R2 for all of these
  for(j in 1:length(all_models)) {
    
    # Differentiate models
    mod_statistics$model[j] <- names(all_models)[[j]]
    # Get AIC
    mod_statistics$AIC[j] <- AIC(all_models[[j]])
    # Get BIC
    mod_statistics$BIC[j] <- BIC(all_models[[j]])
    # Get Deviance Explained
    mod_statistics$Dev_Exp[j] <- summary(all_models[[j]])$dev.expl
    # Get R2
    mod_statistics$R2[j] <- summary(all_models[[j]])$r.sq
    
  }
  
  return(list(all_models, mod_statistics))
  
} # end function

# Run Models (Before Pinks) -----------------------------------------------

# Set up vector of possible vectors
possible_variables <- c("s(q, k = 4, by = Species, bs = 'cr')", 
                        "s(Biomass.drift, k = 4, bs = 'cr')")
# Control Variables
control_variables <- "Biomass ~  s(temp, k = 4, bs = 'cr') + s(TL, by = Species, k = 4, bs = 'cr') +"

# run function to evaluate before pink models
beforepinks <- model_selex(possible_variables = possible_variables,
            control_variables = control_variables,
            df = allsp_pinks_abs, error_dist = tw(link = "log"))

# Get model statistics from selection process
model_stats_before_pinks <- beforepinks[[2]]

# From the above, let's select model 13 as our candidate model
beforepinks_cand_mod <- beforepinks[[1]]$`model 1`

# Look at summary of this
summary(beforepinks_cand_mod)

# Plot diagnostics
par(mfrow = c(2,2))
gam.check(beforepinks_cand_mod)
abline(0,1, col = "red")

# plot these out
par(mfrow = c(3,2))
plot(beforepinks_cand_mod)
dev.off()

# Run Models after pinks --------------------------------------------------

# Set up vector of possible vectors
possible_variables <- c("s(q, k = 4, by = Species, bs = 'cr')", 
                        "s(Biomass.drift, k = 4, bs = 'cr')")

# Control Variables
control_variables <- "Biomass ~ s(temp, bs = 'cr', k = 4) + s(TL, by = Species,  k = 4, bs = 'cr')+"

# run function to evaluate pink present models
pinks_pres <- model_selex(possible_variables = possible_variables,
                           control_variables = control_variables,
                           df = allsp_pinks_pres, error_dist = tw(link = "log"))

# Get model statistics from selection process
model_stats_pinks_pres <- pinks_pres[[2]]

# From the above, let's select model 1 as our candidate model
pinks_pres_cand_mod <- pinks_pres[[1]]$`model 1`

# Look at summary of this
summary(pinks_pres_cand_mod)

# Plot diagnostics
par(mfrow = c(2,2))
gam.check(pinks_pres_cand_mod)
abline(0,1, col = "red")

# plot these out
par(mfrow = c(3,2))
plot(pinks_pres_cand_mod)
dev.off()

# Plot q out! --------------------------------------------------------

# Our before and after pink models
# pinks_pres_cand_mod
# beforepinks_cand_mod

# Our respective datasets for this
# allsp_pinks_pres
# allsp_pinks_abs
#' @param Model = Model we want to use
#' @param cov = what type of prediction/covariate we want to make preds w/ (q_x_sp, temp, and TL)
#' @param Species = What species we want to predict for
#' @param Model_Type = What model type? Pinks Present vs Absent
#' @param data = Which dataset we want to predict onto
flow_df_int <- function(Model, Species, Model_Type, data,
                        cov) {
  # Create a new dataframe to predict onto
  if(cov == "q_x_sp") {
    df <- with(data,
               data.frame(temp = mean(temp),
                          TL = mean(TL), q = q,
                          Species = Species,
                          Biomass = Biomass,
                          Biomass.drift = mean(Biomass.drift, na.rm = TRUE),
                          Model_Type = Model_Type))
  } # if q intearction w/ species is true
  
  if(cov == "temp") {
    df <- with(data,
               data.frame(temp = temp,
                          TL = mean(TL), q = mean(q),
                          Species = "Coho",
                          Biomass = Biomass,
                          Biomass.drift = mean(Biomass.drift, na.rm = TRUE),
                          Model_Type = Model_Type))
  }
  
  if(cov == "TL") {
    df <- with(data,
               data.frame(temp = mean(temp),
                          TL = TL, q = mean(q),
                          Species = Species,
                          Biomass = Biomass,
                          Biomass.drift = mean(Biomass.drift, na.rm = TRUE),
                          Model_Type = Model_Type))
  }
  
  if(cov == "biom_drift") {
    df <- with(data,
               data.frame(temp = mean(temp),
                          TL = mean(TL), q = mean(q),
                          Species = "Coho",
                          Biomass = Biomass,
                          Biomass.drift = Biomass.drift,
                          Model_Type = Model_Type))
  }
  
  if(cov == "doy") {
    df <- with(data,
               data.frame(temp = mean(temp),
                          TL = mean(TL), q = mean(q),
                          Species = "Coho",
                          Biomass = Biomass,
                          Biomass.drift = Biomass.drift,
                          Model_Type = Model_Type))
  }
  
  # Make predictions with this
  predictions <- predict(Model, newdata = df,
                       type = "link", se.fit = TRUE)
  # Create df
  pred_df <- data.frame(preds = predictions$fit, se.fit = predictions$se.fit)
  
  # Bind with observed
  pred_df <- cbind(pred_df, df)

  # Bind these together
  # Create 95% confidence intervals for this
  pred_df <- pred_df %>% 
    mutate(fitted = exp(preds),
           lower = exp(preds - (1.96*se.fit)),
           upper = exp(preds + (1.96*se.fit)))
  
  return(pred_df)
  
} # end function


### Flow --------------------------------------------------------------------

# Make prediction dataframes for flow q interaction w/ species

# Coho Present Pink
pres_coho <- flow_df_int(Model = pinks_pres_cand_mod, Species = "Coho",
              Model_Type = "Pinks Present", data = allsp_pinks_pres,
              cov = "q_x_sp")

# Dolly Present Pink
pres_dolly <- flow_df_int(Model = pinks_pres_cand_mod, Species = "Dolly Varden",
                         Model_Type = "Pinks Present", data = allsp_pinks_pres,
                         cov = "q_x_sp")
# Coho Present Pink
abs_coho <- flow_df_int(Model = beforepinks_cand_mod, Species = "Coho",
                         Model_Type = "Pinks Absent", data = allsp_pinks_abs,
                        cov = "q_x_sp")

# Dolly Present Pink
abs_dolly <- flow_df_int(Model = beforepinks_cand_mod, Species = "Dolly Varden",
                          Model_Type = "Pinks Absent", data = allsp_pinks_abs,
                         cov = "q_x_sp")

# Now bind all of these together for plotting purposes
q_flow_species_int <- rbind(pres_coho, pres_dolly, abs_coho, abs_dolly)

# Plot w/ points
ggplot(q_flow_species_int, aes(x = q, y = fitted, ymin = lower,
                               ymax = upper, color = Species, fill = Species)) +
  geom_jitter(aes(x = q, y = Biomass), alpha = 0.3) +
  geom_line(size = 1.5) +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~Model_Type, scales = "free") +
  theme_bw() +
  theme(legend.position = "top")+
  scale_color_colorblind() +
  scale_fill_colorblind() +
  labs(x = "Flow (q)", y = "Biomass")

# Plot w/o points
Flow.biomass.plot <- ggplot(q_flow_species_int, aes(x = q, y = fitted, ymin = lower,
                               ymax = upper, color = Species, fill = Species)) +
  geom_line(size = 1.5) +
  geom_ribbon(alpha = 0.3) +
  geom_vline(xintercept = 1.32, linetype = "dashed", 
            color = "black", size = .75)+
  geom_vline(xintercept = 2.7, linetype = "dashed", 
            color = "black", size = .75)+
  facet_wrap(~Model_Type) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  labs(x = "Mean 12-Hour Discharge (cms)", y = "Mean Biomass Consumed (mg)")+
  # coord_cartesian(ylim = c(0,107))+
  ylim(0, 122)+
  theme_classic()+
  theme(legend.position = "top", 
        strip.text.x = element_blank(), 
        plot.title = element_blank(), 
        axis.line = element_line(),
        panel.background = element_rect(colour = "black", size=.5),
        text = element_text (size = 20))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

### Temp --------------------------------------------------------------------

pres_temp <- flow_df_int(Model = pinks_pres_cand_mod, 
                         Model_Type = "Pinks Present", data = allsp_pinks_pres,
                         cov = "temp")

abs_temp <- flow_df_int(Model = beforepinks_cand_mod,
                        Model_Type = "Pinks Absent", data = allsp_pinks_abs,
                        cov = "temp")

# Bind these together
temp_df <- rbind(pres_temp, abs_temp)

# Plot w/ points
ggplot(temp_df, aes(x = temp, y = fitted, ymin = lower,
                               ymax = upper)) +
  geom_jitter(aes(x = temp, y = Biomass), alpha = 0.3) +
  geom_line(size = 1.5) +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~Model_Type, scales = "free") +
  theme_bw() +
  theme(legend.position = "top")+
  scale_color_colorblind() +
  scale_fill_colorblind() +
  labs(x = "Temperature (°C)", y = "Dry-Biomass Consumed (mg)")

# Plot w/o points
Temp.plot <- ggplot(temp_df, aes(x = temp, y = fitted, ymin = lower,
                    ymax = upper)) +
  geom_line(size = 1.5) +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~Model_Type, scales = "free") +
  theme_bw() +
  theme(legend.position = "top")+
  scale_color_colorblind() +
  scale_fill_colorblind() +
  labs(x = "Mean 12-hour Temp. (°C)", y = "Mean Biomass Consumed (mg)")+
  theme_classic()+
  ylim(0, 100)+
  theme(legend.position = "top", 
        plot.title = element_blank(), 
        axis.line = element_line(),
        panel.background = element_rect(colour = "black", size=.5),
        text = element_text (size = 20))


### TL ----------------------------------------------------------------------

pres_TL <- flow_df_int(Model = pinks_pres_cand_mod,
                         Model_Type = "Pinks Present", data = allsp_pinks_pres,
                         cov = "TL")

abs_TL <- flow_df_int(Model = beforepinks_cand_mod,
                        Model_Type = "Pinks Absent", data = allsp_pinks_abs,
                        cov = "TL")


# Bind these together
tl_df <- rbind(pres_TL, abs_TL)

# Plot w/ points
ggplot(tl_df, aes(x = TL, y = fitted, ymin = lower,
                    ymax = upper, fill = Species, color = Species)) +
  geom_jitter(aes(x = TL, y = Biomass), alpha = 0.3) +
  geom_line(size = 1.5) +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~Model_Type, scales = "free") +
  theme_bw() +
  theme(legend.position = "top")+
  scale_color_colorblind() +
  scale_fill_colorblind() +
  labs(x = "TL", y = "Biomass")

# Plot w/o points
TL.plot <- ggplot(tl_df, aes(x = TL, y = fitted, ymin = lower,
                  ymax = upper, fill = Species, color = Species)) +
  geom_line(size = 1.5) +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~Model_Type, scales = "free_y") +
  theme(legend.position = "top")+
  scale_color_colorblind() +
  scale_fill_colorblind() +
  labs(x = "Total Length (mm)",y = "Mean Biomass Consumed (mg)")+
  theme_classic()+
 # ylim(0,190)+
  coord_cartesian(ylim = c(0,100))+
  theme(legend.position = c(.075,.875), 
        plot.title = element_blank(), 
        axis.line = element_line(),
        panel.background = element_rect(colour = "black", size=.5),
        text = element_text (size = 20))

### Predict daily consumptive values given flow, temp and TL -------------------


# Figures  ---------------------------------------------------------------------

### Diet composition and model plot code ---------------------------------------

# Read in data frames
coho.dd <- read_csv('Raw Coho Diet Data Final.csv')
dolly.dd <- read_csv('Raw Dolly Diet Data Final.csv')

# create a data frame which includes prey types and species information
diet.prop.coho <- coho.dd %>% 
  filter(Date >= as_date("2021-05-05")) %>% 
  group_by(Date, Origin) %>% 
  summarize(Biomass = sum(Biomass, na.rm = TRUE)) %>%
  drop_na() %>% 
  pivot_wider(names_from = "Origin", values_from = "Biomass") %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  rowwise() %>% 
  summarize(total_biomass = sum(Aquatic, Terrestrial, Marine, Marine.pisces, na.rm = TRUE),
            Freshwater = Aquatic,
            Terrestrial = Terrestrial,
            Marine = Marine,
            Piscine = Marine.pisces) %>% 
  pivot_longer(!c(Date, total_biomass), values_to = "Biomass",
               names_to = "Origin")

diet.prop.dolly <- dolly.dd %>% 
  filter(Date >= as_date("2021-05-05")) %>% 
  group_by(Date, Origin) %>% 
  summarize(Biomass = sum(Biomass, na.rm = TRUE)) %>%
  drop_na() %>% 
  pivot_wider(names_from = "Origin", values_from = "Biomass") %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  rowwise() %>% 
  summarize(total_biomass = sum(Aquatic, Terrestrial, Marine, Marine.pisces, na.rm = TRUE),
            Freshwater = Aquatic,
            Terrestrial = Terrestrial,
            Marine = Marine,
            Piscine = Marine.pisces) %>% 
  pivot_longer(!c(Date, total_biomass), values_to = "Biomass",
               names_to = "Origin")


# Look at the dataframes
diet.prop.coho 
diet.prop.dolly

# Join the physical data

diet.prop.coho <- diet.prop.coho %>% 
  left_join(samp.points, by = c("Date" = "nohms_date"))

diet.prop.dolly <- diet.prop.dolly %>% 
  left_join(samp.points, by = c("Date" = "nohms_date"))

#Add species info to data frames

diet.prop.coho <- diet.prop.coho %>% 
  mutate(Species = 'Coho')

diet.prop.dolly <- diet.prop.dolly %>% 
  mutate(Species = 'Dolly Varden')

# bind species dataframes together
diet.prop.allsp <- rbind(diet.prop.coho, diet.prop.dolly)

# Plot these out by doy

diet.prop.time.series <- diet.prop.allsp %>%
  group_by(Origin, Species,Date) %>% 
  summarize(Biomass = sum(Biomass, na.rm = TRUE)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  group_by(Species, Date) %>% 
  mutate(total.biomass = sum(Biomass))%>% 
  mutate(Proportion = Biomass/total.biomass)

diet.prop.coho.time.series <- diet.prop.time.series %>% filter(Species == "Coho")
diet.prop.dolly.time.series <- diet.prop.time.series %>% filter(Species == "Dolly Varden")

coho.g <- coho.df %>% filter(!is.na(Weight)) %>% group_by(Date) %>% mutate (daily.fish.mass = sum(Weight)) %>% 
  dplyr::select(Date, daily.fish.mass) %>% 
  filter(!duplicated(daily.fish.mass))

dolly.g <- dolly.df %>% filter(!is.na(Weight)) %>% group_by (Date) %>% mutate (daily.fish.mass = sum(Weight)) %>% 
  dplyr::select(Date, daily.fish.mass) %>% 
  filter(!duplicated(daily.fish.mass))

diet.prop.coho.time.series <- diet.prop.coho.time.series  %>% left_join(coho.g %>% select(daily.fish.mass), by = ('Date'))
diet.prop.dolly.time.series <- diet.prop.dolly.time.series %>% left_join(dolly.g %>% select(daily.fish.mass), by = ('Date'))

diet.prop.coho.time.series <- diet.prop.coho.time.series %>% mutate(prop.cons.mass = (Biomass/1000)/daily.fish.mass)
diet.prop.dolly.time.series <- diet.prop.dolly.time.series %>% mutate(prop.cons.mass= (Biomass/1000)/daily.fish.mass)

mycols.1 <- c('steelblue3', 'tan3', 'Gray50','palegreen4')

diets.time.series.1 <- ggplot(data = diet.prop.time.series, aes(x = Date, y = Proportion, fill = Origin))+
  geom_area(position = 'stack') +
  theme_classic()+
  scale_fill_manual(name = "", values = mycols.1)+
  theme(legend.position = 'top') +
  facet_wrap(~ Species, ncol = 1)

Coho.diet.trends <- ggplot(data = diet.prop.coho.time.series, aes(x = Date, y = prop.cons.mass, fill = Origin))+
  geom_area(position = 'stack', color = 'black') +
  theme_classic()+
  scale_fill_manual(name = "", values = mycols.1)+
  theme(legend.position = 'none')+
  ylab('Proportional Biomass (g/g)')+
  theme( axis.title.x = element_blank(),
        text = element_text(size = 14),
        axis.title.y = element_text(vjust = 2.5),
        axis.title.y.right = element_text(color = 'white', vjust = 1, size = 46.1),
        axis.ticks.y.right = element_line(color = 'white'),
        axis.text.y.right = element_blank())+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  scale_y_continuous(sec.axis = sec_axis(~./58, name =bquote("S")))+
  coord_cartesian(ylim = c(0,0.05))+
    facet_wrap(~ Species, ncol = 1)


Dolly.diet.trends <- ggplot(data = diet.prop.dolly.time.series, aes(x = Date, y = prop.cons.mass, fill = Origin))+
  geom_area(position = 'stack', color = 'black') +
  theme_classic()+
  scale_fill_manual(name = "", values = mycols.1)+
  theme(legend.position = c(.1, .6))+
  ylab('Proportional Biomass (g/g)')+
    theme(text = element_text(size = 14),
          axis.title.y = element_text(vjust = 2.5),
          axis.title.y.right = element_text(color = 'white', vjust = 1, size = 46.1),
          axis.ticks.y.right = element_line(color = 'white'),
          axis.text.y.right = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())+
   facet_wrap(~ Species, ncol = 1)+
  scale_y_continuous(sec.axis = sec_axis(~./58, name =bquote("")))+
   scale_x_date(date_labels = "%b", date_breaks = "1 month", limits = as.Date(c('2021-05-05','2021-10-28')))


# separate by pinks present or absent and assign 'bin' based on flow values

diet.prop.pinks.pa <- diet.prop.allsp %>% 
  mutate(Pinks.pres.abs = if_else(Date < '2021-07-29', 'Before Pink Salmon Pulse','During/After Pink Salmon Pulse'))

# group by flow values 
diet.prop.allsp.flow <- diet.prop.pinks.pa %>% mutate(Flow.level = ifelse(q < 1.5 ,'low',
                                                                 ifelse(q >= 1.5 & q < 2.5, "mid", 'high')),
                                                                 Flow.level = factor(Flow.level, level = c ('low', 'mid', 'high'),
                                                                 labels = c('low-flow', 'moderate-flow', 'high-flow')))
# add proportions of prey taxa to dataframes 

diet.prop.final <- diet.prop.allsp.flow %>%
  group_by(Flow.level, Origin, Species, Pinks.pres.abs) %>% 
  summarize(Biomass = sum(Biomass, na.rm = TRUE)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  group_by(Flow.level, Species, Pinks.pres.abs) %>% 
  mutate(total.biomass = sum(Biomass))%>% 
  mutate(Proportion = Biomass/total.biomass)

diet.prop.final <- diet.prop.final %>% mutate(Dummy = '1')


diet.prop.final <- diet.prop.final %>% mutate(Species = factor(Species, level = c('Coho', 'Dolly Varden'),
                            labels = c('Coho', 'Dolly')))


diet.prop.final.real <- diet.prop.final %>%  mutate(Origin = ifelse(Origin == 'Piscine', 'Marine', Origin))
  
diet.prop.final.legend <- diet.prop.final %>%  mutate(Origin = ifelse(Origin == 'Marine.Piscine', 'Marine', Origin))

# Create the stacked bar plot for the composition by flow 

mycols <- c('steelblue3', 'tan3','palegreen4', 'Gray50')

 Diet.comp.plot <- ggplot(diet.prop.final.real, aes(x = Dummy, y = Proportion , fill = Origin))+
  geom_col(position = position_dodge(1.0), color = 'black')+
  scale_fill_manual(name = "", values = mycols)+
  facet_nested(~ Pinks.pres.abs + Flow.level + Species)+
  theme(legend.position = 'top', 
        panel.background = element_rect(fill = 'grey93'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 18), 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(color = 'black'),
        axis.line.x = element_line(color = 'black'),
        strip.background.x = element_rect(fill= 'white', color = 'black'),
        text = element_text (size = 20))+
  easy_remove_x_axis()+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=1, y=1.01, label="Stretch it"), vjust=-1)+
  ylab('Prey Composition')+
  guides(fill = guide_legend(title="Prey Type"))
 
 Diet.comp.plot.legend <- ggplot(diet.prop.final.fake, aes(x = Dummy, y = Proportion , fill = Origin))+
   geom_col(position = position_dodge(1.0), color = 'black')+
   scale_fill_manual(name = "", values = mycols.1)+
   facet_nested(~ Pinks.pres.abs + Flow.level + Species)+
   theme(legend.position = 'top', 
         panel.background = element_rect(fill = 'grey93'),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.text.x = element_text(size = 18), 
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.line.y = element_line(color = 'black'),
         axis.line.x = element_line(color = 'black'),
         strip.background.x = element_rect(fill= 'white', color = 'black'),
         text = element_text (size = 20))+
   easy_remove_x_axis()+
   scale_y_continuous(expand = c(0,0))+
   geom_text(aes(x=1, y=1.01, label="Stretch it"), vjust=-1)+
   ylab('Prey Composition')+
   guides(fill = guide_legend(title="Prey Type"))
 
## Create the same stacked bar charts using a single data frame

# Add Pink Salmon Presence/Absence as a factor
diet.allsp <- diet.prop.allsp %>% 
  mutate(Pinks.pres.abs = if_else(Date < '2021-07-29', 'Pink Salmon Absent','Pink Salmon Present'))

# Add flow level as a factor
dp.allsp <- diet.allsp %>% mutate(Flow.level = ifelse(q < 1.5 ,'low',
                           ifelse(q >= 1.5 & q < 2.5, "mid", 'high')),
                           Flow.level = factor(Flow.level, level = c ('low', 'mid', 'high'),
                                               labels = c('low-flow', 'moderate-flow', 'high-flow')))

# Add proportions of prey biomass
dp.allsp <- dp.allsp %>% mutate(Species = factor(Species, level = c('Coho', 'Dolly Varden'),
                                                 labels = c('Coho', 'Dolly')))
dp.allsp <- dp.allsp %>% mutate(Dummy = '1')

# Plot using the single data frame
diet.props <- ggplot(dp.allsp, aes(x = Dummy, 
                                   y = Biomass, 
                                   fill = Origin, width = 1))+
  geom_bar(stat = 'identity',
           position = "stack") +
  theme_bw() +
  scale_fill_manual(name = "", values = mycols) +
  theme(legend.position = 'top') +
  #facet_grid(~ factor(Pinks.pres.abs, level = c ('Absent', 'Present')) + Flow.level,
  #           labeller = function(df) {
  #             list(as.character(df[,2]))
  #           }) +
  facet_nested(~ Pinks.pres.abs + Flow.level + Species)+
  # xlab('')+
  ylab('Prey Composition')+
  theme_classic()+
  theme(legend.position = 'top', 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_textbox_simple(
          padding = margin(4, 0, 4, 0),
          margin = margin(1, 0, 1, 0),
          size = 11,
          halign = 0.5,
          fill = "white",
          box.color = "black",
          linewidth = .75,
          linetype = "solid"))+
  easy_remove_x_axis()+
  guides(fill = guide_legend(title="Prey Type"))
  

# Stack the flow relationship plots with diet summary figures

# legend.1 <- get_legend(Flow.biomass.plot)
# legend.2 <- get_legend(diet.props)
  
# diet.plots <- ggarrange(diet.props, Flow.biomass.plot + theme(legend.position = c(.075,.9)),
#                         ncol = 1, nrow = 2, align = 'h', 
#                          common.legend = FALSE, legend="none", heights = c(0.4, 0.6))

# diet.legends <- ggarrange(legend.1, legend.2, nrow = 1)


# diet.figure.2 <- ggarrange(diet.legends, diet.plots, ncol = 1, nrow = 2, 
#                       heights = c(0.075, 0.925))

### Figure 3. Model flow output and Diet Composition ---------------------------

Gam.figure <- plot_grid(Diet.comp.plot, 
                         Flow.biomass.plot + theme(legend.position = c(.075,.875)), 
                         ncol = 1, nrow = 2, align = 'hv', axis = 'l',
                         rel_heights = c(0.4, 0.6))

Gam.figure.legend <- plot_grid(Diet.comp.plot.legend, 
                         Flow.biomass.plot + theme(legend.position = c(.075,.875)), 
                         ncol = 1, nrow = 2, align = 'hv', axis = 'l',
                         rel_heights = c(0.4, 0.6))

### GAM temp output --------------------------------------------------

Temp.plot

### GAM TL output ----------------------------------------------------

TL.plot

### Stomach fullness figure code -----------------------------------------------

# create a dataframe for this
avg.sf.allsp <- allsp_df %>% select(Date, Species,stomach.fullness) %>% 
  group_by(Date, Species) %>% drop_na() %>% 
  summarise(mean.stomach.fullness = mean(stomach.fullness),n=n(),
            lower.bound = mean.stomach.fullness - 1.96*(sd(stomach.fullness)/sqrt(n)),
            upper.bound = mean.stomach.fullness + 1.96*(sd(stomach.fullness)/sqrt(n)))

avg.sf.allsp <- avg.sf.allsp %>% drop_na(mean.stomach.fullness)

avg.e.allsp <- allsp_df %>% select(Date, Species, energy.cons.g) %>% 
  group_by(Date, Species) %>% drop_na() %>% 
  summarise(mean.energy.cons = mean(energy.cons.g),n=n(),
            lower.bound = mean.energy.cons - 1.96*(sd(energy.cons.g)/sqrt(n)),
            upper.bound = mean.energy.cons + 1.96*(sd(energy.cons.g)/sqrt(n)))

avg.e.allsp <- avg.e.allsp %>% drop_na(mean.energy.cons)

dates_vline.1 <- as.Date(c("2021-07-29", "2021-09-14"))# Define positions of vline
dates_vline.1 <- which(avg.sf.allsp$Date %in% dates_vline.1)

dates_vline.2 <- as.Date(c("2021-10-28"))
dates_vline.2 <- which(avg.sf.allsp$Date %in% dates_vline.2)


sf_xmin <- as.Date("2021-07-29")
sf_xmax <- as.Date("2021-10-28")


Avg.sf.plot <- ggplot()+
         #geom_rect(aes(xmin = sf_xmin, xmax = sf_xmax, ymin = -Inf, ymax = Inf),alpha = .2, color = 'grey75', fill = 'grey75')+
         #geom_vline(xintercept = as.numeric(avg.sf.allsp$Date[dates_vline.1]), linetype = "longdash", 
         #    color = "Deep Pink 3", fill = 'Deep Pink 3',  size = .75, alpha = 1)+
        # geom_point(Spawn.density, mapping = aes(x = Date, y = 1.5*Est..Spawner.Density), color = 'Deep Pink 3', size = 3.5, shape = 8, stroke = 1.25)+
        # geom_line(Spawn.density, mapping = aes(x = Date, y = 1.5*Est..Spawner.Density), color = 'Deep Pink 3', size = .35, linetype = 'solid', alpha = 1)+
         geom_line(avg.sf.allsp, mapping = aes(x = Date, y = mean.stomach.fullness, color = Species),size = 1.5)+
         geom_point(avg.sf.allsp, mapping = aes(x = Date, y = mean.stomach.fullness, color = Species),size = 3.0)+
         #geom_errorbar(data = avg.sf.allsp, mapping = aes(x = Date, 
         #              y = mean.stomach.fullness,ymin = lower.bound, 
         #              ymax = upper.bound, color = Species, width = 0),inherit.aes = T)+
         scale_y_continuous(name = 'Mean Stomach Fullness (g/g)', sec.axis = sec_axis(~./1.5, name =bquote( "Spawning Density (fish/"~m^2~')')))+
         scale_color_colorblind()+
         scale_fill_colorblind()+
         theme_classic()+
         scale_x_date(date_labels = "%b", date_breaks = "1 month")+
         theme(legend.position = c(.1,.6),
               text = element_text (size = 14),
               axis.title.x = element_blank(),
               axis.title.y = element_text(vjust = 1),
               axis.title.y.right = element_text(color = 'white', size = 10),
               axis.ticks.y.right = element_line(color = 'white'),
               axis.text.y.right = element_text(color = 'white'),
               axis.ticks.x = element_blank(),
               axis.text.x = element_blank()
               )

# Create and energetic composition figure

Avg.e.plot <- ggplot()+
  geom_rect(aes(xmin = sf_xmin, xmax = sf_xmax, ymin = -Inf, ymax = Inf),alpha = .2,color = 'darkgrey', fill = 'darkgrey')+
  geom_line(avg.e.allsp, mapping = aes(x = Date, y = mean.energy.cons, color = Species),size = 1.5)+
  geom_point(avg.e.allsp, mapping = aes(x = Date, y = mean.energy.cons, color = Species),size = 4.0)+
  
  #geom_errorbar(data = avg.sf.allsp, mapping = aes(x = Date, 
  #              y = mean.stomach.fullness,ymin = lower.bound, 
  #              ymax = upper.bound, color = Species, width = 0),inherit.aes = T)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  theme_classic()+
  labs(y = "Mean Energetic Consumption (KJ/g)")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  theme(legend.position = c(.1,.875),
        text = element_text (size = 20))+
  geom_vline(xintercept = as.numeric(avg.sf.allsp$Date[dates_vline.1]), linetype = "solid", 
             color = "darkgrey", fill = 'darkgrey',  size = .75, alpha = .7)

        
# create MDQ plot

dates_vline <- as.Date(c("2021-07-29", "2021-10-28"))# Define positions of vline
dates_vline <- which(Mean.Daily.Q$date %in% dates_vline)

dates_vline.3 <- as.Date(c("2021-10-28"))
dates_vline.3 <- which(avg.sf.allsp$Date %in% dates_vline.2)

mdq_xmin <- as.Date("2021-07-29")
mdq_xmax <- as.Date("2021-10-28")

Spawn.density$Date <- ymd(Spawn.density$Date)
  
  
(MDQ.plot<- ggplot() + 
  geom_rect(aes(xmin = mdq_xmin, xmax = mdq_xmax, ymin= -Inf, ymax=Inf), alpha = .2, color = 'grey75', fill = 'grey75')+
  geom_vline(xintercept = as.numeric(avg.sf.allsp$Date[dates_vline.1]), linetype = "longdash", 
               color = "Deep Pink 3", fill = 'Deep Pink 3',  size = .75, alpha = 1)+
  geom_point(Spawn.density, mapping = aes(x = Date, y = 58*Est..Spawner.Density), color = 'Deep Pink 3', size = 3.5, shape = 8, stroke = 1.25)+
  geom_line(Spawn.density, mapping = aes(x = Date, y = 58*Est..Spawner.Density),color = 'Deep Pink 3', size = .35, linetype = 'solid', alpha = 1)+
  theme_classic()+
  geom_line(Mean.Daily.Q, mapping = aes(x = date, y = Q), size = 1.1, color = 'black')+
  scale_y_continuous(name = 'Discharge (cms)', sec.axis = sec_axis(~./58, name = bquote("Spawning Density (fish /"~ m^2~')')))+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  #geom_hline(yintercept = 1.25, linetype = "dashed", 
  #           color = "black", size = .75) +
  #geom_hline(yintercept = 2.5,linetype = "dashed", 
  #           color = "black", size = .75)+
  theme(legend.position = c(.1,.8),
         axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_text(vjust = 2.5),
         axis.title.y.right = element_text(vjust = 1.5),
         text = element_text (size = 14)))

# Pink Salmon Spawning Density
ss.dens <- ggplot() + 
  geom_point(Spawn.density, mapping = aes(x = Date, y = 58*Est..Spawner.Density), color = 'Deep Pink 3', size = 3.5, shape = 8, stroke = 1.25)+
  geom_line(Spawn.density, mapping = aes(x = Date, y = 58*Est..Spawner.Density),color = 'Deep Pink 3', size = .35, linetype = 'solid', alpha = 1)+
  theme_classic()+
  ylab("Pink Salmon Spawning Density")


### Figure 2. Stomach Fullness Figure ------------------------------------------------------

cons.plots <- plot_grid(Avg.sf.plot, Coho.diet.trends, ncol = 1, nrow = 2, align = 'hv', 
                        axis = 'l', rel_heights = c(0.5, 0.5))

figure.1 <- plot_grid( MDQ.plot, Avg.sf.plot, Dolly.diet.trends, Coho.diet.trends, ncol = 1, nrow = 4, align = 'hv', 
                       axis = 'l', rel_heights = c(0.5, 0.5))


# Proportion vs. doy plots -----------------------------------------------------

diet.props.coho <- coho.d %>% 
  group_by(Date, Origin) %>% 
  summarize(Biomass = sum(Biomass, na.rm = TRUE)) %>%
  drop_na() %>% 
  pivot_wider(names_from = "Origin", values_from = "Biomass") %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  rowwise() %>% 
  summarize(total_biomass = sum(Aquatic, Terrestrial, Marine, na.rm = TRUE),
            Freshwater = Aquatic/total_biomass,
            Terrestrial = Terrestrial/total_biomass,
            Marine = Marine/total_biomass) %>% 
  pivot_longer(!c(Date, total_biomass), values_to = "prop",
               names_to = "Origin")

diet.props.coho <- diet.props.coho %>% mutate(Date = mdy(Date))


diet.props.dolly <- dolly.d %>% 
  group_by(Date, Origin) %>% 
  summarize(Biomass = sum(Biomass, na.rm = TRUE)) %>%
  drop_na() %>% 
  pivot_wider(names_from = "Origin", values_from = "Biomass") %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  rowwise() %>% 
  summarize(total_biomass = sum(Aquatic, Terrestrial, Marine, na.rm = TRUE),
            Freshwater = Aquatic/total_biomass,
            Terrestrial = Terrestrial/total_biomass,
            Marine = Marine/total_biomass)%>% 
  pivot_longer(!c(Date, total_biomass), values_to = "prop",
               names_to = "Origin")

diet.props.dolly <- diet.props.dolly %>% mutate(Date = mdy(Date))

coho.prop.plot <- ggplot(diet.props.coho, aes(x = Date, y = prop , fill = Origin))+
  geom_col(position = "stack") +
  theme_classic() +
  theme(legend.position = 'top')+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.text.x = element_blank(),
        legend.text=element_text(size= 14))+
  scale_fill_manual(name = "Prey Type", labels = c("Freshwater", "Marine", 'Terrestrial'), values = mycols)+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(y = 'Prey Compostion', x = '')

dolly.prop.plot <- ggplot(diet.props.dolly, aes(x = Date, y = prop , fill = Origin))+
  geom_col(position = "stack") +
  theme_classic() +
  theme(legend.position = 'top')+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.text.x = element_blank(),
        legend.text=element_text(size= 14))+
  scale_fill_manual(name = "Prey Type", labels = c("Freshwater", "Marine", 'Terrestrial'), values = mycols)+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(y = 'Prey Compostion', x = '')



### Exceedance Plot figure code ----------------------------------------------------------

years.MDQ <- read.csv('Montana_multi-year_mdq.csv')

Ordered.MDQ.13 <- years.MDQ %>% arrange(desc(Y13))
Ranked.MDQ.13 <- Ordered.MDQ.13 %>% mutate(., Rank = seq(nrow(.)))
Ranked.MDQ.Probs <- Ranked.MDQ.13 %>% mutate(P.13 = 100 *(Rank/(184 + 1)))

Ordered.MDQ.14 <- Ranked.MDQ.Probs %>% arrange(desc(Y14))
Ranked.MDQ.14 <- Ordered.MDQ.14 %>% mutate(., Rank = seq(nrow(.)))
Ranked.MDQ.Probs <- Ranked.MDQ.14 %>% mutate(P.14 = 100 *(Rank/(184 + 1)))

Ordered.MDQ.15 <- Ranked.MDQ.Probs %>% arrange(desc(Y15))
Ranked.MDQ.15 <- Ordered.MDQ.15 %>% mutate(., Rank = seq(nrow(.)))
Ranked.MDQ.Probs <- Ranked.MDQ.15 %>% mutate(P.15 = 100 *(Rank/(184 + 1)))

Ordered.MDQ.16 <- Ranked.MDQ.Probs %>% arrange(desc(Y16))
Ranked.MDQ.16 <- Ordered.MDQ.16 %>% mutate(., Rank = seq(nrow(.)))
Ranked.MDQ.Probs <- Ranked.MDQ.16 %>% mutate(P.16 = 100 *(Rank/(184 + 1)))

Ordered.MDQ.17 <- Ranked.MDQ.Probs %>% arrange(desc(Y17))
Ranked.MDQ.17 <- Ordered.MDQ.17 %>% mutate(., Rank = seq(nrow(.)))
Ranked.MDQ.Probs <- Ranked.MDQ.17 %>% mutate(P.17 = 100 *(Rank/(169 + 1)))

Ordered.MDQ.18 <- Ranked.MDQ.Probs %>% arrange(desc(Y18))
Ranked.MDQ.18 <- Ordered.MDQ.18 %>% mutate(., Rank = seq(nrow(.)))
Ranked.MDQ.Probs <- Ranked.MDQ.18 %>% mutate(P.18 = 100 *(Rank/(184 + 1)))

Ordered.MDQ.19 <- Ranked.MDQ.Probs %>% arrange(desc(Y19))
Ranked.MDQ.19 <- Ordered.MDQ.19 %>% mutate(., Rank = seq(nrow(.)))
Ranked.MDQ.Probs <- Ranked.MDQ.19 %>% mutate(P.19 = 100 *(Rank/(184 + 1)))

Ordered.MDQ.20 <- Ranked.MDQ.Probs %>% arrange(desc(Y20))
Ranked.MDQ.20 <- Ordered.MDQ.20 %>% mutate(., Rank = seq(nrow(.)))
Ranked.MDQ.Probs <- Ranked.MDQ.20 %>% mutate(P.20 = 100 *(Rank/(184 + 1)))

Ordered.MDQ.21 <- Ranked.MDQ.Probs %>% arrange(desc(Y21))
Ranked.MDQ.21 <- Ordered.MDQ.21 %>% mutate(., Rank = seq(nrow(.)))
Ranked.MDQ.Probs <- Ranked.MDQ.21 %>% mutate(P.21 = 100 *(Rank/(184 + 1)))

head(Ranked.MDQ.Probs)

df_transpose <-Ranked.MDQ.Probs %>%
  mutate(observation_number = row_number()) %>%  #Add an observation number column
  gather(axis, value, - observation_number) %>%  #flatten the data
  mutate(group_var = str_extract(axis, "[0-9]+")) %>%  #grab numeric part of the axis name to use as a grouping variable
  mutate(axis = gsub('[0-9]+', '', axis)) %>%  #remove numbers from the axis names
  spread(axis, value) %>% #pivot the axis names back to columns again
  arrange(group_var, observation_number) %>% 
  select(group_var, P., Y, observation_number) %>% 
  drop_na() 
  
head(df_transpose)

exceedence.plot <- ggplot()+
  geom_hline(yintercept = .3, linetype = "dotted", 
             color = "black", size = .75) +
  geom_hline(yintercept = 1.0,linetype = "dashed", 
             color = "black", size = .75) +
  geom_hline(yintercept = 3.4, linetype = "dotted", 
             color = "black", size = .75) +
  geom_line(df_transpose %>% 
             filter(group_var != "21"), mapping = aes(x = as.numeric(P.), y = as.numeric(Y), group = group_var, color = group_var), size = 1.0)+
  geom_line(df_transpose %>% 
              filter(group_var == "21"), mapping = aes(x = as.numeric(P.), y = as.numeric(Y), group = group_var, color = group_var),
            color = "black", size = 1.0)+
#  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))+
  scale_y_continuous(limits = c(0, 7.75), breaks = seq(0, 7.75, by = 1))+
  scale_color_manual(labels = paste(unique_years),
                     values = c(hex_codes)) +
  labs(y = 'Mean Daily Discharge (cms)', x = 'Exceedance Pobability')+
  theme_bw() +
  theme(text = element_text (size = 20),
        legend.position = 'none') 
 # geom_segment(aes(x = 1.35, y = -Inf, xend = 1.35, yend = 2.5),
 #              size = .75, linetype = "dashed", color = 'black')+
 #geom_segment(aes(x = 4.0, y = -Inf, xend = 4.0, yend = 1.5),
 #             size = .75, linetype = "dashed")+
 # geom_segment(aes(x = 5.3, y = -Inf, xend = 5.30, yend = 2.5),
 #             size = .75, linetype = "dashed", color = 'black')+
 # geom_segment(aes(x = 17.9, y = -Inf, xend = 17.9, yend = 1.5),
 #              size = .75, linetype = "dashed", color = 'black')
  #geom_segment(aes(x = 16.0, y = -Inf, xend = 16.0, yend = 2.5),
  #             size = .75, linetype = "solid")+
#  geom_segment(aes(x = 39.75, y = -Inf, xend = 39.75, yend = 1.5),
#               size = .75, linetype = "dashed",  color = 'black')
#  


# Bioenergetic size predictions -------------------------------------------

names(Bio.e.age0) <- c("doy", paste (2013:2021))

Bio.e.age0.long <- Bio.e.age0 %>% pivot_longer(!`doy`, 
                                               names_to = 'year', 
                                               values_to = 'length') %>% 
                                 mutate(Age = 'Age-0')

(Age0.Size.Sims <- ggplot(Bio.e.age0.long, aes(x = `doy`, y = length, color = year))+
    geom_line(size = 1.0)+
    theme_classic()+
    labs(y = 'Age-1 Length (mm)', x = 'Day of Year')+
    theme(legend.position = 'none'))

names(Bio.e.age1) <- c("doy", paste (2013:2021))

Bio.e.age1.long <- Bio.e.age1 %>% pivot_longer(!`doy`, 
                                               names_to = 'year', 
                                               values_to = 'length') %>% 
                                  mutate(Age = 'Age-1')

Bio.e.age1.long <- Bio.e.age1.long %>%  filter(doy <= 212)

Bio.e.age1.long$Date = as.Date(Bio.e.age1.long$doy, origin = '2021-01-01')

(Age1.Size.Sims <- ggplot()+
    geom_hline(yintercept = 73, linetype = "dashed", 
               color = "black", size = .75, alpha = .95) +
    geom_line(Bio.e.age1.long %>% 
                filter(year != "2021"), mapping = aes(x = Date, y = length, group = year, color = year), size = 1.0)+
    geom_line(Bio.e.age1.long %>% 
                filter(year == "2021"), mapping = aes(x = Date, y = length, group = year, color = year),
              color = "black", size = 1.0)+
    scale_color_manual(labels = paste(unique_years),
                       values = c(hex_codes)) +
    geom_line(size = 1.0)+
    theme_bw()+
    labs(y = 'Simulated Age-1 Length (mm)', x = 'Date')+
    theme(legend.position = 'none',
          text = element_text (size = 20)))

Bio.e.all <- rbind(Bio.e.age0.long, Bio.e.age1.long)


(Age.all.Size.sims <- ggplot(Bio.e.all, aes(x = `doy`, y = length, color = year, shape = year))+
                  geom_line(size = 1.0)+
                  facet_wrap(~Age, scales = 'free')+
                  theme_bw())

# Predict consumption by flow across years -------------------------

pred_mdq_df <- years.MDQ %>%  # Pivot longer to easily change values to max of 2021 q
  pivot_longer(!Date, names_to = "year", values_to = "q") %>%
  mutate(q = ifelse(q >= max(allsp_pinks_abs$q), max(allsp_pinks_abs$q), q),
         Date = ymd(Date),
         doy = yday(Date)) %>%
 # group_by(Date) %>%
  mutate(med_q = median(q, na.rm = TRUE),
         q = ifelse(is.na(q), med_q, q)) %>%
  ungroup() %>% 
  # Now, pivot wider to get our prediction dataframe
  pivot_wider(names_from = "year", values_from = "q") %>%
  select( -med_q, -1) # Remove date col


# Rename columns for clarity
names(pred_mdq_df) <- c("doy", paste(2013:2021))


# Now, make predictions for coho for flow across years

# First, create a new dataframe to predict on with base values (hold to mean)
base_df <- with(allsp_pinks_abs,
                data.frame(temp = mean(temp),
                           TL = mean(TL),
                           Species = "Coho"))

# Create empty dataframe to store results in 
cons_store_df <- data.frame()

for(i in 1:(length(names(pred_mdq_df))-1)) {
  
  # Create a dataframe w/ q values to predict on for each individual year,
  # and combine with the above dataframe with base values
  newdata <- data.frame(base_df, q = pred_mdq_df[,i+1]) %>% 
    drop_na()# Drop nas out
  
  names(newdata)[4] <- "q" # Change col name from year to q
  
  # Now predict using the above dataframe
  cons_pred <- predict(beforepinks_cand_mod, newdata = newdata, type = "link", se.fit = TRUE)
  
  # Put these predictions into a dataframe
  cons_pred_df <- data.frame(pred = cons_pred$fit, se.fit = cons_pred$se.fit,
                             q = pred_mdq_df[,i+1][!is.na(pred_mdq_df[,i+1])] , year = names(pred_mdq_df)[i+1],
                             doy = pred_mdq_df[,1][!is.na(pred_mdq_df[,i+1])])
  
  # Now, rbind this dataframe to an empty one and save all results
  cons_store_df <- rbind(cons_store_df, cons_pred_df)
  
} # end i

# Now, create confidence intervals for these and plot them out
cons_store_df <- cons_store_df %>% 
  mutate(fitted = exp(pred),
         lower = exp(pred - (1.96*se.fit)),
         upper = exp(pred + (1.96*se.fit)))
  
cons_store_df<- cons_store_df %>% 
  group_by(year) %>% 
  summarize(cumsum = cumsum(pred),
            doy = doy) %>% filter(doy <= 212)

cons_store_df$Date = as.Date(cons_store_df$doy, origin = '2021-01-01')


# Get unique years from this
unique_years <- unique(cons_store_df$year)

# Get hex codes corresponding to this
hex_codes <- scales::hue_pal()(length(unique_years))

# Replace last year with black
hex_codes[9] <- "black"

Cumulative.Cons  <- ggplot() +
    geom_line(cons_store_df, mapping = aes(x = Date, y = cumsum, group = year, color = year), size = 1.0)+
    # geom_line(cons_store_df %>% 
    #             filter(year == "2021"), mapping = aes(x = doy, y = cumsum, group = year, color = year),
    #           color = "black", size = 1.0)+
  geom_line(size = 1.0)+
  scale_color_manual(labels = paste(unique_years),
                     values = c(hex_codes)) +
  theme_bw()+
  theme(text = element_text (size = 20),
        legend.position = c(.1,.6),
        legend.box.background = element_rect(colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
        )+
  labs(y = 'Predicted Cumulative Consumption (mg)', x = 'Day of Year')

write.csv(cons_store_df, "cons.preds.csv")

# Figure 4. Bioenergetic simualtion --------------------------------------------

Bio.e.final.preds <- plot_grid(Cumulative.Cons, Age1.Size.Sims,
                             ncol = 1, nrow =2, align = 'hv', axis = 'l')

Bio.e.final.fig <- plot_grid(exceedence.plot, Bio.e.final.preds, ncol = 2, nrow = 1)

