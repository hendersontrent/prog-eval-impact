#-----------------------------------------
# This script runs the Rmd code chunks
# to produce graphics for the website
# version of the document
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 31 January 2021
#-----------------------------------------

library(dplyr) # For data manipulation
library(magrittr) # For the '%>%' pipe operator
library(ggplot2) # For producing plots
library(scales) # For making plot axes pretty
library(tscount) # For simulating time series count data
library(mgcv) # For fitting generalised additive models
library(CausalImpact) # For fitting Byesian structural time series models for causal inference
library(changepoint) # For fitting changepoint models
library(bcp) # For fitting Bayesian changepoint models
library(Cairo)

if(!dir.exists('output')) dir.create('output')

#-------------------------- Simulate data --------------------------

#' Function to simulate indicative program evaluation data
#' @param t the number of timepoints to simulate
#' @return a dataframe with 2 columns [t,y] - where t = timepoint, y = response variable value
#' @author Trent Henderson
#'

simulation_engine <- function(t = 180){
  
  if(!is.numeric(t)){
    stop("t should be a single integer number specifying the number of timepoints to simulate.")
  }
  
  # Fix R's seed to ensure reproducibility
  
  set.seed(123)
  
  # Run simulation
  
  sim <- tsglm.sim(t, param = list(intercept = 7, past_obs = NULL, past_mean = NULL,
                                   xreg = NULL), link = c("log"), distr = c("poisson"), n_start = 50)
  
  # Convert to a dataframe
  
  ts_dat <- data.frame(t = c(1:t),
                       y = sim$ts)
  
  # Create a reusable value of where the program was introduced
  
  t_change <- as.integer(0.6*t)
  
  # Apply a linear trend component from the t_change cutpoint to give the visual look of a causal change
  
  ts <- ts_dat %>%
    mutate(y = ifelse(t >= t_change, (0.97*y), y),
           y = as.integer(y))
  
  return(ts)
}

# Run the function

ts <- simulation_engine(t = 180)

#-------------------------- Plotting -------------------------------

# Create a reusable value of where the program was introduced

t_change <- as.integer(0.6*nrow(ts))

# Draw plot

CairoPNG("output/plot-1.png", 800, 600)
ts %>%
  mutate(indicator = ifelse(t < t_change, "Pre-program introduction", "Post-program introduction")) %>% # Binary variable for whether a datapoint is pre-program introduction
  mutate(indicator = factor(indicator, levels = c("Pre-program introduction", "Post-program introduction"))) %>% # Makes sure our legend is in chronological order
  ggplot(aes(x = t, y = y)) +
  annotate(geom = "rect", xmin = t_change, xmax = max(ts$t), ymax = Inf, ymin = -Inf,
           fill = "#ececec", alpha = 0.6) + # Visual reference for when program was introduced
  geom_line(aes(colour = indicator), size = 1.25) +
  labs(title = "Raw time series plot",
       x = "Time",
       y = "Number of homeless people",
       colour = "Cutpoint") +
  scale_y_continuous(labels = comma) +
  scale_colour_manual(values = c("#88C9FA", "#F353D5")) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
dev.off()

CairoPNG("output/plot-2.png", 800, 600)
ts %>%
  ggplot(aes(x = t, y = y)) +
  annotate(geom = "rect", xmin = t_change, xmax = max(ts$t), ymax = Inf, ymin = -Inf,
           fill = "#ececec", alpha = 0.6) +
  geom_point(colour = "#BDC3CB", alpha = 0.4) +
  geom_smooth(formula = y ~ x, method = "glm",
              colour = "#88C9FA", fill = "#88C9FA", size = 1.25, 
              method.args = list(family = "poisson")) +
  labs(title = "Smoothed time series plot",
       x = "Time",
       y = "Number of homeless people") +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
dev.off()

CairoPNG("output/plot-3.png", 800, 600)
ts %>%
  mutate(indicator = ifelse(t < t_change, "Pre-program introduction", "Post-program introduction")) %>%
  mutate(indicator = factor(indicator, levels = c("Pre-program introduction", "Post-program introduction"))) %>%
  ggplot(aes(x = t, y = y)) +
  annotate(geom = "rect", xmin = t_change, xmax = max(ts$t), ymax = Inf, ymin = -Inf,
           fill = "#ececec", alpha = 0.6) +
  geom_point(colour = "#BDC3CB", alpha = 0.4) +
  geom_smooth(formula = y ~ x, method = "glm",
              aes(colour = indicator, fill = indicator), size = 1.25, 
              method.args = list(family = "poisson")) +
  labs(title = "Smoothed time series plot",
       x = "Time",
       y = "Number of homeless people") +
  scale_y_continuous(labels = comma) +
  scale_colour_manual(values = c("#88C9FA", "#F353D5")) +
  scale_fill_manual(values = c("#88C9FA", "#F353D5")) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
dev.off()

annual <- ts %>%
  mutate(year = case_when(
    t <= 1*12              ~ 2006,
    t > 1*12 & t <= 2*12   ~ 2007,
    t > 2*12 & t <= 3*12   ~ 2008,
    t > 3*12 & t <= 4*12   ~ 2009,
    t > 4*12 & t <= 5*12   ~ 2010,
    t > 5*12 & t <= 6*12   ~ 2011,
    t > 6*12 & t <= 7*12   ~ 2012,
    t > 7*12 & t <= 8*12   ~ 2013,
    t > 8*12 & t <= 9*12   ~ 2014,
    t > 9*12 & t <= 10*12  ~ 2015,
    t > 10*12 & t <= 11*12 ~ 2016,
    t > 11*12 & t <= 12*12 ~ 2017,
    t > 12*12 & t <= 13*12 ~ 2018,
    t > 13*12 & t <= 14*12 ~ 2019,
    t > 14*12              ~ 2020)) %>%
  mutate(indicator = ifelse(year < 2014, "Pre-program introduction", "Post-program introduction")) %>%
  mutate(indicator = factor(indicator, levels = c("Pre-program introduction", "Post-program introduction"))) %>%
  group_by(year, indicator) %>%
  summarise(y = sum(y)) %>%
  ungroup()

# Draw plot

CairoPNG("output/plot-4.png", 800, 600)
annual %>%
  ggplot(aes(x = year, y = y)) +
  annotate(geom = "rect", xmin = 2014, xmax = 2020, ymax = Inf, ymin = -Inf,
           fill = "#ececec", alpha = 0.6) +
  geom_line(aes(colour = indicator), size = 1.25) +
  labs(title = "Annual aggregation",
       x = "Year",
       y = "Number of homeless people",
       colour = "Cutpoint") +
  scale_x_continuous(breaks = seq(from = 2006, to = 2020, by = 1)) +
  scale_y_continuous(labels = comma) +
  scale_colour_manual(values = c("#88C9FA", "#F353D5")) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
dev.off()

CairoPNG("output/plot-5.png", 800, 600)
annual %>%
  ggplot(aes(x = year, y = y)) +
  annotate(geom = "rect", xmin = 2014, xmax = 2020, ymax = Inf, ymin = -Inf,
           fill = "#ececec", alpha = 0.6) +
  geom_point(colour = "#BDC3CB", alpha = 0.6) +
  geom_smooth(formula = y ~ x, method = "glm",
              aes(colour = indicator, fill = indicator), size = 1.25, 
              method.args = list(family = "poisson")) +
  labs(title = "Annual aggregation",
       x = "Year",
       y = "Number of homeless people",
       colour = "Cutpoint") +
  scale_x_continuous(breaks = seq(from = 2006, to = 2020, by = 1)) +
  scale_y_continuous(labels = comma) +
  scale_colour_manual(values = c("#88C9FA", "#F353D5")) +
  scale_fill_manual(values = c("#88C9FA", "#F353D5")) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
dev.off()

CairoPNG("output/plot-6.png", 800, 600)
ts %>%
  ggplot(aes(x = y)) +
  geom_density(fill = "#88C9FA", alpha = 0.8, colour = "black") +
  scale_x_continuous(labels = comma) +
  labs(title = "Distribution of response values",
       x = "Number of homeless people",
       y = "Density") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
dev.off()

CairoPNG("output/plot-7.png", 800, 600)
ts %>%
  mutate(indicator = ifelse(t < t_change, "Pre-program introduction", "Post-program introduction")) %>%
  mutate(indicator = factor(indicator, levels = c("Pre-program introduction", "Post-program introduction"))) %>%
  ggplot(aes(x = y)) +
  geom_density(aes(fill = indicator), alpha = 0.6, colour = "black") +
  labs(title = "Distribution of response values by cutpoint",
       x = "Number of homeless people",
       y = "Density",
       fill = "Cutpoint") +
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = c("#88C9FA", "#F353D5")) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
dev.off()

#-------------------------- Stat Modelling -------------------------

# Fit a changepoint model specifying the number of changepoints as 1 (we have no reason to believe otherwise)
fit_cp <- cpt.mean(ts$y, Q = 1)
# Plot the output
CairoPNG("output/plot-8.png", 800, 600)
plot(fit_cp)
dev.off()

# Fit a Bayesian changepoint model
fit_bcp <- bcp(ts$y)
# Plot the output
CairoPNG("output/plot-9.png", 800, 600)
plot(fit_bcp)
dev.off()

# Set up pre and post periods for the model
pre.period <- c(min(ts$t), t_change-1)
post.period <- c(t_change, max(ts$t))
# Get our data into the form of j1 = response variable, j2 = timeperiod which CausalImpact needs
tci <- ts %>%
  dplyr::select(c(y, t))
# Fit a CausalImpact model
impact <- CausalImpact(tci, pre.period, post.period)
# Return the automated plot
CairoPNG("output/plot-10.png", 800, 600)
plot(impact)
dev.off()
