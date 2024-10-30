##################################################################################################################################
##    
##      Assessing Variation in Margins of Pre-Election Swing State Polling
##      Josh Clinton
##      Vanderbilt University
##      October 2024
##
##      1) Analysing Swing State Distribution
##      2) Simulate effect of weightig on variability
##################################################################################################################################

set.seed(42)
library(tidyverse)
library(lubridate)
library(scales)

# Load most recent polling data from 538
# Note the graphs and numbers will change because of newly added polls

president_polls <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_polls.csv")

# Filter data for polls of likely voters in specified states
# Select likely voter polls (lv), but could also do registered voter (rv)
# Doing both seems like it may double count polls with both?

# Wrange the data to select Harris and Trump Vote and Margin in Sept/Oct

president_polls_lv <- president_polls %>%
  filter(hypothetical == FALSE,
         population %in% c("lv"),
         candidate_id %in% c(16661, 16651),  # Harris Trump
         state %in% c("Pennsylvania", "Michigan", "Wisconsin", "Arizona","North Carolina","Georgia","Nevada"))  # Filter to specific states

# Pivot wider to get separate columns for each candidate's percentage
president_polls_wide <- president_polls_lv %>%
  select(pollster, state, end_date, methodology, candidate_name, sample_size, pct) %>%
  pivot_wider(
    names_from = candidate_name,      # Use candidate names as new column headers
    values_from = pct,                # Fill new columns with pct values
    names_prefix = "Pct_",            # Prefix column names with "Pct_"
    values_fn = mean                  # Aggregate duplicates by averaging pct values
  )

# Check decimal vs integer percentages for both Pct_Kamala Harris and Pct_Donald Trump
# Most polls are integers so we should round to nearest percentage
poll_decimal_summary <- president_polls_wide %>%
  summarize(
    kamala_decimal = sum(`Pct_Kamala Harris` %% 1 != 0) / n() * 100,
    kamala_integer = sum(`Pct_Kamala Harris` %% 1 == 0) / n() * 100,
    trump_decimal = sum(`Pct_Donald Trump` %% 1 != 0) / n() * 100,
    trump_integer = sum(`Pct_Donald Trump` %% 1 == 0) / n() * 100
  )
poll_decimal_summary

#   Calculate Margins and Margin of error for Margin: 
#   As Reported and Usnig Two-Candidate Calculation
#   NOTE: Rounding Margins to nearest percentage (so discrete)

president_polls_wide <- president_polls_wide %>%
  mutate(TPV = `Pct_Kamala Harris` + `Pct_Donald Trump`,
         MarginActual = `Pct_Kamala Harris` - `Pct_Donald Trump`, # Reported margin
         MarginActual = round(MarginActual/100,digits=2), # Round Margin to nearest percentage
         Harristpv = `Pct_Kamala Harris` / TPV,
         Trumptpv = `Pct_Donald Trump` / TPV,
         margin = round(Harristpv-Trumptpv,digits=2),  # Rounded two candidate margin
         moe = 1.96 * sqrt(0.25 / sample_size),
         marginmoe = 2*moe) %>%
  mutate(end_date = mdy(end_date)) %>%   # Convert end_date to Date object
  filter(month(end_date) %in% c(9, 10))  # Filter for September and October polls

# Sample Summary Statistics

nrow(president_polls_wide)
mean(president_polls_wide$sample_size,na.rm=TRUE)
mean(president_polls_wide$margin,na.rm=TRUE)              # Avg Margin: Two Candidate 
mean(president_polls_wide$MarginActual,na.rm=TRUE)        # Avg Margin Reported Share
quantile(president_polls_wide$margin,c(.025,.975))        # 95% Confidence Interval

# Initial Example of candidate and margin using nearest integer
SampleSize = round(mean(president_polls_wide$sample_size,na.rm=TRUE),digits=0)
NumPolls = 10000000

# Tied Race Example Simulation
example = rbinom(NumPolls,SampleSize,.5)
sum(round(example/SampleSize,digits=2) == .5)/NumPolls
quantile(example,c(.025,.975))/SampleSize # quantiles of a proportion
margin = round((example - (SampleSize-example))/SampleSize,digits=2)
sum(margin==0)/NumPolls
quantile(margin,c(.025,.975)) # quantiles of a proportion
sum(abs(margin) <= .01)/NumPolls
sum(abs(margin) <= .02)/NumPolls
sum(abs(margin) <= .03)/NumPolls
sum(abs(margin) > .05)/NumPolls

# Raw Data - PA looks very weird
president_polls_wide %>%
  ggplot(aes(x=MarginActual)) +
  geom_histogram() +
  facet_wrap(~state) +
  theme_bw() +
  labs(x = "Number of Polls",
       y = "Reported Margin")

########################################################################################################
#   Now compare variation in actual polls
#   Can use reported margin or two-party voteshare
#   Focus on actual margin for comparability
########################################################################################################

poll_summ = president_polls_wide %>%
  group_by(state) %>%
  summarize(
    PctTie = sum(MarginActual == 0) / n(),    
    NumTie = sum(MarginActual == 0),   # Percentage of exact ties
    MarginMeanActual = mean(MarginActual, na.rm = TRUE),      # Mean of MarginActual
    PctWithin1 = sum(abs(MarginActual - MarginMeanActual) <= .01) / n(),  # Within 1 point of the mean
    NumWithin1 = sum(abs(MarginActual - MarginMeanActual) <= .01), 
    #    PctTieTPV = sum(margin == 0) / n(),                       # Percentage of exact ties for TPV margin
    #    MarginMeanTPV = mean(margin, na.rm = TRUE),               # Mean of TPV margin
    #   PctWithin1TPV = sum(abs(margin - MarginMeanTPV) <= 0.01) / n(),  # Within 1 point of TPV mean
    MarginLower95 = quantile(MarginActual, 0.025, na.rm = TRUE),     # 2.5% quantile
    MarginUpper95 = quantile(MarginActual, 0.975, na.rm = TRUE),      # 97.5% quantile
    PctGT5 = sum(abs(MarginActual - MarginMeanActual) > 0.05) / n(),
    NumGT5 = sum(abs(MarginActual - MarginMeanActual) > 0.05),
    CIWidth = MarginUpper95 - MarginLower95,
    n = n()
  )
poll_summ

# Descriptive Totals
sum(poll_summ$n)
sum(poll_summ$NumTie)
sum(poll_summ$NumWithin1)
sum(poll_summ$NumGT5)

# Largest Margins in each state
president_polls_wide %>%
  group_by(state) %>%
  summarize(MaxMargin = max(MarginActual),
            MinMargin = min(MarginActual))


########################################################################################################
########################################################################################################
########################################################################################################
# For Each state simulate the expected distribution of margins
# Two methods do the same thing, slightly differently just for fun
########################################################################################################
########################################################################################################
########################################################################################################


########################################################################################################
#   Method 1: 
#   Now Simulate using Normal Approximation with state polling averages as the truth and the average moe on the margin as the SE
#########################################################################################################

margin_simulations1 <- president_polls_wide %>%
  group_by(state) %>%
  summarize(
    margin_avg = mean(margin, na.rm = TRUE),             # Average Margin
    poll_se = mean(marginmoe / 1.96, na.rm = TRUE)       # Standard error of Margin
  ) %>%
  rowwise() %>%
  mutate(
    simulated_pcts = list(round(rnorm(NumberSims, mean = margin_avg, sd = poll_se),digits=2))  # Simulate distribution
  ) %>%
  unnest(cols = c(simulated_pcts)) %>%
  rename(Margin = simulated_pcts)  # Rename for clarity in plot

summary_marginsimulation1 = margin_simulations1 %>%
  group_by(state) %>%
  summarize(PctTie = sum(Margin==0)/NumberSims,
            PctWithin1 = sum(abs(Margin - mean(Margin)) <= 0.01) /NumberSims,
            PctWithin2 = sum(abs(Margin - mean(Margin)) <= 0.02) /NumberSims,
            PctWithin2 = sum(abs(Margin - mean(Margin)) <= 0.03) /NumberSims,
            PctGT5 = sum(abs(Margin - mean(Margin)) >= 0.05) /NumberSims,
            MarginLower95 = quantile(Margin, 0.025, na.rm = TRUE),     # 2.5% quantile
            MarginUpper95 = quantile(Margin, 0.975, na.rm = TRUE),      # 97.5% quantile
            CIWidth = MarginUpper95 - MarginLower95
  )
summary_marginsimulation1

########################################################################################################
# Method 2:
# Use binomial distribution and Harris Two Party Support to simulate margin
# Uses Harris TPV to draw Harris Support
# Calculate margin (1- Harris)
########################################################################################################

simulate_state_polls <- function(state_name) {
  Harris_avg <- mean(president_polls_wide %>% filter(state == state_name) %>% pull(Harristpv), na.rm = TRUE)
  sample_size <- round(median(president_polls_wide %>% filter(state == state_name) %>% pull(sample_size), na.rm = TRUE),digits=0)
  simulated_pcts <- rbinom(NumberSims, size = sample_size, prob = Harris_avg) / sample_size
  simulated_margin = simulated_pcts - (1-simulated_pcts)
  tibble(state = state_name, Harris = round(simulated_pcts,digits=2), Margin = round(simulated_margin,digits=2))
}

# Apply the function to each state and combine results
states <- c("Pennsylvania", "Michigan", "Wisconsin", "Arizona","North Carolina","Georgia","Nevada")
margin_simulations2 <- map_dfr(states, simulate_state_polls)

summary_marginsimulation2 = margin_simulations2 %>%
  group_by(state) %>%
  summarize(
    PctTie = sum(Margin == 0) / NumberSims,                    # Percentage of exact ties
    PctWithin1 = sum(abs(Margin - mean(Margin)) <= 0.01) / NumberSims,  # Within 1 point of the mean
    MarginLower95 = quantile(Margin, 0.025, na.rm = TRUE),     # 2.5% quantile
    MarginUpper95 = quantile(Margin, 0.975, na.rm = TRUE),      # 97.5% quantile
    PctGT5 = sum(abs(Margin - mean(Margin)) >= 0.05) /NumberSims,
    CIWidth = MarginUpper95 - MarginLower95
  )
summary_marginsimulation2

# Calculate the polling average (mean margin) for each state
mean_margins <- president_polls_wide %>%
  group_by(state) %>%
  summarize(mean_margin = round(mean(MarginActual / 100, na.rm = TRUE),digits=2))

########################################################################################################
########################################################################################################
#####   Now plot!
########################################################################################################
########################################################################################################

# PA Only

pa_polls = president_polls_wide %>%
  filter(state == "Pennsylvania")

pa_sim = margin_simulations1 %>%
  filter(state == "Pennsylvania")

ggplot() +
  # Histogram for actual margin (black)
  geom_histogram(data = pa_polls, 
                 aes(x = MarginActual, 
                     y = ..count.. / tapply(..count.., ..PANEL.., sum)[..PANEL..] * 100, 
                     fill = "Sept/Oct Polls"), 
                 alpha = 0.5, binwidth = 0.01) +
  
  # Histogram for simulated margin (light grey)
  geom_histogram(data = pa_sim, 
                 aes(x = Margin, 
                     y = ..count.. / tapply(..count.., ..PANEL.., sum)[..PANEL..] * 100, 
                     fill = "Simulated Polls if Tied"), 
                 alpha = 0.5, binwidth = 0.01) +
  
  # Vertical line at x = 0
  geom_vline(xintercept = 0, color = "black") +
  
  # Dashed line at the polling average for each state
  geom_vline(data = mean_margins, aes(xintercept = mean_margin), 
             linetype = "dashed", color = "darkgrey") +
  
  labs(
    title = "PA Polling Margins: Harris % - Trump %",
    subtitle = "September and October Polls",
    x = "Harris - Trump Margin",
    y = "Percentage of State Polls",
    fill = ""
  ) +
  scale_fill_manual(values = c("Sept/Oct Polls" = "black", "Simulated Polls if Tied" = "lightgrey")) +
  
  # Scale y-axis to display percentages
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  
  # Scale x-axis as percentages
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(-0.1, 0.1, by = 0.01),
    limits = c(-0.1, 0.1)
  ) +
  
  # Theme adjustments
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")   #
ggsave(file = "PAHerding.pdf")

president_polls_wide = president_polls_wide %>%
  filter(state != "Pennsylvania")

margin_simulations1 = margin_simulations1 %>%
  filter(state != "Pennsylvania")

mean_margins = mean_margins %>%
  filter(state != "Pennsylvania")

  

ggplot() +
  # Histogram for actual margin (black)
  geom_histogram(data = president_polls_wide, 
                 aes(x = MarginActual, 
                     y = ..count.. / tapply(..count.., ..PANEL.., sum)[..PANEL..] * 100, 
                     fill = "Sept/Oct Polls"), 
                 alpha = 0.5, binwidth = 0.01) +
  
  # Histogram for simulated margin (light grey)
  geom_histogram(data = margin_simulations1, 
                 aes(x = Margin, 
                     y = ..count.. / tapply(..count.., ..PANEL.., sum)[..PANEL..] * 100, 
                     fill = "Simulated Polls"), 
                 alpha = 0.5, binwidth = 0.01) +
  
  # Vertical line at x = 0
  geom_vline(xintercept = 0, color = "black") +
  
  # Dashed line at the polling average for each state
  geom_vline(data = mean_margins, aes(xintercept = mean_margin), 
             linetype = "dashed", color = "darkgrey") +
  
  labs(
    title = "Swing State Polling Margins: Harris % - Trump %",
    subtitle = "September and October Polls",
    x = "Harris - Trump Margin",
    y = "Percentage of State Polls",
    fill = ""
  ) +
  scale_fill_manual(values = c("Sept/Oct Polls" = "black", "Simulated Polls" = "lightgrey")) +
  
  # Scale y-axis to display percentages
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  
  # Scale x-axis as percentages
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(-0.09, 0.09, by = 0.03),
    limits = c(-0.09, 0.09)
  ) +
  
  # Theme adjustments
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +  # Move legend to bottom right
  facet_wrap(~ state)  # Facet by state
# Save the plot
ggsave(file = "SixStateHerding.pdf")

########################################################################################################
########################################################################################################
########################################################################################################
# Simulate the effect of past vote weighting on variability
########################################################################################################
########################################################################################################
########################################################################################################

# Number of simulations and realizations
num_simulations <- 100000
n <- 1000

# If randomly sample with partisans with defect with .05 and indpendents who split
# Define vote probabilities for type
prob_R <- .95
prob_D <- .05
prob_I <- 0.5

n <- 1000 # Number of draws per simulation
num_simulations <- 100 # Number of simulations

Margin = NULL
for(i in 1:num_simulations){
  realizations = sample(c("R", "D", "I"), n, replace = TRUE) # Sample Partisans
  sumR = sum(as.logical(realizations == "R")) # How many are in each
  sumD = sum(as.logical(realizations == "D"))
  sumI = sum(as.logical(realizations == "I"))
  voteR = rbinom(1,sumR,prob_R)
  voteD = rbinom(1,sumD,prob_D)
  voteI = rbinom(1,sumI,prob_I)
  Total1 = voteR + voteD + voteI
  Margin[i] = (Total1 - (n-Total1))/n
}

quantile(Margin,c(.025,.975))

# If we Wgt by PID then we no longer sample because we are choosing sumR,SumD,sumI


Margin = NULL
for(i in 1:num_simulations){
  sumR = 333 # weighting targets
  sumD = 333
  sumI = 333
  voteR = rbinom(1,sumR,prob_R)
  voteD = rbinom(1,sumD,prob_D)
  voteI = rbinom(1,sumI,prob_I)
  Total1 = voteR + voteD + voteI
  Margin[i] = (Total1 - (n-Total1))/n
}

quantile(Margin,c(.025,.975))