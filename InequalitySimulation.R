# CACI inequality distribution simulation 
library(tidyverse)
library(ineq)

# - import CACI data
CACI_data <- readxl::read_xlsx("CACIData.xlsx", sheet = 1)

# Round decimals to whole numbers
CACI_data <- CACI_data %>% mutate_at(vars(N_households:inc200kp), round)

CACI_data <- CACI_data %>%
  mutate(N_households = select(., inc0_5k:inc200kp) %>% rowSums(na.rm = TRUE))

CACI_data <- CACI_data %>% filter(LA != "Isles of Scilly" & LA != "City of London")


# Create empty data frame for a local authority with the correct number of rows for all households

test <- data.frame(LA = rep(CACI_data$LA[1],
                            times = CACI_data$N_households[CACI_data$LA == "Buckinghamshire"]),
                   band = character(length = CACI_data$N_households[CACI_data$LA == "Buckinghamshire"]),
                   income = integer(length = CACI_data$N_households[CACI_data$LA == "Buckinghamshire"])
                   )

test

names(CACI_data)
CACI_data

CACI_data %>% transmute(select(., inc0_5k:inc200kp) %>% rowSums(na.rm = TRUE))

band_data <- c(rep("inc0_5k", times = CACI_data$inc0_5k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc5_10k", times = CACI_data$inc5_10k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc10_15k", times = CACI_data$inc10_15k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc15_20k", times = CACI_data$inc15_20k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc20_25k", times = CACI_data$inc20_25k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc25_30k", times = CACI_data$inc25_30k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc30_35k", times = CACI_data$inc30_35k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc35_40k", times = CACI_data$inc35_40k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc40_45k", times = CACI_data$inc40_45k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc45_50k", times = CACI_data$inc45_50k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc50_55k", times = CACI_data$inc50_55k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc55_60k", times = CACI_data$inc55_60k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc60_65k", times = CACI_data$inc60_65k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc65_70k", times = CACI_data$inc65_70k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc70_75k", times = CACI_data$inc70_75k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc75_80k", times = CACI_data$inc75_80k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc80_85k", times = CACI_data$inc80_85k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc85_90k", times = CACI_data$inc85_90k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc90_95k", times = CACI_data$inc90_95k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc95_100k", times = CACI_data$inc95_100k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc100_120k", times = CACI_data$inc100_120k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc120_140k", times = CACI_data$inc120_140k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc140_160k", times = CACI_data$inc140_160k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc160_180k", times = CACI_data$inc160_180k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc180_200k", times = CACI_data$inc180_200k[CACI_data$LA == "Buckinghamshire"]),
               rep("inc200kp", times = CACI_data$inc200kp[CACI_data$LA == "Buckinghamshire"])
              
)
               
unique(band_data)

length(band_data)
length(test$LA)

test$band <- band_data

test

# --- ^ Structure for simulation is complete - now need to simulate 


# Simulate income distribution with algorithm for deciding final band

income_simulator <- function(x) {
  inc0_5k_sim = rbeta(n = length(x$band[x$band == "inc0_5k"]), 3, 1)*5000
  inc5_10k_sim = sample(5000:10000, length(x$band[x$band == "inc5_10k"]), replace = TRUE)
  inc10_15k_sim = sample(10000:15000, length(x$band[x$band == "inc10_15k"]), replace = TRUE)
  inc15_20k_sim = sample(15000:20000, length(x$band[x$band == "inc15_20k"]), replace = TRUE)
  inc20_25k_sim = sample(20000:25000, length(x$band[x$band == "inc20_25k"]), replace = TRUE)
  inc25_30k_sim = sample(25000:30000, length(x$band[x$band == "inc25_30k"]), replace = TRUE)
  inc30_35k_sim = sample(30000:35000, length(x$band[x$band == "inc30_35k"]), replace = TRUE)
  inc35_40k_sim = sample(35000:40000, length(x$band[x$band == "inc35_40k"]), replace = TRUE)
  inc40_45k_sim = sample(40000:45000, length(x$band[x$band == "inc40_45k"]), replace = TRUE)
  inc45_50k_sim = sample(45000:50000, length(x$band[x$band == "inc45_50k"]), replace = TRUE)
  inc50_55k_sim = sample(50000:55000, length(x$band[x$band == "inc50_55k"]), replace = TRUE)
  inc55_60k_sim = sample(55000:60000, length(x$band[x$band == "inc55_60k"]), replace = TRUE)
  inc60_65k_sim = sample(60000:65000, length(x$band[x$band == "inc60_65k"]), replace = TRUE)
  inc65_70k_sim = sample(65000:70000, length(x$band[x$band == "inc65_70k"]), replace = TRUE)
  inc70_75k_sim = sample(70000:75000, length(x$band[x$band == "inc70_75k"]), replace = TRUE)
  inc75_80k_sim = sample(75000:80000, length(x$band[x$band == "inc75_80k"]), replace = TRUE)
  inc80_85k_sim = sample(80000:85000, length(x$band[x$band == "inc80_85k"]), replace = TRUE)
  inc85_90k_sim = sample(85000:90000, length(x$band[x$band == "inc85_90k"]), replace = TRUE)
  inc90_95k_sim = sample(90000:95000, length(x$band[x$band == "inc90_95k"]), replace = TRUE)
  inc95_100k_sim = sample(95000:100000, length(x$band[x$band == "inc95_100k"]), replace = TRUE)
  inc100_120k_sim = sample(100000:120000, length(x$band[x$band == "inc100_120k"]), replace = TRUE)
  inc120_140k_sim = sample(120000:140000, length(x$band[x$band == "inc120_140k"]), replace = TRUE)
  inc140_160k_sim = sample(140000:160000, length(x$band[x$band == "inc140_160k"]), replace = TRUE)
  inc160_180k_sim = sample(160000:180000, length(x$band[x$band == "inc160_180k"]), replace = TRUE)
  inc180_200k_sim = sample(180000:200000, length(x$band[x$band == "inc180_200k"]), replace = TRUE)
  inc200kp_sim = (rbeta(n = length(x$band[x$band == "inc200kp"]), 1, 3)*10000)+200000
  
  simulation <- c(inc0_5k_sim, inc5_10k_sim, inc10_15k_sim, inc15_20k_sim, inc20_25k_sim, inc25_30k_sim,
           inc30_35k_sim, inc35_40k_sim, inc40_45k_sim, inc45_50k_sim, inc50_55k_sim, inc55_60k_sim,
           inc60_65k_sim, inc65_70k_sim, inc70_75k_sim, inc75_80k_sim, inc80_85k_sim, inc85_90k_sim,
           inc90_95k_sim, inc95_100k_sim, inc100_120k_sim, inc120_140k_sim, inc140_160k_sim,
           inc160_180k_sim, inc180_200k_sim, inc200kp_sim
           )
  
  val = 0
  
  repeat {
    
    val = val + 100
    
    inc200kp_sim_adj = (rbeta(n = length(x$band[x$band == "inc200kp"]), 1, 3)*(10000 + val))+200000
    
    simulation <- c(inc0_5k_sim, inc5_10k_sim, inc10_15k_sim, inc15_20k_sim, inc20_25k_sim, inc25_30k_sim,
                    inc30_35k_sim, inc35_40k_sim, inc40_45k_sim, inc45_50k_sim, inc50_55k_sim, inc55_60k_sim,
                    inc60_65k_sim, inc65_70k_sim, inc70_75k_sim, inc75_80k_sim, inc80_85k_sim, inc85_90k_sim,
                    inc90_95k_sim, inc95_100k_sim, inc100_120k_sim, inc120_140k_sim, inc140_160k_sim,
                    inc160_180k_sim, inc180_200k_sim, inc200kp_sim_adj
                    )
    
    if (mean(simulation) - CACI_data$inc_mean[CACI_data$LA == "Buckinghamshire"] > -1) {
      break
    }
    
    
    }


  return(simulation)
  
}


sim_test <- income_simulator(x = test)

mean(sim_test)
median(sim_test)

hist(sim_test, breaks = 500)

# Real mean = 51,755; real median = 43,876
# Test to see if good enough: run 1000 simulations and check 95% around 



# Write generalised functions for all LAs:

# Simulated empty data frame function:

sim_dataframe <- function(LA) {
  
  empty_df <- data.frame(LA = rep(CACI_data$LA[CACI_data$LA == LA],
                      times = CACI_data$N_households[CACI_data$LA == LA]),
             band = character(length = CACI_data$N_households[CACI_data$LA == LA]),
             income = integer(length = CACI_data$N_households[CACI_data$LA == LA])
  )
  
  band_data <- c(rep("inc0_5k", times = CACI_data$inc0_5k[CACI_data$LA == LA]),
                 rep("inc5_10k", times = CACI_data$inc5_10k[CACI_data$LA == LA]),
                 rep("inc10_15k", times = CACI_data$inc10_15k[CACI_data$LA == LA]),
                 rep("inc15_20k", times = CACI_data$inc15_20k[CACI_data$LA == LA]),
                 rep("inc20_25k", times = CACI_data$inc20_25k[CACI_data$LA == LA]),
                 rep("inc25_30k", times = CACI_data$inc25_30k[CACI_data$LA == LA]),
                 rep("inc30_35k", times = CACI_data$inc30_35k[CACI_data$LA == LA]),
                 rep("inc35_40k", times = CACI_data$inc35_40k[CACI_data$LA == LA]),
                 rep("inc40_45k", times = CACI_data$inc40_45k[CACI_data$LA == LA]),
                 rep("inc45_50k", times = CACI_data$inc45_50k[CACI_data$LA == LA]),
                 rep("inc50_55k", times = CACI_data$inc50_55k[CACI_data$LA == LA]),
                 rep("inc55_60k", times = CACI_data$inc55_60k[CACI_data$LA == LA]),
                 rep("inc60_65k", times = CACI_data$inc60_65k[CACI_data$LA == LA]),
                 rep("inc65_70k", times = CACI_data$inc65_70k[CACI_data$LA == LA]),
                 rep("inc70_75k", times = CACI_data$inc70_75k[CACI_data$LA == LA]),
                 rep("inc75_80k", times = CACI_data$inc75_80k[CACI_data$LA == LA]),
                 rep("inc80_85k", times = CACI_data$inc80_85k[CACI_data$LA == LA]),
                 rep("inc85_90k", times = CACI_data$inc85_90k[CACI_data$LA == LA]),
                 rep("inc90_95k", times = CACI_data$inc90_95k[CACI_data$LA == LA]),
                 rep("inc95_100k", times = CACI_data$inc95_100k[CACI_data$LA == LA]),
                 rep("inc100_120k", times = CACI_data$inc100_120k[CACI_data$LA == LA]),
                 rep("inc120_140k", times = CACI_data$inc120_140k[CACI_data$LA == LA]),
                 rep("inc140_160k", times = CACI_data$inc140_160k[CACI_data$LA == LA]),
                 rep("inc160_180k", times = CACI_data$inc160_180k[CACI_data$LA == LA]),
                 rep("inc180_200k", times = CACI_data$inc180_200k[CACI_data$LA == LA]),
                 rep("inc200kp", times = CACI_data$inc200kp[CACI_data$LA == LA])
                 
  )
  
  empty_df$band <- band_data
  
  empty_df
  
  
}


# Fill simulation data frame with income simulations

income_simulator <- function(x, LA) {
  inc0_5k_sim = rbeta(n = length(x$band[x$band == "inc0_5k"]), 3, 1)*5000
  inc5_10k_sim = sample(5000:10000, length(x$band[x$band == "inc5_10k"]), replace = TRUE)
  inc10_15k_sim = sample(10000:15000, length(x$band[x$band == "inc10_15k"]), replace = TRUE)
  inc15_20k_sim = sample(15000:20000, length(x$band[x$band == "inc15_20k"]), replace = TRUE)
  inc20_25k_sim = sample(20000:25000, length(x$band[x$band == "inc20_25k"]), replace = TRUE)
  inc25_30k_sim = sample(25000:30000, length(x$band[x$band == "inc25_30k"]), replace = TRUE)
  inc30_35k_sim = sample(30000:35000, length(x$band[x$band == "inc30_35k"]), replace = TRUE)
  inc35_40k_sim = sample(35000:40000, length(x$band[x$band == "inc35_40k"]), replace = TRUE)
  inc40_45k_sim = sample(40000:45000, length(x$band[x$band == "inc40_45k"]), replace = TRUE)
  inc45_50k_sim = sample(45000:50000, length(x$band[x$band == "inc45_50k"]), replace = TRUE)
  inc50_55k_sim = sample(50000:55000, length(x$band[x$band == "inc50_55k"]), replace = TRUE)
  inc55_60k_sim = sample(55000:60000, length(x$band[x$band == "inc55_60k"]), replace = TRUE)
  inc60_65k_sim = sample(60000:65000, length(x$band[x$band == "inc60_65k"]), replace = TRUE)
  inc65_70k_sim = sample(65000:70000, length(x$band[x$band == "inc65_70k"]), replace = TRUE)
  inc70_75k_sim = sample(70000:75000, length(x$band[x$band == "inc70_75k"]), replace = TRUE)
  inc75_80k_sim = sample(75000:80000, length(x$band[x$band == "inc75_80k"]), replace = TRUE)
  inc80_85k_sim = sample(80000:85000, length(x$band[x$band == "inc80_85k"]), replace = TRUE)
  inc85_90k_sim = sample(85000:90000, length(x$band[x$band == "inc85_90k"]), replace = TRUE)
  inc90_95k_sim = sample(90000:95000, length(x$band[x$band == "inc90_95k"]), replace = TRUE)
  inc95_100k_sim = sample(95000:100000, length(x$band[x$band == "inc95_100k"]), replace = TRUE)
  inc100_120k_sim = sample(100000:120000, length(x$band[x$band == "inc100_120k"]), replace = TRUE)
  inc120_140k_sim = sample(120000:140000, length(x$band[x$band == "inc120_140k"]), replace = TRUE)
  inc140_160k_sim = sample(140000:160000, length(x$band[x$band == "inc140_160k"]), replace = TRUE)
  inc160_180k_sim = sample(160000:180000, length(x$band[x$band == "inc160_180k"]), replace = TRUE)
  inc180_200k_sim = sample(180000:200000, length(x$band[x$band == "inc180_200k"]), replace = TRUE)
  inc200kp_sim = (rbeta(n = length(x$band[x$band == "inc200kp"]), 1, 3)*5000)+200000
  
  simulation <- c(inc0_5k_sim, inc5_10k_sim, inc10_15k_sim, inc15_20k_sim, inc20_25k_sim, inc25_30k_sim,
                  inc30_35k_sim, inc35_40k_sim, inc40_45k_sim, inc45_50k_sim, inc50_55k_sim, inc55_60k_sim,
                  inc60_65k_sim, inc65_70k_sim, inc70_75k_sim, inc75_80k_sim, inc80_85k_sim, inc85_90k_sim,
                  inc90_95k_sim, inc95_100k_sim, inc100_120k_sim, inc120_140k_sim, inc140_160k_sim,
                  inc160_180k_sim, inc180_200k_sim, inc200kp_sim
  )
  
  val = 0
  
  skew = 0
  
  repeat {
    
    val = val + 100
    
    inc200kp_sim_adj = (rbeta(n = length(x$band[x$band == "inc200kp"]), 1, 3)*(5000 + val))+200000
    
    simulation <- c(inc0_5k_sim, inc5_10k_sim, inc10_15k_sim, inc15_20k_sim, inc20_25k_sim, inc25_30k_sim,
                    inc30_35k_sim, inc35_40k_sim, inc40_45k_sim, inc45_50k_sim, inc50_55k_sim, inc55_60k_sim,
                    inc60_65k_sim, inc65_70k_sim, inc70_75k_sim, inc75_80k_sim, inc80_85k_sim, inc85_90k_sim,
                    inc90_95k_sim, inc95_100k_sim, inc100_120k_sim, inc120_140k_sim, inc140_160k_sim,
                    inc160_180k_sim, inc180_200k_sim, inc200kp_sim_adj
    )
    
    if (mean(simulation) - CACI_data$inc_mean[CACI_data$LA == LA] > -1) {
      
      break

    }
    
    
  }
  
  repeat {
    
    skew = skew + 0.05
    
    inc0_5k_sim_adj = rbeta(n = length(x$band[x$band == "inc0_5k"]), 3 - skew, 1)*5000
    
    simulation <- c(inc0_5k_sim_adj, inc5_10k_sim, inc10_15k_sim, inc15_20k_sim, inc20_25k_sim, inc25_30k_sim,
                    inc30_35k_sim, inc35_40k_sim, inc40_45k_sim, inc45_50k_sim, inc50_55k_sim, inc55_60k_sim,
                    inc60_65k_sim, inc65_70k_sim, inc70_75k_sim, inc75_80k_sim, inc80_85k_sim, inc85_90k_sim,
                    inc90_95k_sim, inc95_100k_sim, inc100_120k_sim, inc120_140k_sim, inc140_160k_sim,
                    inc160_180k_sim, inc180_200k_sim, inc200kp_sim_adj)
    
    
    if (mean(simulation) - CACI_data$inc_mean[CACI_data$LA == LA] < 1 | skew == 2) {
      break
    }
    
  }
  
  
  return(simulation)
  
}


income_simulation <- function(LA) {
  df <- sim_dataframe(LA)
  simulation <- income_simulator(df, LA)
  df$income <- simulation
  df
}

shef_sim <- income_simulation(LA = "Sheffield")

hist(shef_sim$income, breaks = 500)

mean(shef_sim$income) - CACI_data$inc_mean[CACI_data$LA == "Sheffield"]


# Create simulations for each local authority 

LA_names <- unique(CACI_data$LA)

LA_names

length(LA_names)
LA_names

# LA_names = c("Sheffield", "Buckinghamshire")

simulate_income_all <- function(n) {
  
  results <- list()
  
  for (i in LA_names[n]) {
    results[[i]] <- income_simulation(LA = LA_names[LA_names == i])
  }
  
  # Flattens list of identical data frames into one data frame
  
  results <- do.call("bind_rows", results)
  
  return(results)
  
}


simulations <- simulate_income_all(n = 1:80)
simulations2 <- simulate_income_all(n = 81:140)
simulations3 <- simulate_income_all(n = 141:174)

str(simulations)
str(simulations2)
str(simulations3)

simulations <- simulate_income_all(n = 1:length(LA_names))

simulations

hist(simulations$income, breaks = 500)


simulations <- simulations %>% group_by(LA) %>% mutate(
  deciles = ntile(income, 10),
  quintiles = ntile(income, 5)
)



simulations <- simulations %>% group_by(LA, quintiles) %>% 
    mutate(
      inc_quin_total = sum(income)
    ) %>% ungroup %>% group_by(LA) %>%
  mutate(
      inc_LA_total = sum(income)
      ) %>%
        ungroup() %>% mutate(
      quin_ratios = inc_quin_total / inc_LA_total
        )

twentytwenty <- simulations %>% group_by(LA, quintiles) %>% summarise(quin_ratios = first(quin_ratios)) %>%
  filter(quintiles == 1 | quintiles == 5) %>% ungroup() %>%
  group_by(LA) %>% spread(quintiles, quin_ratios) %>% mutate(
    twentytwenty_ratio = `5` / `1`
  )

twentytwenty  


simulations <- simulations %>% group_by(LA, deciles) %>% 
  mutate(
    inc_dec_total = sum(income)
  ) %>% ungroup %>% group_by(LA) %>%
  mutate(
    inc_LA_total = sum(income)
  ) %>%
  ungroup() %>% mutate(
    dec_ratios = inc_dec_total / inc_LA_total
  )

tenten <- simulations %>% group_by(LA, deciles) %>% summarise(dec_ratios = first(dec_ratios)) %>%
  filter(deciles == 1 | deciles == 10) %>% ungroup() %>%
  group_by(LA) %>% spread(deciles, dec_ratios) %>% mutate(
    tenten_ratio = `10` / `1`
  )

tenten

  


simulation_means <- simulations %>% group_by(LA) %>% summarise(
  gini_coef = ineq(income, type = "Gini"),
  robinhood_coef = ineq(income, type = "RS"),
  inc_mean_sim = mean(income),
  inc_med_sim = median(income)
)

sim_summaries <- left_join(simulation_means, CACI_data %>% select(LA, inc_mean, inc_median), by = "LA")

sim_summaries <- sim_summaries %>% mutate(
  mean_diff = inc_mean_sim - inc_mean,
  med_diff = inc_med_sim - inc_median
)

sim_summaries <- left_join(sim_summaries, twentytwenty %>% select(LA, twentytwenty_ratio), by = "LA")
sim_summaries <- left_join(sim_summaries, tenten %>% select(LA, tenten_ratio), by = "LA")

sim_summaries

View(sim_summaries)

write_excel_csv(sim_summaries, "LA_income_inequality.csv")


# Do this several thousand times and store summary statistics and 95% intervals


hist(simulations$income[simulations$LA == "Newham"], breaks = 1000)
hist(simulations$income[simulations$LA == "Sunderland"], breaks = 1000)
hist(simulations$income[simulations$LA == "Richmond upon Thames"], breaks = 1000)

plot(cumsum(sort(simulations$income[simulations$LA == "Newham"])), type = "s", ylim = c(0, 4e09))
plot(cumsum(sort(simulations$income[simulations$LA == "Sunderland"])), type = "s", ylim = c(0, 4e09))



# In analysis - Need to do several simulations and take the average

# Script for bootstrapped average simulation statistics 

CACI_data$LA

bootstrap_sims <- function (i) {

  # empty df for storing results
  
  summaries_empty_df <- tibble(LA = character(),
                               gini_coef = integer(),
                               robinhood_coef = integer(),
                               twentytwenty_ratio = integer(),
                               tenten_ratio = integer(),
                               inc_mean_sim = integer(),
                               inc_med_sim = integer(),
                               simulation_n = integer()
  )
  

 for (i in 1:i) {
    
    # Simulate distribution
    simulation <- simulate_income_all(n = 1:length(LA_names))
    
    # Calculate Deciles and Quintiles and calculate inequality summary statistics
    simulation <- simulation %>% group_by(LA) %>% mutate(
      deciles = ntile(income, 10),
      quintiles = ntile(income, 5)
    ) %>% ungroup() %>% group_by(LA, quintiles) %>% 
      mutate(
        inc_quin_total = sum(income)
      ) %>% ungroup %>% group_by(LA) %>%
      mutate(
        inc_LA_total = sum(income)
      ) %>%
      ungroup() %>% mutate(
        quin_ratios = inc_quin_total / inc_LA_total
      )  %>% group_by(LA, deciles) %>% 
      mutate(
        inc_dec_total = sum(income)
      ) %>% ungroup %>% group_by(LA) %>%
      mutate(
        inc_LA_total = sum(income)
      ) %>%
      ungroup() %>% mutate(
        dec_ratios = inc_dec_total / inc_LA_total
      )
    
    twentytwenty <- simulation %>% 
      group_by(LA, quintiles) %>% 
       summarise(quin_ratios = first(quin_ratios)) %>%
        filter(quintiles == 1 | quintiles == 5) %>% 
         ungroup() %>%
          group_by(LA) %>% 
            spread(quintiles, quin_ratios) %>% 
             mutate(
              twentytwenty_ratio = `5` / `1`
             )
    
    tenten <- simulation %>% 
      group_by(LA, deciles) %>% 
        summarise(dec_ratios = first(dec_ratios)) %>%
          filter(deciles == 1 | deciles == 10) %>% 
            ungroup() %>%
              group_by(LA) %>% 
                spread(deciles, dec_ratios) %>% 
                  mutate(
                    tenten_ratio = `10` / `1`
                  )
    
    simulation_means <- simulation %>% group_by(LA) %>% summarise(
      gini_coef = ineq(income, type = "Gini"),
      robinhood_coef = ineq(income, type = "RS"),
      inc_mean_sim = mean(income),
      inc_med_sim = median(income)
    )
    
    
    sim_summaries <- left_join(simulation_means, CACI_data %>% select(LA, inc_mean, inc_median), by = "LA")
    
    sim_summaries <- sim_summaries %>% mutate(
      mean_diff = inc_mean_sim - inc_mean,
      med_diff = inc_med_sim - inc_median
    )
    
    sim_summaries <- left_join(sim_summaries, twentytwenty %>% select(LA, twentytwenty_ratio), by = "LA")
    sim_summaries <- left_join(sim_summaries, tenten %>% select(LA, tenten_ratio), by = "LA")
    
    # Add results of simulation summaries to summary dataframe
    
    sim_summaries <- sim_summaries %>% select(LA, gini_coef, robinhood_coef, 
                                              twentytwenty_ratio,
                                              tenten_ratio,
                                              inc_mean_sim,
                                              inc_med_sim) %>%
      mutate(simulation_n = i)
    
    
    summaries_empty_df <- rbind(summaries_empty_df, sim_summaries)
    
  }
  
  return(summaries_empty_df)
  
} 


test <- bootstrap_sims(i = 2)

test

system.time(bootstrap_sims(i = 1))


# for 1000 simulations do in chunks of 100 and then rbind, make sure to add 100*n for each set of 100

simulation_summaries_0_100 <- bootstrap_sims(i = 100)

saveRDS(simulation_summaries_0_100, "sims1_100.RDS")
View(simulation_summaries_0_100)


# Run on night 27th May 2019
simulation_summaries_101_300 <- bootstrap_sims(i = 200)

# remember mutate then save:
simulation_summaries_101_300 <- simulation_summaries_101_300 %>% mutate(simulation_n = simulation_n + 100)

View(simulation_summaries_101_300)
saveRDS(simulation_summaries_101_300, "sims101_300.RDS")


# Run on night 28th May 2019
simulation_summaries_301_500 <- bootstrap_sims(i = 200)

simulation_summaries_301_500 <- simulation_summaries_301_500 %>% mutate(simulation_n = simulation_n + 300)
# Remember mutate and save:

saveRDS(simulation_summaries_301_500, "sims301_500.RDS")


# Run on night 29th May 2019
simulation_summaries_501_700 <- bootstrap_sims(i = 200)

simulation_summaries_501_700 <- simulation_summaries_501_700 %>% mutate(simulation_n = simulation_n + 500)
# Remember mutate and save:

saveRDS(simulation_summaries_501_700, "sims501_700.RDS")


# Run on night 30th May 2019
simulation_summaries_701_1000 <- bootstrap_sims(i = 300)

simulation_summaries_701_1000 <- simulation_summaries_701_1000 %>% mutate(simulation_n = simulation_n + 700)

saveRDS(simulation_summaries_701_1000, "sims701_1000.RDS")


# Is there something in a 20 National:20 Local Ratio measure? And 10:10?



# Create full simulations table by combining the above:


simulation_summaries_0_100 <- readRDS("sims1_100.RDS")
simulation_summaries_101_300 <- readRDS("sims101_300.RDS")
simulation_summaries_301_500 <- readRDS("sims301_500.RDS")
simulation_summaries_501_700 <- readRDS("sims501_700.RDS")
simulation_summaries_701_1000 <- readRDS("sims701_1000.RDS")

simulation_summaries_combined <- rbind(simulation_summaries_0_100, simulation_summaries_101_300,
      simulation_summaries_301_500, simulation_summaries_501_700,
      simulation_summaries_701_1000)


unique(simulation_summaries_combined$simulation_n)

hist(simulation_summaries_combined$gini_coef[simulation_summaries_combined$LA == "Sheffield"], breaks = 50)

names(simulation_summaries_combined)

bootstrap_summaries <- simulation_summaries_combined %>% group_by(LA) %>% summarise(
  gini_coef_mu = mean(gini_coef),
  gini_coef_lb = quantile(gini_coef, .05),
  gini_coef_ub = quantile(gini_coef, .95),
  robinhood_coef_mu = mean(robinhood_coef),
  robinhood_coef_lb = quantile(robinhood_coef, .05),
  robinhood_coef_ub = quantile(robinhood_coef, .95),
  twentytwenty_ratio_mu = mean(twentytwenty_ratio),
  twentytwenty_ratio_lb = quantile(twentytwenty_ratio, .05),
  twentytwenty_ratio_ub = quantile(twentytwenty_ratio, .95),
  tenten_ratio_mu = mean(tenten_ratio),
  tenten_ratio_lb = quantile(tenten_ratio, .05),
  tenten_ratio_ub = quantile(tenten_ratio, .95),
  inc_mean_sim_mu = mean(inc_mean_sim),
  inc_med_sim_mu = mean(inc_med_sim)
)


saveRDS(bootstrap_summaries, "inequality_simulation_bootstraps.RDS")

# Facet plot for all distributions colour coded by quintile 1, 2-4, and 5

unique(simulations$LA)

simulations %>% filter(LA %in% c("Newham", "Islington", "Rhondda Cynon Taf", "South Tyneside", "Sunderland")) %>%
  mutate(LA = factor(LA, levels = c("Newham", "Islington", "Rhondda Cynon Taf", "South Tyneside", "Sunderland"))) %>%
  ggplot() +
  geom_density(aes(x = income)) +
  facet_grid(~ as.factor(LA))

simulations %>% filter(LA %in% c("Richmond upon Thames", "Windsor and Maidenhead", 
                                 "Surrey", "Buckinghamshire", "Rutland")) %>%
  mutate(LA = factor(LA, levels = c("Richmond upon Thames", "Windsor and Maidenhead", 
                                    "Surrey", "Buckinghamshire", "Rutland"))) %>%
  ggplot() +
  geom_density(aes(x = income)) +
  facet_grid(~ LA)


simulations %>% filter(LA %in% c("Waltham Forest", "Torbay", "Doncaster", "Redcar and Cleveland",
                                 "Hounslow", "Bournemouth", "North Lincolnshire", "North Tyneside")) %>%
  mutate(LA = factor(LA, levels = c("Waltham Forest", "Torbay", "Doncaster", "Redcar and Cleveland",
                                    "Hounslow", "Bournemouth", "North Lincolnshire", "North Tyneside")
                     )) %>% group_by(LA) %>%
  mutate(median_inc = median(income)) %>% ungroup %>%
  ggplot() +
  geom_density(aes(x = income)) +
  facet_wrap(~ LA, nrow = 2) 
  

plot1 <- simulations %>% filter(LA %in% c("Blackpool", "Warwickshire")) %>%
  mutate(LA = factor(LA, levels = c("Blackpool", "Warwickshire")
  )) %>% group_by(LA) %>%
  mutate(median_inc = median(income)) %>% ungroup %>%
  ggplot() +
  geom_density(aes(x = income, group = LA, colour = LA)) 


plot2 <- simulations %>% filter(LA %in% c("Waltham Forest", "Hounslow")) %>%
  mutate(LA = factor(LA, levels = c("Waltham Forest", "Hounslow")
  )) %>% group_by(LA) %>%
  mutate(median_inc = median(income)) %>% ungroup %>%
  ggplot() +
  geom_density(aes(x = income, group = LA, colour = LA)) 


plot3 <- simulations %>% filter(LA %in% c("Torbay", "Bournemouth")) %>%
  mutate(LA = factor(LA, levels = c("Torbay", "Bournemouth")
  )) %>% group_by(LA) %>%
  mutate(median_inc = median(income)) %>% ungroup %>%
  ggplot() +
  geom_density(aes(x = income, group = LA, colour = LA)) 

plot4 <- simulations %>% filter(LA %in% c("Redcar and Cleveland", "North Tyneside")) %>%
  mutate(LA = factor(LA, levels = c("Redcar and Cleveland", "North Tyneside")
  )) %>% group_by(LA) %>%
  mutate(median_inc = median(income)) %>% ungroup %>%
  ggplot() +
  geom_density(aes(x = income, group = LA, colour = LA)) 


simulations %>% filter(LA %in% c("Waltham Forest", "Torbay", "Doncaster", "Redcar and Cleveland")) %>%
  mutate(LA = factor(LA, levels = c("Waltham Forest", "Torbay", "Doncaster", "Redcar and Cleveland")
  )) %>% group_by(LA) %>%
  ggplot() +
  geom_density(aes(x = income, group = LA, colour = LA)) 

cowplot::plot_grid(plot1, plot2, plot3, plot4)




bootstrap_summaries <- readRDS("inequality_simulation_bootstraps.RDS")

bootstrap_summaries

left_join(bootstrap_summaries)


simulations %>% group_by(LA) %>%
  ggplot() +
  geom_density(aes(x = income, group = LA, colour = LA)) +
  theme(legend.position = "none")


saveRDS(simulations, "simulations.RDS")



# Temp sim 

Palma <- function(x, na.rm = TRUE) {
    if(!na.rm && any(is.na(x))) return(NA_real_)
    x <- as.numeric(na.omit(x))
    x <- as.numeric(x)
    x <- sort(x)
    bf <- sum( x[1:(length(x)*0.4)] )
    tt <- sum( x[-(1:length(x)*0.9)] )
    tt / bf
}

gini_palma_comp <- simulations %>% group_by(LA) %>% summarise(
  Palma = Palma(x = income),
  Gini = ineq(income, type = "Gini")
)

gini_palma_comp

cor(gini_palma_comp$Palma, gini_palma_comp$Gini)

gini_palma_comp <- gini_palma_comp %>% mutate(
  Palma_rank = rank(Palma),
  Gini_rank = rank(Gini)
)

write_excel_csv(gini_palma_comp, "gini_palma_comp.csv")






