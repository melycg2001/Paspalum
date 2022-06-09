library(tidyverse)

# raw data file 
water.data <- read.csv("results/1.raw.water.data_with.dap.csv", stringsAsFactors = F)

# ------------------------------------------------------------------------\
# Generating Water used --------
# ------------------------------------------------------------------------\
# check for outliers in pot weights 
check.outlier <- water.data %>% 
  filter(weight.before < 0)

check.outlier <- water.data %>% 
  filter(weight.after < 0)

# remove this row from the data
water.data <- water.data %>% 
  filter(weight.before > -1) %>% 
  filter(weight.after > -1)


# Getting water amount lost 
g <- water.data %>% 
  group_by(plantbarcode) %>%
  arrange(plantbarcode, DAP, timestamp) %>% 
  mutate(weight.after.lag1 = lag(weight.after, 1)) %>%
  mutate(water.amount.plus = weight.after.lag1 - weight.before)

# for downstream analysis will add column that aggregates water within dap 
# only affects if there is more than 1 watering per day
g <- g %>%
  group_by(plantbarcode, DAP) %>%
  mutate(water.amount.plus.dap = sum(water.amount.plus, na.rm = T))

negative.water <- g %>% filter(water.amount.plus < 0)

# plots water.amounts.plus by DAP
#ggplot(aes(x = factor(DAP), y = water.amount.plus), data = g) +
#  geom_jitter() + 
#  geom_hline(yintercept = 0, color = "red")

# plots water.amounts by DAP
ggplot(aes(x = factor(DAP), y = water.amount), data = g) +
  geom_jitter() + 
  geom_hline(yintercept = 0, color = "red")

# let's remove these rows
g <- g %>% 
  filter(water.amount.plus >= 0)

empty.averages <- g %>%
  filter(str_detect(plantbarcode, "Na000*")) %>%
  group_by(DAP) %>%
  summarise(mean = mean(water.amount, na.rm=T))

empty.pots <- g %>% 
  filter(str_detect(plantbarcode, "Na000*"))

# ------------------------------------------------------------------------\
# Transpiration --------
# ------------------------------------------------------------------------\
library(lubridate)

# numbering daily watering jobs and add column for time elapsed between jobs
t <- g %>% 
  group_by(plantbarcode) %>% 
  mutate(time.water.loss = date(mdy_hm(timestamp)) - date(mdy_hm(lag(timestamp))), 1) %>% 
  group_by(plantbarcode, DAP) %>% 
  arrange(timestamp) %>% 
  mutate(watering.job.num = 1:n()) %>% 
  arrange(plantbarcode, DAP, watering.job.num) 

# get empty pot average for water lost and time elapsed
e <- t %>% 
  ungroup() %>% 
  filter(str_detect(plantbarcode, "Na000*")) %>% 
  ungroup() %>% 
  group_by(DAP, watering.job.num) %>% 
  summarise(empty.mean.water = mean(water.amount.plus),
            empty.mean.time = mean(time.water.loss)) 


r <- left_join(t, e, by = c("DAP", "watering.job.num")) %>% 
  mutate(transpire.plus = water.amount.plus - empty.mean.water) %>% 
  ungroup() %>% 
  group_by(plantbarcode, DAP) %>% 
  mutate(transpire.plus.dap = sum(transpire.plus)) 

table(r$DAP)

# remove first day of water data as the amount lost cannot be calculated 
r <- r %>% 
  filter(DAP != 1)

# ------------------------------------------------------------------------\
# check plots --------
# ------------------------------------------------------------------------\

## transpiration density over time 
ggplot(aes(x = transpire.plus.dap), data = r) +
  geom_density() +
  geom_vline(aes(xintercept = 0)) +
  facet_wrap(vars(factor(DAP)), scales = "free")


## empty pot weights over time

ggplot(aes(x = factor(DAP), y = water.amount.plus), 
       data = filter(r, str_detect(plantbarcode, "Na000*"))) +
  geom_point()


# test if difference in times elapsed is way off 
hist(as.numeric(r$time.water.loss) - as.numeric(r$empty.mean.time))
range(as.numeric(r$time.water.loss) - as.numeric(r$empty.mean.time), na.rm = T)


# ------------------------------------------------------------------------\
# output --------
# ------------------------------------------------------------------------\

write.csv(r, "results/2.water_lost_data.csv", row.names = F)

