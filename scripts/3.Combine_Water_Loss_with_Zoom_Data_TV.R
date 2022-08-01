library(modelr)
library(tidyverse)

# ------------------------------------------------------------------------\
# Combine PlantCV with Watering data --------
# ------------------------------------------------------------------------\
water.lost.data <- read.csv("results/2.2_water_lost_data.csv", stringsAsFactors = F) %>% 
  rename(dap = DAP) %>% 
  distinct()

traits <- read.csv("results/1.1_Zoom.corrected_Plantcv_traitsTV.csv", stringsAsFactors = F) 

meta.cols <- c("plantbarcode", "genotype", "treatment",
               "rep", "source", "dap", "timestamp")


length(unique(water.lost.data$plantbarcode))
length(unique(traits$plantbarcode))

# average frames
traits_meta <- traits %>% 
  select(snapshot.num.tv, any_of(meta.cols)) %>% 
  distinct()

traits_avg <- traits %>% 
  group_by(snapshot.num.tv) %>% 
  select(-frame) %>% 
  summarise_at(vars(-any_of(meta.cols)), mean, na.rm = T)

traits_avg.meta <- left_join(traits_avg, traits_meta, by = "snapshot.num.tv") %>% 
  select(any_of(meta.cols), everything())

# averaging values where both frames were outliers or NA from CV returns "NaN", converting these to NA
# method from: https://stackoverflow.com/questions/52490552/r-convert-nan-to-na
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
traits_avg.meta[is.nan(traits_avg.meta)] <- NA
sum(is.nan(traits_avg.meta))


# joining traits table and water table
g <- left_join(water.lost.data, traits_avg.meta, by = c("plantbarcode", "genotype", "dap"), 
               suffix = c(".water", ".pic")) %>% 
  ungroup()

x <- g %>% 
  arrange(plantbarcode, timestamp.water)

# ------------------------------------------------------------------------\
# Generate a few more traits --------
# ------------------------------------------------------------------------\

water_and_plantcv <- g %>% 
  ungroup()

# cumulative water used 
water_and_plantcv <- water_and_plantcv %>% 
  group_by(plantbarcode) %>%
  arrange(plantbarcode, timestamp.water) %>% 
  mutate(water.amount.plus.cum = round(cumsum(water.amount.plus), 1),
         transpire.plus.cum = round(cumsum(transpire.plus), 1), 
         water.amount.cum = round(cumsum(water.amount), 1))  # round these because some are very small decimals, effectively 0

ggplot(water_and_plantcv, aes(x = factor(dap), y = water.amount.cum)) +
  geom_boxplot()

# wue ratio (using area)
water_and_plantcv <- water_and_plantcv %>%
  group_by(plantbarcode, dap) %>%
  mutate(area.over.water = area / water.amount.cum) 

# transpiration ratio 
water_and_plantcv <- water_and_plantcv %>% 
  mutate(area.over.transpire = area / transpire.plus.cum)

# Creating Output tables
empty.pots.data.timepoint <- water_and_plantcv %>%
  filter(str_detect(genotype, "Blank"))

all.data.timepoint <- water_and_plantcv %>%
  filter(!str_detect(genotype, "Blank"))

all.data.dap <- all.data.timepoint %>%
  group_by(plantbarcode, dap) %>%
  filter(watering.job.num == max(watering.job.num, na.rm = T))

empty.pots.data.dap <- empty.pots.data.timepoint %>%
  group_by(plantbarcode, dap) %>%
  filter(watering.job.num == max(watering.job.num, na.rm = T))


sum(is.na(water_and_plantcv$area)) / nrow(water_and_plantcv)


# ------------------------------------------------------------------------\
# WUEfit and WUEresid --------
# ------------------------------------------------------------------------\

# adding 2 columns to all.data table which includes the wue.fit within treatment as well as the residuals of this model
# must remove na values in area and water in order for the model building to run
sum(is.na(all.data.dap$water.amount.plus.cum))
sum(is.na(all.data.dap$area))
# in order to retain as much info as possible, split table into those that can be used in models (have values for area)
# and those that can't, run models, then merge them back. 

na.area <- all.data.dap %>% 
  filter(is.na(area)) 

all.data.dap.sub <- all.data.dap %>% 
  filter(!is.na(area)) 

nrow(na.area) + nrow(all.data.dap.sub) 
nrow(all.data.dap)

wue.fit.models <- all.data.dap.sub %>%
  group_by(dap) %>%
  do(mod = lm(area ~ water.amount.plus.cum, data = .))

m <- left_join(all.data.dap.sub, wue.fit.models, by = c("dap")) 

x <- m %>%
  group_by(dap) %>%
  do(add_predictions(., first(.$mod), var = 'WUE.fit')) %>%
  do(add_residuals(., first(.$mod), var = 'WUE.resid')) %>%
  select(-mod)

na.area <- na.area %>% 
  add_column(WUE.fit = NA, WUE.resid = NA)

all.data.dap_model.traits <- bind_rows(x, na.area)


# cleanup data, change names, group genos etc. 
all.data.dap_model.traits <- all.data.dap_model.traits 


# write out
write.csv(x = all.data.dap_model.traits, file = "results/3.3_water.and.cv.data.by.dap.csv", quote = FALSE, row.names = FALSE)


