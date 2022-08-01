
library(lubridate)
library(tidyverse)

raw_tv.vis <- read.csv("./data/New_fixed_run_paspalum_TV.csv-single-value-traits.csv", stringsAsFactors = F) %>%
  filter(sample != "paspalum") # removes rows with paspalum(NA)

# ------------------------------------------------------------------------\
# Merge the meta info --------
# ------------------------------------------------------------------------\

barcode.info <- read.csv("./data/EK001_N_062618 barcodes.csv", stringsAsFactors = F)

barcode.info <- barcode.info %>% 
  select(plantbarcode = Barcodes,
         ID = Print.Info.3,
         Genotype = Genotype.ID,
         Replicate,
         Treatment = Treatment.2) %>% 
  filter(plantbarcode != "") %>% 
  distinct()

meta.info <- read.csv("./data/paspalum_genotype_info.csv", stringsAsFactors = F) %>% 
  select(Genotype = Sample.ID,
         Source = Collection.Location,
         plant.date)

all.meta <- left_join(barcode.info, meta.info, by = c("Genotype")) %>% 
  select(plantbarcode, everything()) %>% 
  mutate(plant.date = "6/26/2018") 

all.meta$Genotype <- str_replace(all.meta$Genotype, "DG27B", "DG027B")

all.meta$Genotype <- str_replace(all.meta$Genotype, "PI 526140", "PI 576140")

raw_tv.vis <- raw_tv.vis %>%
  select(-c(zoom, exposure, gain, lifter, treatment,other))

# determine days after planting (DAP) for data
plant.dates <- left_join(all.meta, raw_tv.vis, by = "plantbarcode") %>%
  select(plantbarcode, plant.date, timestamp) %>%
  mutate(DAP = as.integer(date(timestamp) - mdy(plant.date))) %>%
  select (-plant.date) %>%
  distinct()

write.csv(plant.dates, "./results/1.1_plant.dates_with.dap.csv", row.names = F)

vis.data.tv <- left_join(raw_tv.vis, plant.dates, by = c("plantbarcode", "timestamp"))

# add column that links up top-view images, folder name for a single imaging job 
meta.new.tv <- vis.data.tv$image
snapshot.num.tv = str_extract(meta.new.tv, "snapshot[:digit:]*") %>% 
  str_replace("snapshot", "")

vis.data.tv <- data.frame(snapshot.num.tv, vis.data.tv)

# merge meta info with vis data
vis.data.tv <- left_join(vis.data.tv, all.meta, by = "plantbarcode") %>% 
  distinct()

# ------------------------------------------------------------------------\
# Zoom Correction --------
# ------------------------------------------------------------------------\

zoom.lm <- lm(zoom.camera ~ zoom, data = data.frame(zoom = c(1, 6000), zoom.camera = c(1, 6)))

# Download data for a reference object imaged at different zoom levels
if (!file.exists('zoom_calibration_data.txt')) {
  download.file('http://files.figshare.com/2084101/zoom_calibration_data.txt',
                'zoom_calibration_data.txt')
}

# Read zoom calibrartion data
z.data <- read.table(file = "zoom_calibration_data.txt", sep = "\t", header = TRUE)

# Calculate px per cm
z.data$px_cm <- z.data$length_px / z.data$length_cm

# Calculate area for each row
z.data$area_cm <- ifelse(z.data$reference == z.data$reference[[1]], (13.2*13.2), (13.2*3.7))

# Calculate px**2 per cm**2
z.data$px2_cm2 <- z.data$area_px / z.data$area_cm

# Convert LemnaTec zoom units to camera zoom units
z.data$zoom.camera <- predict(object = zoom.lm, newdata = z.data)

# Zoom correction for area
area.coef <- coef(nls(log(px2_cm2) ~ log(a * exp(b * zoom.camera)),
                      z.data, start = c(a = 1, b = 0.01)))
area.coef <- data.frame(a = area.coef[1], b = area.coef[2])

area.nls <- nls(px2_cm2 ~ a * exp(b * zoom.camera),
                data = z.data, start = c(a = area.coef$a, b = area.coef$b))

# Zoom correction for length
len.coef <- coef(nls(log(px_cm) ~ log(a * exp(b * zoom.camera)),
                     z.data, start = c(a = 1, b = 0.01)))
len.coef <- data.frame(a = len.coef[1], b = len.coef[2])
len.poly <- nls(px_cm ~ a * exp(b * zoom.camera),
                data = z.data, start = c(a = len.coef$a, b = len.coef$b))


# Convert LemnaTec zoom units to camera zoom units
vis.data.tv$zoom = 1
vis.data.tv$zoom <- as.integer(gsub('z', '', vis.data.tv$zoom))
vis.data.tv$zoom.camera <- predict(object = zoom.lm, newdata = vis.data.tv)
vis.data.tv$rel_area <- predict(object = area.nls, newdata = vis.data.tv)
vis.data.tv$px_cm <- predict(object = len.poly, newdata = vis.data.tv)
vis.data.tv$px2_cm2 <- predict(object = area.nls, newdata = vis.data.tv)

# ------------------------------------------------------------------------\
# Generate Traits table using zoom correction --------
# ------------------------------------------------------------------------\
vis.data.tv <- vis.data.tv %>%
  distinct()
# meta data 
traits <- data.frame(plantbarcode = vis.data.tv$plantbarcode,
                     plant.num = vis.data.tv$id,
                     genotype = vis.data.tv$Genotype,
                     rep = vis.data.tv$Replicate,
                     treatment = vis.data.tv$Treatment,
                     source = vis.data.tv$Source,
                     dap = vis.data.tv$DAP,
                     snapshot.num.tv = vis.data.tv$snapshot.num.tv,
                     frame = vis.data.tv$frame,
                     timestamp = vis.data.tv$timestamp)

head(vis.data.tv)
length_traits <- c("height",
                   "perimeter",
                   "width")

area_traits <- c("area")

unitless_traits <- c("solidity",
                     "hue_circular_mean",
                     "hue_circular_std",
                     "hue_median")

# Use pixel to cm conversions to generate corrected values
divide.cols <- function(num, denom) num/denom

vis.data_length <- vis.data.tv %>%
  select(any_of(length_traits),
         px_cm,
         snapshot.num.tv,
         frame) %>% 
  mutate_at(vars(all_of(length_traits)), divide.cols, denom = quote(px_cm)) %>% 
  select(-px_cm)

vis.data_area <- vis.data.tv %>%
  select(any_of(area_traits),
         px2_cm2,
         snapshot.num.tv,
         frame) %>% 
  mutate_at(vars(all_of(area_traits)), divide.cols, denom = quote(px2_cm2)) %>% 
  select(-px2_cm2)

vis.data_unitless <- vis.data.tv %>% 
  select(any_of(unitless_traits),
         snapshot.num.tv,
         frame) 

# combine these three trait tables with meta data
traits.corrected <- left_join(traits, vis.data_length, by = c("snapshot.num.tv", "frame")) %>% 
  left_join(., vis.data_area, by = c("snapshot.num.tv", "frame")) %>% 
  left_join(., vis.data_unitless, by = c("snapshot.num.tv", "frame")) %>% 
  filter(str_detect(plantbarcode, "^C", negate = T)) # remove color card rows

write.csv(traits.corrected, "./results/1.1_Zoom.corrected_Plantcv_traitsTV.csv", row.names = F)

# ------------------------------------------------------------------------\
# Watering Data  --------
# ------------------------------------------------------------------------\

snapshot.data <- read.csv("./data/SnapshotInfo.csv", stringsAsFactors = F) %>%
  rename(plantbarcode = plant.barcode) %>%
  mutate(plant.date = mdy("6/26/2018")) %>%
  mutate(DAP = as.integer(date(ymd_hms(timestamp)) - (plant.date))) %>%
  select (-plant.date)

table(snapshot.data$DAP)


# get rid of color cards
snapshot.data = snapshot.data[grep("Na", snapshot.data$plantbarcode),]
# select subset of columns
snapshot.data <- snapshot.data %>%
  select(any_of(c("plantbarcode", "timestamp", "weight.before", 
                  "weight.after", "water.amount", "DAP")))
# remove rows which are -1
snapshot.data <- snapshot.data[snapshot.data$weight.before != -1, ]

# genotype column 
x <- all.meta %>% 
  select(plantbarcode, Genotype)

snapshot.data <- left_join(snapshot.data, x, by = "plantbarcode") %>% 
  select(plantbarcode, genotype = Genotype, DAP, everything())

write.csv(snapshot.data, "./results/1.1_raw.water.data_with.dap.csv", row.names = F)


