# Installing necessary libraries & packages
install.packages("tidyverse")
install.packages("reshape2")
library(tidyverse)
library(reshape2)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------------\
# Making a data frame of clean data --------
# ------------------------------------------------------------------------\

plantcv.dat <- read.csv("results/1.1_Zoom.corrected_Plantcv_traitsTV.csv", stringsAsFactors = F)


trait.dat <- read.csv("results/3.3_water.and.cv.data.by.dap.csv", stringsAsFactors = F)
percent.yellow <- read.csv("results/3.5.percent_of_yellow_pixels.csv", stringsAsFactors = F) 


meta.info <- read.csv("./data/paspalum_genotype_info.csv", stringsAsFactors = F) %>% 
  select(genotype = Sample.ID,
         species = species.designation,
         coll.lat = Collection.Latitude,
         coll.long = Collection.Longitude,
         ecotype = Ecotype,
         ploidy = Inferred.ploidy,
         geno.id = Unique.genotype.ID)

barcode.info <- read.csv("./data/EK001_N_062618 barcodes.csv", stringsAsFactors = F) %>% 
  select(plantbarcode = Barcodes,
         treatment = Treatment.2)

meta.cols <- c("plantbarcode", "genotype", "dap", "species", "ecotype", "ploidy",
               "geno.id", "coll.lat", "coll.long")

traits.of.interest <- c("height", "area", "solidity", "yellow.pct", "water.amount.plus.cum")

clean.df <- left_join(trait.dat, percent.yellow, by = c("plantbarcode", "dap")) %>% 
  left_join(., meta.info, by = "genotype") %>% 
  select(-treatment) %>%
  left_join(., barcode.info, by = "plantbarcode") %>%
  mutate(treatment_new = case_when(treatment == "Tx1" ~ "Control", 
                                   treatment == "Tx2" ~ "Gradual_High",
                                   treatment == "Tx3" ~ "Gradual_Low",
                                   treatment == "Tx4" ~ "Sudden_High", 
                                   treatment == "Tx5" ~ "Sudden_Low",
                                   TRUE ~ NA_character_)) %>% 
  separate(col = treatment_new, into = c("application.type", "application.amount"), sep = "_", remove = F) %>% 
  mutate(application_amount = case_when(is.na(application.amount) ~ "Control",
                                        TRUE ~ application.amount)) %>% 
  select(any_of(meta.cols),
         treatment = treatment_new, 
         application.amount,
         application.type,
         any_of(traits.of.interest))

# ------------------------------------------------------------------------\
# All treatments by trait on last day  --------
# ------------------------------------------------------------------------\

long_format_last_day <- pivot_longer(clean.df[which(clean.df$dap==20),], cols = one_of(traits.of.interest), names_to = "trait", values_to = "values")

all_treatments_last_day <- ggplot(subset(long_format_last_day), aes(x= treatment, 
                                                        y = values, color = treatment)) +
  geom_boxplot() +
  facet_wrap("trait", scales = "free") +
  ggtitle("Traits by Treatment on Last Day ") +
  theme(plot.title = element_text(hjust = 0.5))

print(all_treatments_last_day)

ggsave(filename = "results/4.4_Traits_by_Treatment_on_Last_Day.png",
       device = "png", 
       dpi= "retina",
       units = "in",
       height = 10,
       width = 17)

# ------------------------------------------------------------------------\
# All Genotypes Control Treatment on Last Day --------
# ------------------------------------------------------------------------\

control_genotype_last_day <- subset(long_format_last_day)%>% 
  filter(treatment == "Control")

control_plot <- ggplot(control_genotype_last_day, aes(x= genotype, y= values, color = genotype)) +
  geom_boxplot() + 
  facet_wrap("trait", scales = "free") +
  ggtitle("Control on Last Day (all genotypes)") +
  theme(plot.title = element_text(hjust = 0.5))

print(control_plot)  

ggsave(filename = "results/4.4_Control_Last_Day_all_genotypes.png",
       device = "png", 
       dpi= "retina",
       units = "in",
       height = 10,
       width = 17)

# ------------------------------------------------------------------------\
# Genotypes Mean Over Time (Control) --------
# ------------------------------------------------------------------------\

long_means <- clean.df %>%
  pivot_longer(cols = height:yellow.pct, names_to = "trait", values_to = "value") %>%
  group_by(genotype, dap, trait, treatment) %>% 
  summarize(mean = mean(value, na.rm=T)) %>% 
  filter(treatment == "Control")

genotype_mean_over_time_control <- ggplot(aes(x = dap, y = mean, group = genotype, color = genotype), data = long_means) +
  geom_line(alpha = .5) +
  geom_point(alpha = .5) +
  facet_wrap(~trait, scales = "free") +
  theme(legend.position = "none") +
  ggtitle("Genotype Mean Over Time (control)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "results/4.4_genotype_means_over_time(control).png",
       device = "png", 
       dpi = "retina",
       units = "in",
       height = 8.5,
       width = 11)

print(genotype_mean_over_time_control)

# ------------------------------------------------------------------------\
# Linear Graph of All Traits Over Time --------
# ------------------------------------------------------------------------\

traits <- traits.of.interest

pdf("results/4.4_treatment_linegraph_over_time_all_traits.pdf",
    width = 11,
    height = 8.5)
for (this.trait in traits.of.interest){
  plot.df <- clean.df %>% 
    select(plantbarcode, dap, treatment, trait.col = this.trait) %>% 
    group_by(dap, treatment) %>% 
    mutate(N = n(),
           mean = mean(trait.col, na.rm = T),
           sd = sd(trait.col, na.rm = T),
           se = sd / sqrt(N),
           ci = qt(.95/2 + .5, N - 1) * se)
  
  p <-
    ggplot(aes(x = factor(dap), y = mean, color = treatment), data = plot.df) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) +
    geom_line(aes(group = treatment)) +
    geom_point() +
    theme_minimal() +
    labs(title = this.trait)
  
  print(p)
}
dev.off()

# ------------------------------------------------------------------------\
# Traits Grid Over Time for Each Treatment --------
# ------------------------------------------------------------------------\
long_form_all_traits <- clean.df %>% 
  pivot_longer(cols = height:water.amount.plus.cum, names_to = "trait", values_to = "value") %>%
  group_by(genotype, dap, trait, treatment, ecotype) %>% 
  summarize(mean = mean(value, na.rm=T))

pdf("results/4.4_traits_grid_over_time_for_each_treatment.pdf", 
    width = 11,
    height = 8.5 )
for (this.trait in traits.of.interest) {
  plot.df <- long_form_all_traits %>%
    filter(trait == this.trait)
  
  plot <- ggplot(aes(x= factor(dap), y= mean, color = genotype, group = genotype), data = plot.df) +
    geom_line() +
    facet_wrap(~treatment) +
    labs(title = this.trait) +
    theme(legend.position = "none") +
    theme_minimal()
  
  print(plot)
  
}

dev.off()

# ------------------------------------------------------------------------\
# Ground truth for TV data --------
# ------------------------------------------------------------------------\

groundtruth <- read.csv("data/paspalum_groundtruth.csv", stringsAsFactors = F) %>% 
  rename(plantbarcode = plant)

barcode.sub <- barcode.info <- read.csv("./data/EK001_N_062618 barcodes.csv", stringsAsFactors = F) %>% 
  select(plantbarcode = Barcodes, Replicate, Treatment = Treatment.2)

area.sub <- clean.df %>% 
  select(plantbarcode, dap, area) %>% 
  left_join(., barcode.sub, by = "plantbarcode") %>% 
  filter(dap == 20)

gt_dat <- left_join(groundtruth, area.sub, by = "plantbarcode") 


gtsub <- select(gt_dat, fresh, dry, area) %>% 
  mutate(fresh = as.numeric(fresh),
         dry = as.numeric(dry))

cor(gtsub, use = "pairwise.complete")

ggplot(aes(x = fresh, y = area), dat = gtsub) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

ggsave(filename = "results/4.4_ground_truth.png",
       device = "png", 
       dpi = "retina",
       units = "in",
       height = 8.5,
       width = 11)
#correlation coefficient 
cor(gtsub$fresh, gtsub$area, use = "complete.obs")

# ------------------------------------------------------------------------\
# Making a table with summary of imaging jobs to assess quality of our data --------
# ------------------------------------------------------------------------\
summary_of_image_jobs <- clean.df %>% 
  select(dap, genotype, treatment, area) %>% 
  group_by(dap, genotype, treatment) %>% 
  summarize(reps = sum(!is.na(area)))
  
write.csv(summary_of_image_jobs, "results/4.4_summary_of_image_jobs.csv", row.names = FALSE)

