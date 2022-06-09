setwd("~/Danforth Center/internship_2021")
library(tidyverse)
#install.packages("ggpubr")
library(ggpubr)

trait.dat <- read.csv("results/3.water.and.cv.data.by.dap.csv", stringsAsFactors = F)
percent.yellow <- read.csv("results/3.5.percent_of_yellow_pixels.csv", stringsAsFactors = F)

# for side-view area
trait.dat.sv <- trait.dat %>%
  select(plantbarcode, area, camera, ecotype, species, ploidy, treatment) %>%
  filter(camera == "SV")

# for top-view area
trait.dat.tv <- trait.dat %>%
  select(plantbarcode, area, camera, ecotype, species, ploidy, treatment) %>%
  filter(camera == "TV")

last.day.trait <- trait.dat %>%
  filter(dap == 20) %>% 
  filter(!is.na(treatment))

# boxplot for area (SV)
pdf("results/4.1.Area_boxplot_SVTV.pdf")
p <- ggplot(aes(x = factor(treatment), y = area, fill = factor(treatment)), data = trait.dat.sv) +
  geom_boxplot() +
  labs(title = "Area using side-view data",
       x = "Treatment") +
  theme_light() +
  theme(legend.position = "bottom") 
print(p)

t <- ggplot(aes(x = factor(treatment), y = area, fill = factor(treatment)), data = trait.dat.tv) +
  geom_boxplot() +
  labs(title = "Area using top-view data",
       x = "Treatment") +
  theme_light() +
  theme(legend.position = "bottom") 
print(t)

dev.off()

# scatter plot of last.day area (x=uncorrected, y=corrected)
# regression line and R2 value for correlation
# how much does this effect the raw data?
summary(trait.dat)

pdf("results/4.1.Scatter_area_cal.area.pdf")
p <- ggplot(trait.dat, aes(x = cal.area, y = area), na.rm = TRUE) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(alpha = 0.1, color = "darkseagreen") +
  stat_regline_equation(label.x = 30, label.y = 210) +
  stat_cor(method = "pearson", label.x = 30, label.y = 190) +
  labs(title = "Comparing Calibrated Area to Original Area",
       x = "Calibrated Area",
       y = "Original Area")
  theme_light()
print(p)

dev.off()

# boxplots comparing salt conc. and addition type
# include control for each type
plot.df <- last.day.trait %>% 
  mutate(treatment_new = case_when(treatment == "Tx1" ~ "Control", 
                                   treatment == "Tx2" ~ "Gradual_High",
                                   treatment == "Tx3" ~ "Gradual_Low",
                                   treatment == "Tx4" ~ "Sudden_High", 
                                   treatment == "Tx5" ~ "Sudden_Low",
                                   TRUE ~ NA_character_)) %>% 
  separate(col = treatment_new, into = c("Application_type", "Application_amount"), sep = "_", remove = F) %>% 
  mutate(Application_amount = case_when(is.na(Application_amount) ~ "Control",
                                        TRUE ~ Application_amount))
pdf("results/4.1.Grid_boxplot_treat.ecotype_area.pdf")
ggplot(aes(x = ecotype, y = area, fill = ecotype), data = filter(plot.df, treatment_new != "Control")) +
  geom_boxplot() +
  facet_grid(Application_amount ~ Application_type, margins = TRUE) +
  #facet_wrap(Control)
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.ticks.x=element_blank()) +
  theme_light() +
  scale_fill_brewer(palette="BrBG")
dev.off()

# ANOVA or linear regression for the last day
# model should be something like 
#           area ~ genotype + salt amount + salt addition type + 
#           amount:addition + genotype:addition:amount 
# the last two are interaction terms, the colon is a part of the syntax in R 
# Which terms are significant?

install.packages(c("broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

one.way <- aov(area ~ treatment_new, data = plot.df)
summary(one.way)

two.way <- aov(area ~ treatment + ecotype, data = plot.df)
summary(two.way)


