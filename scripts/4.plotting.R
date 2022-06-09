library(tidyverse)

trait.dat <- read.csv("results/3.water.and.cv.data.by.dap.csv", stringsAsFactors = F)
percent.yellow <- read.csv("results/3.5.percent_of_yellow_pixels.csv", stringsAsFactors = F)

last.day.trait <- trait.dat %>%
  filter(dap == 20) %>% 
  filter(!is.na(treatment))


last.day.yellow <- percent.yellow %>%
  filter(DAP == 20) %>%
  distinct()

treat <- trait.dat %>%
  select(treatment, plantbarcode) %>%
  distinct()


# test <- trait.dat %>% 
#   filter(is.na(treatment))
# 
# meta.info <- read.csv("./data/paspalum_genotype_info.csv", stringsAsFactors = F)
# 
# unique(test$genotype)[!unique(test$genotype) %in% unique(meta.info$Sample.ID)]
# 
# x <- meta.info %>% 
#   filter(Sample.ID == "DG194")
# 
# head(test$plantbarcode)

last.day.yellow <- left_join(last.day.yellow, treat, by = "plantbarcode") 

traits_of_interest <- c("area", "area.over.water", "water.amount.plus.cum", "WUE.resid", "nir_median")
groups <- c("ecotype", "species", "ploidy")

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

test <- plot.df %>% 
  filter(dap == 20) %>% 
  select(genotype, ecotype, treatment_new, Application_type, Application_amount) %>% 
  distinct() 

test2 <- test %>% 
  select(genotype, ecotype) %>% 
  distinct()

ggplot(aes(x = ecotype, y = area), data = filter(plot.df, treatment_new != "Control")) +
  geom_boxplot() +
  facet_grid(Application_amount ~ Application_type) 

# boxplot by treatment only
pdf("results/4.Trait_boxplots.pdf",
    width = 11,
    height = 8.5)
for(this.trait in traits_of_interest){
  #this.trait <- "area"
  
  p <- 
    ggplot(aes_string(x = "factor(treatment_new)", y = this.trait, fill = "factor(treatment_new)"), data = plot.df) +
    geom_boxplot() +
    labs(title = this.trait,
         x = "Treatment") +
    theme_light() +
    theme(legend.position = "bottom") 
  
  print(p)
}
dev.off()

# boxplot by treatment with facet_wrap for other groups
pdf("results/4.Trait_boxplots_facet.wrap.pdf")
for(this.trait in traits_of_interest){
  for(grp in groups) {
    this.trait <- traits_of_interest[1]
    grp <- groups[1]
    
    plot.df <- last.day.trait %>% 
      rename(facet.factor = grp)
    
    p <- 
      ggplot((aes_string(x = "factor(treatment)", y = this.trait, fill = "factor(treatment)")), data = plot.df) +
      geom_boxplot() +
      facet_wrap(~facet.factor, nrow = 1) +
      labs(title = this.trait) 
  
    print(p)
  }
}
dev.off()

for(this.trait in traits_of_interest){
  p <- 
    ggplot((aes_string(x = "factor(treatment)", y = this.trait)), data = last.day.trait, aes_string(fill = groups)) +
    geom_boxplot() +
    labs(title = this.trait)
  
  print(p)
}
dev.off()

# boxplot by treatment and other traits
ggplot(aes(x = factor(treatment), y = area), data = last.day.trait) +
  geom_boxplot() +
  facet_wrap(~ecotype)

ggplot(aes(x = factor(treatment), y = area), data = last.day.trait) +
  geom_boxplot() +
  facet_wrap(~species)

ggplot(aes(x = factor(treatment), y = area), data = last.day.trait) +
  geom_boxplot() +
  facet_wrap(~ploidy)

ggplot(aes(x = factor(treatment), y = yellow.pct), data = last.day.yellow) +
  geom_boxplot() #+
  #facet_wrap(~ecotype)

ggplot(aes(x = factor(dap), y = area), data = trait.dat) + 
  geom_boxplot()

ggplot(aes(x = timestamp.water, y = area), data = trait.dat) + 
  geom_point()

ggplot(aes(x = factor(dap), y = water.amount.cum), data = trait.dat) + 
  geom_boxplot()



ggplot(aes(x = factor(ecotype), y = area), data = trait.dat) +
  geom_boxplot()

ggplot(aes(x = factor(treatment), y = nir_median), data = trait.dat) +
  geom_boxplot()

ggplot(aes(x = factor(treatment), y = WUE.fit), data = trait.dat) +
  geom_boxplot()

yellow_grpd <- percent.yellow %>%
  group_by(plantbarcode) %>%
  summarise(mean = mean(yellow.pct, na.rm=T))

ggplot(aes(x = factor(plantbarcode), y = yellow_grpd), data = percent.yellow) +
  geom_point()


names(trait.dat)
