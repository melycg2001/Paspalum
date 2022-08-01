# Installing libraries
library(grid)
library(gridExtra)
library(lme4)
library(tidyverse)
library(viridis)

#####Loading clean df #####
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

meta.cols <- c("plantbarcode", "genotype", "dap", "species", "ecotype", "ploidy",
               "geno.id", "coll.lat", "coll.long")

traits.of.interest <- c("height", "area", "solidity", "yellow.pct", "water.amount.plus.cum", "WUE")

clean.df <- left_join(trait.dat, percent.yellow, by = c("plantbarcode", "dap")) %>% 
  left_join(., meta.info, by = "genotype") %>% 
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
         any_of(traits.of.interest)) %>% 
  mutate(WUE=area/water.amount.plus.cum)

# ------------------------------------------------------------------------\
# Plotting - CI's  --------
# ------------------------------------------------------------------------\
# Plot Geno trends w/ confidence intervals 
genos <- sort(unique(clean.df$genotype))
#make empty list 
plot.list <- list()
# get legend so don't plot everytime
legend.source <- ggplot(aes(x = genotype, y = area, color = treatment), data = clean.df) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_color_manual(values = c("black", "forestgreen","seagreen3", "lightslateblue", "orchid"))

plot.list[[1]] <- ggpubr::as_ggplot(ggpubr::get_legend(legend.source))
counter <- 2
for(this.geno in genos){
  for(this.trait in traits.of.interest){
    geno.df <- clean.df %>% 
      filter(genotype == this.geno) %>% 
      rename(This.Trait = this.trait)
    
    dat <- geno.df %>% 
      select(genotype,dap,treatment,This.Trait) %>%
      group_by(dap, genotype, treatment) %>%
      mutate(N = n(),
             mean = mean(This.Trait, na.rm = T),
             sd = sd(This.Trait, na.rm = T),
             se = sd / sqrt(N),
             ci = qt(.95/2 + .5, N - 1) * se) 
    
    p <- ggplot(aes(x = factor(dap), y = mean, color = treatment), data = dat) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) +
      geom_line(aes(group = treatment)) +
      geom_point() +
      facet_wrap(~genotype) +
      theme_minimal() +
      labs(title = paste(this.geno, this.trait, sep = " "),
           x = "dap",
           y = this.trait) +
      theme(legend.position = "none") +
      scale_color_manual(values = c("black", "forestgreen", "seagreen3", "lightslateblue", "orchid"))
    
    plot.list[[counter]] <- p
    counter <- counter + 1
  }
}
ggsave("results/4.3_TV_Treatments_Grouped_by_Genotypes_final.pdf", marrangeGrob(grobs = plot.list, nrow=2, ncol=2))

###### Group by trait ##########
genos <- sort(unique(clean.df$genotype))
plot.list <- list()
# get legend so don't plot everytime
legend.source <- ggplot(aes(x = genotype, y = area, color = treatment), data = clean.df) +
  geom_line() +
  geom_point() +
  theme_minimal()+
  scale_color_manual(values = c("black", "forestgreen", "seagreen3","lightslateblue", "orchid"))

plot.list[[1]] <- ggpubr::as_ggplot(ggpubr::get_legend(legend.source))
counter <- 2
for(this.trait in traits.of.interest){
  for(this.geno in genos){
    geno.df <- clean.df %>% 
      filter(genotype == this.geno) %>% 
      rename(This.Trait = this.trait)
    
    dat <- geno.df %>% 
      select(dap, genotype, treatment, This.Trait) %>%
      group_by(dap, genotype, treatment) %>%
      mutate(N = n(),
             mean = mean(This.Trait, na.rm = T),
             sd = sd(This.Trait, na.rm = T),
             se = sd / sqrt(N),
             ci = qt(.95/2 + .5, N - 1) * se) 
    
    p <- ggplot(aes(x = factor(dap), y = mean, color = treatment), data = dat) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) +
      geom_line(aes(group = treatment)) +
      geom_point() +
      facet_wrap(~genotype) +
      theme_minimal() +
      labs(title = paste(this.geno, this.trait, sep = " "),
           x = "dap",
           y = this.trait) +
      theme(legend.position = "none") +
      scale_color_manual(values = c("black", "forestgreen", "seagreen3","lightslateblue", "orchid"))
    
    plot.list[[counter]] <- p
    counter <- counter + 1
  }
}
ggsave("results/4.3_TV_Traits_Grouped_final.pdf", marrangeGrob(grobs = plot.list, nrow=2, ncol=2))

# ------------------------------------------------------------------------\
# T-tests --------
# ------------------------------------------------------------------------\
# values to loop through 
all.days <- unique(clean.df$dap)
treatment <- unique(clean.df$treatment)
trait <- c("area", "solidity", "yellow.pct", "WUE")
genos <- sort(unique(clean.df$genotype))
# storage df 
store <- expand.grid(treatment, all.days, trait, genos)
store <- data.frame(store, p.val = NA, N.Ctrl = NA, N.Exp = NA)
names(store) <- c("treatment", "dap", "trait", "genotype", "P.val", "N.Ctrl", "N.Exp")


for(this.trait in trait){
  for(this.day in all.days){
    for(this.treatment in treatment){
      for(this.geno in genos){
        dat <- clean.df %>% 
          select(treatment, dap, genotype, this.trait) %>% 
          filter(dap == this.day,
                 treatment %in% c(this.treatment, "Control"),
                 genotype == this.geno)
        
        ctrl.dat <- dat[dat$treatment == "Control", 4]
        treatment.dat <- dat[dat$treatment == this.treatment, 4]
        
        row.num <- which(store$genotype == this.geno &
                           store$dap == this.day & 
                           store$trait == this.trait &
                           store$treatment == this.treatment)
        
        N.Ctrl <- sum(!is.na(ctrl.dat))
        N.Exp <- sum(!is.na(treatment.dat))
        
        store[row.num, 6] <- N.Ctrl
        store[row.num, 7] <- N.Exp 
        
        if(N.Ctrl < 3 | N.Exp < 3){
          next()
        }
        
        test <- t.test(ctrl.dat, treatment.dat)
        
        store[row.num, 5] <- round(test$p.value, 7)
      }
    }
  }
}
all.pvals <- na.omit(store)

# calc bonferroni cutoff
alpha <- .05
bonf.cut <- alpha / nrow(all.pvals)
all.pvals <- all.pvals %>%
  mutate(is.signif = case_when(P.val > bonf.cut ~ F,
                               is.na(P.val) ~ F,
                               TRUE ~ T))

# write out p.vals
write.csv(all.pvals, "results/4.3_TV_All.days_all.traits_Ttests.csv", row.names = F)

# ------------------------------------------------------------------------\
# Investigate P-vals --------
# ------------------------------------------------------------------------\
all.pvals <- read.csv("results/4.3_TV_All.days_all.traits_Ttests.csv", header = T, stringsAsFactors = F)

# Plot p-vals over time 
# all traits 
genos <- sort(unique(all.pvals$genotype))
temp <- all.pvals
pdf("results/4.3_TV_Ttests_allGenos.pdf",
    width = 10,
    height = 6)
for(this.geno in genos){
  plot.df <- temp %>% 
    filter(genotype == this.geno) %>% 
    mutate(Exp.group = paste(treatment))
  
  g1 <-  print(
    ggplot(aes(x = factor(dap), y = P.val, group = Exp.group, color = Exp.group), data = plot.df) + 
      geom_point() +
      geom_line() +
      facet_wrap(~trait) +
      labs(title = this.geno,
           x = "dap",
           y = "P-value of T-test vs Control") +
      geom_hline(yintercept = .05) +
      theme_minimal() +
      scale_color_manual(values = c("black", "forestgreen", "seagreen3","lightslateblue", "orchid"))
  )
}
dev.off()

# subset of traits 
temp <- all.pvals %>% 
  filter(trait %in% c("area", "yellow.pct", "WUE")) %>% 
  mutate(Exp.group = paste(treatment))
legend.source <- ggplot(aes(x = factor(dap), y = P.val, group = Exp.group, color = Exp.group), data = temp) + 
  geom_line() +
  geom_point() +
  theme_minimal()+ 
  scale_color_manual(values = c("black", "forestgreen", "seagreen3","lightslateblue", "orchid"))
plot.list <- list()
plot.list[[1]] <- ggpubr::as_ggplot(ggpubr::get_legend(legend.source))
counter <- 2
for(this.geno in genos){
  plot.df <- temp %>% 
    filter(genotype == this.geno) 
  
  p <- ggplot(aes(x = factor(dap), y = P.val, group = Exp.group, color = Exp.group), data = plot.df) + 
    geom_point(aes(shape = is.signif)) +
    geom_line() +
    facet_wrap(~trait, nrow = 2) +
    labs(title = this.geno,
         x = "dap",
         y = "P-value of T-test vs Control") +
    geom_hline(yintercept = .05) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_color_manual(values = c("black", "forestgreen", "seagreen3","lightslateblue", "orchid"))
  plot.list[[counter]] <- p
  counter <- counter + 1
}
ggsave("results/4.3_TV_preliminary_Ttests_Few.Phenos.pdf", marrangeGrob(grobs = plot.list, nrow=2, ncol=2))
table(all.pvals$trait)
x <- all.pvals %>% 
  filter(is.signif == T) %>% 
  arrange(genotype, trait,treatment, dap)
new.bonf = .05 / nrow(temp)
x <- temp %>% 
  filter(P.val < .05) %>% 
  arrange(genotype, trait, treatment, dap) %>% 
  mutate(is.signif = case_when(P.val > new.bonf ~ F,
                               TRUE ~ T))
write.csv(x, "results/4.3_TV_trial_fewPhenos_subset.under.05.csv", row.names = F)
(nrow(x) / nrow(temp)) * 100

# ------------------------------------------------------------------------\
# Table of pval results  --------
# ------------------------------------------------------------------------\
all.pvals <- read.csv("results/4.3_TV_All.days_all.traits_Ttests.csv", header = T, stringsAsFactors = F)
all.genos.sig <- na.omit(all.pvals) %>% 
  unite("Exp.Group", treatment) %>% 
  mutate(under.alpha = case_when(P.val > .05 ~ F,
                                 TRUE ~ TRUE)) 
all.genos.count.sig <- all.genos.sig %>% 
  group_by(genotype, Exp.Group, trait) %>% 
  summarise(num.under.alpha = sum(under.alpha))
table <- all.genos.count.sig %>% 
  filter(trait == "area") %>% 
  select(-trait) %>% 
  pivot_wider(names_from = genotype, values_from = num.under.alpha)
write.csv(table, "results/4.3_TV_Ttest_AREA_countofsignifdays.csv", row.names = F)
twenty <- all.genos.sig %>% 
  filter(dap == 20, 
         trait == "area") %>% 
  select(-c(dap, trait, under.alpha, is.signif), -starts_with("N.")) %>% 
  arrange(Exp.Group) %>% 
  pivot_wider(names_from = genotype, values_from = P.val)
write.csv(twenty, "results/4.3_TV_Ttest_AREA_day20pvals.csv", row.names = F) 

table <- all.genos.count.sig %>% 
  filter(trait == "yellow.pct") %>% 
  select(-trait) %>% 
  pivot_wider(names_from = genotype, values_from = num.under.alpha)
write.csv(table, "results/4.3_TV_Ttest_YELLOW_countofsignifdays.csv", row.names = F)
twenty <- na.omit(all.genos.sig) %>% 
  filter(dap == 20, 
         trait == "yellow.pct") %>% 
  select(-c(dap, trait, under.alpha, is.signif), -starts_with("N.")) %>% 
  arrange(Exp.Group) %>% 
  pivot_wider(names_from = genotype, values_from = P.val)
write.csv(twenty, "results/4.3_TV_Ttest_YELLOW_day20pvals.csv", row.names = F) 

table <- all.genos.count.sig %>% 
  filter(trait == "WUE") %>% 
  select(-trait) %>% 
  pivot_wider(names_from = genotype, values_from = num.under.alpha)
write.csv(table, "results/4.3_TV_Ttest_WUE_countofsignifdays.csv", row.names = F)
twenty <- na.omit(all.genos.sig) %>% 
  filter(dap == 20, 
         trait == "WUE") %>% 
  select(-c(dap, trait, under.alpha, is.signif), -starts_with("N.")) %>% 
  arrange(Exp.Group) %>% 
  pivot_wider(names_from = genotype, values_from = P.val)
write.csv(twenty, "results/4.3_TV_Ttest_WUE_day20pvals.csv", row.names = F) 

