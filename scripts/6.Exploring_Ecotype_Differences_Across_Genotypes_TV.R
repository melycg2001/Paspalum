
# ------------------------------------------------------------------------\
# plotting genotypes of interest --------
# ------------------------------------------------------------------------\
traits.of.interest <- c("height", "area", "WUE","yellow.pct")
traits <- traits.of.interest

pdf("results/4.3_genotypes_of_interest_linegraphs.pdf",
    width = 11,
    height = 8.5)
for (this.trait in traits.of.interest){
  plot.df <- clean.df %>% 
    filter(genotype %in% c("PI 647842","PI 614679","DG179","DG168","DG021", "PI 647915", "DG014","DG140","PI647903", "DG189","DG033", "DG132", "DG088", "DG138", "DG110", "DG152", "DG114", "DG212", "PI 222796",
                           "PI 347894", "PI 377709", "PI 403999", "PI 614678", "PI 647904", "PI 647923", "PI 647918", "DG183", "PI 284500", "PI 576134")) %>%  
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
# linegraph for each genotype of interest --------
# ------------------------------------------------------------------------\

long_form_all_traits <- clean.df %>%
  pivot_longer(cols = height:WUE, names_to = "trait", values_to = "value") %>%
  group_by(genotype, dap, trait, treatment) %>%
  summarize(mean = mean(value, na.rm=T))

pdf("results/4.4_genotypes_of_interest_linegraph_overtime.pdf",
    width = 11,
    height = 8.5 )
for (this.trait in traits.of.interest) {
  plot.df <- long_form_all_traits %>%
    filter(genotype %in% c("PI 647842","PI 614679","DG179","DG168","DG021", "PI 647915", "DG014","DG140","PI647903", "DG189","DG033", "DG132", "DG088", "DG138", "DG110", "DG152", "DG114", "DG212", "PI 222796",
                           "PI 347894", "PI 377709", "PI 403999", "PI 614678", "PI 647904", "PI 647923", "PI 647918", "DG183", "PI 284500", "PI 576134")) %>%
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
# graphing ecotypes side by side across all genotypes --------
# ------------------------------------------------------------------------\
plot.df <- clean.df %>%
  select(dap, treatment, ecotype, area) %>%
  group_by(dap, treatment, ecotype) %>%
  mutate(N = n(),
         mean = mean(area, na.rm = T),
         sd = sd(area, na.rm = T),
         se = sd / sqrt(N),
         ci = qt(.95/2 + .5, N - 1) * se) %>% 
  select(-area) %>% 
  distinct() %>% 
  filter(!is.na(ecotype))

my_palette <- c("blacK",
                "firebrick3",
                "coral1",
                "steelblue4",
                "steelblue1")

ggplot(aes(x = dap, y = mean, color = treatment), data = plot.df) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette)

ggsave("Coarse_fine_distichum_comparison.png")

# ------------------------------------------------------------------------\
# Highlighting sudden treatments --------
# ------------------------------------------------------------------------\
plot.df <- clean.df %>%
  select(dap, treatment, ecotype, area) %>%
  group_by(dap, treatment, ecotype) %>%
  mutate(N = n(),
         mean = mean(area, na.rm = T),
         sd = sd(area, na.rm = T),
         se = sd / sqrt(N),
         ci = qt(.95/2 + .5, N - 1) * se) %>% 
  select(-area) %>% 
  distinct() %>% 
  filter(!is.na(ecotype))

my_palette <- c("blacK",
                "grey88",
                "grey88",
                "steelblue4",
                "steelblue1")

ggplot(aes(x = dap, y = mean, color = treatment), data = plot.df) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette)

ggsave("Sudden_treatments_highlighted.png")

# ------------------------------------------------------------------------\
# Highlighting gradual treatments --------
# ------------------------------------------------------------------------\
plot.df <- clean.df %>%
  select(dap, treatment, ecotype, area) %>%
  group_by(dap, treatment, ecotype) %>%
  mutate(N = n(),
         mean = mean(area, na.rm = T),
         sd = sd(area, na.rm = T),
         se = sd / sqrt(N),
         ci = qt(.95/2 + .5, N - 1) * se) %>% 
  select(-area) %>% 
  distinct() %>% 
  filter(!is.na(ecotype))

my_palette <- c("blacK",
                "firebrick3",
                "coral1",
                "grey88",
                "grey88")

ggplot(aes(x = dap, y = mean, color = treatment), data = plot.df) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) 

ggsave("Gradual_treatments_highlighted.pdf")

# ------------------------------------------------------------------------\
# Plotting genotypes against U.S map  --------
# ------------------------------------------------------------------------\
library(maps)
library(usmap)

#making dataframe for plotting to map
genotypes.to.map <- table %>% 
  pivot_longer(cols = 2:53, names_to = "genotype", values_to = "days.of.signif") %>% 
  rename(treatment = Exp.Group) %>% 
  left_join(new.frame, genotypes.to.map, by = c("genotype", "treatment"))

#making base map 
MainStates <- map_data("state")

m <- ggplot() +
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color = "black", fill = "grey")+
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


points.map <- m + 
  geom_point(data = genotypes.to.map, aes(x= coll.long, y = coll.lat, size = days.of.signif, color= days.of.signif)) +
  guides(size= "none", color = guide_legend("Days of Signifance",override.aes = list(size=8))) +
  scale_color_gradient2(low = "green", mid= "orange",high = "purple") +
  coord_cartesian(xlim = c(-110,-70), ylim = c(25,35)) + 
  scale_size_continuous(range = c(4, 9))+
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 14))

ggsave("Days_of_signif_U.S_map.png",points.map, dpi = "retina",
       width = 15,
       height = 7.5)

# ------------------------------------------------------------------------\
# comparing best to worst performing genotypes  --------
# ------------------------------------------------------------------------\
best.genotypes <- clean.df %>% 
  select(dap, treatment, ecotype, area, coll.lat, coll.long, genotype) %>%
  group_by(dap, treatment, ecotype, genotype) %>%
  mutate(N = n(),
         mean = mean(area, na.rm = T),
         sd = sd(area, na.rm = T),
         se = sd / sqrt(N),
         ci = qt(.95/2 + .5, N - 1) * se) %>% 
  select(-area) %>% 
  distinct() %>% 
  filter(!is.na(ecotype)) %>% 
  filter(genotype %in% c("DG027B", "DG110"))


worst.genotypes <- clean.df %>% 
  select(dap, treatment, ecotype, area, coll.lat, coll.long, genotype) %>%
  group_by(dap, treatment, ecotype, genotype) %>%
  mutate(N = n(),
         mean = mean(area, na.rm = T),
         sd = sd(area, na.rm = T),
         se = sd / sqrt(N),
         ci = qt(.95/2 + .5, N - 1) * se) %>% 
  select(-area) %>% 
  distinct() %>% 
  filter(!is.na(ecotype)) %>% 
  filter(genotype %in% c("DG140", "DG183","DG014"))

#### Plotting the DG027B Best-Genotypes all treatments
my_palette <- c("black",
                "firebrick3",
                "coral1",
                "darkgreen",
                "yellowgreen")

b <- best.genotypes %>%
  filter(genotype == "DG027B") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG027B All Treatments")

print(one)

ggsave("Best_genotypes_DG027B_linegraphs_all_treatment.png")


##### Plotting the best Genotypes DG027B Gradual

my_palette <- c("black",
                "firebrick3",
                "coral1",
                "grey88",
                "grey88")

b <- best.genotypes %>%
  filter(genotype == "DG027B") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG027B Gradual Treatments")

print(one)

ggsave("Best_genotypes_DG027B_linegraphs_gradual_treatments.png")

####Plotting Best Genotype DG027B Sudden 

my_palette <- c("black",
                "grey88",
                "grey88",
                "darkgreen",
                "yellowgreen")

b <- best.genotypes %>%
  filter(genotype == "DG027B") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG027B Sudden Treatments")

print(one)

ggsave("Best_genotypes_DG027B_linegraphs_sudden_treatments.png")


####Plotting best genotype DG110 All treatments
my_palette <- c("black",
                "firebrick3",
                "coral1",
                "darkgreen",
                "yellowgreen")

b <- best.genotypes %>%
  filter(genotype == "DG110") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG110 All Treatments")



print(one)

ggsave("Best_genotypes_DG110_linegraphs_all_treatment.png")


####Plotting best genotype DG110 Gradual Treatments
my_palette <- c("black",
                "firebrick3",
                "coral1",
                "grey88",
                "grey88")

b <- best.genotypes %>%
  filter(genotype == "DG110") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG110 Gradual Treatments")

print(one)

ggsave("Best_genotypes_DG110_linegraphs_gradual_treatments.png")



####Plotting best genotypes DG110 Sudden Treatments 

my_palette <- c("black",
                "grey88",
                "grey88",
                "darkgreen",
                "yellowgreen")

b <- best.genotypes %>%
  filter(genotype == "DG110") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG110 Sudden Treatments")

print(one)

ggsave("Best_genotypes_DG110_linegraphs_sudden_treatments.png")


##### Plotting Worst Genotypes DG140 all treatments
my_palette <- c("black",
                "firebrick3",
                "coral1",
                "darkgreen",
                "yellowgreen")

b <- worst.genotypes %>%
  filter(genotype == "DG140") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG140 All Treatments")



print(one)

ggsave("Worst_genotypes_DG140_linegraphs_all_treatment.png")


##### Plotting Worst Genotypes DG140 Gradual treatments
my_palette <- c("black",
                "firebrick3",
                "coral1",
                "grey88",
                "grey88")

b <- worst.genotypes %>%
  filter(genotype == "DG140") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG140 Gradual Treatments")



print(one)

ggsave("Worst_genotypes_DG140_linegraphs_Gradual_treatment.png")



##### Plotting Worst Genotypes DG140 Sudden treatments
my_palette <- c("black",
                "grey88",
                "grey88",
                "darkgreen",
                "yellowgreen")

b <- worst.genotypes %>%
  filter(genotype == "DG140") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG140 Sudden Treatments")



print(one)

ggsave("Worst_genotypes_DG140_linegraphs_Sudden_treatment.png")



##### Plotting Worst Genotypes DG183 all treatments
my_palette <- c("black",
                "firebrick3",
                "coral1",
                "darkgreen",
                "yellowgreen")

b <- worst.genotypes %>%
  filter(genotype == "DG183") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG183 All Treatments")



print(one)

ggsave("Worst_genotypes_DG183_linegraphs_all_treatment.png")


##### Plotting Worst Genotypes DG183 Gradual treatments
my_palette <- c("black",
                "firebrick3",
                "coral1",
                "grey88",
                "grey88")

b <- worst.genotypes %>%
  filter(genotype == "DG183") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG183 Gradual Treatments")



print(one)

ggsave("Worst_genotypes_DG183_linegraphs_gradual_treatment.png")


#### Plotting Worst Genotypes DG183 Sudden treatments
my_palette <- c("black",
                "grey88",
                "grey88",
                "darkgreen",
                "yellowgreen")

b <- worst.genotypes %>%
  filter(genotype == "DG183") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG183 Sudden Treatments")



print(one)

ggsave("Worst_genotypes_DG183_linegraphs_Sudden_treatment.png")

##### Plotting Worst Genotypes DG014 all treatments
my_palette <- c("black",
                "firebrick3",
                "coral1",
                "darkgreen",
                "yellowgreen")

b <- worst.genotypes %>%
  filter(genotype == "DG014") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG014 All Treatments")



print(one)

ggsave("Worst_genotypes_DG014_linegraphs_all_treatment.png")

##### Plotting Worst Genotypes DG014 Gradual treatments
my_palette <- c("black",
                "firebrick3",
                "coral1",
                "grey88",
                "grey88")

b <- worst.genotypes %>%
  filter(genotype == "DG014") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG014 Gradual Treatments")



print(one)

ggsave("Worst_genotypes_DG014_linegraphs_gradual_treatment.png")

##### Plotting Worst Genotypes DG014 sudden treatments
my_palette <- c("black",
                "grey88",
                "grey88",
                "darkgreen",
                "yellowgreen")

b <- worst.genotypes %>%
  filter(genotype == "DG014") 

one <-ggplot(aes(x = dap, y = mean, color = treatment), data = b) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1.5) +
  geom_line(aes(group = treatment),
            size = 2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ecotype, nrow = 2, ncol = 2) +
  scale_color_manual(values = my_palette) +
  labs(title = "DG014 Sudden Treatments")



print(one)

ggsave("Worst_genotypes_DG014_linegraphs_Sudden_treatment.png")


# ------------------------------------------------------------------------\
# Plotting Best and Worst Genotypes on a U.S Map --------
# ------------------------------------------------------------------------\
#making dataframe for plotting to map

table <- rbind(best.genotypes, worst.genotypes,deparse.level = 0)

#making base map 
MainStates <- map_data("state")

m <- ggplot() +
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color = "black", fill = "grey")+
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


points.map <- m + 
  geom_point(data = table, aes(x= coll.long, y = coll.lat, color= genotype), size = 6.5) +
  guides(color = guide_legend("Genotypes",override.aes = list(size=8))) +
  scale_colour_manual(values = c("brown1", "forestgreen", "olivedrab2","brown4","indianred2")) +
  coord_cartesian(xlim = c(-110,-70), ylim = c(25,35)) + 
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 14))

print(points.map)

ggsave("Best_and_Worst_genotypes_US_Map.pdf",points.map, dpi = "retina",
       width = 15,
       height = 7.5)


# print(MainStates)

# ------------------------------------------------------------------------\
# Graphing coarse, fine, and distichum in the same graph and highlighting sudden differences --------
# ------------------------------------------------------------------------\
plot.df <- clean.df %>%
  select(dap, treatment, ecotype, area) %>%
  group_by(dap, treatment, ecotype) %>%
  mutate(N = n(),
         mean = mean(area, na.rm = T),
         sd = sd(area, na.rm = T),
         se = sd / sqrt(N),
         ci = qt(.95/2 + .5, N - 1) * se) %>% 
  select(-area) %>% 
  distinct() %>% 
  filter(!is.na(ecotype)) %>% 
  filter(treatment %in% c("Control", "Sudden_High"))


my_palette <- c("forestgreen",
                "firebrick3",
                #"coral1",
                "magenta",
                "steelblue4",
                "steelblue1",
                "purple")

ggplot(aes(x = dap, y = mean, color = ecotype, shape= treatment), data = plot.df) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 1) +
  geom_line(aes(group = ecotype),
            size = 2) +
  geom_point(size =5) +
  scale_shape_manual (values = c(8,4))+ 
  theme_minimal() +
  labs(x = "Days in the Phenotyper",
       y= "Mean Biomass", 
       color = "Ecotype", 
       shape = "Treatments") +
  theme(axis.title = element_text(size = 14), 
        legend.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 14)) +
  scale_color_manual(values = my_palette) 



ggsave("Coarse_fine_distichum_comparison_on_same_graph.png")


##### New try to make it cleaner
ggplot(aes(x = dap, y = mean, color = ecotype), data = plot.df) +
  geom_smooth(se = F, aes(linetype = treatment)) +
  scale_shape_manual (values = c(8,4))+ 
  theme_minimal() +
  labs(x = "Days in the Phenotyper",
       y= "Mean Biomass", 
       color = "Ecotype", 
       linetype = "Treatments") +
  theme(axis.title = element_text(size = 14), 
        legend.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 14)) +
  scale_color_manual(values = my_palette)


#####Newer try and final 
plot.df2 <- plot.df %>% 
  mutate(eco.treat = paste(ecotype,",", treatment))

paired_palette <- c("dodgerblue4",
                    "dodgerblue",
                    "darkorange4",
                    "darkorange2",
                    "green4",
                    "green2")

ggplot(aes(x = dap, y = mean, color = eco.treat), data = plot.df2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 5,
                alpha = 0.5) +
  geom_line(aes(group = eco.treat, linetype = treatment),
            size = 8.5) +
  geom_point(size = 12) +
  scale_shape_manual (values = c(8,4))+ 
  theme_minimal() +
  labs(x = "Days in the Phenotyper",
       y= "Mean Biomass", 
       color = "Ecotype", 
       linetype = "Treatments") +
  theme(axis.title = element_text(size = 80), 
        legend.title = element_text(size = 75),
        legend.key.size = unit(5, 'cm'), 
        legend.text = element_text(size = 70), 
        legend.position = "bottom", 
        legend.background = element_rect(fill = "lightgrey"),
        axis.text = element_text(size = 70)) +
  scale_color_manual(values = paired_palette) +
  guides(linetype = FALSE)


ggsave("Coarse_fine_distichum_comparison_on_same_graph_new2.png", 
       width = 60,
       height = 35,
       dpi = "retina",
       units = "in", 
       limitsize = FALSE)
########### Overall trend adding just control first 

plot.df2 <- plot.df %>% 
  mutate(eco.treat = paste(ecotype,",", treatment)) %>% 
  filter(treatment %in% "Control")

paired_palette <- c("dodgerblue4",
                    #"dodgerblue",
                    "darkorange4",
                    #"darkorange2",
                    "green4")
                    #"green2")

ggplot(aes(x = dap, y = mean, color = eco.treat), data = plot.df2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 5,
                alpha = 0.5) +
  geom_line(aes(group = eco.treat, linetype = treatment),
            size = 8.5) +
  geom_point(size = 12) +
  scale_shape_manual (values = c(8,4))+ 
  theme_minimal() +
  labs(x = "Days in the Phenotyper",
       y= "Mean Biomass", 
       color = "Ecotype", 
       linetype = "Treatments",
       title = "Comparing Controls Over Time for Fine-Texture, Coarse-Texture, and Distichum") +
  theme(axis.title = element_text(size = 80), 
        legend.title = element_text(size = 75),
        legend.key.size = unit(5, 'cm'), 
        legend.text = element_text(size = 70), 
        legend.position = "bottom", 
        legend.background = element_rect(fill = "lightgrey"),
        axis.text = element_text(size = 70),
        plot.title = element_text(size = 85))+
  scale_color_manual(values = paired_palette) +
  guides(linetype = FALSE)


ggsave("Just_the_controls_overall_trend.png", 
       width = 60,
       height = 35,
       dpi = "retina",
       units = "in", 
       limitsize = FALSE)

###### Attempting to plot just axis and then best and worst genotypes for click through on presentation
plot.df <- clean.df %>%
  select(dap, treatment, ecotype, area, genotype) %>%
  group_by(dap, treatment, ecotype) %>%
  mutate(N = n(),
         mean = mean(area, na.rm = T),
         sd = sd(area, na.rm = T),
         se = sd / sqrt(N),
         ci = qt(.95/2 + .5, N - 1) * se) %>% 
  select(-area) %>% 
  distinct() %>% 
  filter(!is.na(ecotype)) %>% 
  filter(treatment %in% c("Control", "Sudden_High")) %>% 
  filter(genotype %in% c("DG027B", "DG140"))


plot.df3 <- plot.df %>% 
  mutate(eco.treat = paste(ecotype,",", treatment)) %>% 
  filter(treatment %in% "Control")

paired_palette <- c("dodgerblue4",
                    "green4")

ggplot(aes(x = dap, y = mean, color = eco.treat), data = plot.df3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 4,
                alpha = 0.4) +
  geom_line(aes(group = eco.treat),
            size = 7) +
  geom_point(size = 10) +
  scale_shape_manual (values = c(8,4))+ 
  theme_minimal() +
  labs(x = "Days in the Phenotyper",
       y= "Mean Biomass", 
       color = "Ecotype", 
       shape = "Treatments") +
  theme(axis.title = element_text(size = 60), 
        legend.title = element_text(size = 57),
        legend.key.size = unit(3, 'cm'), 
        legend.text = element_text(size = 50), 
        legend.position = "bottom", 
        legend.background = element_rect(fill = "lightgrey"),
        axis.text = element_text(size = 50)) +
  scale_color_manual(values = paired_palette) 


ggsave("Control Comparison of best and worst genotypes.png", 
       width = 40,
       height = 22,
       dpi = "retina",
       units = "in")

####Plotting the sudden high 
plot.df3 <- plot.df %>% 
  mutate(eco.treat = paste(ecotype,",", treatment)) %>% 
  filter(treatment %in% c("Sudden_High", "Control"))

paired_palette <- c("dodgerblue4",
                    "dodgerblue",
                    #"darkorange4",
                    #"darkorange2",
                    "green4",
                    "green2")

ggplot(aes(x = dap, y = mean, color = eco.treat), data = plot.df3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = 4,
                alpha = 0.4) +
  geom_line(aes(group = eco.treat, linetype = treatment),
            size = 7) +
  geom_point(size = 10) +
  scale_shape_manual (values = c(8,4))+ 
  theme_minimal() +
  labs(x = "Days in the Phenotyper",
       y= "Mean Biomass", 
       color = "Ecotype", 
       linetype = "Treatments") +
  theme(axis.title = element_text(size = 60), 
        legend.title = element_text(size = 57),
        legend.key.size = unit(3, 'cm'), 
        legend.text = element_text(size = 50), 
        legend.position = "bottom", 
        legend.background = element_rect(fill = "lightgrey"),
        axis.text = element_text(size = 50)) +
  scale_color_manual(values = paired_palette) + 
  guides(linetype = FALSE)


ggsave("Sudden Comparison of best and worst genotypes.png", 
       width = 43,
       height = 24,
       dpi = "retina",
       units = "in")

