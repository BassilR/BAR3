install.packages("ggpubr")
remotes::install_github("allisonhorst/palmerpenguins")

library(palmerpenguins)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rstatix)
library(kableExtra)
library(ggpubr)


#Do the species have the same average weights?




#DATA VISUALIZATION:

#Compute summary statistics:
df1 <- penguins %>%
  select(., c("species","body_mass_g")) %>%
  group_by(., species) %>%
  summarise(
    count = n(),
    mean = mean(body_mass_g, na.rm = TRUE),
    sd = sd(body_mass_g, na.rm = TRUE)
  )

#Clean table of summary statistics:
kable(df1) %>%
  kable_styling("striped", full_width = F)

#Box plot:
ggplot(penguins, aes(x=species, y=body_mass_g, fill = species)) + 
  geom_boxplot(notch=TRUE) +
  theme_minimal() + 
  labs(title = "Box Plot of Penguin Body Masses by Species",
       x = "Species",
       y = "Body Mass (g)",
       fill = "Species")

#Density plot:
ggplot(penguins, aes(x=body_mass_g, fill=species)) +
  geom_density(alpha=.3) +
  ylab('Density')+
  xlab('Average Body Mass (g)') +
  ggtitle('Density Plot of Average Body Mass by Species') +
  labs(fill = "Species")

#STATISTICAL TESTS:

#Assess the difference in average weight among all penguin species using ANOVA.
oneway.test(body_mass_g ~ species, data = penguins)

#Create dataframes from t-tests:
penguins.a <- penguins %>%
  filter(., species == "Adelie") %>%
  select(., c("body_mass_g"))  %>%
  filter(!is.na(body_mass_g)) %>%
  as.matrix()

penguins.c <- penguins %>%
  filter(., species == "Chinstrap") %>%
  select(., c("body_mass_g"))  %>%
  filter(!is.na(body_mass_g)) %>%
  as.matrix()

penguins.g <- penguins %>%
  filter(., species == "Gentoo") %>%
  select(., c("body_mass_g")) %>%
  filter(!is.na(body_mass_g)) %>%
  as.matrix()

#Assess the difference in average weight among penguin species using a t test. 
t.test(penguins.g, penguins.c)
t.test(penguins.a, penguins.c)
t.test(penguins.g, penguins.a)

