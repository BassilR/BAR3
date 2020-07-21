#call up library
library(remotes)
remotes::install_github("allisonhorst/palmerpenguins")
library(palmerpenguins)
library(tidyverse)

#data viz scatter size x species

ggplot(data =penguins,
          aes(x = flipper_length_mm,
              y = body_mass_g))+
  geom_point(aes(color = island,
                 shape = species),
             size = 3,
             alpha = 0.8) +
  #theme_minimal() +
  scale_color_manual(values = c("cyan", "purple","red"))+
  labs(title = "Penguin Size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for each island",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)",
       color = "Penguin Island",
       shape = "Penguin Species") +
  theme_minimal()



##Data
#library(remotes)
#remotes::install_github("allisonhorst/palmerpenguins")
#library(palmerpenguins)
#library(tidyverse)

#Variable class
class(penguins$sex)
class(penguins$body_mass_g)
class(penguins$species)
class(penguins$island)
class(penguins$flipper_length_mm)
class(penguins$bill_length_mm)
class(penguins$bill_depth_mm)

#Variable levels
levels(penguins$sex)

#Missing Data
is.na(penguins)
is.na(penguins$flipper_length_mm)
is.na(penguins$sex)

#Analysis with NA value
penguins %>%
  group_by(island) %>%
  summarise(mean(bill_length_mm))

#NA counts bar graph
penguins %>%
  #group_by(species) %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  pivot_longer(cols = 1:7, names_to = 'Variables', values_to = 'NA_count') %>%
  arrange(desc(NA_count)) %>%
  ggplot(aes(y = Variables, x = NA_count)) + geom_col(fill = '#F01777') +
  geom_label(aes(label = NA_count)) +
    scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  theme_minimal() +
  labs(title = 'Data Gap Frequency by Variable')


# Bar graph counts
penguins %>%
  count(species) %>%
  ggplot() + geom_col(aes(x = species, y = n, fill = species)) + 
  geom_label(aes(x = species, y = n, label = n)) + 
  scale_fill_manual(values = c("grey","green","black")) +
  theme_minimal() + 
  labs (title = 'Graph of Species Frequency')


penguins %>%
  #group_by(species) %>%
  count(species) %>%
  ggplot(aes(x = species, y = n)) + geom_col(fill = species) +
 #//////////////////////////////////////////slide 14//////////////////////////////////////// geom_label(aes(label = NA_count)) +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  theme_minimal() +
  labs(title = 'Data Gap Frequency by Variable')




#Variable class
class(penguins$sex)
class(penguins$body_mass_g)
class(penguins$species)
class(penguins$island)
class(penguins$flipper_length_mm)
class(penguins$bill_length_mm)
class(penguins$bill_depth_mm)


levels(penguins$sex)
levels(penguins$body_mass_g)
levels(penguins$species)
levels(penguins$island)
levels(penguins$flipper_length_mm)
levels(penguins$bill_length_mm)
levels(penguins$bill_depth_mm)

#Summary
summary(penguins)

summary(penguins$sex)
summary(penguins$island)
summary(penguins$body_mass_g)
