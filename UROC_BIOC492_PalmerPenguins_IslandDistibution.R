library(palmerpenguins)
library(tidyverse)
library(dplyr)
library(base)

remotes::install_github("allisonhorst/palmerpenguins")

view (penguins)

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


# Bar graph by species counts
  penguins %>%
    count(species) %>%
    ggplot() + geom_col(aes(x = species, y = n, fill = species)) + 
    geom_label(aes(x = species, y = n, label = n)) + 
    scale_fill_manual(values = c("grey","green","black")) +
    theme_minimal() + 
    labs (title = 'Graph of Species Frequency')

# Bar graph island counts
  penguins %>%
    count(island) %>%
    ggplot() + geom_col(aes(x = island, y = n, fill = island)) + 
    geom_label(aes(x = island, y = n, label = n)) + 
    scale_fill_manual(values = c("grey","green","black")) +
    theme_minimal() + 
    labs (title = 'Graph of Island Frequency')
  


PenguinsSpeciesBiscoe <- penguins %>%
  filter(., island == 'Biscoe', na.rm = TRUE) %>%
  group_by(., species)
PenguinsSpeciesDream <- penguins %>%
  filter(., island == 'Dream', na.rm = TRUE)
PenguinsSpeciesTorgersen <- penguins %>%
  filter(., island == 'Torgersen', na.rm = TRUE)

summary(PenguinsSpeciesBiscoe)
summary(PenguinsSpeciesDream)
summary(PenguinsSpeciesTorgersen)

##Visualize Island V Penguins
distribTable <- table(penguins$species,penguins$island)
barplot(distribTable, ylab = 'Frequency of Penguins',xlab = 'Islands', col = 8:10, legend = rownames(propTable))


propTable <- prop.table(distribTable)
barplot(propTable, ylab = 'Percent of Penguins', xlab = 'Islands', col = 5:10, legend = rownames(propTable))

#NOTED: Only the Adelie Penguins occur on more than one island. The remainder of this research will focus on this.
#As Adelie penguins are the only penguins that are on Torgersen Island, the methodology will be adjusted.
#We will now perform a 2 proportion z test to determine 
#if the proportion of Adelie Penguins over the total penguin population of Biscoe and Dream Island.
#This will be conducted with an alpha of .05.

biscoeSplit <- PenguinsSpeciesBiscoe # %>%

DreamProp <- filter(PenguinsSpeciesDream)




  

penguins %>%
  #group_by(penguins$species) %>%
  filter(penguins$island) #%>%
  #as.data.frame() %>%
  #rowid_to_column(., penguins)
  
  
view(PenguinsSpeciesIsland)



