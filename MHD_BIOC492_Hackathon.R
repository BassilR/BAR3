
library(tidyverse)
library(remotes)

#Hackathon pizza v subway fare data
#Conversion Library

#install.packages(c("ISwR","languageR"),dependencies = T)
#library(ISwR)
#library(languageR)

#Add data to library
library(readxl)
Fare_Data <- read_excel("C:/Users/bassi/OneDrive/Desktop/MHD_BIOC492_HackathonData_072020NYCMTAJuly2014.xlsx", 
                        sheet = "Sheet1", n_max = 40)
View(Fare_Data)

#Add Pizza cost data to library
library(readxl)
Pizza_Cost <- read_excel("C:/Users/bassi/OneDrive/Desktop/MHD_BIOC492_HackathonData_072020NYCMTAJuly2014.xlsx", 
                         sheet = "Sheet2")
View(Pizza_Cost)






#QUESTION 1
#Where does the pizza principle hold true?
##Check all occasions when the media cost in an area is equal to $2.50

Pizza_Cost %>% filter(Pizza_Cost$Median == 2.5)
view(Pizza_Cost$'MTA Station', which(Pizza_Cost$'Median'==2.5))




#data viz scatter sizes x island  
ggplot(data = penguins,        
       aes(x = flipper_length_mm,             
           y = body_mass_g)) +   
  geom_point(aes(color = island,                   
                 shape = species),               
             size = 3,              
             alpha = 0.8) +   
  #theme_minimal() +   
  scale_color_manual(values = c("black","pink","grey")) +   
  labs(title = "Penguin size, Palmer Station LTER",        
       subtitle = "Flipper length and body mass for each island",        
       x = "Flipper length (mm)",        
       y = "Body mass (g)",        
       color = "Penguin island",        
       shape = "Penguin species") +   
  theme_minimal()





#QUESTION 2
#Where does the pizza principle not hold true?
##Check all occasions when the median cost in an area is not equal to $2.50

Pizza_Cost %>% filter(Pizza_Cost$Median != 2.5)
view(Pizza_Cost$'MTA Station', which(Pizza_Cost$'Median'!=2.5))

#data visualization
##First graph will show differential vs neighborhoods


ggplot(data = Pizza_Cost,
       aes(x = mean,
           y = MTA_Station))+


##Second graph which will show number of riders v differential



