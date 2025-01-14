2+2
2+3
library(tidyverse)
sample_data <- read_csv("sample_data.csv")



#assign values to objects 

name <- "agar"
name 
name <- "Fanny Hesse"
name 

#bad names for objects 
#numbers are not good at beginning 
#objects ae case-sensitive 

Sys.Date()
getwd() #outputs the current working directory

sum(5,6) #adds numbers 

sample_data <- read_csv("sample_data.csv") #reads in csv file 

#creating first plot 
ggplot(data = sample_data) + 
  aes(x = temperature) +
  labs(x = 'Temperature (C)') + 
  aes(y = cells_per_ml/1000000,) +
  labs(y = 'Cells (million/mL)') + 
  geom_point() +
  labs(title = 'Does temperature affect microbial abundance?') + 
  aes(color = env_group) + 
  aes(size = chlorophyll) + 
  aes(shape = env_group) + #size of points representative of sample relative to others 
  labs(size = 'Chlorophyll (ug/L', 
       color = 'Environmental Group', 
       shape = 'Environmental Group')

#combined neater code 
ggplot(data = sample_data) + 
  aes(x = temperature, 
      y = cells_per_ml/1000000,
      color = env_group,
      size = chlorophyll) + 
  geom_point() +
  labs(x = 'Temperature (C)', 
       y = 'Cells (millions/mL)', 
       title = 'Does temperature affect microbial abundance?',
       size = 'Chlorophyll (u/mL)', 
       color = 'Environmental Group')


#importing datasets 
buoy_data <- read_csv('buoy_data.csv')
View(buoy_data)

dim(buoy_data) #dimensions
head(buoy_data)
tail(buoy_data)

#plot some more 
#introduce facets 
ggplot(data = buoy_data) + 
  aes(x = day_of_year, 
      y = temperature, 
      group = sensor,
      color = depth) +
  geom_line() + 
  facet_wrap(~buoy, scales = 'free_y')

# facet grid 
ggplot(data = buoy_data) + 
  aes(x = day_of_year, 
      y = temperature, 
      group = sensor,
      color = depth) +
  geom_line() + 
  facet_grid(rows = vars(buoy))

#structure of data object 
str(buoy_data)

#discrete plots 
#box plot 
ggplot(data = sample_data) + 
  aes(x = env_group,
      y = cells_per_ml) + 
  geom_boxplot(aes(fill = env_group)) + 
  scale_fill_manual(values = c('pink', 'tomato', 'blue'))

#colors - bwerer 
ggplot(data = sample_data) + 
  aes(x = env_group,
      y = cells_per_ml) + 
  geom_boxplot(aes(fill = env_group)) + 
  scale_fill_brewer(palette = 'PIYG')

#custom palette time 
library(wesanderson)
#install.packages('wesanderson')
ggplot(data = sample_data) + 
  aes(x = env_group,
      y = cells_per_ml) + 
  geom_boxplot(aes(fill = env_group)) + 
  scale_fill_manual(values = wes_palette('Royal2'))

#install.packages('ghibli')
library(ghibli)
ggplot(data = sample_data) + 
  aes(x = env_group,
      y = cells_per_ml) + 
  geom_boxplot(aes(fill = env_group)) + 
  scale_fill_manual(values = ghibli_palette('KikiLight'))

#install.packages("devtools")
#devtools::install_github("kevinsblake/NatParksPalettes")
library(devtools)
library(NatParksPalettes)
ggplot(data = sample_data) + 
  aes(x = env_group,
      y = cells_per_ml) + 
  geom_boxplot(aes(fill = env_group)) + 
  scale_fill_manual(values = natparks.pals("Olympic"))

#change transparency 
ggplot(data = sample_data) + 
  aes(x = env_group,
      y = cells_per_ml) + 
  geom_boxplot(fill = 'darkblue',
               alpha = 0.3)


#univariate plots 
ggplot(sample_data) + 
  aes(x = cells_per_ml) +
  geom_density(aes(fill = env_group), alpha = 0.5) + 
  theme_linedraw()

#rotate x axis 
box_plot <- 
  ggplot(data = sample_data) + 
  aes(x = env_group,
      y = cells_per_ml) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#saving plots 
ggsave('awesome_plot.jpg', width = 6, height = 4, dpi = 500)

#add changes to plot 
box_plot + theme_bw()

box_plot <- box_plot + theme_bw()
box_plot
