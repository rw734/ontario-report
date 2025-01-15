#loading packages 

library(tidyverse)

# reading in data

sample_data <- read_csv('data/sample_data.csv')

summarize(sample_data, average_cells = mean(cells_per_ml))

sample_data %>% 
  summarize(average_cells = mean(cells_per_ml))

#filtering rows 

sample_data %>%
  filter(env_group %in% c('Deep', 'Shallow_may')) %>%
  summarize(average_cells = mean(cells_per_ml))

sample_data %>%
  filter(str_detect(env_group, 'Shallow*')) %>%
  summarize(average_cells = mean(cells_per_ml))

#calculate avergare chlorphyll in entire dataset 

sample_data %>% 
    summarize(average_chloro = mean(chlorophyll))

#calculate average chlorophyll just in september 

sample_data %>% 
  filter(str_detect(env_group, 'September*')) %>%
  summarize(average_chloro = mean(chlorophyll))

#calculate average chlorophyll just in shallow september

sample_data %>%
  filter(env_group == 'Shallow_September') %>%
  summarize(avg_chl = mean(chlorophyll))

#group_by 

sample_data %>% 
  group_by(env_group) %>%
  summarize(average_cells = mean(cells_per_ml), 
            min_cells = min(cells_per_ml))

# calculate the average temp per env_group 

sample_data %>% 
  group_by(env_group) %>% 
  summarize(avg_temp = mean(temperature))

# mutate
# TN:TP

sample_data %>% 
  mutate(tn_tp_ratio = total_nitrogen / total_phosphorus) %>% view 

#another example 
sample_data %>% 
  mutate(temp_is_hot = temperature > 8) %>% 
  group_by(env_group, temp_is_hot) %>% 
  summarize(avg_temp = mean(temperature), 
            avg_cells = mean(cells_per_ml))

# selecting columns with the select function 

sample_data %>% 
  select(sample_id, depth)

# take out columns in dataset 

sample_data %>% 
  select(-env_group)

# more on select 

sample_data %>% 
  select(sample_id:temperature)

sample_data %>% 
  select(starts_with('total'))

#(above) - these are called "helper functions" 

# create a dataframe with only sample_id, env_group, depth, temp, cells_per_mil 

sample_data %>% 
  select(sample_id, env_group, depth, temperature, cells_per_ml)

sample_data %>% 
  select(sample_id:temperature)
 
sample_data %>% 
  select(1:5)

sample_data %>% 
  select(-starts_with('total'), -chlorophyll, -diss_org_carbon)

sample_data %>% 
  select(-(total_nitrogen:chlorophyll))

# CLEANING DATA 
# pro-tip: read in data w/o assigning to object just to see what you're working with first, then, assign to objecy 

taxon_clean <- read_csv('data/taxon_abundance.csv', skip = 2) %>% 
  select(-...9, -...10, -Lot_Number) %>% view 

# compare long and wide data 
# tip to discern long data- when a single column has repeat values or multiple columns have the same units 
# oftentimes tidyverse insists on long format 

taxon_long <- taxon_clean %>% 
  pivot_longer(cols = Proteobacteria:Cyanobacteria, 
               names_to = 'Phylum', 
               values_to = 'Abundance')

taxon_long %>% 
  group_by(Phylum) %>% 
  summarize(avg_abund = mean(Abundance))

taxon_long %>% 
  ggplot() + 
  aes(x = sample_id,
      y = Abundance, 
      fill = Phylum) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

# making long data wide 
taxon_long %>% 
  pivot_wider(names_from = 'Phylum',
              values_from = 'Abundance')

# JOINING DATA FRAMES 
head(sample_data)

head(taxon_clean)
 #types of joins: inner join, full join, left join, right join, anti join (only keeping key pairs that are different e.g. containing info and having NA value)

inner_join(sample_data, taxon_clean, by = 'sample_id')

anti_join(sample_data, taxon_clean, by = 'sample_id')

sample_data$sample_id
taxon_clean$sample_id

taxon_clean_goodSep <- taxon_clean %>% 
  mutate(sample_id = str_replace(sample_id, pattern = 'Sep', replacement = 'September'))

sample_and_taxon <- inner_join(sample_data, taxon_clean_goodSep, by = 'sample_id')

write_csv(sample_and_taxon, file = 'data/sample_and_taxon.csv') 

# make a plot 
# ask: where does chloroflexi like to live? 
library(ggpubr)
sample_and_taxon %>% 
  ggplot() +
  aes(x = depth, 
      y = Chloroflexi) + 
  geom_point() + 
  labs(x = 'Depth (m)',
       y = 'Chloroflexi relative abundance') + 
  geom_smooth(method = 'lm') +
  #stat_regline_equation() + 
  stat_cor() + 
  annotate(geom = 'text', 
           x = 25, y = .3,
           label = 'this is a text label' )

# what is the average abundance and standard deviation 
# of chloroflexi in ou 3 env_groups 

sample_and_taxon %>% 
  group_by(env_group) %>% 
  summarize(avg_abund = mean(Chloroflexi), 
            stand_dev = sd(Chloroflexi))
  
