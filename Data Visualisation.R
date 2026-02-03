library(readxl)
library(tidyverse)
library(ggrepel)
library(patchwork)
library(viridis)

### Import and Process Business Demography Data
Business_Demography_Data <- 
  read_excel("Business Demography Data.xlsx") %>%
  # Remove region and county data
  filter(! Area %in% c('Yorks and Humber', 'North Yorks County', 
                       'South Yorks Met', 'West Yorks Met')) %>%
  # Convert values to numeric and round) 
  mutate(across(3:ncol(.), ~ round(as.numeric(.), 0))) %>% 
  ## Reshape data
  # Lengthen
  pivot_longer( 
    cols = 3:ncol(.), # number of years in dataset
    names_to = "Year",
    values_to = "Values") %>%
  # Widen
  pivot_wider(
    names_from  = Category,
    values_from = Values,
    names_sep = "-") %>%
  # Add new columns
  mutate(
    Net = Births - Deaths,
    Net_pct = (Births - Deaths) / Active * 100,
    Births_pct = Births / Active * 100,
    Deaths_pct = Deaths / Active * 100,
    Year = as.numeric(Year))


# Get baseline (2009) enterprise values for each area
baseline <- Business_Demography_Data %>%
  filter(Year == 2009) %>%
  select(Area, baseline_active = Active) 

# Calculate percentage change in active enterprises using baseline column
Business_Demography_Data <- Business_Demography_Data %>% 
  left_join(baseline, by = "Area") %>% 
  mutate(Active_pct_change = (Active - baseline_active) / baseline_active * 100) %>%
  select(-'baseline_active') # Remove baseline column

# Build summary df
area_summary <- Business_Demography_Data %>%
  group_by(Area) %>%
  summarise(
    avg_births = mean(Births), 
    avg_deaths = mean(Deaths),
    avg_births_pct = mean(Births_pct), 
    avg_deaths_pct = mean(Deaths_pct),
    avg_net = mean(Net), 
    avg_net_pct = mean(Net_pct),
    active_pct_growth_22 = Active_pct_change[Year == "2022"]) %>%
  arrange(Area)




### Import and Process Survival Rate Data 
Survival_Rate_Data_R <- read_excel("Survival Rate Data R.xlsx") %>%
  mutate(Year = as.numeric(Year)) # convert year to numeric

# Build dataframe summarising survival rates
survival_rate_summary <- Survival_Rate_Data_R %>%
  group_by(Area) %>%
  summarise(
    '1' = mean(`Year 1 Survival %`, na.rm = TRUE),
    '2' = mean(`Year 2 Survival %`, na.rm = TRUE),
    '3' = mean(`Year 3 Survival %`, na.rm = TRUE),
    '4' = mean(`Year 4 Survival %`, na.rm = TRUE),
    '5' = mean(`Year 5 Survival %`, na.rm = TRUE))


### Figure A: Bar Chart showing growth
area_summary %>% 
  # Set alpha values for areas
  mutate(alpha_value = if_else(Area == "Doncaster", 1, 0.9)) %>%
  
  ggplot(aes(
    x = reorder(Area, active_pct_growth_22), # order by growth
    y = active_pct_growth_22)) +
  
  geom_bar(
    aes(
      alpha = alpha_value), # use alpha values to highlight Doncaster
      fill = 'blue',
      stat = 'identity') +
  
  scale_alpha_continuous(range = c(0.5,1)) + # scale for alpha
  
  # Add percentage growth values
  geom_text(
    aes(
      label = paste0(round(active_pct_growth_22), "%")),
      hjust = -0.1,
      size = 3) +
  
  # Add labels
  labs(
    # underline title
    title = expression(underline("Figure A: Growth in Number of Businesses 2009-2022")), 
    subtitle = "Doncaster's Performance in this Period was Exceptional",
    y = '% Growth', x = 'Area') +
  
  # Edit theme
  theme_classic() +
  theme(
    legend.position = 'none', # remove legend for alpha scale
    # edit subtitle
    plot.subtitle = element_text(size = 10, color="gray30", vjust = 2)) +
  
  # Flip to horizontal  
  coord_flip(ylim = c(0,60)) 
  



### Figure B, Dumbbell chart, Births and Death rates by Area

# Data for Figure B
fig_b_data <- area_summary %>%
  
  # get birth and death rates
  select(Area, avg_births_pct, avg_deaths_pct) %>% 
  
  # find difference in birth and death rate
  mutate(diff = avg_births_pct - avg_deaths_pct) %>% # calculate net change
  
  # Create column containing maximum value by area
  group_by(Area) %>%
  mutate(max = pmax(avg_births_pct, avg_deaths_pct)) %>%
  ungroup() %>%
  
  # Arrange Areas by absolute difference
  mutate(Area = forcats::fct_reorder(Area, abs(diff))) %>%
  
  # pivot longer so birth and death points can plotted by category
  pivot_longer(c(avg_births_pct, avg_deaths_pct))

# Code for the main part of the figure
fig_b_main <- fig_b_data %>%
  mutate(
    # Round values
    value = round(value, 1),
    # change names of categories for display
    name = recode(name, 
                  avg_births_pct = "Birth Rate",
                  avg_deaths_pct = "Death Rate")) %>%
  
  ggplot(aes(x = value, y = Area)) + 
  
  # Add line between points
  geom_line(aes(
    group = Area), # lines to be drawn for each area's values
    color="azure3", 
    linewidth = 3.5) + # change line width to fit between points
  
  # Add points
  geom_point(aes(
    color = name, # colour and shape based on category
    shape = name), 
    size = 3) +
  
  # Add birth and death rate labels to top of graph
  geom_text(
    aes(label = name, color = name), 
    # position text in line with points at the top
    data =. %>% filter(diff == max(diff)), # position is area with biggest difference
    nudge_y = 1, # adjust label positions
    fontface = "bold", size = 3.25) + 
  
  # Add Values to points
  geom_text(
    aes(label = value, color = name), size=3.25,
            
    # Position text based on whether it should be the right or left of points
    nudge_x = if_else( 
      fig_b_data$value == fig_b_data$max, # If its the larger value for an area
      0.1, # Move it to the right
      -0.1), # Else move it left
            
    hjust = if_else(
      fig_b_data$value == fig_b_data$max, # If it's the larger value
      0, # Don't move it
      1 # Move it left
            ))+
  
  
  # Edit theme                                                       
  theme_classic() +
  theme(
    legend.position = 'none', # Remove Legend
    plot.subtitle = element_text(size = 10, color="gray15", vjust = 2) 
    ) + 

  # Set colours and shapes for points
  scale_color_manual(values = c('Birth Rate' = 'dark green', 'Death Rate' = 'red')) +
  scale_shape_manual(values = c('Birth Rate' = 15, # Square
                                'Death Rate' = 16 # Circle
                                )) +
  
  labs(x = 'Births/Deaths Per 100 Businesses', 
       title = expression(underline('Figure B: Business Birth and Death Rates')),
       subtitle = 'Birth: Creation of a business\nDeath: Closure of a business.') +
  
  # Add label
  annotate('text', x = 14.5, y = 16, size = 3,
     
                   label = 'Doncaster has\nthe highest annual\n birth, death and\nnet change rates.') +
  # Change limits so text fits
  coord_cartesian(xlim = c(7.75,16), ylim=c(1, 22)) 

# Data for second part of figure
fig_b_data_part_two <- area_summary %>%
  select(Area, avg_births_pct, avg_deaths_pct) %>%
  mutate(gap = avg_births_pct - avg_deaths_pct) %>%
  arrange(-abs(gap))

# Code for the second part of the figure
fig_b_minor <- fig_b_data_part_two %>%
  ggplot() +
  
  # Plot values
  geom_text(aes(x = 11.75, y = reorder(Area, abs(gap)), label = round(gap,2)), size = 3.25) +
  
  # Label
  geom_text(aes(x = 11.6, y = 22, label = 'Net'), size = 3.25) +
  
  # Remove theme, gives a blank canvas to work with
  theme_void() +
  theme(
    plot.margin = margin(l=0, r=0, b=0, t=0), # otherwise it adds too much space
    panel.background = element_rect(fill="azure2", color="azure2"), # Change background colour
    legend.position = "none")+
  
  # Needs to be same size as part 1
  coord_cartesian(xlim = c(7.75,16), ylim=c(1, 22)) 

# Combine plots using plotwise
fig_b <-  fig_b_main + fig_b_minor + plot_layout(
  design =
    # 
    c(area(l = 0,  r = 101, t = 0, b = 1), # area for the main figure
    area(l = 92, r = 97, t = 0, b = 1)))  # area for the second figure
                                  

# Plot second figure
fig_b




### Figure 3, survival reates heatmap
  
## Create summary df for survival data
# Average survival % for each year by area
fig_3_data <- survival_rate_summary %>%
  pivot_longer(cols = 2:6,
                names_to = 'Year',
                values_to = 'Survival_pct') %>%
  arrange(Year)

fig_3_data %>%
  # add column containing area final surival rate, in order to rank them
  group_by(Area) %>%
  mutate(final_survival = min(Survival_pct)) %>% 
  ungroup() %>%
  
  ggplot() +
  
  # Geom_tile creates the heatmap 
  geom_tile(
    aes(
      x = Year, 
      y = reorder(Area, -final_survival), # order by final survival rate
      fill = Survival_pct)) + # colour inside aesthetics so set by survival %
  
  # Add text showing final survival rates
  geom_text(
    data = . %>% filter(Year == '5'),
    aes(
    x = 6,
    y = reorder(Area, -final_survival), # order text same way as areas
    label = paste0(round(Survival_pct), '%')), # paste adds % 
    size = 3,
    hjust = 0.2)+
  
  # Choose colour scale
  scale_fill_viridis(discrete = FALSE) +
  
  theme_classic() +
  
  #Adjust position of title
  theme(
    plot.title  = element_text(hjust = 0.5, size = 12)) +
  
  labs(
    title = 'Figure C: Doncaster has the Lowest 5 Year\nSurvival Rate for New Businesses',
    y = 'Area',
    fill = 'Survival %') +
  
  coord_fixed(xlim = c(0,6.5))


### Figure 4: stacked bar chart comparing births and five year survival 
fig_4_data <- area_summary %>%
  select(Area, avg_births_pct) %>%
  mutate(
    # Calculate how many survive 5 years
    survive_five_years = avg_births_pct * (survival_rate_summary$'5'/100),
    # Set value to colour bar outlines
    colour_value = if_else(Area == "Doncaster", 'Doncaster', 'Other areas')) 

fig_4_data %>%
  ggplot() +
  
  # Add bars for businesses that survived
  geom_bar(
    aes(
      x = survive_five_years,
      y = reorder(Area, survive_five_years)),
    stat = 'identity', 
    fill = 'deepskyblue') +
  
  # Add bars for businesses that died
  geom_bar(
    aes(
      x = avg_births_pct, 
      y = reorder(Area, survive_five_years),
      # outline of bars based on colour_value (highlights Doncaster)
      color = colour_value), 
    stat = 'identity', 
    alpha = 0.5, # lower alpha means bars for surviving businesses are overlayed
    fill = 'gray60') +
  
  # Add values for businesses that survived
  geom_text(aes(
      x = survive_five_years,
      y = reorder(Area, survive_five_years),
      label = round(survive_five_years, 1)),
      hjust = 1.2,
      size = 2.8) +
  
  # Add values for businesses that died
  geom_text(aes(
    x = avg_births_pct, 
    y = reorder(Area, survive_five_years), 
    label = round(avg_births_pct, 1)),
    hjust = 1.2,
    size = 2.8) +
  
  # Label bars 
  annotate('text', x = 3, y = 22, size = 4, color = 'deepskyblue4', fontface = 'bold',
           label = 'Survived 5 Years') +
  
  annotate('text', x = 8, y = 22, size = 4, color = 'gray30', fontface = 'bold',
           label = 'Births') +
  
  theme_classic() +
  
  theme(legend.position = 'none', # remove legend
        plot.subtitle = element_text(size = 11, color="gray15", vjust = 2)) +
  
  labs(
    title = expression(underline('Figure D: Births and Five Year Survival')),
    subtitle = 'As a result of its low survival rates, Doncaster is not expectional\nfor the number of new businesses that survive five years',
    x = 'Per 100 Businesses',
    y = 'Area') +
  
  # set colour of bar outlines so Doncaster stands out
  scale_color_manual(values = c('Doncaster' = 'black', 'Other areas' ='white')) +
  
  coord_cartesian(ylim = c(0,22))
  


### Figure E, stacked bar, compare growth in all and established businesses

## Calculate Established Businesses Data
num_young_businesses <- Survival_Rate_Data_R %>%
  select(-'Year 1 Survival %', -'Year 2 Survival %', -'Year 3 Survival %', -'Year 4 Survival %',
         -'Year 5 Survival %') %>%
  arrange(Area, Year) %>%
  group_by(Area) %>%
  mutate(
    Young_businesses =
      # lag goes up one column and along one row to total surviving businesses from each year
      lag(`Year 1 Survival`, 1) + 
      lag(`Year 2 Survival`, 2) +
      lag(`Year 3 Survival`, 3) +
      lag(`Year 4 Survival`, 4) +
      lag(`Year 5 Survival`, 5)) %>%
  filter(Year > 2013)

# subset active enterprises data
Business_Demography_Data_14_to_22 <- Business_Demography_Data %>%
  filter(Year > 2013) %>%
  arrange(Area, Year) %>%
  select(Area, Year, Active) 

# Combine processed survival rate data with subset of active enterprises data
Established_businesses <- num_young_businesses %>%
  select(Area, Year, Young_businesses) %>% # subset survival rate data
  arrange(Area, Year) %>%
  # combine with active enterprises data
  left_join(Business_Demography_Data_14_to_22, join_by('Area', 'Year')) %>%
  # Calculate number of established businesses
  mutate(Established = Active - Young_businesses)
  
# Get baseline established and all enterprise values for each area
baselines <- Established_businesses %>%
  filter(Year == 2014) %>%
  select(Area, baseline_Established = Established,
         baseline_active = Active) 

# Calculate percentage change for all and established enterprises using baseline columns
Established_businesses <- Established_businesses %>% 
  left_join(baselines, by = "Area") %>% # add baseline column
  filter(Year == 2022) %>%
  
  # calculate growth in all and established enterprises
  mutate(Active_pct_change_14 = (Active - baseline_active) / baseline_active * 100,
         Established_pct_change = (Established - baseline_Established) / baseline_Established * 100) %>%
  
  # select columns for use in visualisation
  select(Area, Active_pct_change_14, Established_pct_change) %>%
  
  # pivot to longer form for plotting as stacked bar chart
  pivot_longer(
    cols = 2:3,
    names_to = 'Metric',
    values_to = 'Growth'
  )

## Plot figure E
Established_businesses %>%
  # Rename Metrics
  mutate(Metric = recode(Metric, 
                         'Established_pct_change' = 'Established Businesses',
                         'Active_pct_change_14' = 'All Businesses')) %>% 
  
  # Order Areas by growth in Established Businesses
  group_by(Area) %>%
  mutate(order_by = Growth[Metric == 'Established Businesses']) %>%
  ungroup() %>%
  arrange(order_by) %>%
  
  # Plot
  ggplot() +
  
  # Add stacked bars
  geom_bar(aes(
    x = Growth, # x = value for horizontal graph
    y = reorder(Area, order_by), # Rank by growth in established businesses
    fill = Metric), 
    stat = 'identity', 
    position = position_stack()) + # stack bars
  
  # Set colours for bars 
  scale_fill_manual(values = c('All Businesses' = 'light blue', 
                               'Established Businesses' = 'blue')) +
  
  theme_classic() +
  theme(
    legend.position = 'none', # remove legend
    plot.subtitle = element_text(size = 10, color="gray15", vjust = 2),
    plot.caption = element_text(hjust = -0.2)) +
  
  labs(title = expression(underline('Figure E: Areas ranked by Growth in Established Businesses')),
       subtitle = 'Established Businesses are 5 years and older',
       x = 'Area', y = '% Growth', 
       caption = 'Data 2014-2022 due to calculation of 5 year survival'
       ) +
  
  # Label bars 
  annotate('text', x = 10, y = 22, size = 4, fontface = 'bold',
           label = 'Established', vjust = 0.2) +
  
  annotate('text', x = 40, y = 22, size = 4, fontface = 'bold',
           label = 'All Businesses', vjust = 0.2) +
  
  # Add text 
  annotate('text', x = 45, y = 5, size = 3.5, 
           label = "Doncaster's growth in established businesses\nis much lower due to a high number of\nyoung businesses that don't survive.") +
  
  coord_cartesian(ylim = c(0,22))










