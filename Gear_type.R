#this is a script for exploring length distribution by gear type. 
#shouldn't be that too in-depth. just an exploration


#first let's look at all of the data together, from all lakes. 

library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

install.packages("tm")
extract_gear<- function(string)
{
  extracted_text <- str_extract_all(string, "Gear[^;]*")
  
  # Print the extracted text
  return(extracted_text)
}

shorten_name<-function(string)
{
  if(string == "Gear = Ice Fishing, use of bait unknown but likely")
    return("Ice")
  if(string == "Gear = Angling, unknown if bait was used")
    return("Ang")
  if(string == "Gear = Unbaited Lure")
    return("Un_lre")
  if(string == "Gear = Baited Lure")
    return("Bt_Lre")
  if(string == "Gear = Bait")
    return("Bt")
  if(string == "Gear = Baited Hookless Jug Line" )
    return("BHJL")
  else
    print(string)
  return("null")
  
}

data_full$Fork_length<- as.numeric(data_full$Fork_length)
gear_types<- unlist(lapply(data_full$Fish_notes, extract_gear))
gear_types<- unlist(lapply(gear_types, shorten_name))


#make a new dataframe to accomodate these gear types

data_geartype<- data_full
data_geartype$gear_type_filtered<- gear_types

#TODO: remember that weird thing about the gear types that someone told you at some time. 

#notes from matt
#during April 2023 I neglected to instruct the crew to record what each fish was caught with (i.e., unbaited lure, baited lure, or just bait on a hook).  Therefore, I just used “ice fishing” as the gear type for all fish captured in April 2023.

new_labels <- c("Ang" = "Angling, bait use unknown", 
                "BHJL" = "Baited Hook Jug Line", 
                "Bt" = "Bait on Hook",
                "Bt_Lre" = "Baited Lure",
                "Ice" ="Ice Fishing (undefined)",
                "Un_lre" = "Unbaited Lure")

ggplot(data_geartype, aes(x = gear_type_filtered, y = Fork_length)) +
  geom_boxplot() +                            
  geom_jitter(aes(color = gear_type_filtered), 
              shape = 18,                     #jitter points to prevent overlap 
              size = 1,                       
              width = 0.2,                    
              alpha = 0.7)+                    
  scale_color_discrete(labels = new_labels)+
  labs(x = "",                        # Change x-axis label
       y = "Fork Length (mm)",                 # Change y-axis label
       color = "Gear Type") 


#ok. this is a nice barplot. Let's try now to panel by year captured

data_geartype <- data_geartype %>%
  mutate(Year_capped = year(Date), month = month(Date))

ggplot(data_geartype, aes(x = gear_type_filtered, y = Fork_length)) +
  geom_boxplot() +                            
  geom_jitter(aes(color = gear_type_filtered), 
              shape = 18,                     #jitter points to prevent overlap 
              size = 1,                       
              width = 0.2,                    
              alpha = 0.7)+                    
  scale_color_discrete(labels = new_labels)+
  labs(x = "",                        # Change x-axis label
       y = "Fork Length (mm)",                 # Change y-axis label
       color = "Gear Type")+
  facet_wrap(vars(Year_capped))


#now let's just look at lower tangle lake, across months 


data_lower_geartype<- data_geartype%>%
  filter(Lake == "Lower")%>%
  mutate(month = month(Date))


ggplot(data_lower_geartype, aes(x = gear_type_filtered, y = Fork_length)) +
  geom_boxplot() +                            
  geom_jitter(aes(color = gear_type_filtered), 
              shape = 18,                     #jitter points to prevent overlap 
              size = 1,                       
              width = 0.2,                    
              alpha = 0.7)+                    
  scale_color_discrete(labels = new_labels)+
  labs(x = "",                        # Change x-axis label
       y = "Fork Length (mm)",                 # Change y-axis label
       color = "Gear Type")+
  facet_wrap(vars(Year_capped, month))


#here, month 4 is april and month 6 is June. 

#another way to visualize with proportion histograms. 

ggplot(data_geartype, aes(x = Fork_length, fill = gear_type_filtered)) +
  geom_histogram(aes(y = after_stat(density)), 
                 position = "identity", 
                 alpha = 0.6, 
                 bins = 30) +
  scale_fill_discrete(labels = new_labels) +
  labs(
    x = "Fork Length (mm)",
    y = "Proportion",
    fill = "Gear Type"
  ) +
  facet_wrap(vars(Year_capped)) +
  theme_minimal()


ggplot(data_geartype, aes(x = Fork_length, fill = gear_type_filtered)) +
  geom_histogram(aes(y = after_stat(density)), 
                 position = "identity", 
                 alpha = 0.6, 
                 bins = 30) +
  scale_fill_discrete(labels = new_labels) +
  labs(
    x = "Fork Length (mm)",
    y = "Proportion",
    fill = "Gear Type"
  ) +
  facet_wrap(vars(Year_capped, month)) +
  theme_minimal()

#let's visualize continuous length distributions by gear type and year now. 
ggplot(data_geartype, aes(x = Fork_length, fill = gear_type_filtered)) +
  geom_histogram(aes(y = after_stat(density)), 
                 position = "identity", 
                 alpha = 0.6, 
                 bins = 30) +
  scale_fill_discrete(labels = new_labels) +
  labs(
    x = "Fork Length (mm)",
    y = "Proportion (per gear type)",
    fill = "Gear Type"
  ) +
  facet_wrap(~gear_type_filtered) +
  theme_minimal()

ggplot(data_geartype, aes(x = Fork_length, fill = gear_type_filtered)) +
  geom_histogram(aes(y = after_stat(density)), 
                 position = "identity", 
                 alpha = 0.6, 
                 bins = 30) +
  scale_fill_discrete(labels = new_labels) +
  labs(
    x = "Fork Length (mm)",
    y = "Proportion (per gear type)",
    fill = "Gear Type"
  ) +
  facet_grid(gear_type_filtered ~ Year_capped) +
  theme_minimal()


#so upon visualization, it looks like we catch bigger fish with a jug line, which we knew already. 


#we could also separate by lake. 

ggplot(data_geartype, aes(x = Fork_length, fill = gear_type_filtered)) +
  geom_histogram(aes(y = after_stat(density)), 
                 position = "identity", 
                 alpha = 0.6, 
                 bins = 30) +
  scale_fill_discrete(labels = new_labels) +
  labs(
    x = "Fork Length (mm)",
    y = "Proportion (per gear type)",
    fill = "Gear Type"
  ) +
  facet_grid(gear_type_filtered ~ lake_combined)+
  theme_minimal()



#######################################
#let's do some statistics. 
#distributions look sort of normal, but there is kind of sparse data for some stuff becuase of the data error in geartype recording in 2024. 

#first, filter to geartypes that are present in both years


data_geartype_stat<- data_geartype%>%
  filter(gear_type_filtered %in% c("BHJL", "Bt_Lre", "Un_lre"))
#perform an anova 
res_aov <- aov(Fork_length ~ gear_type_filtered,
               data = data_geartype_stat
)


ggplot(data_geartype_stat, aes(x = Fork_length, fill = gear_type_filtered)) +
  geom_histogram(aes(y = after_stat(density)), 
                 position = "identity", 
                 alpha = 0.6, 
                 bins = 30) +
  scale_fill_discrete(labels = new_labels) +
  labs(
    x = "Fork Length (mm)",
    y = "Proportion (per gear type)",
    fill = "Gear Type"
  ) +
  facet_grid(gear_type_filtered ~ Year_capped+lake_combined) +
  theme_minimal()

hist(res_aov$residuals)
library(car)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

#here we see the residuals are not normal, so we cannot use an anova/assumption of normal distribution. 
shapiro.test(res_aov$residuals)

#we use a test that does not require normality, and we see that there is a significant difference between 
kruskal.test(Fork_length ~ gear_type_filtered, data = data_geartype_stat)

#we will use the ART to test for interaction of year

install.packages("ARTool")
library(ARTool)

# Run aligned rank transform
art_model_year <- art(Fork_length ~ factor(gear_type_filtered) * factor(Year_capped), data = data_geartype_stat%>%drop_na(Fork_length))

# ANOVA table on aligned ranks
anova(art_model_year)

#we can see here by the high interaction p value that there is a significant interaction of year. However, this is likely because 2023 data is much more sparse than 2024 data 
#for fishing types that might have been "ice fishing". 

art_model_lake <- art(Fork_length ~ factor(gear_type_filtered) * factor(lake_combined), data = data_geartype_stat%>%drop_na(Fork_length))
anova(art_model_lake)

#but there is not a significant interaction of lake. 

#in conclusion, we can see that the three consistent gear types have significantly different fork length associations, that does not depend on the lake but might have a mild dependency on the year. 
#this is usefulbecause now we can just bin all the lakes together. 


###we can go on and bin the fork lengths like we did for the length distribution analysis and then just group by geartype. 

#bin the whole dataset. 

data_full_binned<- assign_bins(data_geartype_stat%>%drop_na(Fork_length))
binned_length_stats_data_full<- binned_length_statistics(data_full_binned, geartype = TRUE)
view(binned_length_stats_data_full)


ggplot(binned_length_stats_data_full, aes(x = bin, y = proportion)) +
  geom_col( fill = "skyblue", color = "black", alpha = 0.7) +
  geom_errorbar(aes(ymin = proportion - sqrt(variance),
                    ymax = proportion + sqrt(variance)),
                width = 0.2, color = "red") +
  labs(
    x = "Length Bin (mm) ",
    y = "Proportion",
    title = "Length Distribution with Error By Gear Type"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.margin = margin(10, 20, 20, 20)
  )+
  facet_wrap(~gear_type_filtered)


#great. now we have a visual and tabular form of the length distribution for all three lakes across gear type. 