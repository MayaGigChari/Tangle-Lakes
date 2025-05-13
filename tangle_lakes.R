#this is a script for playing with preliminary lake trout data for the tangle lakes project

#load data from an excel file 

library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

install.packages("tm")


#questions: what is ANG vs OTH in gear type

raw_df<-read_excel("Tangles_LT_M-R_2023-24.xlsx")

raw_df$`Fish Notes`

na_data<- raw_df %>% filter(if_any(c(`Fork Length (mm)`), is.na))


#first step is to remove these things that were not sampled for the purpose of this particular exploration
#also remove this one weird tuple that has a question mark for fork length 
question_tibble<-raw_df[raw_df$`Fork Length (mm)` == '?',]

data_nona$`Fork Length (mm)`
#probably could have done this in a more neat way. 
data_nona<- raw_df %>% anti_join(na_data) %>% anti_join(question_tibble)

extract_gear<- function(string)
{
  extracted_text <- str_extract_all(string, "Gear[^;]*")
  
  # Print the extracted text
  return(extracted_text)
}

gear_types<- unlist(lapply(data_nona$`Fish Notes`, extract_gear))


#some fish were not measured? 

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


gear_types<- unlist(lapply(gear_types, shorten_name))
data_nona$gear_type_filtered<- gear_types

unique(data_nona$gear_type_filtered)


#step one: want to look at gear selectivity of length 


data_nona$`Fork Length (mm)`<-as.double(data_nona$`Fork Length (mm)`)


#plot this. 

new_labels <- c("Ang" = "Angler caught", 
                "BHJL" = "Baited Hook Jug Line", 
                "Bt" = "Bait",
                "Bt_Lre" = "Baited Lure",
                "Ice" ="Ice Fishing",
                "Un_lre" = "Unbaited Lure")

ggplot(data_nona, aes(x = gear_type_filtered, y = `Fork Length (mm)`)) +
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

#another plot: histograms of length distributions across all gear types. 

ggplot(data_nona, aes(x =`Fork Length (mm)` )) +
  geom_histogram()              

#hists of each gear type overlaying each other 

ggplot(data_nona, aes(x = `Fork Length (mm)`, fill = gear_type_filtered)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) + 
  labs(x = "Fork Length (mm)", y = "Count", fill = "Gear Type") + 
  theme_minimal()   # Optional: cleaner background

#basic bar plot of each total 

total_catch<- data_nona%>%
  group_by(gear_type_filtered)%>%
  count()


ggplot(total_catch, aes(x = gear_type_filtered, y= n))+
  geom_bar(stat = "identity")+
  labs(x = "gear type abbreviated",                        
       y = "fish caught")


##END preliminary gear-type analysis 

#spatial tests mark recapture? 


#do tests for length difference now. 