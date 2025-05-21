#this is a script to clean up the raw dataframe. 

install.packages("fishmethods")
install.packages("recapr")
install.packages("lubridate")

library(readxl)
library(fishmethods)
library(recapr)
library(dplyr)
library(tidyr)
library(lubridate)


#something to remember: n2 is n2 + m2. all recapped that were marked are also just recapped! 
#this first part is only for recaps!

#only had 5 morts and they were all in 2023 and none of them were recaps. 
#can use the morts for length stratification BUT NOT for anything else. 

#the following is including the morts. 


#read df and remove totally useless columns
raw_df<-read_excel("Tangles_LT_M-R_2023-24.xlsx")
raw_df<- raw_df%>%
  select(-c(Event_recapped_original,...27))


#read.csv(raw_df, file = "raw_data.csv") #saving a copy of the raw raw data just in case. 

colnames(raw_df)

#remove columns that are not useful now. 
trimmed_df = subset(raw_df, select = -c(18:23) ) 

#rename columns 
colnames(trimmed_df)<- c("Lake", "Unique_event", "Project_Type", "Data_origin", "Gear", "Set", "Lat", "Lon", "Date", "Species", "Fork_length", "Weight", "Sex", "Mark", "Floy", "PIT", "Recap", "cap_label", "Fish_notes")

#assign an event to each tuple
event_converter<- function(date)
{
  if(date <= "2023-05-01")
    return(1)
  if(date >= "2023-05-01" && date <= "2024-01-01")
    return(2)
  if(date >= "2024-01-01" && date <="2024-05-01")
    return(3)
  if(date >= "2024-05-01")
    return(4)
}

event_num<- unlist(lapply(trimmed_df$Date, event_converter))

trimmed_df$event_num<- event_num

trimmed_df = subset(trimmed_df, select = -c(2:4))

#"Cleaned" data. 

#now want to add a status that allows morts to be considered for length distribution 

#step 1: convert the morts captured in event 1 and 2 to "n1"'s
#I checked and all morts occurred in either april or june of 2023, and none of the june 2023 were recap from april 2023. 
#Thus, we can convert "Mort" labels to n1 labels for the purpose of the length composition analysis. 

#these morts are not true N1's for the purpose of the mark recapture.
#what about morts in event 2 and event 3? 
status_finder <- function(event_num, cap_status) {
  #print(cap_status)
  if (event_num == 1 || event_num == 2) {
    if (cap_status == "Mort") {
      return("n1")
    }
  }
  if (event_num == 3 || event_num ==4) {
    if (cap_status == "Mort") {
      print("n2 mort")
    }
  }
  return(cap_status)
}

#add a column for the shallow/round = same case. 
lake_data_mod<- trimmed_df %>%
  mutate(lake_combined = case_when(
    Lake %in% c("Shallow", "Round") ~ "Shallow_Round",  # Combine shallow and round
    TRUE ~ Lake  # Keep other lakes as they are
  ))

#turns the morts into n1's for the length composition. THERE WERE NO MORTS IN events 3 and 4.
#stat is a new column that is ONLY useful for length composition analysis, nothing else. 
stat <- mapply(status_finder, 
               event_num = lake_data_mod$event_num, 
               cap_status = lake_data_mod$cap_label)

#add this to the dataframe. stat is the amended status for LENGTH COMP ONLY. 
lake_data_mod$stat<- stat


#test 1: April + June 2023 ----> April + June 2024

lakes = list("Shallow","Round", "Lower", "Upper")
mod_lakes<-list("Shallow_Round", "Lower", "Upper")
events = c("n1", "n2", "m2")

#here below are some modifications to make sure that all the m2's are also counted as n2's (total captured in 2nd event = m2+n2)
#essentially making a new column called grouping that allows for the m2's to be counted both as m2's and when necessary as n2's.


lake_data_mod_plot<- lake_data_mod

#make m2 an n2 into one for n2 

#double check this logic. 
#this grouping assigns m2's to n2's to allow for proper counts of n2.
#stat remains as m2, n2 etc. 
lake_data_mod_plot <- lake_data_mod_plot %>%
  mutate(grouping = case_when(
    stat == "n1" ~ "n1",
    stat %in% c("m2", "n2") ~ "n2",  # Combine m2 and n2 together
  ))

m2_fish<- lake_data_mod_plot%>%
  filter(stat == "m2")

m2_fish$grouping = "m2"


#this contains the replicate fish that are m2 and n2
total_data<- rbind(lake_data_mod_plot, m2_fish)


total_data$lake_combined
#this redundancy is useful for the ks tests. 

#now we have a dataframe that we can use in all subsequent analyses. 


###CREATE a long df that pairs all paired data for length correction. 


#problem 1: 2 fish were captured in lower tangle lake and recaptured in shallow tangle lake, which means there is movement between lower and shallow 
#simplest solution: let's treat these two fish as if they were captured in shallow tangle lake as well. 


#let's take a look at all the recapped fish
#need to deal with fish 8330 who is also supposed to be included in this. 8330 is a recapture from June 2023
#8330 was allegedly marked in June 2023 and recaptured in 

m2_data<- trimmed_df%>%
  filter(cap_label == "m2")

recap_8330<- trimmed_df%>%
  filter(PIT == 8330)


PIT_FLOY<- trimmed_df%>%
  filter(!is.na(PIT))%>%
  filter(!(PIT == "UNK"))%>%
  select(PIT, Floy)%>%
  unique()
#there should be a unique map to each PIT tag. 

PIT_trimmed<- PIT_FLOY[10:20,]

#a floy tag may be mapped to a pit tag but it might also just be mapped to "NA" if the pit is not recorded on the found fish, it is pit tagged later etc. 

#this works 
#need to manually add the pit = 8330 to this 
#going off the assumption that *usually* the floy tag is not lost. 

#THIS IS not right. it does not properly account for the fish that have been recapped multiple times. 
#This just finds the initial and final date basically that a fish was caught. but what if the fish was caught in april 2024 and then also in June 2024?

#dataframe that is trimmed to just have dates and lengths
lengths_data<- trimmed_df%>%
  select(Floy, Date, Fork_length)


#new try 
#NO Fish was ever recapped more than 4 times. 
recapped_fish_floy<- trimmed_df %>%
  filter(!(is.na(Floy) & is.na(PIT)))%>%
  filter(Floy!= "UNK")%>%
  group_by(Floy)%>%
  filter(n() >= 2)%>%
  arrange(Date)%>%
  reframe(
    Date_marked = first(Date),
    Date_recapped_1  = nth(Date,2),
    Date_recapped_2  = nth(Date,3),
    Date_recapped_3  = nth(Date,4),
    Date_recapped_4  = nth(Date,5),
    event_marked = min(event_num),
    event_recapped_1 = nth(event_num,2),
    event_recapped_2 = nth(event_num,3),
    event_recapped_3 = nth(event_num,4),
    event_recapped_4 = nth(event_num,5))%>%
  left_join(PIT_FLOY)%>%
  filter(!(PIT == "OBFA" & Floy == "18033"))
#now want to join a bunch of times to get lengths. 

#somehow we end up here with 114 fish that are distinct. 
recapped_fish_floy_lengths<- recapped_fish_floy%>%
  left_join(lengths_data, join_by(Floy, Date_marked == Date))%>%
  rename(Length_1 = Fork_length)%>%
  left_join(lengths_data, join_by(Floy, Date_recapped_1 == Date))%>%
  rename(Length_2 = Fork_length)%>%
  left_join(lengths_data, join_by(Floy, Date_recapped_2 == Date))%>%
  rename(Length_3 = Fork_length)%>%
  left_join(lengths_data, join_by(Floy, Date_recapped_3 == Date))%>%
  rename(Length_4 = Fork_length)%>%
  left_join(lengths_data, join_by(Floy, Date_recapped_4 == Date))%>%
  rename(Length_5 = Fork_length)%>%
  distinct()

#check this by filtering within event recaps. also note: the 8330 pit tag is not included in any of these so we should have one less recap than what matt sees. 

#this gives us 81 different fish, which when we consider the one pit tagged fish with an unknown matching mark, we have the same number of recaps as matt. 
recapped_fish_no_within_event<- recapped_fish_floy_lengths%>%
  filter(!(pmax(event_recapped_1, event_recapped_2, event_recapped_3, event_recapped_4, na.rm = T)==event_marked))%>%
  filter(!(event_marked == 1 & pmax(event_recapped_1, event_recapped_2, event_recapped_3, event_recapped_4, na.rm = T)==2))


#now I want to write a function that plots the changes in length across all these fish 

#plotting_recaps
library(tidyr) 
plotting_recaps<- recapped_fish_no_within_event%>%
  select(Floy, Date_marked, Date_recapped_1, Date_recapped_2, Date_recapped_3, Date_recapped_4, Length_1, Length_2, Length_3, Length_4, Length_5)


#here Date_1 corresponds to date captured and Length 1 correponds to length captured. 
colnames(plotting_recaps)<- c("Floy", "Date_1", "Date_2", "Date_3", "Date_4", "Date_5", "Length_1", "Length_2","Length_3","Length_4","Length_5")


#this is useful for the purpose of plotting. 
df_long <-  plotting_recaps %>%
  # Create a long format where each row corresponds to one length and one date
  gather(key = "Date_id", value = "Date", starts_with("Date")) %>%
  gather(key = "length_id", value = "length", starts_with("Length")) %>%
  filter(!is.na(Date) & !is.na(length)) %>%
  # Now, correctly pair each length with the corresponding Date_id
  mutate(length_id = gsub("Length_", "", length_id),
         Date_id = gsub("Date_", "", Date_id)) %>%
  filter(length_id == Date_id)%>%
  arrange(Floy)# Only keep rows where length_id matches Date_id

install.packages("plotly")
library(plotly)
plotly::ggplotly(
  ggplot(df_long, aes(x = Date, y = length, group = Floy, color = factor(Floy))) +
    geom_point()+
    geom_line() +
    labs(title = "Fish Length vs Recapture Date", x = "Date", y = "Length (cm)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_discrete(name = "Fish ID") +
    theme_bw()+
    theme(legend.position = "none")
) 
ggsave("fish_growth_plot.png", 
       height = 12,  # Adjust the height (in inches)
       width = 6,    # Adjust the width (in inches)
       dpi = 300)


