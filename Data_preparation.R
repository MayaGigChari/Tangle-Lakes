#this is a script to clean up the raw dataframe. 

install.packages("fishmethods")
install.packages("recapr")

library(readxl)
library(fishmethods)
library(recapr)
library(dplyr)
library(tidyr)


install.packages("lubridate")
library(lubridate)


#something to remember: n2 is n2 + m2. all recapped that were marked are also just recapped! 
#this first part is only for recaps!

#only had 5 morts and they were all in 2023 and none of them were recaps. 
#can use the morts for length stratification but not for anything else

#the following is including the morts. 


#read df and remove totally useless columns
raw_df<-read_excel("Tangles_LT_M-R_2023-24.xlsx")
raw_df<- raw_df%>%
  select(-c(Event_recapped_original,...27))

trimmed_df%>%
  filter(Fish_notes == 	"Did not tag because eye hooked;  Gear = Unbaited Lure")

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


###########################
#LENGTH COMPOSITION ANALYSIS
###########################

###Step 0: make some histograms 

#total length distributions

#GOAL: estimate the length composition of lake trout population susceptible to capture during April and June 2023 and 2024... such that precision criterion are met. 

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

#maybe I don't need to do this. try without. 
#lake_data_mod_plot<-lake_data_mod%>%
#  filter(stat %in% events)

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

t
total_data$lake_combined
#this redundancy is useful for the ks tests. 



###KS TESTS

#first, in order for the sample size conditions to be satisfied, we need at LEAST
#113 fish from lower 
#115 fish from round and shallow 
#64 fish from upper 

#check to see if these criterion are met in n1
#these can be morts but probably not recaps. 

#for these purposes, morts have already been turned into n1's as seen above. 
sample_sizes_2022<- total_data%>%
  filter(grouping == "n1")%>%
  group_by(lake_combined)%>%
  count()

#sample size criterion are satisfied for lengths for 2022 but not for 2023. 

sample_sizes_2023<- total_data%>%
  filter(grouping == "n2")%>%
  group_by(lake_combined)%>%
  count()

#criteria from lower and round/shallow are met. criterion for upper are met for 85% instead of 90% confidence. 


#In this example we do not correct for growth with a regression, and we use the recap length for m2. 
ks_results <- data.frame(
  lake = character(),
  ks_test = character(),
  statistic = numeric(),
  p_value = numeric(),
  reject_null = logical(),
  stringsAsFactors = FALSE
)

# Loop over each lake
#either lake or mod_lakes depending on whether we group round and shallow or not. 
for (lake in mod_lakes) {
  # Loop over all event combinations (n1, n2, m2)
  print(lake)
  for (event in events) {
    # Filter data for the current event
      event_data <- total_data %>%
      filter(grouping ==event) %>%
      filter(lake_combined == lake)%>%
      pull("Fork_length") %>%
      as.integer()
    #create a temporary dataframe 
    assign(paste0("data_", event), event_data)
  }
  
  # Perform the KS tests and collect results into dataframe above
  #get a bunch of warnings due to duplicate values (discrete data)

  ks_1 <- ks.test(data_n1, data_m2)
  ks_2 <- ks.test(data_n2, data_m2)
  ks_3 <- ks.test(data_n1, data_n2)

  ks_results <- ks_results %>%
    add_row(lake = lake, 
            ks_test = "M vs R", 
            statistic = ks_1$statistic, 
            p_value = ks_1$p.value,
            reject_null= ks_1$p.value<0.05) %>%
    add_row(lake = lake, 
            ks_test = "C vs R", 
            statistic = ks_2$statistic, 
            p_value = ks_2$p.value,
            reject_null= ks_2$p.value<0.05) %>%
    add_row(lake = lake, 
            ks_test = "M vs C", 
            statistic = ks_3$statistic, 
            p_value = ks_3$p.value,
            reject_null= ks_3$p.value<0.05)
}

write.csv(ks_results, file = "ks_results_combined_lakes_no_length_correction.csv")

# Now, create the plot with the new grouping to reflect the length distributions we compare above
p1<-ggplot(total_data, aes(x = as.integer(Fork_length), color = stat)) +
  stat_ecdf(geom = "step") +  # ECDF with step lines
  labs(x = "Fork_length", y = "CDF", color = "Group") +
  theme_minimal() +
  facet_wrap(~ lake_combined) 

#I have decided that length composition analyses will be conducted only from each independent year



#Below I will look at length composition analyses for just the lower tangle lake, to determine if both sampling events can be used. 






##Values of the KS tests 
##According to the KS tests, no stratification by size/sex is necessary. However, this might be due to low power of the tests. 

?NPetersen

#under business as usual: 
#lower tangle lakes actually has the best. 
#I get really different results than logan here... 

#here we do not consider morts. Will default to cap_label instead of stat
#I will sort by the "cap label" field instead of the "stat" field to remove morts from the equation. 

#here, n2 does NOT include m2. n2 = those captured in second event that are NOT marked. 
cap_summary_petersen<- lake_data_mod%>%
  group_by(lake_combined, cap_label)%>%
  count()%>%
  filter(cap_label %in% c("n1", "n2", "m2"))

petersen_funcs<- list(NPetersen, sePetersen, vPetersen)
Chapman_funcs<- list(NChapman, seChapman, vChapman)
Bailey_funcs<- list(NBailey, seBailey, vBailey)

pop_calc<-function(funcs, lakes)
{
  pop_results <- data.frame(
    lake = character(),
    pop = numeric(),
    se = numeric(),
    var = numeric(),
    norm_95_low = numeric(),
    norm_95_high = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(lake in lakes)
  {
    cap_temp<- cap_summary_petersen%>%
      filter(lake_combined == lake)
    n1_temp<- cap_temp$n[2]
    n2_temp<- cap_temp$n[3]
    m2_temp<- cap_temp$n[1]
    #add n2 and m2 to make the true "n2" (captured in 2nd even=t + recaptured in 2nd event)
    pop_temp<- funcs[[1]](n1_temp,n2_temp+m2_temp, m2_temp)
    se_temp<- funcs[[2]](n1_temp, n2_temp+m2_temp, m2_temp)
    var_temp<- funcs[[3]](n1_temp, n2_temp+m2_temp, m2_temp)
    norm_95_low<- pop_temp - 1.96 * sqrt(var_temp)
    norm_95_high<- pop_temp + 1.96 * sqrt(var_temp)
    pop_results <- pop_results %>%
      add_row(lake = lake, 
              pop = pop_temp,
              se = se_temp,
              var = var_temp,
              norm_95_low = norm_95_low,
              norm_95_high = norm_95_high)
  }
  return(pop_results)
  
}

#below are the population estimates for the three methods of mark recapture population analysis. 
petersen_estimate<- pop_calc(petersen_funcs, lakes)
Chapman_estimate<- pop_calc(Chapman_funcs, lakes)
bailey_estimate<- pop_calc(Bailey_funcs, lakes)

write.csv(petersen_estimate, file = "petersen_estimate_nostrat.csv")
write.csv(Chapman_estimate, file = "Chapman_estimate_nostrat.csv")
write.csv(bailey_estimate, file = "Bailey_estimate_nostrat.csv")

##WE still need to test for CONSISTENCY of the petersen estimator (or I suppose... other estimators?)


#

#check<-lake_data_mod%>%
#  filter(Lake == "Round")%>%
#  filter(stat == "m2")

#recap_round<- left_join(check, lake_data_mod, join_by("Floy"))%>%
#  select(Date.x, Date.y, Floy)
#there is a missing case which I believe is the one that was pit/floy tagged in maybe a weird way but IDK. 
#missint two round-round recaps. 
library(tidyr)
library(textshape)
lake_data_mod

#chec<- inner_join(check, TagMix)
#the original dataset should have all the tuples we need
#it frankly seems like there are only 12
#issue: in a few cases, the pit tag was NOT recorded on recap. 

#maybe I should just group by floy
#not sure if this leads to any data loss. 
#colnames(lake_data_mod)

#could probably make this way less redundant but don't know how to pass the column name as an attribute
#am I supposed to include the column counts$n1-counts$m2? ask Logan


##############
#Functions for tests of petersen consistency. 
##############

#test #1
chi2_markrecap<- function(dataframe, lake_field)
{
    chi2<- dataframe %>%
      filter(!(is.na(Floy) & is.na(PIT)))%>%
      group_by(Floy)%>%
      filter(n() >= 2)%>%
      reframe(
        area_marked = first({{lake_field}}[event_num %in% c(1,2)]),
        area_recaptured = first({{lake_field}}[event_num %in% c(3,4)]))%>%
      count(area_marked, area_recaptured) %>%
      drop_na()%>%
      spread(area_recaptured, value = n, fill = 0)%>%
      column_to_rownames()
    
    counts <- dataframe %>% group_by({{lake_field}}) %>% 
      summarize(n1 = sum(cap_label == "n1"),
                m2 = sum(cap_label == "m2",na.rm = TRUE))
    
    Mixtable_matrix <- as.data.frame(chi2)
    Mixtable_matrix$n1_m2 = counts$n1-counts$m2
    
  return(Mixtable_matrix)
}


#test for complete mixing stuff in event 1 and/or event 2.  
#test #2 and 3
petersen_consistency<- function(data, strat)#strat can either be "Lake" or "Lake_Combined"
{
  
  
  pet_cons_res <- data.frame(
    type = character(),
    X_squared = numeric(),
    df = numeric(),
    p_value = numeric(),
    reject_null = logical(),
    stringsAsFactors = FALSE
  )
  
  complete_mix<- data %>%
    group_by({{strat}}) %>%
    summarize(m2 = sum(cap_label == "m2"),
              n2 = sum(cap_label == "n2")) %>%
    pivot_longer(cols = c(m2, n2), names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = {{strat}}, values_from = value) %>%
    as.data.frame()
  
  rownames(complete_mix) <- complete_mix$variable
  complete_mix$variable <- NULL
  
  # Conduct the chi-square test
  chisq_result_complete_mixing <- chisq.test(complete_mix)
  
  
  #test for equal probability of capture during Event 2 slash equal proportions test? 
  equal_prop<- data%>%
    group_by({{strat}})%>%
    summarize(m2 = sum(cap_label == "m2"),
              n1_noMark = sum(cap_label == "n1")-m2)%>%
    pivot_longer(cols = c(m2, n1_noMark), names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = {{strat}}, values_from = value) %>%
    as.data.frame()
  rownames(equal_prop)<- equal_prop$variable
  equal_prop$variable<- NULL
  print(equal_prop)
  
  # Conduct the chi-square test
  chisq_result_equal_proportions <- chisq.test(equal_prop)
  
  pet_cons_res <- pet_cons_res %>%
    add_row(type = "complete_mixing", 
            X_squared = chisq_result_complete_mixing$statistic,
            p_value = chisq_result_complete_mixing$p.value,
            reject_null= chisq_result_complete_mixing$p.value<0.05)%>%
    add_row(type = "equal_prop", 
            X_squared = chisq_result_equal_proportions$statistic,
            p_value = chisq_result_equal_proportions$p.value,
            reject_null= chisq_result_equal_proportions$p.value<0.05)
  print(equal_prop)
  
  return(pet_cons_res)
  
  
}


out_lake_data_mod<- chi2_markrecap(no_april_mark, Lake)
pet_diag_1<- chisq.test(out_lake_data_mod)  

#as we can see, we have two fish caught in lower tangle lakes and recaptured in shallow tangle lakes. Otherwise there is strong homogeinity between lakes.
#this is sort of a useless test as we were expecting these lakes to be separate anyways. 

#### Test for equal probability of capture during Event 1 slash complete mixing test (H0 = Every fish has an equal probability of being captured and marked during event 1)
#will need to do this for each lake so should probably make a function out of it. 

#below is the data with April sampling removed entirely. Double check this 

#all the april data that has floy tag
April_data_floy<- lake_data_mod%>%
  filter(event_num %in% c(1,3))%>%
  filter(!is.na(Floy))

aj<- anti_join(lake_data_mod, April_data_floy, join_by(Floy) )%>%
  filter(event_num %in% c(2,4))


#take out just april marks
no_april_2023<- lake_data_mod%>%
  filter(event_num ==1 )%>%
  filter(!is.na(Floy))
  
no_april_mark<- anti_join(lake_data_mod, no_april_2023, join_by(Floy) )%>%
  filter(event_num %in% c(2,3,4))

## when we take out the april data we get an entirely different situation 
petersen_consistency_with_april<- petersen_consistency(lake_data_mod, Lake)
petersen_consistency_no_april<- petersen_consistency(no_april_mark, Lake)

#we can only get consistency if we omit the April data completely. 

#this one barely works. 

#the issue here is that there seems to be a much higher probability of recap for lower than the rest when we do include the april data. 
  
#all these tests fail. 
  
#attempt to omit the April data: this might be very difficult. 



###Let's consider only Lower Tangle Lake, because this one seems to be giving the most trouble.

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


#should do this by lake: 

#separate lakes 
Lower_lake<- trimmed_df%>%
  filter(Lake == "Lower")


Upper_lake<- trimmed_df%>%
  filter(Lake == "Upper")


Shallow_lake<- trimmed_df%>%
  filter(Lake == "Shallow")


Round_lake<- trimmed_df%>%
  filter(Lake == "Round")

#manipule this manually


#this has been done for all four lake types, and the images have been saved. 
floys_Shallow<-df_long%>%
  filter(Floy %in% Shallow_lake$Floy)

floys_Upper<-df_long%>%
  filter(Floy %in% Upper_lake$Floy)

floys_Round<-df_long%>%
  filter(Floy %in% Round_lake$Floy)

floys_Lower<-df_long%>%
  filter(Floy %in% Lower_lake$Floy)


#one fish caught in lower is not represented. this checks out. 
#the two fish caught between lower and shallow are represented in both the lower and shallow datasets.
length(unique(floys_Shallow$Floy))
#this includes the two fish recapped in shallow. 

plotly::ggplotly(
  ggplot(floys_Shallow, aes(x = Date, y = length, group = Floy, color = factor(Floy))) +
    geom_point()+
    geom_line() +
    labs(title = "Fish Length over time Shallow Lake", x = "Date", y = "Length (cm)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_discrete(name = "Fish ID") +
    theme_bw()+
    theme(legend.position = "none")
) 
ggsave("fish_growth_Shallow.png", 
       height = 12,  # Adjust the height (in inches)
       width = 6,    # Adjust the width (in inches)
       dpi = 300)


###now I will use this information to correct recapped lengths(and potentially all lengths in event 2, although I don't know if that is useful.)

#for Round, Shallow and Upper, this is simple because the lake was only sampled once in 2023 and once in 2024. 
#we can assume that constant growth/birth/death processes keep the whole population representative (n2 = n1 in length composition if natural processes remain the same?)
#however, capped all grow, basically, a significant amount which may change the length stratification results.

floys_shallow_change<- floys_Shallow%>%
  group_by(Floy)%>%
  arrange(length_id)%>%
  summarise(
    catpure_length = first(length),
    recapture_length = last(length),
    change_length = last(length)-first(length)
  )

avg_change_shallow<- floys_shallow_change%>%
  filter(change_length>0)%>%
  summarise(
    mean = mean(change_length),
    sd = sd(change_length)
  )

floys_Upper_change<- floys_Upper%>%
  group_by(Floy)%>%
  arrange(length_id)%>%
  summarise(
    change_length = last(length)-first(length)
  )

avg_change_upper<- floys_Upper_change%>%
  filter(change_length>0)%>%
  summarise(
    mean = mean(change_length),
    sd = sd(change_length)
  )


floys_Round_change<- floys_Round%>%
  group_by(Floy)%>%
  arrange(length_id)%>%
  summarise(
    change_length = last(length)-first(length)
  )

avg_change_round<- floys_Round_change%>%
  filter(change_length>0)%>%
  summarise(
    mean = mean(change_length),
    sd = sd(change_length)
  )


#now I also want to consider lower

floys_lower

#first I will just group into recapped April and recpped June
#if they were recapped in April they probably grew a little less and therefore should be corrected for a little less growth. 
#the alternate can be said about june

#should there be 4 of these? 

#fish caught April, recapped April --> grow less than 
#fish caught April, recapped June --> grow different than 
#fish caught June, recapped April --> grow different than 
#fish caught June, recapped June

#should maybe do these tests. 

floys_lower_change<- floys_Lower%>%
  group_by(Floy)%>%
  arrange(length_id)%>%
  summarise(
    change_length = last(length)-first(length),
    Date_recap = max(Date), 
    Date_cap = min(Date)
  )

#it is possible that the following 4 chunks make no sense
floys_lower_change_april_april<- floys_lower_change%>%
  filter(Date_recap < as_datetime("2024-05-01"))%>%
  filter(Date_cap < as_datetime("2023-05-01"))

floys_lower_change_june_april<- floys_lower_change%>%
  filter(Date_recap < as_datetime("2024-05-01"))%>%
  filter(Date_cap > as_datetime("2023-05-01"))

floys_lower_change_june_june<- floys_lower_change%>%
  filter(Date_recap > as_datetime("2024-05-01"))%>%
  filter(Date_cap > as_datetime("2023-05-01"))

floys_lower_change_april_june<- floys_lower_change%>%
  filter(Date_recap > as_datetime("2024-05-01"))%>%
  filter(Date_cap > as_datetime("2023-05-01"))

###now time to correct based on the average changes
##note: there was a much bigger issue of mis-measurement in april, probably because it was so cold. 

#3 discrete growth periods: April to June 2023, June 2023- April 2024, April- June 2024

#need to do a bunch of tests to see if there is actually significant growth. 
lower_growth_ap23Jun23<- floys_Lower%>%
  group_by(Floy)%>%
  arrange(length_id)%>%
  filter(Date < as_datetime("2024-01-01"))%>%
  summarise(
    change_length = last(length)-first(length),
    Date_recap = max(Date), 
    Date_cap = min(Date)
  )%>%
  filter(Date_recap != Date_cap )

lower_growth_ap24Jun24<- floys_Lower%>%
  group_by(Floy)%>%
  arrange(length_id)%>%
  filter(Date > as_datetime("2024-01-01"))%>%
  summarise(
    change_length = last(length)-first(length),
    Date_recap = max(Date), 
    Date_cap = min(Date)
  )%>%
  filter(Date_recap != Date_cap )

lower_growth_jun23ap24<- floys_Lower%>%
  group_by(Floy)%>%
  arrange(length_id)%>%
  filter(Date > as_datetime("2023-05-01") & Date < as_datetime("2024-05-01"))%>%
  summarise(
    change_length = last(length)-first(length),
    Date_recap = max(Date), 
    Date_cap = min(Date)
  )%>%
  filter(Date_recap != Date_cap )


###let's say that the fish did not grow between mark recapture (ie: let the length at recapture be the length at mark)

#if we assume constant birth, death, etc. and assume that since there seems to be no difference in the underlying 
#length distributions of the mark and the recapture event, we don't have to change the data for other stuff. 
#

#INDIVIDUALS HAHVE DEFINITELY RECRUITED INTO THE POPULATION OF INFERENCE. 


#tests for growth in individual lakes 

#Upper tangle lake

#MAYBE SHOULD  MAKE a new column called "recap length" that just figures out the recap length 


#if some within event condition is satisfied

#this only works for the ones that have the ish. 
#TODO later: exactly calcualte this. this is just a test. 

#there is growth within each lake. 
t.test(as.integer(easy_length_regression$cap_length), as.integer(easy_length_regression$recap_length), paired = TRUE)


Upper_caprecap<- recapped_fish_no_within_event%>%
  filter(Floy %in% floys_Upper$Floy)

t.test(as.integer(Upper_caprecap$Length_1),as.integer(Upper_caprecap$Length_2), paired = TRUE )


Shallow_caprecap<-recapped_fish_no_within_event%>%
  filter(Floy %in% floys_Shallow$Floy)

t.test(as.integer(Shallow_caprecap$Length_1),as.integer(Shallow_caprecap$Length_2), paired = TRUE )


Round_caprecap<-recapped_fish_no_within_event%>%
  filter(Floy %in% floys_Round$Floy)

t.test(as.integer(Round_caprecap$Length_1),as.integer(Round_caprecap$Length_2), paired = TRUE )


#the fish in shallow, round and upper tangle lakes can only be caught in event 2 and recapped in event 4. 

#################corrected K-S tests: 
#second event recapture (R2) lenghts are used in captured vs recaptured (C vs. R), and first event recapture lengths were used in "M vs R"



m2_dates<- trimmed_df%>%
  filter(cap_label == "m2")%>%
  select(Floy, Date)

n1_dates<- trimmed_df%>%
  filter(cap_label == "n1")%>%
  select(Floy, Date)


 recap_recorded<- m2_dates%>%
  inner_join(df_long)
  
cap_recorded<- df_long%>%
  group_by(Floy)%>%
  reframe(
    Date = min(Date),
  )%>%
  left_join(df_long)

cap_recap_recorded<- rbind(cap_recorded, recap_recorded)



#do the regression for growth between various mark and recap periods 
recap_recorded<- cap_recap_recorded%>%
  group_by(Floy)%>%
  arrange(Date)%>%
  summarise(
    change_length = last(length)-first(length),
    Length_cap = first(length),
    Length_recap = last(length),
    Date_recap = max(Date), 
    Date_cap = min(Date),
  )%>%
  filter(Date_recap != Date_cap )


#attach floys to lakes
lake_floy<- trimmed_df%>%
  select(Lake, Floy)

#join to include lakes with the floys. Some floys will have multiple lakes. 
#now there are 73 distinct rows 
#there is one duplicate floy because the fish was caught in the lower tangle lake and recapped in shallow or something like that. 
#how should we deal with this? For now, I will write this code so that they are assigned to shallow instead of lower. 

lake_cap<- left_join(recap_recorded, lake_floy)%>%
  distinct()

floys_of_interest<-lake_cap%>%
  group_by(Floy)%>%
  summarize(count = n())

#19970 is the floy of interest,the one associated with two lakes. 
#wait what about 18608
#there are two fish caught in lower and recapped in round/shallow, so these are the two fish 

#maybe should filter everything where there was a negative change 
#perhaps this will allow us to have a better estimate? 
#maybe we should also try to fit an exponential model instead of a logistic regression. 

recap_recorded<- lake_cap%>%
  filter(!(Floy == "19970" & Lake == "Lower"))%>%
  filter(!(Floy == "18606" & Lake == "Lower"))

pn <- ggplot(recap_recorded, aes(x = Length_cap, y = Length_recap)) +
  geom_point() +
  geom_smooth(method = lm)+
  geom_abline(slope = 1, intercept = 0) +
  xlim(0, 800) + 
  ylim(0, 800)    


ggsave("cap vs recap lengths plot walford.png", plot = pn, dpi= 300)

#in this, the intercept between y = x and the best fit linear regression is the value of Linf, allegedly. And the slope is e^-kt, so from that we can calucluate k. 
#this is only useful if we want to 

#now proceed as usual 
#date cap and date recap don't necessarily apply for the case of lower. 
#but still it sort of does. shit still grows. 

recap_recorded_jun_jun<- recap_recorded%>%
  filter(Date_cap > "2023-05-01")%>%
  filter(Date_recap > "2024-05-01")

recap_recorded_june_apr<-recap_recorded %>%
  filter(Date_cap > "2023-05-01")%>%
  filter(Date_recap < "2024-05-01")

recap_recorded_apr_apr<-recap_recorded %>%
  filter(Date_cap < "2023-05-01" )%>%
  filter(Date_recap < "2024-05-01")

recap_recorded_apr_jun<-recap_recorded %>%
  filter(Date_cap < "2023-05-01" )%>%
  filter(Date_recap > "2024-05-01")


#get best fit lines for all the data. 
#can we drop negative values? 

#want to separate these by lake. 
jun_jun_linreg<- lm(recap_recorded_jun_jun$change_length~ recap_recorded_jun_jun$Length_cap)
sig_jun_jun_linreg<- summary(jun_jun_linreg)

Lakes = c("Round", "Shallow", "Upper", "Lower")


jun_jun_linreg_list<- list()
for(i in 1:length(Lakes))
{
  temp<- recap_recorded_jun_jun%>%
    filter(Lake == Lakes[i])
  temp_linreg<- lm(temp$change_length~ temp$Length_cap)
  jun_jun_linreg_list[[i]]<- temp_linreg
}
jun_jun_linreg_list[[1]]

linregs_junjun<-ggplot(recap_recorded_jun_jun,aes(x = Length_cap, y = change_length))+
  facet_wrap(~Lake)+
  geom_point(color = "red", size = 4)+
  geom_smooth(method = lm)
ggsave("linear_regressions_four_lakes_separate_jun_jun_growth.png", linregs_junjun, dpi = 300)


#wonder how this deals with the negative growth since log negative number is imaginary. 
total_linreg_junjun<-ggplot(recap_recorded_jun_jun,aes(x = Length_cap, y = log(change_length)))+
  geom_point(color = "red", size = 4)+
  geom_smooth(method = lm)
ggsave("linear_regressions_logchange.png", total_linreg_junjun, dpi = 300)


#maybe should plot log log and see what happens. 

  
#need to do a test to see if these three slopes are significantly different. 


#here we have a bunch of linear models we can plot 

#these only apply to lower. 

positive_growth_only<- recap_recorded_jun_jun%>%
  filter(change_length > 0)
jun_jun_linreg_log<- lm(log(positive_growth_only$change_length)~ positive_growth_only$Length_cap)

jun_apr_linreg<- lm(recap_recorded_june_apr$change_length~recap_recorded_june_apr$Length_cap)
sig_jun_apr_linreg<- summary(jun_apr_linreg)


apr_apr_linreg<- lm(recap_recorded_apr_apr$change_length~recap_recorded_apr_apr$Length_cap)
sig_apr_apr_linreg<- summary(apr_apr_linreg)


apr_jun_linreg<- lm(recap_recorded_apr_jun$change_length~recap_recorded_apr_jun$Length_cap)
sig_apr_jun_linreg<- summary(apr_jun_linreg)

library(ggplot2)
#plot these 

ggplot(recap_recorded_jun_jun,aes(x = Length_cap, y = change_length))+
  geom_point(color = "red", size = 4)+
  geom_smooth(method = lm)
ggplot(recap_recorded_june_apr,aes(x = Length_cap, y = change_length))+
  geom_point(color = "red", size = 4)+
  geom_smooth(method = lm)
ggplot(recap_recorded_apr_apr,aes(x = Length_cap, y = change_length))+
  geom_point(color = "red", size = 4)+
  geom_smooth(method = lm)
ggplot(recap_recorded_apr_jun,aes(x = Length_cap, y = change_length))+
  geom_point(color = "red", size = 4)+
  geom_smooth(method = lm)

#when we pool the four lakes, there is a significant relationship between growth and length at capture for june 2023-june 2024 and april 2023-june 2024.
#there is not a significant relationship from june to april or april to april. 

#if we are trying to estimate the population given marks in June 2023 and recaps in June 2024, we should use the Jun-Jun correction to correct the population. 

#if we decinde to truncate the population at about 400, we probably won't have this problem anymore. 



#get all the fish captured in the June 2024 event from the total data thing: 

#filter by grouping to get rid of the m2's, which we will correct separately since we know their original lengths. 
#I think this only works for fish that were capped in June 2023 and recapped in June 2024. 
#that should be allt he fish in all the other three lakes. 

#this makes sense. 
recaps_nolower_2024<- total_data%>%
  filter(!Lake == "Lower")%>%
  filter(grouping == "n2")

#something fucked up with this linear model. 

#changed for log model, take out. 
jun_jun_correction<- function(length, model)
{
  slope<- coef(model)[[2]]
  intercept<- coef(model)[[1]]
  length_cap<- length - exp(abs((intercept + length*slope)))
}

#create a corrected fork length for the second capture event
corrected_Fork_length<- sapply(as.numeric(recaps_nolower_2024$Fork_length),jun_jun_correction, jun_jun_linreg_log)

recaps_nolower_2024$corrected_Fork_length<- corrected_Fork_length


caps_nolower_2024<- total_data%>%
  filter(!Lake == "Lower")%>%
  filter(cap_label %in% c("n1", "Mort"))


# make data for each lake 

#shallow

#this next part can also combine shallow and round. depending on how we decide to treat them. 
shallow_n1<- caps_nolower_2024%>%
  filter(Lake == "Shallow")%>%
  select(Fork_length)

shallow_n2<- recaps_nolower_2024%>%
  filter(Lake == "Shallow")%>%
  select(Fork_length = corrected_Fork_length)

shallow_m2<- recap_recorded%>%
  filter(Floy %in% Shallow_lake$Floy)%>%
  select(Length_cap)

#upper
upper_n1<- caps_nolower_2024%>%
  filter(Lake == "Upper")%>%
  select(Fork_length)

upper_n2<- recaps_nolower_2024%>%
  filter(Lake == "Upper")%>%
  select(Fork_length = corrected_Fork_length)

upper_m2<- recap_recorded%>%
  filter(Floy %in% Upper_lake$Floy)%>%
  select(Length_cap)


#round 
Round_n1<- caps_nolower_2024%>%
  filter(Lake == "Round")%>%
  select(Fork_length)

Round_n2<- recaps_nolower_2024%>%
  filter(Lake == "Round")%>%
  select(Fork_length = corrected_Fork_length)

Round_m2<- recap_recorded%>%
  filter(Floy %in% Round_lake$Floy)%>%
  select(Length_cap)

#starter dataframe: 
df <- data.frame(
  length = numeric(),
  lake = character(),
  event = character()
)

#ks tests for each lake 
ks_results2 <- data.frame(
  lake = character(),
  ks_test = character(),
  statistic = numeric(),
  p_value = numeric(),
  reject_null = logical(),
  stringsAsFactors = FALSE
)

n1s<- list(shallow_n1, upper_n1, Round_n1)
n2s<- list(shallow_n2, upper_n2, Round_n2)
m2s<- list(shallow_m2, upper_m2, Round_m2)
lakes <- list("shallow", "upper", "round")
for(i in 1:3)
{
  ks_1 <- ks.test(as.integer(n1s[[i]]$Fork_length), m2s[[i]]$Length_cap)
  ks_2 <- ks.test(as.integer(n2s[[i]]$Fork_length), m2s[[i]]$Length_cap)
  ks_3 <- ks.test(as.integer(n1s[[i]]$Fork_length), as.integer(n2s[[i]]$Fork_length))
  print("done")
  
  ks_results2 <- ks_results2 %>%
    add_row(lake = lakes[[i]], 
            ks_test = "M vs R", 
            statistic = ks_1$statistic, 
            p_value = ks_1$p.value,
            reject_null= ks_1$p.value<0.05) %>%
    add_row(lake = lakes[[i]], 
            ks_test = "C vs R", 
            statistic = ks_2$statistic, 
            p_value = ks_2$p.value,
            reject_null= ks_2$p.value<0.05) %>%
    add_row(lake = lakes[[i]], 
            ks_test = "M vs C", 
            statistic = ks_3$statistic, 
            p_value = ks_3$p.value,
            reject_null= ks_3$p.value<0.05)
  
  #add something for shallowround

  
  tempn1<- data.frame(n1s[[i]])
  colnames(tempn1)<- "length"
  tempn1$Lake = lakes[[i]]
  tempn1$event = "n1"
  
  tempm2<- data.frame(m2s[[i]])
  colnames(tempm2)<- "length"
  tempm2$Lake = lakes[[i]]
  tempm2$event = "m2"
  
  tempn2<- data.frame(n2s[[i]])
  colnames(tempn2)<- "length"
  tempn2$Lake = lakes[[i]]
  tempn2$event = "n2"
  
  df<- rbind(df, tempn1, tempn2, tempm2)
  
}


shallowroundn1<- rbind(n1s[[1]], n1s[[3]])
shallowroundn2<- rbind(n2s[[1]], n2s[[3]])
shallowroundm2<- rbind(m2s[[1]], m2s[[3]])

ks_1_sr<- ks.test(as.integer(shallowroundn1$Fork_length), shallowroundm2$Length_cap)
ks_2_sr <- ks.test(as.integer(shallowroundn2$Fork_length), shallowroundm2$Length_cap)
ks_3_sr <- ks.test(as.integer(shallowroundn1$Fork_length), as.integer(shallowroundn2$Fork_length))

ks_results2 <- ks_results2 %>%
  add_row(lake = "Shallow_round", 
          ks_test = "M vs R", 
          statistic = ks_1_sr$statistic, 
          p_value = ks_1_sr$p.value,
          reject_null= ks_1_sr$p.value<0.05) %>%
  add_row(lake = "Shallow_round", 
          ks_test = "C vs R", 
          statistic = ks_2_sr$statistic, 
          p_value = ks_2_sr$p.value,
          reject_null= ks_2_sr$p.value<0.05) %>%
  add_row(lake = "Shallow_round", 
          ks_test = "M vs C", 
          statistic = ks_3_sr$statistic, 
          p_value = ks_3_sr$p.value,
          reject_null= ks_3_sr$p.value<0.05)

write.csv(ks_results2, file= "Ks_results2.csv")
write.csv(ks_results, file = "Ks_results_no_correction.csv")

#all the ks tests seem to show stuff comes from the same distribution.

#plots of upper, shallow and round 
#can make a dataframe that's just really long. 

#make a dataframe


lakemod_detect<- function(Lake)
{
  if(Lake == "shallow" || Lake == "round")
    return("shallow_round")
  return(Lake)
}

df$Lake_mod<-
  sapply(df$Lake, lakemod_detect)

p1<-ggplot(df, aes(x = as.integer(length), color = event)) +
  stat_ecdf(geom = "step") +  # ECDF with step lines
  labs(x = "length", y = "CDF", color = "Group") +
  theme_minimal() +
  facet_wrap(~ Lake_mod) 

ggsave(filename = "cdf_len_corrected_linreg_nolower_shallowround.png", p1, dpi = 300)



###################here, I will truncate all the n2 length-corrected fish to only keep the ones that could have possibly been caught in the n1 event (remove the recruits)

#this is POSSIBLE min lenght given gear so can be across all the lakes, I think. maybe not. Try for each lake. 

min_round<- Round_lake%>%
  summarize(min_fork_length = min(Fork_length, na.rm = TRUE))
  
min_shallow<- Shallow_lake%>%
  summarize(min_fork_length = min(Fork_length, na.rm = TRUE))

min_lower<- Lower_lake%>%
  summarize(min_fork_length = min(Fork_length, na.rm = TRUE))

min_upper<- Upper_lake%>%
  summarize(min_fork_length = min(Fork_length, na.rm = TRUE))

  
#the absolute shortest is the round minimum, which is much shorter than the other minimums. however, the gear can technically still catch these size fish. 
#BUT: are they there, is the question. Just because the gear can catch it, does it mean we should include it in the length distribution? 

#step 1: try to correct given the global minimum (232 mm)




recaps_nolower_2024_truncated<- recaps_nolower_2024%>%
  filter(corrected_Fork_length >300)


#copypasta code from up there: clean this up later. 

#this next part can also combine shallow and round. depending on how we decide to treat them. 
shallow_n1<- caps_nolower_2024%>%
  filter(Lake == "Shallow")%>%
  select(Fork_length)

shallow_n2<- recaps_nolower_2024_truncated%>%
  filter(Lake == "Shallow")%>%
  select(Fork_length = corrected_Fork_length)

shallow_m2<- recap_recorded%>%
  filter(Floy %in% Shallow_lake$Floy)%>%
  select(Length_cap)

#upper
upper_n1<- caps_nolower_2024%>%
  filter(Lake == "Upper")%>%
  select(Fork_length)

upper_n2<- recaps_nolower_2024_truncated%>%
  filter(Lake == "Upper")%>%
  select(Fork_length = corrected_Fork_length)

upper_m2<- recap_recorded%>%
  filter(Floy %in% Upper_lake$Floy)%>%
  select(Length_cap)


#round 
Round_n1<- caps_nolower_2024%>%
  filter(Lake == "Round")%>%
  select(Fork_length)

Round_n2<- recaps_nolower_2024_truncated%>%
  filter(Lake == "Round")%>%
  select(Fork_length = corrected_Fork_length)

Round_m2<- recap_recorded%>%
  filter(Floy %in% Round_lake$Floy)%>%
  select(Length_cap)

#starter dataframe: 
df <- data.frame(
  length = numeric(),
  lake = character(),
  event = character()
)

#ks tests for each lake 
ks_results2 <- data.frame(
  lake = character(),
  ks_test = character(),
  statistic = numeric(),
  p_value = numeric(),
  reject_null = logical(),
  stringsAsFactors = FALSE
)

n1s<- list(shallow_n1, upper_n1, Round_n1)
n2s<- list(shallow_n2, upper_n2, Round_n2)
m2s<- list(shallow_m2, upper_m2, Round_m2)
lakes <- list("shallow", "upper", "round")
for(i in 1:3)
{
  ks_1 <- ks.test(as.integer(n1s[[i]]$Fork_length), m2s[[i]]$Length_cap)
  ks_2 <- ks.test(as.integer(n2s[[i]]$Fork_length), m2s[[i]]$Length_cap)
  ks_3 <- ks.test(as.integer(n1s[[i]]$Fork_length), as.integer(n2s[[i]]$Fork_length))
  print("done")
  
  ks_results2 <- ks_results2 %>%
    add_row(lake = lakes[[i]], 
            ks_test = "M vs R", 
            statistic = ks_1$statistic, 
            p_value = ks_1$p.value,
            reject_null= ks_1$p.value<0.05) %>%
    add_row(lake = lakes[[i]], 
            ks_test = "C vs R", 
            statistic = ks_2$statistic, 
            p_value = ks_2$p.value,
            reject_null= ks_2$p.value<0.05) %>%
    add_row(lake = lakes[[i]], 
            ks_test = "M vs C", 
            statistic = ks_3$statistic, 
            p_value = ks_3$p.value,
            reject_null= ks_3$p.value<0.05)
  
  #add something for shallowround
  
  
  tempn1<- data.frame(n1s[[i]])
  colnames(tempn1)<- "length"
  tempn1$Lake = lakes[[i]]
  tempn1$event = "n1"
  
  tempm2<- data.frame(m2s[[i]])
  colnames(tempm2)<- "length"
  tempm2$Lake = lakes[[i]]
  tempm2$event = "m2"
  
  tempn2<- data.frame(n2s[[i]])
  colnames(tempn2)<- "length"
  tempn2$Lake = lakes[[i]]
  tempn2$event = "n2"
  
  df<- rbind(df, tempn1, tempn2, tempm2)
  
}


shallowroundn1<- rbind(n1s[[1]], n1s[[3]])
shallowroundn2<- rbind(n2s[[1]], n2s[[3]])
shallowroundm2<- rbind(m2s[[1]], m2s[[3]])

ks_1_sr<- ks.test(as.integer(shallowroundn1$Fork_length), shallowroundm2$Length_cap)
ks_2_sr <- ks.test(as.integer(shallowroundn2$Fork_length), shallowroundm2$Length_cap)
ks_3_sr <- ks.test(as.integer(shallowroundn1$Fork_length), as.integer(shallowroundn2$Fork_length))

ks_results2 <- ks_results2 %>%
  add_row(lake = "Shallow_round", 
          ks_test = "M vs R", 
          statistic = ks_1_sr$statistic, 
          p_value = ks_1_sr$p.value,
          reject_null= ks_1_sr$p.value<0.05) %>%
  add_row(lake = "Shallow_round", 
          ks_test = "C vs R", 
          statistic = ks_2_sr$statistic, 
          p_value = ks_2_sr$p.value,
          reject_null= ks_2_sr$p.value<0.05) %>%
  add_row(lake = "Shallow_round", 
          ks_test = "M vs C", 
          statistic = ks_3_sr$statistic, 
          p_value = ks_3_sr$p.value,
          reject_null= ks_3_sr$p.value<0.05)


lakemod_detect<- function(Lake)
{
  if(Lake == "shallow" || Lake == "round")
    return("shallow_round")
  return(Lake)
}

df$Lake_mod<-
  sapply(df$Lake, lakemod_detect)

p1<-ggplot(df, aes(x = as.integer(length), color = event)) +
  stat_ecdf(geom = "step") +  # ECDF with step lines
  labs(x = "length", y = "CDF", color = "Group") +
  theme_minimal() +
  facet_wrap(~ Lake_mod) 

ggsave(filename = "cdf_len_corrected_linreg_nolower_shallowround_truncated_log_junjun.png", p1, dpi = 300)

#so this doesn't exactly help the situation. just truncating at some value still makes the distribution all weird. 




#now to deal with lower tangle lake: how to deal with the growth situation: ask Albert! 


#length histograms (after correction)

#histograms for the captures, recaptures and corrected 
#since the K-S tests do not show size-stratification, we will use the lenght data from event 1 and event 2 to show the distribution. 

#length distribution for shallow_round after correction.
#not sure how to get these error bars or do this measurement. 

#this is after correction. 
#is it possible that the round fish are smaller? 

#these are all corrected lengths. 
all_lengths_hist<- ggplot(df, aes(as.numeric(length))) +
  geom_histogram(aes(y=after_stat(density)),binwidth = 5, fill = "skyblue", color = "b+lack", alpha = 0.7) +
  geom_density(color = "red", size = .5)+
  facet_wrap(~ Lake)

ggsave(filename = "histogram of corrected lengths.png", all_lengths_hist, dpi = 300)

trimmed_df
#lengths before correction 

all_lakes<- trimmed_df %>%
  filter(cap_label%in% c("n1", "n2", "m2", "mort"))

all_lakes$Fork_length<- as.numeric(all_lakes$Fork_length)

all_lengths_hist_nocorrect<- ggplot(all_lakes, aes(Fork_length)) +
  geom_histogram(aes(y=after_stat(density)),binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = .5)+
  facet_wrap(~ Lake)

ggsave(filename = "histogram of original lengths.png", all_lengths_hist_nocorrect, dpi = 300)


n1_lakes<- all_lakes%>%
  filter(cap_label%in%c("n1", "mort"))


n1_lengths_hist_nocorrect<- ggplot(n1_lakes, aes(Fork_length)) +
  geom_histogram(aes(y=after_stat(density)),binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = .5)+
  facet_wrap(~ Lake)

ggsave(filename = "histogram_n1_lakes_nocorrections.png", all_lengths_hist_nocorrect, dpi = 300)


#another option is to truncate the smallest fish. 

#maybe want to make a thing for matt with the least cost paths (movement)


#below, I will tackle the lower tangle lakes. 


#what if we fit an exponential decay to dN/dt instead. 

