#this builds from Data_preparation
#basically trying to justify using one regression for all the growth. 

###TODO: deal with the floy "UNK" category. 

source('Data_Preparation_forRmd.R')

library(readxl)
library(fishmethods)
library(recapr)
library(tidyverse)
library(ggplot2)
library(emmeans)

#First, given the dataframe compiled in Data_preparation_forRmd, let's make a plot of the growth just to visualize
#we will save this plot. 

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
       height = 12,  
       width = 6,    
       dpi = 300)


#this dataframe gives the change length and the cap length for each floy, for use in length correction analyses. 
floys_total_change<- df_long%>%
  group_by(Floy) %>%
  arrange(length_id) %>%
  reframe(
    expand_grid(i = row_number(), j = row_number()) %>%
      filter(i < j) %>% 
      mutate(
        length_1 = length[i],
        length_2 = length[j],
        Date_cap = Date[i],
        Date_recap = Date[j],
        change_length = as.numeric(length_2) - as.numeric(length_1)
      ) %>%
      select(Date_cap, Date_recap, change_length,cap_length = length_1, recap_length = length_2, change_length)
  )

#now need to match up the floys with their respective lakes, including manually matching the two 

#lake_floy is defined in Data_preparation. 

floys_to_shallow<-c("19970", "18608")

#correct the data: pretend that the two floys above were both caught and recaptured in the shallow lake. we can edit this later if we want to relax this assumption. 

total_data_shallow_correct<- total_data
for(i in 1: length(total_data_shallow_correct$Floy))
{
  if(total_data_shallow_correct$Floy[i] %in% floys_to_shallow)
  {
    total_data_shallow_correct$lake_combined[i] = "Shallow_Round"
    total_data_shallow_correct$Lake[i] = "Shallow"
    print(total_data_shallow_correct[i,])
  }
}

#view(total_data%>%
#  filter(Floy == 18603 ))


total_data_shallow_correct_floys<- total_data_shallow_correct%>%
  select(lake_combined, Floy)%>%
  filter(!is.na(Floy))%>%
  distinct()

floys_total_change_withLake<- left_join(floys_total_change, total_data_shallow_correct_floys, join_by(Floy))

#sanity check: count the recaps in each lake system
#lower is inflated because there are many more chances for recaps. 
floys_total_change_withLake_counts<- floys_total_change_withLake%>%
  group_by(lake_combined)%>%
  count()

#now I need to make another fixed effect which is sort of a "capture/recapture time" thing. 

capture_recapture_class <- function(df) {
  
  # Make sure dates are Date type. Probably don't need this
  df$Date_cap <- as.Date(df$Date_cap)
  df$Date_recap <- as.Date(df$Date_recap)
  
  #create a class vector
  class <- rep(NA, nrow(df))
  
  for(i in 1:nrow(df)) {
    
    cap_date <- df$Date_cap[i]
    recap_date <- df$Date_recap[i]
    
    cap_year <- format(cap_date, "%Y")
    recap_year <- format(recap_date, "%Y")
    
    cap_month <- as.numeric(format(cap_date, "%m"))
    recap_month <- as.numeric(format(recap_date, "%m"))
    
    #assigns string values to the cap and recap periods 
    cap_period <- ifelse(cap_month %in% 4, paste0("April", cap_year),
                         ifelse(cap_month %in% 6, paste0("June", cap_year), NA))
    
    recap_period <- ifelse(recap_month %in% 4, paste0("April", recap_year),
                           ifelse(recap_month %in% 6, paste0("June", recap_year), NA))
    
    # Now combine. usually there are no NA's but there might be one or two cases-- deals with any discrepancy.
    if(!is.na(cap_period) & !is.na(recap_period)) {
      class[i] <- paste0(cap_period, "_to_", recap_period)
    } else {
      class[i] <- NA
    }
  }
  
  return(class)
}
              

#discretise the different possible events pairing each floy cap-recap incedent. 
floys_total_change_withLake$class<- capture_recapture_class(floys_total_change_withLake)

floys_total_change_withLake$cap_length<- as.numeric(floys_total_change_withLake$cap_length)
floys_total_change_withLake$recap_length<- as.numeric(floys_total_change_withLake$recap_length)

#before we run regressions, we first need to test for significant growth in all lakes and across time periods. 
#can remove the "filter p value line" to get a full table of all the p values for growths. 

#TODO: fix this, it doesn't work. 

#view(floys_total_change_withLake)
growth_evidence <-floys_total_change_withLake %>%
  group_by(lake_combined, class) %>%
  summarise(
    p_value = tryCatch(
      t.test(recap_length, cap_length, paired = TRUE) %>%
        broom::tidy() %>%
        pull(p.value),
      error = function(e) NA_real_
    )
  )%>%
  drop_na()%>%
  filter(p_value< 0.05)

#sanity check. 
counts_per_lake_pair<- floys_total_change_withLake%>%
  group_by(lake_combined, class)%>%
  count()

#as we can see, there is not evidence for significant growth within years, 
#but there is evidence for significant growth between years. 
#this makes it slightly easier for us to generalize growth patterns. 

#first we will make some plots

#plot 1: consider ALL pairs of cap_lenght, change_length values, including within-season pairs

ggplot(floys_total_change_withLake, aes(x = cap_length, y = change_length, color = lake_combined)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Growth Regression all lakes + all dates") +
  theme_bw()
ggsave("Growth_Regression_All_Data_lake_specific.png", dpi = 300)
#TODO: save this plot. 


#plot 2: consider only pairs of cap_length, change_length values that are significant. 
year_data<- floys_total_change_withLake%>%
  filter(class %in% growth_evidence$class)

ggplot(year_data, aes(x = cap_length, y = change_length, color = lake_combined)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Growth Regression all lakes only significant growth periods") +
  theme_bw()
ggsave("Significant_Growth_Regressions_all_lakes.png", dpi = 300)

#save this plot. 


#plot 3: consider only pairs of cap_lenght, change_length values from june mark to june recap. 
jun_jun_data<- floys_total_change_withLake%>%
  filter(class == "June2023_to_June2024")

ggplot(jun_jun_data, aes(x = cap_length, y = change_length, color = lake_combined)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Growth Regression only June to June datapoints") +
  theme_bw()

ggsave("Significant_Growth_Regressions_June-June_data_only.png", 
       dpi = 300)


#plot 4: this is just for lower tangle lake, separated by mark/recap "classes"


year_data_lower<- year_data%>%
  filter(lake_combined == "Lower")
ggplot(year_data_lower, aes(x = cap_length, y = change_length, color = class)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "lower lake growth between different mark/recap periods"
  )+
  theme_bw()

ggsave("Lower_Lake_growth_over_different_mark_recap_periods.png", 
       dpi = 300)


#we can see visually depending on how we separate the data, there are some differences in  the growth regression. 

#now we want to do some statistical tests.


#first we will test to see if there is different growth just in lower lake, between sets of cap/recaps across years 
#Ie: is capping in april 2023 and recapping in June 2024 different than June to april etc. 

#save this plot. 
#as we can sort of see, there seems to be a more defined growth curve for June to June and April to June than from June to April and April to April. 
#we have to test to see if this is significant. 
#what does this mean for our analysis? 
#this means that fish caught in april 2023 and recapped in april 2024 have different growth than fish caught in June 2023 and June 2024. 


#First we make a linear model with no interaction 
#then we make a model with interaction between cap_length and class as predictors. this is essentially equivalent to the model: 
#change length<- a1 + b1cap_length + b2class + b3cap_length*class (where b3 is the interaction coefficient)
#we determine if the interaction is significant. 

model_nointeraction_lower<- lm(change_length~ cap_length + class, data = year_data_lower)
model_interaction_lower <- lm(change_length ~ cap_length*class, data = year_data_lower)
anova(model_interaction_lower)
anova(model_interaction_lower, model_nointeraction_lower)

#here, the p value above 0.05 indicates that there is not a significant interaction between class and cap length to predict change length in lower tangle lake. 
#this allows us to "pool" the data (class doesn't matter anymore. everything can be thought of now as "2023-2024")
#This gives us more power for our length correction. 

#now, let's test for the interaction of lake and cap_length, in all the pooled data. 
model_nointeraction_lake <- lm(change_length ~ cap_length+lake_combined, data = year_data)
model_interaction_lake <- lm(change_length ~ cap_length*lake_combined, data = year_data)
anova(model_nointeraction_lake, model_interaction_lake)

#this is an extremely high p value, telling us that there is no interaction between lake and cap length. 
#This allows us to further pool the data. Instead of considering growth in each lake separately, we can say that lake does not significantly 
#affect the slope/intercept of the growth regression. 


#a bit of extra analysis: 
#We can experiment with another tool, emtrends, to compare the slopes. 

emtrends(model_interaction_lower, ~ class, var = "cap_length") %>%
  pairs()  

emtrends(model_interaction_lake, ~ lake_combined, var = "cap_length") %>%
  pairs()  

#confirming: nothing is significant in slope differentiation . further evidence that we can pool the data to create a single growth regression. 


#comparing the intercepts. 
emm_lower <- emmeans(model_interaction_lower, ~ class, at = list(x = 0))
summary(emm_lower)       # gives you the estimated means at x = 0
pairs(emm_lower)         # pairwise contrasts of those intercepts

emm_total <- emmeans(model_interaction_lake, ~ lake_combined, at = list(x = 0))
summary(emm_total)       # gives you the estimated means at x = 0
pairs(emm_total)         # pairwise contrasts of those intercepts

#great. Let's proceed with making a pooled regression we can use to correct the "n2" event data. 
#we could probably do some kind of non-parametric bootstrap as well but for now we will accept the high p-values from these tests. 
#let's build a function that can correct back to original lengths given the recap_length and the model 

#recap_length-cap_length = m*cap_length + b

#-cap_length = m*cap_length + b - recap_length 

#cap_length = recap_length - m*cap_length - b

#cap_length+m*cap_length = recap_length - b

#cap_length*(1+m) = recap_length - b

#cap_length = (recap_length -b)/(1+m)

##########################################
#######Length correction for all data
##########################################


length_correction<- function(length, model)
{
  slope<- coef(model)[[2]]
  intercept<- coef(model)[[1]]
  length_cap<- (length-intercept)/(1+slope)
}

#get a linear model that uses all our data 

model_all_data<- lm(change_length~ cap_length, data = floys_total_change_withLake)


#in our dataset with corrected lengths, we will want to only correct N2 individuals that were recaptured. M2 individuals do not need to be corrected, because 
#we have their length values for the N1 event. 
#let's take a look at the total data data structure 

#remember, this dataframe has some redundancy, to allow for the m2 individuals to also be considered as n2 individuals. 


#this is the true n2 Not including the m2s! 
true_n2s<- total_data%>%
  filter(cap_label == "n2")

#we have a total of 415 recaps recorded here. 
#this matches with the original excel sheet, so we're on track so far. 


#These are the true m2s that don't need to be corrected. 

#TODO: check this! there might still be some redundancy here. 
#there is redundancy but that's ok. we just want to make sure we're not correcting recap lengths. 
true_m2s<- total_data%>%
  filter(cap_label == "m2")

#view(true_m2s)
#there is one tuple not represented here: 
#technically 2 in the total_data dataframe because of the m2/n2 redundancy. 

recap_8330<- total_data%>%
  filter(PIT == "8330")

#the rest of the data
rest<- total_data%>%
  anti_join(true_n2s)%>%
  anti_join(true_m2s)%>%
  anti_join(recap_8330)

rest$cap_label
#as we can see, these are all n1's or within events. 

#need to filter floys_total_change_withLake so the cap date is the first date in 2023 captured and the recap length is the first date in 2024 captured. 


filtered_floy_change_23_24 <- floys_total_change_withLake %>%
  mutate(
    year_cap = year(Date_cap),
    year_recap = year(Date_recap)
  ) %>%
  filter(year_cap == 2023, year_recap == 2024) %>%  # ensure cap is in 2023 and recap is in 2024
  group_by(Floy) %>%
  filter(
    Date_cap == min(Date_cap),
    Date_recap == min(Date_recap)
  ) %>%
  ungroup()

#sanity check:count
#we appear to be missing one. It might be this pit tag thing? 
filtered_floy_change_23_24%>%
  group_by(lake_combined)%>%
  count()

#add the pit tag one 
#since there is no original length for this fish because it lost its' floy tag, we will assign it a length based on our regression. 

original_length_recap8330<- length_correction(as.numeric(recap_8330$Fork_length), model_all_data) 

#ok we have the original length of recap 8330 now. 

recap_8330$corrected_length<- original_length_recap8330
recap_8330$lake_combined<- "Lower"

#we will add this guy to the total dataset later. 
true_m2s<- right_join(true_m2s, filtered_floy_change_23_24, join_by(Floy, lake_combined))%>%
  select(-c(Date_cap, Date_recap, change_length, recap_length), corrected_length = cap_length)

corrected_Fork_length_n2s<- sapply(as.numeric(true_n2s$Fork_length),length_correction, model_all_data)

#this leads to one mysterious "NA" caused by a "?" in place of a fork length in the original data
true_n2s$corrected_length<- corrected_Fork_length_n2s

#deal with rest of the data, which is the n1 events basically (and also the within event recaps and stuff, which we don't really care about.)

rest$corrected_length<- as.numeric(rest$Fork_length)

corrected_total_data<- rest%>%
  bind_rows(true_m2s)%>%
  bind_rows(true_n2s)%>%
  bind_rows(recap_8330)

#recaps are all doubled. 
counts<- corrected_total_data%>%  
  group_by(lake_combined)%>%
  filter(grouping == "m2")%>%
  count()

#view(corrected_total_data%>%
#  filter(Floy == "UNK"))


#we will save this dataframe so we don't have to keep running this script over and over. 
write.csv(corrected_total_data, "Total_data_corrected_lengths.csv", row.names = FALSE)

#reading this gives us the exact format we want. Except maybe some minor discrepancy between <NA> and NA
data<- read.csv("Total_data_corrected_lengths.csv")


#next, we can look at individual lakes as we will be treating them separately. 


