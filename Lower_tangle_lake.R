#this is a script for just lower tangle lake. it takes some of the stuff from data preparation. 
install.packages("plotly")
library(plotly)
#add other packages later

total_data

summary(total_data)

#do we count within event recaps when determining size selectivity as different fish? 

#first, we consider lower lake by filtering for only the fish that were ever captured in lower lake
lower_lake_data<- total_data%>%
  filter(Lake == "Lower")

#we do know that two fish were first captured in lower and recaptured in shallow. for now we will count these fish as "shallow" and exclude them from the lower count. 

low_shallow_recaps<- total_data%>%
  group_by(Floy)%>%
  summarise(unique_lakes = n_distinct(Lake))%>%
  filter(unique_lakes>1)


#three tags appear to have more than one unique lake, so we'll look more into them.

#here it looks like the 19970 floy was caught first in lower and recapped in shallow 
floy_19970<- total_data%>%
  filter(Floy == '19970')

#this one was a within-event recap between round and shallow, caught on the 10 of June in round and the 17 of June in Shallow 
floy_18005<- total_data%>%
  filter(Floy == '18005')

#Floy 18608 was also caught in lower and recapped in shallow the following year. 
floy_18608<- total_data%>%
  filter(Floy == '18608')

#for simplicity, we will treat the tags 19970 and 18608 as having been caught and recapped in shallow lake. 

lower_lake_data_noshal<- lower_lake_data%>%
  filter(!Floy %in% c("19970", "18608"))
  
#we see that we have two fewer rows now. 

##### GROWTH 

#fish in lower tangle lake had four technical capture times (April 2023/2024 and June 2023/2024)
#Thus, we can look at growth within seasons and between the two years. 

#lots of data manipulation happens in the data_preparation file, but we will use this data structure created in the Data_preparation file: 
#data called "floys lower" which was created in the data preparation file. 

#here let's first visualize the total growth of the fish. 
#this still contains possibly some data we don't want. 
plotly::ggplotly(
  ggplot(floys_Lower, aes(x = Date, y = length, group = Floy, color = factor(Floy))) +
    geom_point()+
    geom_line() +
    labs(title = "Fish Length over time Shallow Lake", x = "Date", y = "Length (cm)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_discrete(name = "Fish ID") +
    theme_bw()+
    theme(legend.position = "none")
) 
ggsave("fish_growth_Lower.png", 
       height = 12,  # Adjust the height (in inches)
       width = 6,    # Adjust the width (in inches)
       dpi = 300)



# In this code, we are essentially making a pairwise comparison between all 
#instances of a tag and it's following instances, to look at growth between each
#individual capture event. 
floys_lower_change <- floys_Lower %>%
  group_by(Floy) %>%
  arrange(length_id) %>%
  reframe(
    expand_grid(i = row_number(), j = row_number()) %>%
      filter(i < j) %>% #avoids repetition and i = j case. 
      mutate(
        length_1 = length[i],
        length_2 = length[j],
        Date_cap = Date[i],
        Date_recap = Date[j],
        change_length = length_2 - length_1
      ) %>%
      select(Date_cap, Date_recap, change_length,cap_length = length_1, recap_length = length_2, change_length)
  )
#sometimes, like in the case of 2024 june 13-18 there is a huge negative growth. This is something 
#to consider, but for now I will just keep these in the dataset. 


#for simplicitly, let's consider three growth periods
#really, there are 6 ways we can choose 2 dates from 4 possible dates, so we omit 3 that don't help us too much. 


floys_april_june_2023<- floys_lower_change%>%
  filter(Date_cap < as_datetime("2023-05-01"))%>%
  filter(Date_recap > as_datetime("2023-05-01") & Date_recap < as_datetime("2024-01-01"))

floys_june_april_year<- floys_lower_change%>%
  filter(Date_cap > as_datetime("2023-05-01") & Date_cap < as_datetime("2024-01-01"))%>%
  filter(Date_recap < as_datetime("2024-05-01"))

floys_april_june_2024<- floys_lower_change%>%
  filter(Date_cap > as_datetime("2024-01-01") & Date_cap < as_datetime("2024-05-01"))%>%
  filter(Date_recap > as_datetime("2024-05-01") & Date_recap < as_datetime("2025-01-01"))


#now we will do some tests to see if there is growth in all these periods. 
apr_jun_2023<- t.test(as.integer(floys_april_june_2023$recap_length),as.integer(floys_april_june_2023$cap_length), paired = TRUE )
jun_apr_year<- t.test(as.integer(floys_june_april_year$recap_length),as.integer(floys_june_april_year$cap_length), paired = TRUE )
apr_jun_2024<- t.test(as.integer(floys_april_june_2024$recap_length),as.integer(floys_april_june_2024$cap_length), paired = TRUE )




#arrange into a table

#for some reason I have one less 
lower_lake_growth_ttests <- tibble(
  Test = c("Apr-Jun 2023", "Jun-Apr Year", "Apr-Jun 2024"),
  p_value = c(apr_jun_2023$p.value, jun_apr_year$p.value, apr_jun_2024$p.value),
  t_statistic = c(apr_jun_2023$statistic, jun_apr_year$statistic, apr_jun_2024$statistic),
  df = c(apr_jun_2023$parameter, jun_apr_year$parameter, apr_jun_2024$parameter),
  conf_int_low = c(apr_jun_2023$conf.int[1], jun_apr_year$conf.int[1], apr_jun_2024$conf.int[1]),
  conf_int_high = c(apr_jun_2023$conf.int[2], jun_apr_year$conf.int[2], apr_jun_2024$conf.int[2])
)

#now I will attempt to see if removing the negative growth changes these results. 
floys_april_june_2023_noneg<- floys_april_june_2023%>%
  filter(change_length > 0)
floys_june_april_year_noneg<-floys_june_april_year%>%
  filter(change_length > 0)
floys_april_june_2024_noneg<- floys_april_june_2024%>%
  filter(change_length > 0)

apr_jun_2023_noneg<- t.test(as.integer(floys_april_june_2023_noneg$recap_length),as.integer(floys_april_june_2023_noneg$cap_length), paired = TRUE )
jun_apr_year_noneg<- t.test(as.integer(floys_june_april_year_noneg$recap_length),as.integer(floys_june_april_year_noneg$cap_length), paired = TRUE )
apr_jun_2024_noneg<- t.test(as.integer(floys_april_june_2024_noneg$recap_length),as.integer(floys_april_june_2024_noneg$cap_length), paired = TRUE )

#arrange into a table
lower_lake_growth_ttests_noneg <- tibble(
  Test = c("Apr-Jun 2023 Noneg", "Jun-Apr Year Noneg", "Apr-Jun 2024 Noneg"),
  p_value = c(apr_jun_2023_noneg$p.value, jun_apr_year_noneg$p.value, apr_jun_2024_noneg$p.value),
  t_statistic = c(apr_jun_2023_noneg$statistic, jun_apr_year_noneg$statistic, apr_jun_2024_noneg$statistic),
  df = c(apr_jun_2023_noneg$parameter, jun_apr_year_noneg$parameter, apr_jun_2024_noneg$parameter),
  conf_int_low = c(apr_jun_2023_noneg$conf.int[1], jun_apr_year_noneg$conf.int[1], apr_jun_2024_noneg$conf.int[1]),
  conf_int_high = c(apr_jun_2023_noneg$conf.int[2], jun_apr_year_noneg$conf.int[2], apr_jun_2024_noneg$conf.int[2])
)
#when we exclude negative growth we have very low degrees of freedom for 2024 april to june but all p values are significant and show positive growth. 



