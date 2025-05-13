#this is a script for the analyses of all marks (n1) = April+ June 2023
#(n2) = April + June 2024 
#(m2) = recapped April + June 2024

#data suggestion: please add a column for WITHIN EVENT RECAP!!!

#first I will check for changes in size
install.packages("dgof")
library("dgof")
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
install.packages("PairedData")
library(PairedData)
install.packages("ggpubr")
library(ggpubr)

raw_df<-read_excel("Tangles_LT_M-R_2023-24.xlsx")


#these were all the fish caught in 2023 including some redundancies. but hopefully this will work out 
#remove every fish that does not have a length (for the purposes of this correction. )
#marked_2023<- raw_df%>%
#  subset(`Set Date Time` > "2023-01-01" & `Set Date Time` < "2024-01-01")%>%
#  subset(!is.na(`Fork Length (mm)`))
#assu

#this should be the same as: 

#here there are only 492 rows 
#should probably use this one below since its 
marked_2023_check<- raw_df%>%
  subset(!is.na(`Fork Length (mm)`))%>%
  subset(`All 4 Tangle Lakes n1/n2/m2`== "n1")


#m2 is recapped in event 2 total. 
recapped_2024<- raw_df%>%
  subset(`Set Date Time` > "2024-01-01")%>%
  subset(!is.na(`Fork Length (mm)`))%>%
  subset(`All 4 Tangle Lakes n1/n2/m2`== "m2")

#to see if fish grew in length, we need to take the fish recapped in 2024 and the fish marked in 2023 and make sure that they're all 
#assume that the primary key in these is the pit tag. 

#question: did they ever pit tag and not flow tag? 

marked_2023_forgrowth<- marked_2023_check%>%
  dplyr::select(mark_Lake = Lake, "Fork Length (mm)","Gray Floy Tag", "Mark",mark_notes = "Fish Notes", "Set Date Time", Pit = "PIT Tag"  )
  

recapped_2024_forgrowth<- recapped_2024%>%
  dplyr::select(recap_lake= Lake, recap_length = "Fork Length (mm)","Gray Floy Tag", "Mark",recap_notes = "Fish Notes", recap_day = "Set Date Time", Pit = "PIT Tag")

marked_recapped_forgrowth_floy<- left_join( recapped_2024_forgrowth, marked_2023_forgrowth, by = join_by(`Gray Floy Tag`))


#remove the unknown floy tag issue. 
marked_recapped_forgrowth_floy<- marked_recapped_forgrowth_floy%>%
  filter(`Gray Floy Tag` != "UNK")

#this is the dataset with important information about all fish that were recapped. uniquely identifiable by the flow tag or the
total_recapped<- left_join(recapped_2024_forgrowth, marked_recapped_forgrowth_floy)


#this is not necessary if we separate by the n1 label instead of separating by date. 
#unique_tags<- marked_recapped_forgrowth%>%
#  group_by(`Gray Floy Tag`)%>%
#  filter(`Set Date Time` == min(`Set Date Time`)) %>%
#  ungroup()  


#prepare the data so that there is an easy paried t-test performed 
easy_length_regression<-total_recapped%>%
  dplyr::select(cap_length = `Fork Length (mm)`, recap_length, fishID = `Gray Floy Tag`)

easy_length_regression$cap_length<- as.integer(easy_length_regression$cap_length)
easy_length_regression$recap_length<- as.integer(easy_length_regression$recap_length)

#null hypothesis: there is no growth  between the mark and recap events. 
growth_test<-t.test(as.integer(easy_length_regression$cap_length), as.integer(easy_length_regression$recap_length), paired = TRUE)


ggpaired(d = easy_length_regression, cond1 = "cap_length", cond2= "recap_length", color = "white", line.color = "black", line.size = 0.4,
         palette = "npg")


#conclusion: with p<0.05 we reject the null; there is growth between period 1 and period 2. 


#k-s tests: 

#traditional K-s test

#length distribution overall: 
#need to plot the cumulative frequency up to the maximum for each lake 
#for lower tangle lake 2022-2023: had ice fishing and normal fishing I believe 
lower_lake_n1<- raw_df%>%
  filter(Lake == "Lower", `All 4 Tangle Lakes n1/n2/m2` == "n1")
lower_lake_n2<-raw_df%>%
  filter(Lake == "Lower", `All 4 Tangle Lakes n1/n2/m2`== "n2")
lower_lake_m2<-raw_df%>%
  filter(Lake == "Lower", `All 4 Tangle Lakes n1/n2/m2`== "m2")

lake_data<- raw_df%>%
  filter( `All 4 Tangle Lakes n1/n2/m2`%in% c("n1", "n2", "m2"))%>%
  filter("Fork Length (mm)" == "?")

lake_data_mod<- lake_data %>%
  mutate(lake_combined = case_when(
    Lake %in% c("Shallow", "Round") ~ "shallow_round",  # Combine shallow and round
    TRUE ~ Lake  # Keep other lakes as they are
  ))

#this is the data for all lower lakes


#plotting everythign by combined lake
ggplot(lake_data_mod, aes(x = as.integer(`Fork Length (mm)`), color = stat)) +
  stat_ecdf(geom = "step") +  # ECDF with step lines
  labs(x = "Fork Length (mm)", y = "cdf", color = "Group") +
  theme_minimal() +
  facet_wrap(~ lake_combined)  # This creates separate plots for each lake

#need to have a way to do k-s tests for each thing 
lakes = c("shallow_round", "Lower", "Upper")
events = c("n1", "n2", "m2")

# Create an empty data frame to store the results
ks_results <- data.frame(
  lake = character(),
  ks_test = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each lake
for (lake in lakes) {
  
  # Create a subset of lake data for the current lake
  lake_data_filtered <- lake_data_mod %>%
    filter(lake_combined == lake) %>%
    dplyr::select("Fork Length (mm)", stat)
  
  # Loop over all event combinations (n1, n2, m2)
  for (event in events) {
    # Filter data for the current event
    event_data <- lake_data_filtered %>%
      filter(stat == event) %>%
      pull("Fork Length (mm)") %>%
      as.integer()
    
    # Store event data in a list for later use
    assign(paste0("data_", event), event_data)
  }
  
  # Perform the KS tests and collect results
  ks_1 <- ks.test(data_n1, data_m2)
  ks_2 <- ks.test(data_n2, data_m2)
  ks_3 <- ks.test(data_n1, data_n2)
  
  # Append the results to the ks_results data frame
  ks_results <- ks_results %>%
    add_row(lake = lake, 
            ks_test = "n1 vs m2", 
            statistic = ks_1$statistic, 
            p_value = ks_1$p.value) %>%
    add_row(lake = lake, 
            ks_test = "n2 vs m2", 
            statistic = ks_2$statistic, 
            p_value = ks_2$p.value) %>%
    add_row(lake = lake, 
            ks_test = "n1 vs n2", 
            statistic = ks_3$statistic, 
            p_value = ks_3$p.value)
}


#ks test check: 
#maybe it would be beneficial to clean the data just for the ks test
#for the n1 values, we accept n1+ morts
#for the n2 values, we accept n2+ morts 
#for the m2 values, we accept m2

#want to drop values that are not lengths (null and "?")
#is it easiest to filter by lake and event? 

#maybe I'll make a new event numbering system: 
#event 1 is spring 2023, event 2 is summer 2023, event 3 is spring 2024, event 4 is summer 2024 

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

event_num<- unlist(lapply(raw_df$`Set Date Time`, event_converter))

raw_df$event_num<- event_num

data_trimmed<- raw_df%>%
  dplyr::select("Lake",  `Set Date Time`,`Fork Length (mm)`, "Mark", `Gray Floy Tag`,`PIT Tag`, Recapture, `All 4 Tangle Lakes n1/n2/m2`,`Fish Notes`, event_num)%>%
  filter(!is.na(`Fork Length (mm)`))

#all recaps should be marked properly with "m2" in "All 4 Tangle Lakes n1/n2/m2"\
#need to include morts. 

status_finder <- function(event_num, cap_status) {
  if (event_num == 1 | event_num == 2) {
    print("t")
    if (cap_status == "Mort") {
      return("n1")
    }
  }
  if (event_num == 2 | event_num == 3) {
    if (cap_status == "Mort") {
      return("n2")
    }
  }
  return(cap_status)  # If no conditions match, return the cap_status
}


stat <- mapply(status_finder, 
               event_num = data_trimmed$event_num, 
               cap_status = data_trimmed$`All 4 Tangle Lakes n1/n2/m2`)

data_trimmed$stat<- stat

lake_data_mod<- data_trimmed %>%
  mutate(lake_combined = case_when(
    Lake %in% c("Shallow", "Round") ~ "shallow_round",  # Combine shallow and round
    TRUE ~ Lake  # Keep other lakes as they are
  ))

#now we can drop all the in event recaps

lake_data_mod<- lake_data_mod%>%
  filter(stat %in% c("n1", "n2", "m2")
  )

# View the final table
print(ks_results)

data_transform = cut(length_data$CAPLENGTHS, lengths,
                     right=FALSE)
# creating the frequency table
freq_table = table(data_transform)
# printing the frequency table
print("Frequency Table")
print(freq_table)

#were seems to be one marked fishthat didn't have a recorded flow tag number
#this would match with a recapped crab that didn't have a match in the marked flow tags. 

#there is one recappd mark fish with a flow tag that didn't have a recoreded flowtag number but DID have a pit tag number. 

