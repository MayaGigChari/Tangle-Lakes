#Tangle Lakes K-S tests 

#in this script I will take a better look at the data. 

#question: do we factor mortalities into length distribution? seems like yes?
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

#stratification by lake? not just gear type?


#questions: what is ANG vs OTH in gear type

raw_df<-read_excel("Tangles_LT_M-R_2023-24.xlsx")


fish_apr_2023<- raw_df%>% 
  subset(`Set Date Time` < "2023-05-01")

fish_sum_2023<- raw_df%>%
  subset(`Set Date Time` > "2023-05-01" & `Set Date Time` < "2024-01-01")

fish_apr_2024<-raw_df%>%
  subset(`Set Date Time` > "2024-01-01" & `Set Date Time` < "2024-05-01")

fish_sum_2024<-raw_df%>%
  subset(`Set Date Time` > "2024-05-01")

#first for all the lakes together 

#all the possibilities: 
#1. marked in apr 2023, recapped in June 2023 (inverse: all capped in June 2023)
#2. marked in apr 2023, recapped in apr 2024(inverse: all capped in apr 2024)
#3. marked in apr 2023, recapped in June 2024(inverse: all capped in June 2024)
#4. marked in june 2023, recapped in apr 2024 (inverse: all cpaped in apr 2024)
#5. marked in june 2023, recapped in june 2024( inverse: all capped in june 2024)
#6. marked in apr 2024, recapped in june 2024( inverse: all capped in june 2024)

#7. marked in 2023, recapped in 2024(inverse: all capped in 2024)
#8. marked in 2023, recapped in apr 2024(inverse: all capped in apr 2024)
#9. marked in 2023, recapped in June 2024(inverse: all capped in june 2024)
#10. marked in apr 2023, recapped in 2024(inverse: all capped in 2024)
#11. marked in jun 2023, recapped in 2024(inverse: all capped in 2024)


#step 1: look at a the total marks and recaps for all lakes (case 7)

#remove all values that are duplicate by removing the na's (corresponds to a duplicate sample)
#should be the same a selecting by unique flow tag
marked_2023<- raw_df%>%
  subset(`Set Date Time` > "2023-01-01" & `Set Date Time` < "2024-01-01")%>%
  subset(!is.na(`Fork Length (mm)`))%>%
  group_by(`Gray Floy Tag`)%>%
  count()

#the floy tag should be the unique identifier. 

marked_2023_check<- raw_df%>%
  subset(`Set Date Time` > "2023-01-01" & `Set Date Time` < "2024-01-01")%>%
  group_by(`Gray Floy Tag`)%>%
  count()

#this is weird. want to join by gray floy tag and see which ones have a count greater than 1. 

dup_2023<- left_join(marked_2023_check, raw_df)%>%
  subset(n > 1)

#maybe it's best to find all the unique things within each discrete event and then join everything together that way.
#logic: fish captured and clipped in april 2024 were definitely captured in april 2024. 
#some fish were not tagged properly 
#are there any other uniuqe identifiers besides the pit tag? 

unique_fish_apr_2023<- fish_apr_2023%>% 
  subset(!is.na(`Fork Length (mm)`))%>%
  group_by(`Gray Floy Tag`)%>%
  count()


unique_fish_sum_2023<- fish_sum_2023%>%
  subset(!is.na(`Fork Length (mm)`))%>%
  group_by(`Gray Floy Tag`)%>%
  count()%>%
  filter(n<2)
# Step 2: Select rows from the original dataset with those tags
selected_fish_data_sum_2023 <- fish_sum_2023 %>%
  filter(`Gray Floy Tag` %in% unique_fish_sum_2023$`Gray Floy Tag`)

#not sure about pit tag 18005

unique_fish_apr_2024<- fish_apr_2024%>%
  subset(!is.na(`Fork Length (mm)`))%>%
  group_by(`Gray Floy Tag`)%>%
  count()%>%
  filter(n>1)
selected_fish_data_apr_2024 <- fish_apr_2024 %>%
  filter(`Gray Floy Tag` %in% unique_fish_apr_2024$`Gray Floy Tag`)


###new idea: 

#all fish recapped in event 2 (in all of 2024) 
#some fish are not flow tagged but are still part of the whole system



#these were all the fish caught in 2023 including some redundancies. but hopefully this will work out 
marked_2023<- raw_df%>%
  subset(`Set Date Time` > "2023-01-01" & `Set Date Time` < "2024-01-01")%>%
  subset(!is.na(`Fork Length (mm)`))
  
#in 2024, to measure actual recaps we can only consider the fish that were originally marked, I think
#what is the most reliable way to determine which fish were marked when and how? looking for GROWTH in particular here. 

#would be nice to have the fin marks. 
  
#m2 is recapped in event 2 total. 
recapped_2024<- raw_df%>%
  subset(`Set Date Time` > "2024-01-01")%>%
  subset(!is.na(`Fork Length (mm)`))%>%
  subset(`All 4 Tangle Lakes n1/n2/m2`== "m2")

#to see if fish grew in length, we need to take the fish recapped in 2024 and the fish marked in 2023 and make sure that they're all 
#assume that the primary key in these is the pit tag. 

#question: did they ever pit tag and not flow tag? 
colnames(marked_2023)
marked_2023_forgrowth<- marked_2023%>%
  select(Lake, "Fork Length (mm)","Gray Floy Tag", "Mark",mark_notes = "Fish Notes", "Set Date Time")

recapped_2024_forgrowth<- recapped_2024%>%
  select(recap_lake= Lake, recap_length = "Fork Length (mm)","Gray Floy Tag", "Mark",recap_notes = "Fish Notes", recap_day = "Set Date Time")

marked_recapped_forgrowth<- left_join( recapped_2024_forgrowth, marked_2023_forgrowth, by = join_by(`Gray Floy Tag`))

marked_recapped_forgrowth%>%
  group_by(recap_lake)%>%
  count()



#this gives us the mark date and the recap date. the FIRST of each of these dates. filters by the minimum date 
unique_tags<- marked_recapped_forgrowth%>%
  group_by(`Gray Floy Tag`)%>%
  filter(`Set Date Time` == min(`Set Date Time`)) %>%
  ungroup()  


easy_length_regression<-unique_tags%>%
  select(cap_length = `Fork Length (mm)`, recap_length, fishID = `Gray Floy Tag`)

t.test(as.integer(easy_length_regression$cap_length), as.integer(easy_length_regression$recap_length), paired = TRUE)

#strongly reject the p value here. 


#lake-specific blah blah blah 

#ks tests. 

#need to compare lengths 

lengths = seq(0, max(length_data$ALLLENGTHS))

# transforming the data
data_transform = cut(length_data$CAPLENGTHS, lengths,
                     right=FALSE)
# creating the frequency table
freq_table = table(data_transform)
# printing the frequency table
print("Frequency Table")
print(freq_table)

