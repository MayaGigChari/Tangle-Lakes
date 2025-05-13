#this is a script for just lower tangle lake. it takes some of the stuff from data preparation. 
install.packages("plotly")
library(plotly)
#add other packages later

total_data

summary(total_data)




###THIS IS ALL FOR JUST lower lake. 
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


#in the growth correction file, we go into detail about 2023-2024 growth in different lakes. Here we want to test if there is any growth within years 


floys_april_june_2023<- floys_lower_change%>%
  filter(Date_cap < as_datetime("2023-05-01"))%>%
  filter(Date_recap > as_datetime("2023-05-01") & Date_recap < as_datetime("2024-01-01"))

#floys_june_april_year<- floys_lower_change%>%
#  filter(Date_cap > as_datetime("2023-05-01") & Date_cap < as_datetime("2024-01-01"))%>%
#  filter(Date_recap < as_datetime("2024-05-01"))

floys_april_june_2024<- floys_lower_change%>%
  filter(Date_cap > as_datetime("2024-01-01") & Date_cap < as_datetime("2024-05-01"))%>%
  filter(Date_recap > as_datetime("2024-05-01") & Date_recap < as_datetime("2025-01-01"))


#now we will do some tests to see if there is growth in all these periods. 
apr_jun_2023<- t.test(as.integer(floys_april_june_2023$recap_length),as.integer(floys_april_june_2023$cap_length), paired = TRUE )
#jun_apr_year<- t.test(as.integer(floys_june_april_year$recap_length),as.integer(floys_june_april_year$cap_length), paired = TRUE )
apr_jun_2024<- t.test(as.integer(floys_april_june_2024$recap_length),as.integer(floys_april_june_2024$cap_length), paired = TRUE )




#arrange into a table

#for some reason I have one less 
lower_lake_growth_ttests <- tibble(
  Test = c("Apr-Jun 2023", "Apr-Jun 2024"),
  p_value = c(apr_jun_2023$p.value, apr_jun_2024$p.value),
  t_statistic = c(apr_jun_2023$statistic,  apr_jun_2024$statistic),
  df = c(apr_jun_2023$parameter,  apr_jun_2024$parameter),
  conf_int_low = c(apr_jun_2023$conf.int[1],  apr_jun_2024$conf.int[1]),
  conf_int_high = c(apr_jun_2023$conf.int[2],  apr_jun_2024$conf.int[2])
)

#we do not see significant growth within years. 
#we did not remove "negative" growths because this may bias the data as we are not removing outliers in the opposite direction. 

#now that we know that there is no within-year significant growth, we can turn to the growth-correction instructions outlined 
######################################################
##Length corrrection and data correction using ALL the data 
#####################################################

#load the dataframe, with corrected growths already builtin. 

#there are two floys that may or may not be included in lower. 

data_full<- read.csv("Total_data_corrected_lengths.csv")
data_full_Lower<- data_full %>%
  filter(lake_combined == "Lower")%>%
  filter(cap_label %in% events_list)

#first let's look at the ecdf without truncating the distribution. 
ecdf_plot_withcorrection_Lower_full_regression<-ggplot(data_full_Lower, aes(x = as.numeric(Fork_length), color = grouping)) +
  stat_ecdf(geom = "step") +  # ECDF with step lines
  labs(x = "length", y = "CDF", color = "Group", title = "m2-length corrected ECDF Lower using full regression, no min-truncation") +
  theme_minimal()
ggsave("ecdf_plot_withcorrection_Lower_full_regression.png", ecdf_plot_withcorrection_shallowround_full_regression, dpi = 300)

#Now let's truncate to the minimum captured size in event 1, which as we said above is min_size_capture_shallowround

min_size_capture_Lower<- data_full_Lower%>%
  filter(Date < "2024-01-01")%>%
  filter(!is.na(Fork_length))%>%
  summarize(
    min = min(as.numeric(Fork_length))
  )

#we keep fewer allowable fish using the full regression than just the shallow-round regression
data_full_Lower_truncated<- data_full_Lower%>%
  filter(corrected_length>= min_size_capture_Lower$min)


ecdf_plot_withcorrection_Lower_full_regression_truncated<-ggplot(data_full_Lower_truncated, aes(x = as.numeric(corrected_length), color = grouping)) +
  stat_ecdf(geom = "step") +  # ECDF with step lines
  labs(x = "length", y = "CDF", color = "Group", title = "m2-length corrected ECDF Shallow-Round using full regression, with min-truncation") +
  theme_minimal()
ggsave("ecdf_plot_withcorrection_Lower_full_regression_truncated.png", ecdf_plot_withcorrection_Lower_full_regression_truncated, dpi = 300)


#Now we will perform k-s tests with and without this full-data length correction. 

events_list = c("n1", "n2", "m2")

Lower_ks_nocorrection<- ks_test(data_full_Lower, events_list, "Lower", length_variable = "Fork_length" )

Lower_ks_withcorrection<- ks_test(data_full_Lower_truncated, events_list, "Lower", length_variable = "corrected_length")

#because the ks tests passed (distributions are not significantly different between the two years and the recaps). 

#################################################################
################LENGTH COMPOSITION ANALYSES
####################################################################


#when making these distributions, we want to make sure that we are only considering n1 and n2, and not using the m2 data twice. 

data_full_Lower_n1n2<- data_full_Lower%>%
  filter(grouping %in% c("n1", "n2"))

data_full_Lower_n1n2 <- data_full_Lower_n1n2 %>%
  mutate(Year_capped = year(Date))

#now we can run some of the functions from the Length_distribution file. 


#just visualizing the continuous distribution first 

Lower_continuous_viz<- plot_length_dist_continuous(data_full_Lower_n1n2, lakeSystem = "Lower")

#THIS is a very strange distribution for 2024 Lower lake. Not many fish were caught, and it looks like one in each size bin was caught. 
Lower_continuous_viz$full_distribution_years_sep
#now we can do some statistics. 

#bin the lengths

data_full_Lower_n1n2<- data_full_Lower_n1n2%>%
  drop_na(Fork_length)%>%
  filter(Fork_length!= "?")

data_full_Lower_n1n2_binned<- assign_bins(data_full_Lower_n1n2)

#do an analysis for both years, 2023 and 2024

Lower_length_stats_bothyears<- binned_length_statistics(data_full_Lower_n1n2_binned)

#the preservation needs to happen here. 
Lower_length_stats_2023<- binned_length_statistics(data_full_Lower_n1n2_binned, yearSort = TRUE, year = 2023)

Lower_length_stats_2024<- binned_length_statistics(data_full_Lower_n1n2_binned, yearSort = TRUE, year = 2024)


#make some plots for both years, 2023 and 2024

Lower_length_stats_bothyears_plot<- plot_length_binnedWithError(Lower_length_stats_bothyears)

Lower_length_stats_2023_plot<- plot_length_binnedWithError(Lower_length_stats_2023)

Lower_length_stats_2024_plot<- plot_length_binnedWithError(Lower_length_stats_2024)

#there is very sparse data. 

##########################################################################
####LENGHT-BY-WEIGHT
############################################################################

#fit the LBV length-by weight relationship allometric model. 

#aL^b is the model, we estimate parameters for a and b. 

#this is the allometric weight-length model 

#one poorly recorded weight of zero is fixed with this line. 
wl.lin_lower <- lm_lw(data_full_Lower_n1n2%>%drop_na(Fork_length)%>%filter(Weight >0))


summary(wl.lin_lower)

# Remember, as a linear model the intercept is equal to ln(alpha) and the slope coefficient is equal to the beta parameter

coef(wl.lin_lower)

# Finally lets save our parameter estimates
# Remember if we want the mean-unbiased estimate of the alpha parameter we need
#   to account for the lognormal correction when re-transforming

wl.lin.sigma_lower <- sigma(wl.lin_lower) 
wl.lin.sigma_lower
# There is an analytical solution for the error standard deviation 

# ln(alpha)
wl.lin.ln_alpha_lower <- as.numeric(coef(wl.lin_lower)[1])

# alpha (mean-unbiased)# this is from the fish 622 course. 
wl.lin.alpha_lower <- exp(wl.lin.ln_alpha_lower)*exp((wl.lin.sigma_lower^2)/2)

# Beta
wl.lin.beta_lower <- as.numeric(coef(wl.lin_lower)[2])
wl.lin.beta_lower

# Lets plot the model fit
plot_wl(data=data_full_Lower_n1n2, alpha=wl.lin.alpha_lower, beta=wl.lin.beta_lower)

#this is probably good information for some bayesian model.

#we can see, interestingly that the beta of about 3.57 is very similar to the beta value in the Fielding lake 2024 survey. 

#length proportions are in 25 mm fl categories 


#now we have done the length-weight relationship and the length composition analysis. 

#For fun, maybe we can see what these statistics and graphs would look like if we used the length-corrected data. 

####################################################################
####LENGTH COMPOSITION WITH LENGTH-CORRECTED DAta
###################################################################

#BELOW is the same analysis as above for length composition, but with length-corrected data (no cutoff. )

data_full_Lower

#because the functions were built for a dataframe with fiel "Fork_length", we will make a new dataframe and just assign the corrected_length 
#value to the Fork_length value. 

#make a copy
data_full_Lower_ld_copy<- data_full_Lower

#do the assignment 
data_full_Lower_ld_copy$Fork_length<- data_full_Lower_ld_copy$corrected_length

#now we can proceed with the same analyses as above. 

#first we will do some data wrangling and visualization 

data_full_Lower_n1n2_corrected<- data_full_Lower_ld_copy%>%
  filter(grouping %in% c("n1", "n2"))

data_full_Lower_n1n2_corrected<- data_full_Lower_n1n2_corrected %>%
  mutate(Year_capped = year(Date))%>%
  filter(Fork_length!= "?")

Lower_continuous_viz_correct<- plot_length_dist_continuous(data_full_Lower_n1n2_corrected, lakeSystem = "Lower")

#it only makes sense to look at both years now because the 2024 data has been corrected, so no longer really represents 2024.

#if we look at both years overlayed, we can see that with the correction the 2024 histogram has a maximum density at a much lower fork length. 

Lower_continuous_viz_correct$full_distribution_bothyears

#let's look at the total combined distribution 
Lower_continuous_viz_correct$full_distribution


#now let's look at the years in separate panels. observe how shifted the data has become. 

Lower_continuous_viz_correct$full_distribution_years_sep

#bin the lengths
data_full_Lower_n1n2_corrected_binned<- assign_bins(data_full_Lower_n1n2_corrected)



#do an analysis for both years, 2023 and 2024


Lower_length_stats_bothyears_corrected<- binned_length_statistics(data_full_Lower_n1n2_corrected_binned)

Lower_length_stats_2023_corrected<- binned_length_statistics(data_full_Lower_n1n2_corrected_binned, yearSort = TRUE, year = 2023)

Lower_length_stats_2024_corrected<- binned_length_statistics(data_full_Lower_n1n2_corrected_binned, yearSort = TRUE, year = 2024)


#these are the "length-corrected" binned distributions. 
Lower_length_stats_bothyears_plot_corrected<- plot_length_binnedWithError(Lower_length_stats_bothyears_corrected)

Lower_length_stats_2023_plot_corrected<- plot_length_binnedWithError(Lower_length_stats_2023_corrected)

Lower_length_stats_2024_plot_corrected<- plot_length_binnedWithError(Lower_length_stats_2024_corrected)

###Finally, let's do this one more time, but with length-corrected data that IS cutoff from 2024 to the minimum size captured in 2023. 
#this truncated data has corrected lengths for 2024 corrected to the minimum size for 2023. 

data_full_Lower_truncated


data_full_Lower_trunc_copy<- data_full_Lower_truncated

#do the assignment 
data_full_Lower_trunc_copy$Fork_length<- data_full_Lower_trunc_copy$corrected_length

#now we can proceed with the same analyses as above. 

#first we will do some data wrangling and visualization 

data_full_Lower_n1n2_corrected_trunc<- data_full_Lower_trunc_copy%>%
  filter(grouping %in% c("n1", "n2"))

data_full_Lower_n1n2_corrected_trunc<- data_full_Lower_n1n2_corrected_trunc %>%
  mutate(Year_capped = year(Date))

Lower_continuous_viz_trunc<- plot_length_dist_continuous(data_full_Lower_n1n2_corrected_trunc, lakeSystem = "Lower")

#it only makes sense to look at both years now because the 2024 data has been corrected, so no longer really represents 2024.

#if we look at both years overlayed, we can see that with the correction the 2024 histogram has a maximum density at a much lower fork length. 

Lower_continuous_viz_trunc$full_distribution_bothyears

#let's look at the total combined distribution 
Lower_continuous_viz_trunc$full_distribution


#now let's look at the years in separate panels. observe how shifted the data has become. 

#we really mess up the 2024 distribution when we truncate it to fit 2023. (really for this it would be an "extension" of the 2023 data. )
Lower_continuous_viz_trunc$full_distribution_years_sep

#bin the lengths
data_full_Lower_n1n2_trunc_binned<- assign_bins(data_full_Lower_n1n2_corrected_trunc)


Lower_length_stats_bothyears_trunc<- binned_length_statistics(data_full_Lower_n1n2_trunc_binned)

Lower_length_stats_2023_corrected_trunc<- binned_length_statistics(data_full_Lower_n1n2_trunc_binned, yearSort = TRUE, year = 2023)

Lower_length_stats_2024_corrected_trunc<- binned_length_statistics(data_full_Lower_n1n2_trunc_binned, yearSort = TRUE, year = 2024)


#make some plots for both years, 2023 and 2024

Lower_length_stats_bothyears_plot_corrected_trunc<- plot_length_binnedWithError(Lower_length_stats_bothyears_trunc)

Lower_length_stats_2023_plot_corrected_trunc<- plot_length_binnedWithError(Lower_length_stats_2023_corrected_trunc)

Lower_length_stats_2024_plot_corrected_trunc<- plot_length_binnedWithError(Lower_length_stats_2024_corrected_trunc)




#####################################################################
########### CONSISTENCY TESTS FOR THE PETERSON ESTIMATOR: Mark-recapture 
#####################################################################

#first we need to figure out the consistency tests. stuff. let's look at just shallow tangle lakes I guess. 

#pull the data from the maps and viz file. MAKE SURE TO GO BACK TO THAT SCRIPT AND SAVE EVERYTHING!

#basically just getting all n1, n2 guys

#this is not correct. this just gets all the n1s and the recaps, not the n2s. 
#this has way fewer things than the original dataset. I don't know why. 
#this dataset filters out the redundancy, so it should not be used for the peterson estimate. 
sf_df_nounk <- sf_df %>%
  filter(Floy != "UNK" | is.na(Floy))




#need to go by stats now. 
sf_df_Lower_r1<- st_crop(sf_df_nounk, lower_lake_region_1)
sf_df_Lower_r1$region<-1
sf_df_Lower_r2<- st_crop(sf_df_nounk, lower_lake_region_2)
sf_df_Lower_r2$region<-2
sf_df_Lower_r3<- st_crop(sf_df_nounk, lower_lake_region_3)
sf_df_Lower_r3$region<-3

sf_df_Lower_regions<- sf_df_Lower_r1%>%
  rbind(sf_df_Lower_r2)%>%
  rbind(sf_df_Lower_r3)


#not sure what the correct format is for the chi2 test here. 
#review Logan's code. 
#looks like Logan did not have totals. 


#TEST FOR COMPLETE MIXING WITHOUT LENGTH-CORRECTED DATA. 

#wonder if this is correct. 

#there appears to be one fish where area_marked is not given but area recapped is 1. What is going on with this fish/floy? 
#perhaps it is a floy that has an NA for 2023? 
test1_Lower<- chi2_markrecap(tibble(sf_df_Lower_regions))
pet_diag_1<- chisq.test(test1_Lower) 
fisher.test(test1_Lower)
#try matt tyers's test. 

#redo the test, filtering the dataframe for corrected lengths that do not meet the cutoff for 2023. 
#this doesn't seem to affect the outcome, because n2 individuals that are not recaps are not really considered. 
test1_corrected_Lower<- chi2_markrecap(tibble(sf_df_Lower_regions%>%filter(corrected_length >= min_size_capture_Lower$min)))
pet_diag_1_corrected_Lower<- chisq.test(test1_corrected_Lower)  
fisher.test(test1_corrected_Lower)
#seems like this passed. But I also sort of arbitrarily chose region cutoffs and there is a very small sample size. 

#there are tiny sample sizes. 
#next: Test for equal probability of capture during Event 1 (H0 = Every fish has an equal probability of being captured and marked during event 1)

#also test for Test for equal probability of capture during Event 2 (H0 = Every fish has an equal probability of being captured and marked during event 2)

### TEST FOR EQUAL PROBABILITY OF CAPTURE IN EVENT 1 AND EVENT 2
ret_Lower<- petersen_consistency(tibble(sf_df_Lower_regions), region)
ret_Lower$consistency_p_values

#redo the test, filtering the dataframe for corrected lengths that do not meet the cutoff for 2023. 

ret_corrected_Lower<- petersen_consistency(tibble(sf_df_Lower_regions%>%filter(corrected_length >= min_size_capture_Lower$min)), region)
ret_corrected_Lower$consistency_p_values
#the ultimate outcome of the test does not change. Cool! 
#these all have a very small sample size


##################################
###MATT TYERS RECAPR CODE FOR CONSISTENCY STUFF
##################################


#total sample sizes by stratum: 
#remember, we separated m2 and n2, so need to add for matt's code. 

sf_df_Lower_regions_nosmall<- sf_df_Lower_regions%>%filter(corrected_length >= min_size_capture_Lower$min)
counts_by_strata_Lower <- sf_df_Lower_regions_nosmall %>% group_by(region) %>% 
  summarize(n1 = sum(stat == "n1"), n2 = sum(stat == "n2")+sum(stat == "m2"), m2 = sum(stat == "m2"))

matrix_Lower<- petersen_matrix_tyerscode(sf_df_Lower_regions)
matrix_Lower$"NA"<-NULL

consistencytest(counts_by_strata_Lower$n1, counts_by_strata_Lower$n2, stratamat = matrix_Lower)


#at least one passed, so we are good to use petersen. 



#############################################################
#################### POPULATION ESTIMATION 
##############################################################=


###Below, this is ALL done without using the corrected-length dataset. This is NOT the correct final outcome, and is just a reference. 


Lower_lake_data_nomorts<- 
  lower_lake_data%>%
  filter(stat == grouping)%>%
  filter(cap_label %in% c("n1", "n2", "m2"))


Lower_lake_data_nomovers<- Lower_lake_data_nomorts%>%
  filter(!Floy %in% c("19970", "18608"))

#why are there so many less in this pop estimate than when we use a different dataset for shallowround lakes???
#now finally these estimates are good. 

#double check that the n2s are being counted correctly.
#they are being correctly counted! 

petersen_estimate_Lower<- pop_calc(petersen_funcs,  Lower_lake_data_nomorts)
Chapman_estimate_Lower<- pop_calc(Chapman_funcs, Lower_lake_data_nomorts)
bailey_estimate_Lower<- pop_calc(Bailey_funcs, Lower_lake_data_nomorts)


#great.we now have the three different population estimates for shallow/round lake. 


####however, these pop estimates do not consider the 2023 allowable catch size. Let's now use our corrected length dataset to filter for that. 
####Below is the final pop estimate with the corrected length dataset. 
#we use the truncated dataset defined above
data_full_Lower_truncated

#we apply the filters. 

data_full_Lower_truncated_pop<- data_full_Lower_truncated%>%
  filter(stat == grouping)%>%
  filter(cap_label %in% c("n1", "n2", "m2"))

#now we do the pop estimates. 

petersen_estimate_trunc_Lower<- pop_calc(petersen_funcs,  data_full_Lower_truncated_pop)
Chapman_estimate_trunc_Lower<- pop_calc(Chapman_funcs, data_full_Lower_truncated_pop)
bailey_estimate_trunc_Lower<- pop_calc(Bailey_funcs, data_full_Lower_truncated_pop)

#there was a VERY small sample size in the n2 event. We will discuss this in the discussion. 
#A sample size of 50 is required to attain the desired precision criteria. A sample of 19 fish was captured in round tangle lake in event 2. Therefore, the 
#precision criterion are NOT met for round tangle lakes.



