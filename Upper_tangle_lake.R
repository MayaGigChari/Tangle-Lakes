#here we will deal with round and shallow tangle lakes. 
###TODO:
#Determine if 300 is an accurate cutoff.
#For the m2 ecdf distribution, should I be using m2 from event 2 lengths or m2 from event 1 lengths. 
#Make sure that the one "UNK" floy was not used in the length-correction estimates. 
#I think I should be using the m2 from event 1. 
#Determine some way to do a power analysis. 


library(tidyverse)
library(sf)
library(recapr)

#######################################
###STEP 1: EVALUATE GROWTH
#######################################


#Below I evaluate growth in two ways. The first considers only this particular lake, the second uses pooled data. 

total_data

summary(total_data)

#do we count within event recaps when determining size selectivity as different fish? 

#first, we consider lower lake by filtering for only the fish that were captured in round/shallow lake. 
#however, two fish that were caught in lower and recapped in shallow are also considered here. 

#this is also considering the two floys that moved into shallowround. 
Upper_lake_data<- total_data%>%
  filter(lake_combined == "Upper")


Upper_lake_data_floys<-Upper_lake_data%>%
  select(Floy)%>%
  filter(!is.na(Floy))

floys_upper<-df_long%>%
  filter(Floy %in% Upper_lake_data_floys$Floy)

#now we will check for growth between June 2023 and June 2024. This is much simpler than lower because there is only one possible period of growth. 
#except for within event recaps


#plot

plotly::ggplotly(
  ggplot(floys_upper, aes(x = Date, y = length, group = Floy, color = factor(Floy))) +
    geom_point()+
    geom_line() +
    labs(title = "Fish Length over time Shallow Lake", x = "Date", y = "Length (cm)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_discrete(name = "Fish ID") +
    theme_bw()+
    theme(legend.position = "none")
) 
ggsave("fish_growth_upper.png", 
       height = 12,  # Adjust the height (in inches)
       width = 6,    # Adjust the width (in inches)
       dpi = 300)

#as we can see, there are a lot fewer data points for upper. 

#Because there is such limited data, and we have found that growth regressions are not significantly affected by lake, 
#we will use the pooled regression to correct for round changes


##THIS IS IMPORTANT and we will use it a lot. 
#upper tangle lake has BIG fish!
min_size_capture_Upper<- Upper_lake_data%>%
  filter(Date < "2024-01-01")%>%
  filter(!is.na(Fork_length))%>%
  summarize(
    min = min(as.numeric(Fork_length))
  )

######################################################
##Length corrrection and data correction using ALL the data 
#####################################################

#load the dataframe, with corrected growths already builtin. 

data_full<- read.csv("Total_data_corrected_lengths.csv")
data_full_Upper<- data_full %>%
  filter(lake_combined == "Upper")%>%
  filter(cap_label %in% events_list)

#first let's look at the ecdf without truncating the distribution. 
ecdf_plot_withcorrection_Upper_full_regression<-ggplot(data_full_Upper, aes(x = as.numeric(Fork_length), color = grouping)) +
  stat_ecdf(geom = "step") +  # ECDF with step lines
  labs(x = "length", y = "CDF", color = "Group", title = "m2-length corrected ECDF Upper using full regression, no min-truncation") +
  theme_minimal()
ggsave("ecdf_plot_withcorrection_Upper_full_regression.png", ecdf_plot_withcorrection_shallowround_full_regression, dpi = 300)

#Now let's truncate to the minimum captured size in event 1, which as we said above is min_size_capture_shallowround


#we keep fewer allowable fish using the full regression than just the shallow-round regression
data_full_Upper_truncated<- data_full_Upper%>%
  filter(corrected_length>= min_size_capture_Upper$min)


ecdf_plot_withcorrection_Upper_full_regression_truncated<-ggplot(data_full_Upper_truncated, aes(x = as.numeric(corrected_length), color = grouping)) +
  stat_ecdf(geom = "step") +  # ECDF with step lines
  labs(x = "length", y = "CDF", color = "Group", title = "m2-length corrected ECDF Shallow-Round using full regression, with min-truncation") +
  theme_minimal()
ggsave("ecdf_plot_withcorrection_Upper_full_regression_truncated.png", ecdf_plot_withcorrection_Upper_full_regression_truncated, dpi = 300)


#Now we will perform k-s tests with and without this full-data length correction. 

events_list = c("n1", "n2", "m2")

Upper_ks_nocorrection<- ks_test(data_full_Upper, events_list, "Upper", length_variable = "Fork_length" )

Upper_ks_withcorrection<- ks_test(data_full_Upper_truncated, events_list, "Upper", length_variable = "corrected_length")

#somehow, the ks tests are all passing even though there is obviously a difference. check this, probably a sample size thing. 



#because the ks tests passed (distributions are not significantly different between the two years and the recaps). 

#################################################################
################LENGTH COMPOSITION ANALYSES
####################################################################


#when making these distributions, we want to make sure that we are only considering n1 and n2, and not using the m2 data twice. 

data_full_Upper_n1n2<- data_full_Upper%>%
  filter(grouping %in% c("n1", "n2"))

data_full_Upper_n1n2 <- data_full_Upper_n1n2 %>%
  mutate(Year_capped = year(Date))

#now we can run some of the functions from the Length_distribution file. 


#just visualizing the continuous distribution first 

Upper_continuous_viz<- plot_length_dist_continuous(data_full_Upper_n1n2, lakeSystem = "Upper")

#THIS is a very strange distribution for 2024 upper lake. Not many fish were caught, and it looks like one in each size bin was caught. 
Upper_continuous_viz$full_distribution_years_sep
#now we can do some statistics. 

#bin the lengths
data_full_Upper_n1n2_binned<- assign_bins(data_full_Upper_n1n2)

#do an analysis for both years, 2023 and 2024

Upper_length_stats_bothyears<- binned_length_statistics(data_full_Upper_n1n2_binned)

#the preservation needs to happen here. 
Upper_length_stats_2023<- binned_length_statistics(data_full_Upper_n1n2_binned, yearSort = TRUE, year = 2023)

Upper_length_stats_2024<- binned_length_statistics(data_full_Upper_n1n2_binned, yearSort = TRUE, year = 2024)


#make some plots for both years, 2023 and 2024

Upper_length_stats_bothyears_plot<- plot_length_binnedWithError(Upper_length_stats_bothyears)

Upper_length_stats_2023_plot<- plot_length_binnedWithError(Upper_length_stats_2023)

Upper_length_stats_2024_plot<- plot_length_binnedWithError(Upper_length_stats_2024)

#there is very sparse data. 

##########################################################################
####LENGHT-BY-WEIGHT
############################################################################

#fit the LBV length-by weight relationship allometric model. 

#aL^b is the model, we estimate parameters for a and b. 

#this is the allometric weight-length model 

#one poorly recorded weight of zero is fixed with this line. 
wl.lin <- lm_lw(data_full_Upper_n1n2%>%drop_na(Fork_length)%>%filter(Weight >0))


summary(wl.lin)

# Remember, as a linear model the intercept is equal to ln(alpha) and the slope coefficient is equal to the beta parameter

coef(wl.lin)

# Finally lets save our parameter estimates
# Remember if we want the mean-unbiased estimate of the alpha parameter we need
#   to account for the lognormal correction when re-transforming

wl.lin.sigma <- sigma(wl.lin) 
wl.lin.sigma
# There is an analytical solution for the error standard deviation 

# ln(alpha)
wl.lin.ln_alpha <- as.numeric(coef(wl.lin)[1])

# alpha (mean-unbiased)# this is from the fish 622 course. 
wl.lin.alpha <- exp(wl.lin.ln_alpha)*exp((wl.lin.sigma^2)/2)

# Beta
wl.lin.beta <- as.numeric(coef(wl.lin)[2])
wl.lin.beta

# Lets plot the model fit
plot_wl(data=data_full_Upper_n1n2, alpha=wl.lin.alpha, beta=wl.lin.beta)

#this is probably good information for some bayesian model.

#we can see, interestingly that the beta of about 3.39 is very similar to the beta value in the Fielding lake 2024 survey. 

#length proportions are in 25 mm fl categories 


#now we have done the length-weight relationship and the length composition analysis. 

#For fun, maybe we can see what these statistics and graphs would look like if we used the length-corrected data. 

####################################################################
####LENGTH COMPOSITION WITH LENGTH-CORRECTED DAta
###################################################################

#BELOW is the same analysis as above for length composition, but with length-corrected data (no cutoff. )

data_full_Upper

#because the functions were built for a dataframe with fiel "Fork_length", we will make a new dataframe and just assign the corrected_length 
#value to the Fork_length value. 

#make a copy
data_full_Upper_ld_copy<- data_full_Upper

#do the assignment 
data_full_Upper_ld_copy$Fork_length<- data_full_Upper_ld_copy$corrected_length

#now we can proceed with the same analyses as above. 

#first we will do some data wrangling and visualization 

data_full_Upper_n1n2_corrected<- data_full_Upper_ld_copy%>%
  filter(grouping %in% c("n1", "n2"))

data_full_Upper_n1n2_corrected<- data_full_Upper_n1n2_corrected %>%
  mutate(Year_capped = year(Date))

Upper_continuous_viz_correct<- plot_length_dist_continuous(data_full_Upper_n1n2_corrected, lakeSystem = "Upper")

#it only makes sense to look at both years now because the 2024 data has been corrected, so no longer really represents 2024.

#if we look at both years overlayed, we can see that with the correction the 2024 histogram has a maximum density at a much lower fork length. 

Upper_continuous_viz_correct$full_distribution_bothyears

#let's look at the total combined distribution 
Upper_continuous_viz_correct$full_distribution


#now let's look at the years in separate panels. observe how shifted the data has become. 

Upper_continuous_viz_correct$full_distribution_years_sep

#bin the lengths
data_full_Upper_n1n2_corrected_binned<- assign_bins(data_full_Upper_n1n2_corrected)



#do an analysis for both years, 2023 and 2024

#perhaps there is one missing. 

#one na. 
Upper_length_stats_bothyears_corrected<- binned_length_statistics(data_full_Upper_n1n2_corrected_binned)

#something was binned as NA

#the na is exisitng in the bins category. 
#the preservation needs to happen here. 
Upper_length_stats_2023_corrected<- binned_length_statistics(data_full_Upper_n1n2_corrected_binned, yearSort = TRUE, year = 2023)

Upper_length_stats_2024_corrected<- binned_length_statistics(data_full_Upper_n1n2_corrected_binned, yearSort = TRUE, year = 2024)


#make some plots for both years, 2023 and 2024
#shifts everything way low., 
Upper_length_stats_bothyears_plot_corrected<- plot_length_binnedWithError(Upper_length_stats_bothyears_corrected)

Upper_length_stats_2023_plot_corrected<- plot_length_binnedWithError(Upper_length_stats_2023_corrected)

Upper_length_stats_2024_plot_corrected<- plot_length_binnedWithError(Upper_length_stats_2024_corrected)





###Finally, let's do this one more time, but with length-corrected data that IS cutoff from 2024 to the minimum size captured in 2023. 
#this truncated data has corrected lengths for 2024 corrected to the minimum size for 2023. 

data_full_Upper_truncated


data_full_Upper_trunc_copy<- data_full_Upper_truncated

#do the assignment 
data_full_Upper_trunc_copy$Fork_length<- data_full_Upper_trunc_copy$corrected_length

#now we can proceed with the same analyses as above. 

#first we will do some data wrangling and visualization 

data_full_Upper_n1n2_corrected_trunc<- data_full_Upper_trunc_copy%>%
  filter(grouping %in% c("n1", "n2"))

data_full_Upper_n1n2_corrected_trunc<- data_full_Upper_n1n2_corrected_trunc %>%
  mutate(Year_capped = year(Date))

Upper_continuous_viz_trunc<- plot_length_dist_continuous(data_full_Upper_n1n2_corrected_trunc, lakeSystem = "Upper")

#it only makes sense to look at both years now because the 2024 data has been corrected, so no longer really represents 2024.

#if we look at both years overlayed, we can see that with the correction the 2024 histogram has a maximum density at a much lower fork length. 

Upper_continuous_viz_trunc$full_distribution_bothyears

#let's look at the total combined distribution 
Upper_continuous_viz_trunc$full_distribution


#now let's look at the years in separate panels. observe how shifted the data has become. 

#we really mess up the 2024 distribution when we truncate it to fit 2023. (really for this it would be an "extension" of the 2023 data. )
Upper_continuous_viz_trunc$full_distribution_years_sep

#bin the lengths
data_full_Upper_n1n2_trunc_binned<- assign_bins(data_full_Upper_n1n2_corrected_trunc)


Upper_length_stats_bothyears_trunc<- binned_length_statistics(data_full_Upper_n1n2_trunc_binned)

Upper_length_stats_2023_corrected_trunc<- binned_length_statistics(data_full_Upper_n1n2_trunc_binned, yearSort = TRUE, year = 2023)

Upper_length_stats_2024_corrected_trunc<- binned_length_statistics(data_full_Upper_n1n2_trunc_binned, yearSort = TRUE, year = 2024)


#make some plots for both years, 2023 and 2024

Upper_length_stats_bothyears_plot_corrected_trunc<- plot_length_binnedWithError(Upper_length_stats_bothyears_trunc)

Upper_length_stats_2023_plot_corrected_trunc<- plot_length_binnedWithError(Upper_length_stats_2023_corrected_trunc)

Upper_length_stats_2024_plot_corrected_trunc<- plot_length_binnedWithError(Upper_length_stats_2024_corrected_trunc)



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
sf_df_Upper_r1<- st_crop(sf_df_nounk, upper_lake_region_1)
sf_df_Upper_r1$region<-1
sf_df_Upper_r2<- st_crop(sf_df_nounk, upper_lake_region_2)
sf_df_Upper_r2$region<-2
sf_df_Upper_r3<- st_crop(sf_df_nounk, upper_lake_region_3)
sf_df_Upper_r3$region<-3

sf_df_Upper_regions<- sf_df_Upper_r1%>%
  rbind(sf_df_Upper_r2)%>%
  rbind(sf_df_Upper_r3)


#not sure what the correct format is for the chi2 test here. 
#review Logan's code. 
#looks like Logan did not have totals. 


#TEST FOR COMPLETE MIXING WITHOUT LENGTH-CORRECTED DATA. 

#wonder if this is correct. 

#there appears to be one fish where area_marked is not given but area recapped is 1. What is going on with this fish/floy? 
#perhaps it is a floy that has an NA for 2023? 
test1_upper<- chi2_markrecap(tibble(sf_df_Upper_regions))
pet_diag_1<- chisq.test(test1_upper) 
fisher.test(test1_upper)
#try matt tyers's test. 

#redo the test, filtering the dataframe for corrected lengths that do not meet the cutoff for 2023. 
#this doesn't seem to affect the outcome, because n2 individuals that are not recaps are not really considered. 
test1_corrected_upper<- chi2_markrecap(tibble(sf_df_Upper_regions%>%filter(corrected_length >= min_size_capture_Upper$min)))
pet_diag_1_corrected_upper<- chisq.test(test1_corrected_upper)  
fisher.test(test1_corrected_upper)
#seems like this passed. But I also sort of arbitrarily chose region cutoffs and there is a very small sample size. 

#there are tiny sample sizes. 
#next: Test for equal probability of capture during Event 1 (H0 = Every fish has an equal probability of being captured and marked during event 1)

#also test for Test for equal probability of capture during Event 2 (H0 = Every fish has an equal probability of being captured and marked during event 2)

### TEST FOR EQUAL PROBABILITY OF CAPTURE IN EVENT 1 AND EVENT 2
ret_upper<- petersen_consistency(tibble(sf_df_Upper_regions), region)
ret_upper$consistency_p_values

#redo the test, filtering the dataframe for corrected lengths that do not meet the cutoff for 2023. 

ret_corrected_upper<- petersen_consistency(tibble(sf_df_Upper_regions%>%filter(corrected_length >= min_size_capture_Upper$min)), region)
ret_corrected_upper$consistency_p_values
#the ultimate outcome of the test does not change. Cool! 
#these all have a very small sample size


##################################
###MATT TYERS RECAPR CODE FOR CONSISTENCY STUFF
##################################


#total sample sizes by stratum: 
#remember, we separated m2 and n2, so need to add for matt's code. 
counts_by_strata_upper <- sf_df_Upper_regions %>% group_by(region) %>% 
  summarize(n1 = sum(stat == "n1"), n2 = sum(stat == "n2")+sum(stat == "m2"), m2 = sum(stat == "m2"))

matrix_upper<- petersen_matrix_tyerscode(sf_df_Upper_regions)
matrix_upper$"NA"<-NULL

consistencytest(counts_by_strata_upper$n1, counts_by_strata_upper$n2, stratamat = matrix_upper)


#at least one passed, although there is very very low power and very small sample size. 

#############################################################
#################### POPULATION ESTIMATION 
##############################################################=


###Below, this is ALL done without using the corrected-length dataset. This is NOT the correct final outcome, and is just a reference. 

Upper_lake_data%>%
  filter(!is.na(Fork_length))%>%
  summarize(
    min = min(Fork_length)
  )

Upper_lake_data_nomorts<- Upper_lake_data%>%
  filter(stat == grouping)%>%
  filter(cap_label %in% c("n1", "n2", "m2"))


Upper_lake_data_nomovers<- Upper_lake_data_nomorts%>%
  filter(!Floy %in% c("19970", "18608"))

#why are there so many less in this pop estimate than when we use a different dataset for shallowround lakes???
#now finally these estimates are good. 

#double check that the n2s are being counted correctly.
#they are being correctly counted! 

petersen_estimate_Upper<- pop_calc(petersen_funcs,  Upper_lake_data_nomorts)
Chapman_estimate_Upper<- pop_calc(Chapman_funcs, Upper_lake_data_nomorts)
bailey_estimate_Upper<- pop_calc(Bailey_funcs, Upper_lake_data_nomorts)


#great.we now have the three different population estimates for shallow/round lake. 


####however, these pop estimates do not consider the 2023 allowable catch size. Let's now use our corrected length dataset to filter for that. 
####Below is the final pop estimate with the corrected length dataset. 
#we use the truncated dataset defined above
data_full_Upper_truncated

#we apply the filters. 

data_full_Upper_truncated_pop<- data_full_Upper_truncated%>%
  filter(stat == grouping)%>%
  filter(cap_label %in% c("n1", "n2", "m2"))

#now we do the pop estimates. 

petersen_estimate_trunc_Upper<- pop_calc(petersen_funcs,  data_full_Upper_truncated_pop)
Chapman_estimate_trunc_Upper<- pop_calc(Chapman_funcs, data_full_Upper_truncated_pop)
bailey_estimate_trunc_Upper<- pop_calc(Bailey_funcs, data_full_Upper_truncated_pop)

#these are the management-based estimates. 