#here we will deal with round and shallow tangle lakes. 
###TODO:
#Determine if 300 is an accurate cutoff.
#For the m2 ecdf distribution, should I be using m2 from event 2 lengths or m2 from event 1 lengths. 
#Make sure that the one "UNK" floy was not used in the length-correction estimates. 


library(tidyverse)

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
shallow_round_lake_data<- total_data%>%
  filter(lake_combined == "Shallow_Round" | Floy %in% c("19970", "18608"))

shallow_round_lake_data$lake_combined = "Shallow_Round"

shallow_round_lake_data_floys<-shallow_round_lake_data%>%
  select(Floy)%>%
  filter(!is.na(Floy))
  
floys_shallow_round<-df_long%>%
  filter(Floy %in% shallow_round_lake_data_floys$Floy)

#now we will check for growth between June 2023 and June 2024. This is much simpler than lower because there is only one possible period of growth. 
#except for within event recaps


#plot

plotly::ggplotly(
  ggplot(floys_shallow_round, aes(x = Date, y = length, group = Floy, color = factor(Floy))) +
    geom_point()+
    geom_line() +
    labs(title = "Fish Length over time Shallow Lake", x = "Date", y = "Length (cm)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_discrete(name = "Fish ID") +
    theme_bw()+
    theme(legend.position = "none")
) 
ggsave("fish_growth_shallowround.png", 
       height = 12,  # Adjust the height (in inches)
       width = 6,    # Adjust the width (in inches)
       dpi = 300)


#next we will quantify the change. 

floys_shallowround_change <-floys_shallow_round %>%
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


shallow_round_ttest<- t.test(as.integer(floys_shallowround_change$recap_length),as.integer(floys_shallowround_change$cap_length), paired = TRUE )

shallow_round_growth_ttest <- tibble(
  p_value = shallow_round_ttest$p.value,
  t_statistic = shallow_round_ttest$statistic,
  df = shallow_round_ttest$parameter,
  conf_int_low = shallow_round_ttest$conf.int[1],
  conf_int_high = shallow_round_ttest$conf.int[2], 
)
#there is definitely a significant growth here in shallow-round. 
#we also find this out in the growth_correction section. 

#the next step is to regress growth against original size, to determine how much to correct for growth. 

#first, we will plot growth as a function of capture size? 

shallowround_growth_regression<- ggplot(floys_shallowround_change, aes(x = cap_length, y = change_length))+
  geom_point(color = "red", size = 4)+
  geom_smooth(method = lm)
ggsave("shallowround_growth_regression.png", shallowround_growth_regression, dpi = 300)

#visually, it looks like growth is not constant across ages. Let's make sure by running a linear regression. 

shallowround_growth_lm<- lm(floys_shallowround_change$change_length~ floys_shallowround_change$cap_length)

summary(shallowround_growth_lm)

#from this model summary we can see that the p value is below 0.05.However, there is a low R squared value indicating that there is a lot of unexplained variance. 

#let's now create a new column in our original dataset that accounts for changes in growth in the shallow data. 


#first let's filter for the n2 using the stat column. We will not use the grouping column because then the recaps would be labeled as "n2" instead of "m2"
#we don't need to correct recaps because we know their original lengths. 

shallow_round_lake_data_n2<- shallow_round_lake_data%>%
  filter(stat == "n2")

shallow_round_lake_data_m2<- shallow_round_lake_data%>%
  filter(grouping == "m2")

shallow_round_lake_data_non2<- shallow_round_lake_data%>%
  filter(grouping == "n1")


shallow_round_lake_data_m2<- left_join(shallow_round_lake_data_m2, floys_shallowround_change, join_by(Floy))%>%
  select(-c(Date_cap, Date_recap, change_length, recap_length), corrected_length = cap_length)

shallow_round_lake_data_non2$corrected_length<- as.numeric(shallow_round_lake_data_non2$Fork_length)

corrected_Fork_length<- sapply(as.numeric(shallow_round_lake_data_n2$Fork_length),length_correction, shallowround_growth_lm)

shallow_round_lake_data_n2$corrected_length<- corrected_Fork_length

corrected_shallow_round_lake_data<- shallow_round_lake_data_non2%>%
  bind_rows(shallow_round_lake_data_n2)%>%
  bind_rows(shallow_round_lake_data_m2)

#now we have a new dataframe with the corrected lengths and the old lengths. 

#next, we need to filter out m2 fish that could not have been caught in the first year.

#we will determine the minimium size of capture in 2023 below. 

##THIS IS IMPORTANT and we will use it a lot. 
min_size_capture_shallowround<- shallow_round_lake_data%>%
  filter(Date < "2024-01-01")%>%
  filter(!is.na(Fork_length))%>%
  summarize(
    min = min(as.numeric(Fork_length))
  )

#here we see that the minimum size of capture in round/shallow lakes in 2023 is 300 mm. Thus, we will truncate all tuples from the corrected size 
#dataframe that are corrected to be below the minimum size. 

#this removes a significant number of tuples from the data, although half of them are within event recaps anyways and were removed because NA < 300.
corrected_shallow_round_lake_data_truncated<- corrected_shallow_round_lake_data%>%
  filter(corrected_length>= 300)


#TODO: determine if 300 is actually an accurate cutoff. 

#take  a look at the fish who didn't make it. 
tuples_removed<- anti_join(corrected_shallow_round_lake_data, corrected_shallow_round_lake_data_truncated)


#now I will do some visualizations. Below is the ECDF plot when the n2 data is not corrected for length. 
#first I will remove the things that are not caps or recaps (basically, within event recaps. )

shallow_round_lake_data_nowithineven<- shallow_round_lake_data%>%
  filter(stat%in%c("m2", "n2", "n1"))

corrected_shallow_round_lake_data_truncated_nowthinevent<- corrected_shallow_round_lake_data_truncated%>%
  filter(stat%in%c("m2", "n2", "n1"))

ecdf_plot_nocorrection_shallowround<-ggplot(shallow_round_lake_data_nowithineven, aes(x = as.numeric(Fork_length), color = grouping)) +
  stat_ecdf(geom = "step") +  # ECDF with step lines
  labs(x = "length", y = "CDF", color = "Group", title = "m2-length original ECDF Shallow-Round") +
  theme_minimal()
ggsave("ecdf_plot_nocorrection_shallowround.png", ecdf_plot_nocorrection_shallowround, dpi = 300)

#as we can see, the n2 event in 2024 has a left-shifted distribution-- smaller fish were caught than in 2023. 

#now I visualize when n2 is corrected for length (ie: n2 are corrected for their annual growth)

ecdf_plot_withcorrection_shallowround<-ggplot(corrected_shallow_round_lake_data_truncated_nowthinevent, aes(x = as.numeric(corrected_length), color = grouping)) +
  stat_ecdf(geom = "step") +  # ECDF with step lines
  labs(x = "length", y = "CDF", color = "Group", title = "m2-length corrected ECDF Shallow-Round") +
  theme_minimal()
ggsave("ecdf_plot_withcorrection_shallowround.png", ecdf_plot_nocorrection_shallowround, dpi = 300)

#as we can see this is a very strange distribution. We lose the general logistic shape and the curve becomes much more sigmoidal. 

#Now we will perform k-s tests with and without this length correction. 

events_list = c("n1", "n2", "m2")

shallow_round_ks_nocorrection<- ks_test(shallow_round_lake_data_nowithineven, events_list, "Shallow-Round", length_variable = "Fork_length" )

shallow_round_ks_withcorrection<- ks_test(corrected_shallow_round_lake_data_truncated_nowthinevent, events_list, "Shallow-Round", length_variable = "corrected_length")

#as we can see here, when we do not correct for length the ks tests pass meaning the distributions are the same. However, when we do correct for length, C vs. R and M vs. C appear to require stratification. 
#we have a case IV,



######################################################
##Length corrrection and data correction using ALL the data 
#####################################################

#load the dataframe, with corrected growths already builtin. 

data_full<- read.csv("Total_data_corrected_lengths.csv")
data_full_shallowround<- data_full %>%
  filter(lake_combined == "Shallow_Round")%>%
  filter(cap_label %in% events_list)

#first let's look at the ecdf without truncating the distribution. 
ecdf_plot_withcorrection_shallowround_full_regression<-ggplot(data_full_shallowround, aes(x = as.numeric(Fork_length), color = grouping)) +
  stat_ecdf(geom = "step") +  # ECDF with step lines
  labs(x = "length", y = "CDF", color = "Group", title = "m2-length corrected ECDF Shallow-Round using full regression, no min-truncation") +
  theme_minimal()
ggsave("ecdf_plot_withcorrection_shallowround_full_regression.png", ecdf_plot_withcorrection_shallowround_full_regression, dpi = 300)

#Now let's truncate to the minimum captured size in event 1, which as we said above is min_size_capture_shallowround


#we keep fewer allowable fish using the full regression than just the shallow-round regression
data_full_shallowround_truncated<- data_full_shallowround%>%
  filter(corrected_length>= min_size_capture_shallowround$min)


ecdf_plot_withcorrection_shallowround_full_regression_truncated<-ggplot(data_full_shallowround_truncated, aes(x = as.numeric(corrected_length), color = grouping)) +
  stat_ecdf(geom = "step") +  # ECDF with step lines
  labs(x = "length", y = "CDF", color = "Group", title = "m2-length corrected ECDF Shallow-Round using full regression, with min-truncation") +
  theme_minimal()
ggsave("ecdf_plot_withcorrection_shallowround_full_regression_truncated.png", ecdf_plot_withcorrection_shallowround_full_regression_truncated, dpi = 300)


#as we can see this is a very strange distribution. We lose the general logistic shape and the curve becomes much more sigmoidal. 

#Now we will perform k-s tests with and without this full-data length correction. 

events_list = c("n1", "n2", "m2")

shallow_round_ks_nocorrection<- ks_test(data_full_shallowround, events_list, "Shallow-Round", length_variable = "Fork_length" )

shallow_round_ks_withcorrection<- ks_test(data_full_shallowround_truncated, events_list, "Shallow-Round", length_variable = "corrected_length")

#when we truncate, we really mess up the distribution and get a very very low p value for rejecting the null. 

#moving forward, first let's take the full length distribution of all fish (grouping = n2 and m2 I believe) for all the dates. 

#because the ks tests passed (distributions are not significantly different between the two years and the recaps). 

#################################################################
################LENGTH COMPOSITION ANALYSES
####################################################################


#when making these distributions, we want to make sure that we are only considering n1 and n2, and not using the m2 data twice. 

data_full_shallowround_n1n2<- data_full_shallowround%>%
  filter(grouping %in% c("n1", "n2"))

data_full_shallowround_n1n2 <- data_full_shallowround_n1n2 %>%
  mutate(Year_capped = year(Date))

#now we can run some of the functions from the Length_distribution file. 


#just visualizing the continuous distribution first 

shallow_round_continuous_viz<- plot_length_dist_continuous(data_full_shallowround_n1n2, lakeSystem = "Shallow-Round")

#now we can do some statistics. 

#bin the lengths
data_full_shallowround_n1n2_binned<- assign_bins(data_full_shallowround_n1n2)

#do an analysis for both years, 2023 and 2024

shallowround_length_stats_bothyears<- binned_length_statistics(data_full_shallowround_n1n2_binned)

#the preservation needs to happen here. 
shallowround_length_stats_2023<- binned_length_statistics(data_full_shallowround_n1n2_binned, yearSort = TRUE, year = 2023)

shallowround_length_stats_2024<- binned_length_statistics(data_full_shallowround_n1n2_binned, yearSort = TRUE, year = 2024)


#make some plots for both years, 2023 and 2024

shallowround_length_stats_bothyears_plot<- plot_length_binnedWithError(shallowround_length_stats_bothyears)

shallowround_length_stats_2023_plot<- plot_length_binnedWithError(shallowround_length_stats_2023)

shallowround_length_stats_2024_plot<- plot_length_binnedWithError(shallowround_length_stats_2024)


##########################################################################
####LENGHT-BY-WEIGHT
############################################################################

#fit the LBV length-by weight relationship allometric model. 

#aL^b is the model, we estimate parameters for a and b. 

#this is the allometric weight-length model 


wl.lin <- lm_lw(data_full_shallowround_n1n2)

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
plot_wl(data=data_full_shallowround_n1n2, alpha=wl.lin.alpha, beta=wl.lin.beta)

#this is probably good information for some bayesian model.

#we can see, interestingly that the beta of about 3.4 is very similar to the beta value in the Fielding lake 2024 survey. 

#length proportions are in 25 mm fl categories 


#now we have done the length-weight relationship and the length composition analysis. 

#For fun, maybe we can see what these statistics and graphs would look like if we used the length-corrected data. 

####################################################################
####LENGTH COMPOSITION WITH LENGTH-CORRECTED DAta
###################################################################

#BELOW is the same analysis as above for length composition, but with length-corrected data (no cutoff. )

data_full_shallowround

#because the functions were built for a dataframe with fiel "Fork_length", we will make a new dataframe and just assign the corrected_length 
#value to the Fork_length value. 

#make a copy
data_full_shallowround_ld_copy<- data_full_shallowround

#do the assignment 
data_full_shallowround_ld_copy$Fork_length<- data_full_shallowround_ld_copy$corrected_length

#now we can proceed with the same analyses as above. 

#first we will do some data wrangling and visualization 

data_full_shallowround_n1n2_corrected<- data_full_shallowround_ld_copy%>%
  filter(grouping %in% c("n1", "n2"))

data_full_shallowround_n1n2_corrected<- data_full_shallowround_n1n2_corrected %>%
  mutate(Year_capped = year(Date))

shallow_round_continuous_viz_correct<- plot_length_dist_continuous(data_full_shallowround_n1n2_corrected, lakeSystem = "Shallow-Round")

#it only makes sense to look at both years now because the 2024 data has been corrected, so no longer really represents 2024.

#if we look at both years overlayed, we can see that with the correction the 2024 histogram has a maximum density at a much lower fork length. 

shallow_round_continuous_viz_correct$full_distribution_bothyears

#let's look at the total combined distribution 
shallow_round_continuous_viz_correct$full_distribution


#now let's look at the years in separate panels. observe how shifted the data has become. 

shallow_round_continuous_viz_correct$full_distribution_years_sep

#bin the lengths
data_full_shallowround_n1n2_corrected_binned<- assign_bins(data_full_shallowround_n1n2_corrected)



#do an analysis for both years, 2023 and 2024

#perhaps there is one missing. 

#one na. 
shallowround_length_stats_bothyears_corrected<- binned_length_statistics(data_full_shallowround_n1n2_corrected_binned)

#something was binned as NA

#the na is exisitng in the bins category. 
#the preservation needs to happen here. 
shallowround_length_stats_2023_corrected<- binned_length_statistics(data_full_shallowround_n1n2_corrected_binned, yearSort = TRUE, year = 2023)

shallowround_length_stats_2024_corrected<- binned_length_statistics(data_full_shallowround_n1n2_corrected_binned, yearSort = TRUE, year = 2024)


#make some plots for both years, 2023 and 2024

shallowround_length_stats_bothyears_plot_corrected<- plot_length_binnedWithError(shallowround_length_stats_bothyears_corrected)

shallowround_length_stats_2023_plot_corrected<- plot_length_binnedWithError(shallowround_length_stats_2023_corrected)

shallowround_length_stats_2024_plot_corrected<- plot_length_binnedWithError(shallowround_length_stats_2024_corrected)





###Finally, let's do this one more time, but with length-corrected data that IS cutoff from 2024 to the minimum size captured in 2023. 
#this truncated data has corrected lengths for 2024 corrected to the minimum size for 2023. 

data_full_shallowround_truncated


data_full_shallowround_trunc_copy<- data_full_shallowround_truncated

#do the assignment 
data_full_shallowround_trunc_copy$Fork_length<- data_full_shallowround_trunc_copy$corrected_length

#now we can proceed with the same analyses as above. 

#first we will do some data wrangling and visualization 

data_full_shallowround_n1n2_corrected_trunc<- data_full_shallowround_trunc_copy%>%
  filter(grouping %in% c("n1", "n2"))

data_full_shallowround_n1n2_corrected_trunc<- data_full_shallowround_n1n2_corrected_trunc %>%
  mutate(Year_capped = year(Date))

shallow_round_continuous_viz_trunc<- plot_length_dist_continuous(data_full_shallowround_n1n2_corrected_trunc, lakeSystem = "Shallow-Round")

#it only makes sense to look at both years now because the 2024 data has been corrected, so no longer really represents 2024.

#if we look at both years overlayed, we can see that with the correction the 2024 histogram has a maximum density at a much lower fork length. 

shallow_round_continuous_viz_trunc$full_distribution_bothyears

#let's look at the total combined distribution 
shallow_round_continuous_viz_trunc$full_distribution


#now let's look at the years in separate panels. observe how shifted the data has become. 

#we really mess up the 2024 distribution when we truncate it to fit 2023. (really for this it would be an "extension" of the 2023 data. )
shallow_round_continuous_viz_trunc$full_distribution_years_sep

#bin the lengths
data_full_shallowround_n1n2_trunc_binned<- assign_bins(data_full_shallowround_n1n2_corrected_trunc)


shallowround_length_stats_bothyears_trunc<- binned_length_statistics(data_full_shallowround_n1n2_trunc_binned)

shallowround_length_stats_2023_corrected_trunc<- binned_length_statistics(data_full_shallowround_n1n2_trunc_binned, yearSort = TRUE, year = 2023)

shallowround_length_stats_2024_corrected_trunc<- binned_length_statistics(data_full_shallowround_n1n2_trunc_binned, yearSort = TRUE, year = 2024)


#make some plots for both years, 2023 and 2024

shallowround_length_stats_bothyears_plot_corrected_trunc<- plot_length_binnedWithError(shallowround_length_stats_bothyears_trunc)

shallowround_length_stats_2023_plot_corrected_trunc<- plot_length_binnedWithError(shallowround_length_stats_2023_corrected_trunc)

shallowround_length_stats_2024_plot_corrected_trunc<- plot_length_binnedWithError(shallowround_length_stats_2024_corrected_trunc)


#as we can see, truncating the distribution is not necessarily the move! 


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
sf_df_shallowround_r1<- st_crop(sf_df_nounk, shallowround_lake_region_1)
sf_df_shallowround_r1$region<-1
sf_df_shallowround_r2<- st_crop(sf_df_nounk, shallowround_lake_region_2)
sf_df_shallowround_r2$region<-2
sf_df_shallowround_r3<- st_crop(sf_df_nounk, shallowround_lake_region_3)
sf_df_shallowround_r3$region<-3

sf_df_shallowround_regions<- sf_df_shallowround_r1%>%
  rbind(sf_df_shallowround_r2)%>%
  rbind(sf_df_shallowround_r3)
#total of 255. double check against this 
sf_df_nounk_shallowround<- sf_df_nounk%>%
  filter(Lake %in% c("Shallow", "Round"))%>%
  count()


#not sure what the correct format is for the chi2 test here. 
#review Logan's code. 
#looks like Logan did not have totals. 


#TEST FOR COMPLETE MIXING WITHOUT LENGTH-CORRECTED DATA. 
test1<- chi2_markrecap(tibble(sf_df_shallowround_regions))
pet_diag_1<- chisq.test(test1)  


#redo the test, filtering the dataframe for corrected lengths that do not meet the cutoff for 2023. 
#this doesn't seem to affect the outcome, because n2 individuals that are not recaps are not really considered. 
test1_corrected<- chi2_markrecap(tibble(sf_df_shallowround_regions%>%filter(corrected_length >= min_size_capture_shallowround$min)))
pet_diag_1_corrected<- chisq.test(test1_corrected)  

#seems like this passed. But I also sort of arbitrarily chose region cutoffs and there is a very small sample size. 

#next: Test for equal probability of capture during Event 1 (H0 = Every fish has an equal probability of being captured and marked during event 1)

#also test for Test for equal probability of capture during Event 2 (H0 = Every fish has an equal probability of being captured and marked during event 2)

### TEST FOR EQUAL PROBABILITY OF CAPTURE IN EVENT 1 AND EVENT 2
ret<- petersen_consistency(tibble(sf_df_shallowround_regions), region)
ret$consistency_p_values

#redo the test, filtering the dataframe for corrected lengths that do not meet the cutoff for 2023. 

ret_corrected<- petersen_consistency(tibble(sf_df_shallowround_regions%>%filter(corrected_length >= min_size_capture_shallowround$min)), region)
ret_corrected$consistency_p_values
#the ultimate outcome of the test does not change. Cool! 


#given that we have no stratification by length/sex necessary, and the petersen estimator is consistent, let's go ahead with a traditional population estimate. 
#############################################################
#################### POPULATION ESTIMATION 
##############################################################=


###Below, this is ALL done without using the corrected-length dataset. This is NOT the correct final outcome, and is just a reference. 

shallow_round_lake_data%>%
  filter(!is.na(Fork_length))%>%
  summarize(
    min = min(Fork_length)
  )

shallow_round_lake_data_nomorts<- shallow_round_lake_data%>%
  filter(stat == grouping)%>%
  filter(cap_label %in% c("n1", "n2", "m2"))


shallow_round_lake_data_nomovers<- shallow_round_lake_data_nomorts%>%
  filter(!Floy %in% c("19970", "18608"))

#why are there so many less in this pop estimate than when we use a different dataset for shallowround lakes???
#now finally these estimates are good. 

#double check that the n2s are being counted correctly.
#they are being correctly counted! 

petersen_estimate<- pop_calc(petersen_funcs,  shallow_round_lake_data_nomorts)
Chapman_estimate<- pop_calc(Chapman_funcs, shallow_round_lake_data_nomorts)
bailey_estimate<- pop_calc(Bailey_funcs, shallow_round_lake_data_nomorts)


#estimates if we don't include the fish that moved from lower to shallow 

petersen_estimate_nomovement<- pop_calc(petersen_funcs,  shallow_round_lake_data_nomovers)
Chapman_estimate_nomovement<- pop_calc(Chapman_funcs, shallow_round_lake_data_nomovers)
bailey_estimate_nomovement<- pop_calc(Bailey_funcs, shallow_round_lake_data_nomovers)


#great.we now have the three different population estimates for shallow/round lake. 


####however, these pop estimates do not consider the 2023 allowable catch size. Let's now use our corrected length dataset to filter for that. 
####Below is the final pop estimate with the corrected length dataset. 
#we use the truncated dataset defined above
data_full_shallowround_truncated

#we apply the filters. 

data_full_shallowround_truncated_pop<- data_full_shallowround_truncated%>%
  filter(stat == grouping)%>%
  filter(cap_label %in% c("n1", "n2", "m2"))

#now we do the pop estimates. 

petersen_estimate_trunc<- pop_calc(petersen_funcs,  data_full_shallowround_truncated_pop)
Chapman_estimate_trunc<- pop_calc(Chapman_funcs, data_full_shallowround_truncated_pop)
bailey_estimate_trunc<- pop_calc(Bailey_funcs, data_full_shallowround_truncated_pop)


#as we can see, population estimates are significantly lower when we remove "uncatchable" n2 fish from the population. 



#great! now we have a cool pipeline for everything. For the rest of the lakes, we will not do ALL these steps, we will use the simplifications where we can. 




