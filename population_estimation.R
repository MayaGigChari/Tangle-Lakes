#this is a script for universal functions for the mark/recap population estimation 
#TODO: 

#ask: is the test for complete mixing column s+1 supposed to represent the number of fish tagged in that strat that were not recapped at all, or the number of fish tagged in a strata that were not recapped in that strata? 
#TODO: REALLY ASK THIS ^

###GLOBAL VARIABLES: 

petersen_funcs<- list(NPetersen, sePetersen, vPetersen)
Chapman_funcs<- list(NChapman, seChapman, vChapman)
Bailey_funcs<- list(NBailey, seBailey, vBailey)


#KS test
#this must be performed with the data that has duplicates, so n2 and m2 are properly counted. 
ks_test<- function(data, events, lakename, length_variable)
{
  ks_results <- data.frame(
    lake = character(),
    ks_test = character(),
    statistic = numeric(),
    p_value = numeric(),
    reject_null = logical(),
    stringsAsFactors = FALSE
  )
  
  #n2 must also include m2, that is why there is replication in the dataset. 
  for (event in events) {
    # Filter data for the current event
    event_data <- data %>%
      filter(grouping == event) %>%
      pull(length_variable) %>%
      as.numeric()
    #create a temporary dataframe 
    assign(paste0("data_", event), event_data)
  }
  
  
  #print(data_n1)
  #print(data_n2)
  #print(data_m2)
  ks_1 <- ks.test(data_n1, data_m2)
  ks_2 <- ks.test(data_n2, data_m2)
  ks_3 <- ks.test(data_n1, data_n2)
  
  ks_results <- ks_results %>%
    add_row(lake = lakename, 
            ks_test = "M vs R", 
            statistic = ks_1$statistic, 
            p_value = ks_1$p.value,
            reject_null= ks_1$p.value<0.05) %>%
    add_row(lake = lakename, 
            ks_test = "C vs R", 
            statistic = ks_2$statistic, 
            p_value = ks_2$p.value,
            reject_null= ks_2$p.value<0.05) %>%
    add_row(lake = lakename, 
            ks_test = "M vs C", 
            statistic = ks_3$statistic, 
            p_value = ks_3$p.value,
            reject_null= ks_3$p.value<0.05)
  
  return(ks_results)
  
}


#


#Test 1: Test for complete mixing. 
#not recaptured are those that were marked in n1 and not recaptured in n1? 

#this can't have the m2 redundancy. 

#this does assume that every fish is marked with a floy. But that's not always the case, so I will base this on event num labels instead. 
#is the not recapped column not recapped in that region, or not recpaped in general 
#should be fixed now. 

#if there is no floy/pit tag and no notes but the fish is still labeled as n1, we will remove it from the dataset for this test. 



petersen_matrix_tyerscode<- function(dataframe)
{
  all_regions <- unique(dataframe$region)
  
  chi2 <- dataframe %>%
    group_by(Floy) %>%
    reframe(
      area_marked = region[Year_capped == 2023][1],
      area_recaptured = region[Year_capped == 2024][1]
    ) %>%
    count(area_marked, area_recaptured) %>%
    complete(area_marked = all_regions, area_recaptured = all_regions, fill = list(n = 0)) %>%
    pivot_wider(names_from = area_recaptured, values_from = n, values_fill = 0) %>%
    drop_na(area_marked)%>%#it is not helpful to us if we have no idea where the fish was caught originally. 
    as.data.frame()
  
  chi2$"NA"<-NULL
  
  rownames(chi2)<- chi2$area_marked
  chi2$area_marked<-NULL
  return(chi2)
  
}
chi2_markrecap<- function(dataframe)
{
  all_regions <- unique(dataframe$region)
  
  # Build chi2 matrix with all region combinations, even missing ones
  chi2 <- dataframe %>%
    group_by(Floy) %>%
    reframe(
      area_marked = region[Year_capped == 2023][1],
      area_recaptured = region[Year_capped == 2024][1]
    ) %>%
    count(area_marked, area_recaptured) %>%
    complete(area_marked = all_regions, area_recaptured = all_regions, fill = list(n = 0)) %>%
    pivot_wider(names_from = area_recaptured, values_from = n, values_fill = 0) %>%
    drop_na(area_marked)%>%#it is not helpful to us if we have no idea where the fish was caught originally. 
    as.data.frame()
  
  chi2$"NA"<-NULL

  rownames(chi2)<- chi2$area_marked
  chi2$area_marked<-NULL
  
  counts <- dataframe %>% group_by(region) %>% 
    summarize(n1 = sum(stat == "n1"))
  

  not_recapped<- counts$n1- rowSums(chi2)
  chi2$not_recapped<- not_recapped
  
  
  Mixtable_matrix <- as.data.frame(chi2)

  
  #totals <- colSums(Mixtable_matrix, na.rm = TRUE)
  
  # Add "Total" row to matrix
  #Mixtable_matrix <- rbind(Mixtable_matrix, Total = totals)
  
  
  return(Mixtable_matrix)
  
}

#Test 2/3: equal probabiltiy of capture and such 
#both built into one function. 
petersen_consistency<- function(data, strat)#strat is region
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
    summarize(m2 = sum(stat == "m2"),
              n2_nomark = sum(stat == "n1")-m2) %>% #this is not n2-m2 because it is already formulated that way. 
    pivot_longer(cols = c(m2, n2_nomark), names_to = "variable", values_to = "value")%>%
    pivot_wider(names_from = {{strat}}, values_from = value) %>%
    as.data.frame()
  
  print(complete_mix)
  rownames(complete_mix) <- complete_mix$variable
  complete_mix$variable <- NULL
  
  # Conduct the chi-square test
  chisq_result_complete_mixing <- chisq.test(complete_mix)
  
  
  #test for equal probability of capture during Event 2 slash equal proportions test?
  #fix this. 
  equal_prop<- data%>%
    group_by({{strat}})%>%
    summarize(m2 = sum(stat == "m2"),
              n1_norecap = sum(stat == "n1")-sum(stat == "m2"))%>%
    pivot_longer(cols = c(m2, n1_norecap), names_to = "variable", values_to = "value") %>%
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
  
  return(list(consistency_p_values = pet_cons_res, complete_mixing_table= complete_mix,equal_prop_table = equal_prop ))
  
  
}


#Population calculation 


pop_calc<-function(funcs, data)
{
  pop_results <- data.frame(
    pop = numeric(),
    se = numeric(),
    var = numeric(),
    norm_95_low = numeric(),
    norm_95_high = numeric(),
    stringsAsFactors = FALSE
  )
  
  #this is a silly and hacky way to do this, but it can be done a number of ways. 
  cap_summary_petersen<- data%>%
    group_by(stat)%>%
    count()
  
  #this corrects for the fact that we need to add the m2's to the n2's to get a total count of n2's. 
  #print(cap_summary_petersen)
  cap_temp<- cap_summary_petersen
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
    add_row(pop = pop_temp,
            se = se_temp,
            var = var_temp,
            norm_95_low = norm_95_low,
            norm_95_high = norm_95_high)
  
  return(pop_results)
  
}
