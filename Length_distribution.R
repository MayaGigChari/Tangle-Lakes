
library(tidyverse)

#this is a script with some functions that might be useful for length distribution analysis and length-weight stuff. 


#for all the length-weight relationship stuff, the data structure has to have a  Fork_length field and a Weight field/attribute

#length-weight relationships
pred_wl <- function(length, alpha, beta) {
  pred.weight <- alpha*length^beta
  return(pred.weight)
}


#for plotting  best fit of length-weight relationships
plot_wl <- function(dataz, alpha, beta) {
  # Determine range of lengths across which to predict weight
  pred.length <- seq(from = 200, to = max(as.numeric(dataz$Fork_length), na.rm = TRUE), length.out = 200)
  
  # Generate predictions for weight, given length range
  pred.weight <- pred_wl(length = pred.length, alpha = alpha, beta = beta)
  
  # Plot species observations with prediction line
  ggplot(dataz, aes(x = as.numeric(Fork_length), y = Weight)) +
    geom_point(color = "blue") +
    geom_line(data = data.frame(pred.length, pred.weight),
              aes(x = pred.length, y = pred.weight),
              color = "red", size = 1)+
    labs(
      x = "Fork Length (mm)",
      y = "Weight (mg)", 
      title = "Allometric weight-by-length model Shallow-Round"
    )
}

#for getting the linear model of allometric weight-length 

lm_lw<- function(dataz)
{
  wl.lin <- lm(log(Weight) ~ log(as.numeric(Fork_length)), data=dataz)
  return(wl.lin)
}



###for the length distribution stuff: 
#data needs to have a component called Year_capped in order for this function to work. 


#this is just a continuous distribution visualization. It does not bin into the 25 ml categories that seem to be the standard. 
plot_length_dist_continuous<- function(data, lakeSystem)
{
  full_distribution<- ggplot(data, aes(as.numeric(Fork_length))) +
    geom_histogram(aes(y=after_stat(density)),binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(color = "red", size = .5)+
    labs(
      x = "Fork Length",
      y = "Density",
      title = paste("Length Distribution 2023 and 2024", lakeSystem, sep= " ")
    )+
    theme_minimal()
  full_distribution_by_year<- ggplot(data, aes(x = as.numeric(Fork_length), fill = as.factor(Year_capped))) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 5, color = "black", alpha = 0.7, position = "identity") +
    geom_density(aes(color = as.factor(Year_capped)), size = 0.5, alpha = 0.2) +
    labs(
      x = "Fork Length",
      y = "Density",
      fill = "Year_capped",
      color = "Year_capped",
      title = paste("Length Distribution 2023 and 2024 ", lakeSystem, sep= " ")
    ) +
    theme_minimal()
  full_distribution_byyear_sep <- ggplot(data, aes(x = as.numeric(Fork_length), fill = as.factor(Year_capped))) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 5, color = "black", alpha = 0.7, position = "identity") +
    geom_density(aes(color = as.factor(Year_capped)), size = 0.5, alpha = 0.2) +
    facet_wrap(~ Year_capped, ncol = 1) +  # one panel per year, stacked vertically
    labs(
      x = "Fork Length",
      y = "Density",
      fill = "Year",
      color = "Year",
      title = paste("Length Distribution 2023 and 2024 ", lakeSystem, sep= " ")
    ) +
    theme_minimal()
  
  return(list(full_distribution = full_distribution, full_distribution_bothyears= full_distribution_by_year, full_distribution_years_sep = full_distribution_byyear_sep))
}

#now we will do some stuff for length binning

#this takes the whole dataframe and bins it. 
assign_bins<- function(data)
{
  min_length<- round(min(as.numeric(data$Fork_length)),0)-1
  print(min_length)
  max_length<- round(max(as.numeric(data$Fork_length)),0)
  breaks <- seq(from = min_length, to = max_length, by = 25)
  breaks <- c(breaks, Inf)  # Add final bin for > max_length
  
  # Create labels for bins
  labels <- paste0(
    breaks[-length(breaks)], 
    "-", 
    ifelse(is.finite(breaks[-1]), breaks[-1] - 1, "max")
  )
  print(breaks)

  data$bin <- cut(
    as.numeric(data$Fork_length),
    breaks = breaks,
    labels = labels,
    right = FALSE,
    include.lowest = TRUE
  )
  return(data)
}

binned_length_statistics <- function(binned_data, yearSort = FALSE, year = NULL, geartype = FALSE)
{
  bin_levels <- unique(binned_data$bin)
  #print(bin_levels)
  
  if (yearSort ) 
  {
    binned_data <- binned_data %>% filter(Year_capped == year)
  }
  if (geartype) {
    binned_stats <- binned_data %>%
      mutate(bin = factor(bin, levels = bin_levels)) %>%
      group_by(gear_type_filtered, bin) %>%
      summarize(count = n(), .groups = "drop") %>%
      complete(gear_type_filtered, bin = bin_levels, fill = list(count = 0)) %>%
      group_by(gear_type_filtered) %>%
      mutate(
        proportion = count / sum(count),
        variance = (proportion * (1 - proportion)) / (sum(count) - 1)
      ) 
  } 
  else 
  {
  binned_stats <- binned_data %>%
    mutate(bin = factor(bin, levels = bin_levels)) %>%
    group_by(bin) %>%
    summarize(count = n(), .groups = "drop") %>%
    complete(bin = bin_levels, fill = list(count = 0)) %>%
    mutate(
      proportion = count / sum(count),
      variance = (proportion * (1 - proportion)) / (sum(count) - 1)
    )
  }
  return(binned_stats)
}


#first before using this you have to do some statistics to get el variance (according to the formula in the operational plan)

plot_length_binnedWithError<- function(binned_data_stat)
{
  full_data <- ggplot(binned_data_stat, aes(x = bin, y = proportion)) +
    geom_col( fill = "skyblue", color = "black", alpha = 0.7) +
    geom_errorbar(aes(ymin = proportion - sqrt(variance),
                      ymax = proportion + sqrt(variance)),
                  width = 0.2, color = "red") +
    labs(
      x = "Length Bin (mm) ",
      y = "Proportion",
      title = "Length Distribution with Error"
    ) +
    theme_minimal()+
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      plot.margin = margin(10, 20, 20, 20)
    ) 
  
  return(full_data)
  
}


#I will test these all out in Round_shallow_tangle_lakes.R now. 