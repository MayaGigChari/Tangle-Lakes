#I want to look at the lakes and all the data. 


#lake maps and visualizations

install.packages("sf")
library(readxl)
library(sf)
library(dplyr)
library(tidyr)
library(plotly)
library(tidyverse)



tangle_lakes<-read_sf("GIS/Tanana_lakes.shp")
tangle_streams<- read_sf("GIS/Tanana_streams.shp")
plot(tangle_lakes$geometry)
plot(tangle_streams[1])
#streams aren't that useful. 


coords_lakes = st_crs(tangle_lakes)

tangle_lakes = st_transform(tangle_lakes, "WGS84")

#can crop the image to the spatial extent of the data once we have the data. 

#we will take the data from our growth correction data probably. 

geog_data<- read.csv("Total_data_corrected_lengths.csv")

#need to filter this so the m2's aren't totall over-represented 

#for some reason, we are getting THREE copies of every recap. 
#this is because we have one extra copy of the recap event for labeling purposes. either can be picked for this. 
geog_data %>%
  filter(Floy == 18603 )


#just the fish that are properly tagged? 
#add NR's to make the geog data type happy

#why would I need unique? Why would there be duplicates in this dataset? 

geog_data$Fish_notes
df_spatial<- geog_data%>%
  #filter(grouping %in% c("n1", "n2"))%>%
  filter(stat == grouping)%>%
  filter(cap_label %in% c("n1", "n2", "m2"))%>%
  dplyr::select(Lake, Lat, Lon, Date, Floy, cap_label,year_cap, year_recap, PIT, grouping,stat, Fish_notes, corrected_length)%>%
  mutate(Year_capped = year(Date))%>%
  drop_na(Lat, Lon)


#2 fish didn't have lat or long data or a date. 

sf_df<- st_as_sf(df_spatial,coords = c("Lon", "Lat"), crs= "WGS84")

colnames(raw_df)

fish_extent <- st_bbox(sf_df)

fish_extent<- st_bbox(c(xmin = fish_extent[[1]]-0.005, xmax = fish_extent[[3]]+0.05, ymin = fish_extent[[2]]-0.005, ymax = fish_extent[[4]]+0.005), crs = st_crs(sf_df))

sf_df <- sf_df %>%
  mutate(CaptureEvent = interaction(Year_capped, Lake, sep = " - "))

sf_df%>%
  filter(Floy == 18603 )
  

#create a function for nicely plotting data

niceplot<- function(data, extent)
{
  # Add lat/long to the data
  sf_df_Data <-data %>%
    mutate(
      lon = st_coordinates(.)[, 1],
      lat = st_coordinates(.)[, 2]
    )
  
  # ggplot so that we can hover. 
  whole_lake_plot <- ggplot() +
    geom_sf(data = st_crop(tangle_lakes, extent)) +
    geom_point(data = sf_df_Data, aes(x = lon, y = lat, color = CaptureEvent,
                                 text = paste0("Lat: ", round(lat, 5), 
                                               "<br>Lon: ", round(lon, 5),
                                               "<br>Event: ", CaptureEvent)),
               size = 0.8) +
    scale_color_discrete() +
    theme_bw() +
    labs(color = "Capture Event") +
    theme(
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14)
    )
  return(whole_lake_plot)
  
}

# Convert to interactive plot

all_lakes<- niceplot(sf_df, fish_extent)

#for some reason this doesn't work anymore. 
interactive_plot_alllakes <- ggplotly(all_lakes, tooltip = "text")
#now when we hover over points we can see what the exact lat lon is. 

ggsave("whole_system_mark_recap_2022_2023.png", plot = whole_lake_plot, dpi = 300, width = 10, height = 15)  # Adjust width and height

#now I want to separate into multiple lake files. 

#use the interactive component to find the lake boundaries. 

crop_box_lower <- st_bbox(c(xmin = fish_extent[[1]], xmax = fish_extent[[3]], ymin = 63.10, ymax = fish_extent[[4]]), crs = st_crs(sf_df))
lower_lake <- st_crop(sf_df, crop_box_lower)
lower_lake_plot<- niceplot(lower_lake, crop_box_lower)


crop_box_shallowround <- st_bbox(c(xmin = fish_extent[[1]], xmax = fish_extent[[3]], ymin = 63.0494, ymax = 63.091),crs = st_crs(sf_df))
shallowround_lake<- st_crop(sf_df, crop_box_shallowround)                            
shallowround_lake_plot<- niceplot(shallowround_lake, crop_box_shallowround )


crop_box_upper<-st_bbox(c(xmin = fish_extent[[1]], xmax = fish_extent[[3]], ymin = fish_extent[[2]], ymax = 63.048),crs = st_crs(sf_df))
upper_lake<- st_crop(sf_df, crop_box_upper)                            
upper_lake_plot<- niceplot(upper_lake, crop_box_upper)


#now we have some good geometries for the individual lakes. Now for each individual lake we need to do some area separation. 

#I believe the latitude/longitude information may not have carried over from the very original data. 
#let's check

sf_df %>%
  filter(Floy == 18603 )
#let's take advantage of the fact that we have a good mark-recap dataset. 
#let's make it easier and just filter for recapped floys. 

recapped_floys<- geog_data%>%
  filter(grouping == "m2")%>%
  select(Floy)

sf_df_matters<- sf_df%>%
  filter(grouping == "m2")

#for some reason, only the m2s have the 2024 label for year capped. duh! because they're m2's

points_2023 <- data.frame(sf_df %>% filter(Year_capped == 2023)%>%drop_na(Floy))
points_2024 <- data.frame(sf_df %>% filter(Year_capped == 2024)%>%drop_na(Floy))



#need to convert out of the sf format to do the join, then convert back into the sf format. 
matched_points <- inner_join(points_2023, points_2024, join_by("Floy"), suffix = c("_2023", "_2024"))

#here the issue is: many of these guys are captured more than twice. 

#here we create line segments 
lines_sf <- matched_points %>%
  rowwise() %>%
  mutate(geometry = st_sfc(st_cast(st_union(geometry_2023, geometry_2024), "LINESTRING"), crs = st_crs(sf_df))) %>%
  select(-(c(geometry_2023, geometry_2024)))%>%
  st_as_sf()

plot(lines_sf$geometry)

# Step 4: Plot
ggplot() +
  geom_sf(data = st_crop(tangle_lakes, fish_extent)) +
  geom_sf(data = lines_sf, color = "black", size = 1, alpha = 0.6) +
  scale_color_discrete(name = "Capture Event") +
  theme_bw()


upper_lake_plot<- niceplot(upper_lake, crop_box_upper)+geom_sf(lines_sf)

lines_plot<- function(lines_data, crop_box, original_geom)
{
  # ggplot so that we can hover. 
  whole_lake_plot <- ggplot() +
    geom_sf(data = st_crop(original_geom, crop_box)) +
    geom_sf(data = st_crop(lines_data, crop_box))+
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14)
    )
  return(whole_lake_plot)
}
lower_lakes_movement<-lines_plot(lines_sf, crop_box_lower, tangle_lakes)
ggsave("lower_lake_movement.png", plot = lower_lakes_movement,
       width = 10, height = 5, units = "in", dpi = 300)

shallow_round_movement<- lines_plot(lines_sf, crop_box_shallowround, tangle_lakes)
ggsave("shallowround_lake_movement.png", plot = shallow_round_movement,
       width = 10, height = 5, units = "in", dpi = 300)

upper_movement<- lines_plot(lines_sf, crop_box_upper, tangle_lakes)
ggsave("upper_lake_movement.png", plot = 
         upper_movement,
       width = 10, height = 5, units = "in", dpi = 300)

#given these maps we can now determine good cutoffs for our capture/recapture regions for each lake. 
#let's have three regions per lake. 


#Lower lake

lower_lake_region_1<-  st_bbox(c(xmin = crop_box_lower[[1]], xmax = crop_box_lower[[3]], ymin = 63.10, ymax = 63.12), crs = st_crs(sf_df))
lower_lake_region_2<- st_bbox(c(xmin = crop_box_lower[[1]], xmax = crop_box_lower[[3]], ymin = 63.12, ymax = 63.14), crs = st_crs(sf_df))
lower_lake_region_3<- st_bbox(c(xmin = crop_box_lower[[1]], xmax = crop_box_lower[[3]], ymin = 63.14, ymax =crop_box_lower[[4]]), crs = st_crs(sf_df))

#Upper lake 


upper_lake_region_1<-  st_bbox(c(xmin = crop_box_upper[[1]], xmax = crop_box_upper[[3]], ymin = 63.015, ymax = 63.0255), crs = st_crs(sf_df))
upper_lake_region_2<- st_bbox(c(xmin = crop_box_upper[[1]], xmax = crop_box_upper[[3]], ymin = 63.0255, ymax = 63.035), crs = st_crs(sf_df))
upper_lake_region_3<- st_bbox(c(xmin = crop_box_upper[[1]], xmax = crop_box_upper[[3]], ymin = 63.035, ymax =crop_box_upper[[4]]), crs = st_crs(sf_df))

#Round/Shallow lake: these lakes do not appear to be mixed at all, but perhaps another test will tell us differently. 
#split into 4 regions because this is 2 lakes? maybe. try this for now. 

shallowround_lake_region_1<-  st_bbox(c(xmin = crop_box_shallowround[[1]], xmax = crop_box_shallowround[[3]], ymin = 63.05, ymax = 63.062), crs = st_crs(sf_df))
shallowround_lake_region_2<- st_bbox(c(xmin = crop_box_shallowround[[1]], xmax = crop_box_shallowround[[3]], ymin = 63.062, ymax = 63.072), crs = st_crs(sf_df))
shallowround_lake_region_3<- st_bbox(c(xmin = crop_box_shallowround[[1]], xmax = crop_box_shallowround[[3]], ymin = 63.072, ymax =crop_box_shallowround[[4]]), crs = st_crs(sf_df))


#this might return to a more original form of the dataframe, where caps and recaps are individual tuples. actually, this dataframe has that. 
#first, filter the geographic dataset for only the recapped floys. It is important that we have 2 tuples for each floy. 
#these bounding boxes are what we want to use when we source this file in the child lake RMD files. 


#plotting out the strata 

niceplot_strata <- function(extent, strata_lat_lines_1, strata_lat_lines_2, strata_lat_lines_3) {
  # Add lat/long to the data
  # Base map with lake and points
  whole_lake_plot <- ggplot() +
    geom_sf(data = st_crop(tangle_lakes, extent), fill = "lightblue", alpha = 0.3)+
    # Add horizontal lines for strata boundaries
    geom_hline(yintercept = strata_lat_lines_1, linetype = "dashed", color = "blue", alpha = 1) +
    geom_hline(yintercept = strata_lat_lines_2, linetype = "dashed", color = "orange",alpha = 1) +
    geom_hline(yintercept = strata_lat_lines_3, linetype = "dashed", color = "purple",alpha = 1) +
    
    scale_color_discrete() +
    theme_bw() +
    labs(color = "Capture Event") +
    theme(
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14)
    )
  
  return(whole_lake_plot)
}


strata_lat_lines_upper <- c( 63.017, 63.0255, 63.035, 63.046)

strat_lat_lines_shallowround <- c(63.05, 63.062, 63.072, 63.081)

strat_lines_lower<- c(63.10, 63.12, 63.14,63.15)

# Call the function
strata_plot<-niceplot_strata(extent = fish_extent, strata_lat_lines_1 = strata_lat_lines_upper, strata_lat_lines_2 = strat_lat_lines_shallowround, strata_lat_lines_3 = strat_lines_lower)
ggsave("strata_plot.png", strata_plot, dpi = 300)
