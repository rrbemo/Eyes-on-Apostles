library(tidyverse)
library(lubridate)
library(plotly)

#locations <- read_csv("data/CT_loci.csv") # old file
#events <- read_csv("data/PA_all_full.csv") # old file
locations <- read_csv("data/apis_camera_location.csv")
events <- read_csv("data/df1_combined_cleaned.csv")

#event_types <- unique(events$EventType)
#species <- unique(events$Final_Species)
#sites <- unique(events$Site)

head(locations)
head(events)
#print(species)
#print(event_types)

#### Aggregate by location ####
agg_by_location <- function(events_df)
{
  #### Process old data ####
  # If the data is the old format (Camelot), clean data the old way
  if ("Location" %in% colnames(events_df))
  {
    # Should we remove missing IDs?
    # What is an EventType? Does it have any significance? 
    agg_df <- events_df %>%
      drop_na(ID, Date) %>%
      mutate(Final_Species = ifelse(is.na(Final_Species), 
                                    "Not Specified",
                                    Final_Species),
             # Need to remove some extra letters so things match up. (H, G, F... specifically from Stockton Island)
             # This takes the 3rd and 4th index and joins it to all numeric values then sets it to lower case (CNSTE01 -> st01)
             Location = str_to_lower(paste0(str_sub(Location, 3, 4), 
                                            str_replace_all(Location, "[[:alpha:]]", ""))),
             EventDate = as.Date(Date, "%m/%d/%y"),
             EventYear = year(EventDate),
             EventMonth = month(EventDate),
             EventDayOfMonth = day(EventDate),
             EventDayOfYear = yday(EventDate),
             EventHour = hour(Time),
             EventMinute = minute(Time)) %>%
      group_by(Site, Location, EventDate, EventYear, EventMonth, EventDayOfMonth, EventDayOfYear, EventHour, EventMinute, Final_Species) %>%
      summarise(EventCount = n(), .groups = "drop")
    
    # Clean up some known misspellings
    agg_df <- agg_df %>%
      mutate(Location = ifelse(str_sub(Location, 3, 4) == "ML",
                               str_replace(Location, "ML", "MD"),
                               Location),
             Final_Species = ifelse(Final_Species == "Bear",
                                    "Black Bear", 
                                    Final_Species))
  
  # The above still needs to include all species
  }
  #### Process new data ####
  else ## Else do it the new way
  {
    agg_df <- events_df %>%
      mutate(Site = str_to_title(site_name),
             Final_Species = ifelse(is.na(species_common_name),
                                    "Not Specified",
                                    species_common_name),
             Location = trap_station_name,
             EventDateTime = ymd_hms(date_time, truncated = 3, tz = "America/Chicago"), # truncated parameter allows for missing times
             EventDate = as.Date(EventDateTime),
             EventYear = year(EventDate),
             EventMonth = month(EventDate),
             EventDayOfMonth = day(EventDate),
             EventDayOfYear = yday(EventDate),
             EventHour = hour(EventDateTime),
             EventMinute = minute(EventDateTime)) %>%
      group_by(Site, Location, EventDate, EventYear, EventMonth, EventDayOfMonth, EventDayOfYear, EventHour, EventMinute, Final_Species) %>%
      summarise(EventCount = n(), .groups = "drop")
    
    # Replace _ with " " and set all names to title case
    agg_df <- agg_df %>%
      mutate(Final_Species = str_to_title(str_replace_all(Final_Species, "_", " ")))
  }
}

loc_clean <- function(loc_df)
{
  clean_loc_df <- loc_df %>%
    # mutate(location = str_to_title(location),
    #        site = toupper(paste0("CN", str_replace_all(site, "[[:digit:]]", ""), str_pad(str_replace_all(site, "[[:alpha:]]", ""), 2, pad = "0")))) %>%
    mutate(location = str_to_title(location)) %>%
    unique()
  # TODO: CNRO05 needs the longitude changed. I've been assuming it should be -90.67479
}

build_species_list <- function(cleaned_events_df)
{
  species <- cleaned_events_df$Final_Species %>%
    unique() %>%
    str_sort()
}

build_species_plot_data <- function(loc_agg_df,
                                    plot_site,
                                    plot_species)
{
  # Get the locations we want to display no matter what (locs for site)
  site_locs <- unique(loc_agg_df$Location[loc_agg_df$Site == plot_site])
  
  # Create a dataframe to join later (force each location-month to be in the plot)
  keep_df <- data.frame(Location = rep(site_locs, each = 12),
                        EventMonth = rep(1:12, length(site_locs)))
  print(keep_df)
  plot_df <- loc_agg_df %>%
    filter(Site == plot_site,
           Final_Species == plot_species) %>%
    group_by(Location, EventMonth) %>%
    summarise(MonthCount = sum(EventCount), .groups = "drop") %>%
    complete(Location, EventMonth, fill = list(MonthCount = 0))
  
  plot_df <- plot_df %>%
    right_join(keep_df, by = c("Location", "EventMonth")) %>%
    mutate(MonthCount = ifelse(is.na(MonthCount),
                               0,
                               MonthCount)) %>%
    arrange(EventMonth)
  
  print(plot_df)
  # plot_df is returned which should contain every month for each location at the
  # given site.
}


my_data <- agg_by_location(events)
my_loc <- loc_clean(locations)
species_list <- build_species_list(my_data)
write_csv(my_data, paste0("data/", format(Sys.Date(), "%Y-%m-%d"), "_Events.csv"))
write_csv(my_loc, paste0("data/", format(Sys.Date(), "%Y-%m-%d"), "_Locations.csv"))
write_lines(species_list, paste0("data/", format(Sys.Date(), "%Y-%m-%d"), "_Species.txt"))


plot_site <- function(site_id, species_name) 
{
  
  plot_df <- build_species_plot_data(my_data,
                                     this_site,
                                     this_species)
  
  plot_ly(plot_df, x = ~EventMonth, y = ~MonthCount, color = ~Location,
          type = "scatter",
          mode = "lines+markers") %>%
    layout(title = sprintf("Monthly detections of %s at %s Site", this_species, this_site),
           xaxis = list(title = "Month"),
           yaxis = list(title = "Detections"))
}

# This will plot the site and species 3
#plot_site(my_data$Site[3], my_data$FinalSpecies[3])