library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# R read data
incarceration <- get_data()
incarceration_jail <- get_data2()
year_end_prison <- get_data3()

state_highest_cases <- incarceration %>% filter(total_pop == max(total_pop)) %>% pull(state)
total_change <- year_end_prison %>% mutate(year_diff = total_prison_pop_2019 - total_prison_pop_2021)
max_in_county <- incarceration %>% filter(year == max(year)) %>% group_by(state) %>% filter(total_pop_15to64 == max(total_pop_15to64)) %>%
  pull(county_name)
avg_change_year <- incarceration %>%
  group_by(state)%>%
  summarise(ave_year = sum(total_pop) / length(year))
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
# This function should return a data frame that is suitable for visualization
get_year_jail_pop <- function() {
  total <- incarceration_jail %>% 
    group_by(year) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(total) 
}
s <- get_year_jail_pop()

# This function should return the chart
plot_jail_pop_for_us <- function()  {
  p<- ggplot(s, aes(x = year, y = total_jail_pop)) +
    geom_bar(stat = "identity")+
    geom_col() +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)") +
    xlab("Year") +
    ylab("Total Jail Population") +
    scale_y_continuous(labels = scales::comma)
    labs(caption = "Figure 1. Increase of Jail Population in U.S. (1970-2018).")
  return(p)
} 
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#

#This data wrangling function should return a data frame that is suitable for visualization
get_year_jail_pop_states <- function(states) {
  growth_of_states <- incarceration %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarise(jail_pop = sum(total_pop,na.rm = TRUE))
  return(growth_of_states)
}

#This plotting function should return the chart. The parameter states should be a vector of states.
select_state <- c("CA", "WA", "WY")
plot_jail_pop_for_states <- function(states) {
  chart <- ggplot(get_year_jail_pop_states(states)) + 
    geom_line(mapping = aes(x = year, y = jail_pop, color = state)) + 
    ggtitle("Increase of Jail Population by states. (1970-2018)") +
    xlab("Year") +
    ylab("Jail Population by State") +
    scale_y_continuous(labels = scales::comma)
    labs(caption = "Figure 2. Increase of Jail Population in U.S by states. (1970-2018).")
  return(chart)
}

plot_jail_pop_for_states(select_state)
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
get_latinx_pop_by_states <- function() {
  latinx <- incarceration %>% 
    group_by(year, state) %>% 
    summarise(latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE))
  return(latinx)
}

latinx_pop <- get_latinx_pop_by_states()

plot_latinx_pop_by_states <- function() {
  chart_latinx <- ggplot(latinx_pop, aes(x = year, y = latinx_jail_pop)) +
    geom_line(aes(color = state)) +
    ggtitle("Latinx Population by States. (1970-2018)") +
    xlab("Year") +
    ylab("Latinx Jail Population")
    labs(caption = "Figure 3. Increase of Latinx Population in U.S by states. (1970-2018).")
  return(chart_latinx)
}

plot_latinx_pop_by_states()
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Get data function
get_jail_pop <- function() {
  jail_pop_1970 <- incarceration %>%
    filter(year == 1970) %>%
    select(state, total_jail_pop) %>%
    rename("Code" = state)
  states <- get_data4() %>%
    select(Code, State)
  jail_pop_1970 <- jail_pop_1970 %>%
    left_join(states) %>%
    rename("states" = State) %>%
    mutate(state = tolower(states))
  state_map <- map_data("state") %>%
    rename(state = region) %>%
    left_join(jail_pop_1970, by = "state")
  return(state_map)
}

# Plotting the map
plot_jail_pop_1970 <- function() { 
  ggplot(get_jail_pop()) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop), color = "white", size = .1) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Navy") +
    labs(title = "Jail Population of U.S. in 1970", fill = "Proportion") +
    labs(caption = "Figure 4. Distribution of Jail Population in U.S. in 1970.") +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )
}

plot_jail_pop_1970()

# Getting another data in 2016
get_jail_pop2 <- function() {
  jail_pop_2016 <- incarceration %>%
    filter(year == 2016) %>%
    select(state, total_jail_pop) %>%
    rename("Code" = state)
  states <- get_data4() %>%
    select(Code, State)
  jail_pop_2016 <- jail_pop_2016 %>%
    left_join(states) %>%
    rename("states" = State) %>%
    mutate(state = tolower(states))
  state_map <- map_data("state") %>%
    rename(state = region) %>%
    left_join(jail_pop_2016, by = "state")
  return(state_map)
}

# Plotting the map
plot_jail_pop_2016 <- function() { 
  ggplot(get_jail_pop2()) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop), color = "white", size = .1) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Red") +
    labs(title = "Jail Population of U.S. in 2016", fill = "Proportion") +
    labs(caption = "Figure 4. Distribution of Jail Population in U.S. in 1970.") +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )
}

plot_jail_pop_2016()
#----------------------------------------------------------------------------#

## Load data frame ---- 


