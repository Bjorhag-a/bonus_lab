#' @title Visualization of mean departure delay by airport.
#'
#' @description
#' This function will show a ggplot where the circles will increase in size if
#' the mean departure delay is bigger at a specific airport. 
#' 
#' @returns A plot of mean departure delay by airport where the size of the 
#' circle increases depending on the mean departure delay.
#' 
#' @export
#'
#' @examples
#' @import ggplot2
#' @import dplyr
#' @import magrittr
#' @import nycflights13
#' 
#' 
visualize_airport_delays <- function(){
  airports <- nycflights13::airports
  flights <- nycflights13::flights
  
  flights <- flights %>% 
    group_by(dest) %>%
    select(dep_delay, dest) %>%
    filter(!is.na(dep_delay)) %>% 
    summarise(
      dep_delay = mean(dep_delay)
    )
  
  new_data <- flights %>% 
    left_join(airports, by = c("dest" = "faa"))
  
  #There is 4 airports that have no coordinators (Longitude and Longitude)
  #So the plot cant plot them so here is a code that removes the "missing values"
  #If there is any problems with the warning in the function 
  
# new_data <- new_data %>% 
#    filter(!is.na(name))
  
  
  ggplot(new_data, aes(x = lon, y = lat)) +
    geom_point(aes(size = dep_delay)) +
    theme_bw() +
    labs(title = "Mean departure delay by airport",
         x = "Longitude",
         y = "Latitude",
         size = "Mean delay (min)") +
    theme(plot.title = element_text(hjust = 0.5))
  

  
}

