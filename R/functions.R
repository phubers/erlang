#
# R package "Erlang"
#
# Copyright (c) 2013 Patrick Hubers
#
# This packages contains functions related to (contact center) staffing,
# using the Erlang C function
#

#' Calculate traffic intensity
#' 
#' @param arrival_rate: Number of arrivals per interval
#' @param avg_handle_time: Average handling time in seconds
#' @param interval_length: Length of interval in minutes (default = 60)
#' @return Traffic intensity in Erlangs
#' @export
intensity <- function(arrival_rate, avg_handle_time, interval_length = 60) {
  return ((arrival_rate / (60*interval_length))* avg_handle_time)
}

#' Calculate agent occupancy
#' 
#' @param number_of_agents: Number of available agents
#' @param intensity: Traffic intensity in Erlangs
#' @return Occupancy
#' @export
occupancy <- function(number_of_agents, intensity) {
  return (intensity / number_of_agents)
}

pow_fact <- function(a, b) {
  return (b^a/factorial(a))
}

#' Calculate the chance of a blocked call (Erlang C function)
#' 
#' @param number_of_agents: Number of available agents
#' @param intensity: Traffic intensity in Erlangs
#' @return Chance of blocking
#' @export
erlang_c <- function(number_of_agents, intensity) {
  term <- pow_fact(number_of_agents, intensity)
  sum <- 0
  for (k in 0:(number_of_agents-1)) {
    sum <- sum + pow_fact(k, intensity)
  }
  ec <- term / (term + (1-occupancy(number_of_agents,intensity))*sum)
  return(ec)
}

#' Calculate the average waiting time
#' 
#' @param number_of_agents: Number of available agents
#' @param arrival_rate: Number of arrivals per interval
#' @param avg_handling_time: Average handling time in seconds
#' @param interval_lengt: Length of an interval in minutes
#' @return Average waiting time in seconds
#' @export 
avg_wait_time <- function(number_of_agents, arrival_rate, avg_handle_time, interval_length) {
  int <- intensity(arrival_rate, avg_handle_time, interval_length)
  awt <- (erlang_c(number_of_agents, int)*avg_handle_time)/(number_of_agents - int)
  return(awt)
}

#' Calculate the servicelevel
#' 
#' Calculates the percentage of calls that are answered within the acceptable waiting time
#' 
#' @param number_of_agents: Number of available agents
#' @param arrival_rate: Number of arrivals per interval
#' @param avg_handling_time: Average handling time in seconds
#' @param interval_length: Length of an interval in minutes
#' @param wait_time: Acceptable waiting time
#' @return Service level (% of calls answered within acceptable waiting time)
#' @export 
service_level <- function(number_of_agents, arrival_rate, avg_handle_time, interval_length, wait_time) {
  a <- intensity(arrival_rate, avg_handle_time, interval_length)
  sl <- 1 - erlang_c(number_of_agents, a) * exp(-(number_of_agents - a)*(wait_time/avg_handle_time))
  return(sl)
}

#' Calculate the number of needed agents 
#' 
#' Calculates the number of agents that are needed to achieve a required service level. Currently only
#' calculates a whole (integer) number of agents.
#' 
#' @param arrival_rate: Number of arrivals per interval
#' @param avg_handling_time: Average handling time in seconds
#' @param interval_length: Length of an interval in minutes
#' @param wait_time: Acceptable waiting time (i.e. 20 seconds in a 80/20 SL goal)
#' @param service_level_goal: Service level goal, the percentage of calls answered within the acceptable waiting time
#' @return Number of agents needed to achieve service level
#' @export
number_of_agents <- function(arrival_rate, avg_handling_time, interval_length, wait_time, service_level_goal) {
  int <- intensity(arrival_rate, avg_handling_time, interval_length)
  agents <- int
  while(service_level(agents,arrival_rate, avg_handling_time, wait_time) < service_level_goal) {
    agents <- agents + 1
  }
  return(agents)
}