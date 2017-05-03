#
# R package "Erlang"
#
# Copyright (c) 2013-2017 Patrick Hubers
#
# This packages contains functions related to (contact center) staffing,
# using the Erlang C function
#

#' Calculate traffic intensity (a.k.a. workload)
#' 
#' @param arrival_rate Number of arrivals per interval
#' @param avg_handle_time Average handling time in seconds
#' @param interval_length Length of interval in minutes (default = 60)
#' @return Traffic intensity in Erlangs
#' @export
intensity <- function(arrival_rate, avg_handle_time, interval_length = 60) {
  return ((arrival_rate / (60*interval_length))* avg_handle_time)
}

#' Calculate agent occupancy
#' 
#' @param number_of_agents Number of available agents
#' @param intensity Traffic intensity in Erlangs
#' @return Occupancy
#' @export
occupancy <- function(number_of_agents, intensity) {
  return (intensity / number_of_agents)
}

#' Calculate the chance of a blocked call (Erlang B function)
#' 
#' @param number_of_servers Number of available trunks or agents
#' @param intensity Traffic intensity in Erlangs
#' @return Chance of blocking
#' @export
erlang_b <- function(number_of_servers, intensity) {
  inverse_b <- 1.0
  for(i in 1:number_of_servers) {
    inverse_b <- 1.0 + inverse_b * i / intensity
  }
  return(1.0 / inverse_b)
}

#' Calculate the chance of a queued call (Erlang C function)
#' 
#' @param number_of_agents Number of available agents
#' @param intensity Traffic intensity in Erlangs
#' @return Chance of queueing
#' @export
erlang_c <- function(number_of_agents, intensity) {
  ec <- number_of_agents * erlang_b(number_of_agents, intensity) / (number_of_agents - intensity * (1 - erlang_b(number_of_agents, intensity)))
  return(ec)
}

#' Calculate the average waiting time
#' 
#' @param number_of_agents Number of available agents
#' @param arrival_rate Number of arrivals per interval
#' @param avg_handle_time Average handling time in seconds
#' @param interval_length Length of an interval in minutes
#' @return Average waiting time in seconds
#' @export 
avg_wait_time <- function(number_of_agents, arrival_rate, avg_handle_time, interval_length = 60) {
  int <- intensity(arrival_rate, avg_handle_time, interval_length)
  awt <- (erlang_c(number_of_agents, int)*avg_handle_time)/(number_of_agents - int)
  return(awt)
}

#' Calculate the servicelevel
#' 
#' Calculates the percentage of calls that are answered within the acceptable waiting time
#' 
#' @param number_of_agents Number of available agents
#' @param arrival_rate Number of arrivals per interval
#' @param avg_handle_time Average handling time in seconds
#' @param interval_length Length of an interval in minutes
#' @param wait_time Acceptable waiting time
#' @return Service level (% of calls answered within acceptable waiting time)
#' @export 
service_level <- function(number_of_agents, arrival_rate, avg_handle_time, interval_length, wait_time) {
  a <- intensity(arrival_rate, avg_handle_time, interval_length)
  sl <- 1 - erlang_c(number_of_agents, a) * exp(-(number_of_agents - a)*(wait_time/avg_handle_time))
  return(sl)
}

#' Calculate the number of needed agents for SL goal
#' 
#' Calculates the number of agents that are needed to achieve a required service level. Currently only
#' calculates a whole (integer) number of agents.
#' 
#' @param arrival_rate Number of arrivals per interval
#' @param avg_handle_time Average handling time in seconds
#' @param interval_length Length of an interval in minutes
#' @param wait_time Acceptable waiting time (i.e. 20 seconds in a 80/20 SL goal)
#' @param service_level_goal Service level goal, the percentage of calls answered within the acceptable waiting time
#' @return Number of agents needed to achieve service level
#' @export
number_of_agents_for_sl <- function(arrival_rate, avg_handle_time, interval_length, wait_time, service_level_goal) {
  int <- intensity(arrival_rate, avg_handle_time, interval_length)
  agents <- ceiling(int)
  while(service_level(agents, arrival_rate, avg_handle_time, interval_length, wait_time) < service_level_goal) {
    agents <- agents + 1
  }
  return(agents)
}


#' Calculate the number of needed agents for achieve an ASA goal
#' 
#' Calculates the number of agents that are needed to achieve a required average speed of answer. Currently only
#' calculates a whole (integer) number of agents.
#' 
#' @param arrival_rate Number of arrivals per interval
#' @param avg_handle_time Average handling time in seconds
#' @param interval_length Length of an interval in minutes
#' @param wait_time Waiting time goal in seconds
#' @return Number of agents needed to achieve ASA
#' @export
number_of_agents_for_asa <- function(arrival_rate, avg_handle_time, interval_length, wait_time) {
  int <- intensity(arrival_rate, avg_handle_time, interval_length)
  agents <- ceiling(int)
  while(avg_wait_time(agents, arrival_rate, avg_handle_time, interval_length) > wait_time) {
    agents <- agents + 1
  }
  return(agents)
}
