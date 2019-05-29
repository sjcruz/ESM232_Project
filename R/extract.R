#' Extracting Function
#'
#' Allows user to extract the last abundance values of focal prey and predator
#' @author Seleni Cruz and Juliette Verstaen
#' @return vector of last abundance values or focal prey and predator

extract_state_vars <- function(scenario){
  last_values <- scenario %>%
  filter(Time == max(Time)) %>%
  group_by(Organism) %>% select(-H) %>% 
  spread(Organism, Abundance)
  
return(c(last_values$X, last_values$P)) }