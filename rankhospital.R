
# Rankhospital function ---------------------------------------------------


library(dplyr)
library(stringr)
        
rankhospital <- function(State, Outcome, num = "best") {
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        names(data) <- tolower(names(data))
        
        ## Create check for correct arguements
        listOutcome <- c('heart attack', 'heart failure', 'pneumonia')
        listStates <- unique(data$state)
        
        ## Check for invalid entries
        if(!(Outcome %in% listOutcome)) {
                return(stop('Invalid Outcome'))
        }
        if(!(State %in% listStates)) {
                return(stop("Invalid State"))
        }
        
        ## Identify best hospital
        Outcome = str_replace(Outcome, pattern = ' ', replacement = '.')
        outcomeFieldName <- paste0('hospital.30.day.death..mortality..rates.from.', Outcome)
        
        output <- data %>% 
                select(state, hospital.name, rate = starts_with(outcomeFieldName)) %>% 
                filter(state == State, rate != "Not Available") %>% 
                mutate(rate = as.numeric(rate)) %>% 
                arrange(rate, hospital.name)
        
        if(num == 'best') {
                num <- 1
        } else if(num == 'worst') {
                num = nrow(output)
        }
        
        return(output$hospital.name[num])
        
}
