
# Rankall function --------------------------------------------------------

library(dplyr)
library(stringr)

rankall <- function(Outcome, num = "best") {
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        names(data) <- tolower(names(data))
        
        ## Create check for correct argument
        listOutcome <- c('heart attack', 'heart failure', 'pneumonia')
        listStates <- unique(data$state)
        
        ## Check for invalid entries
        if(!(Outcome %in% listOutcome)) {
                return(stop('Invalid Outcome'))
        }
        
        ## select rate, hospital and state and filter missing values
        Outcome = str_replace(Outcome, pattern = ' ', replacement = '.')
        outcomeFieldName <- paste0('hospital.30.day.death..mortality..rates.from.', Outcome)
        
        data <- data %>% 
                select(hospital.name, state, rate = starts_with(outcomeFieldName)) %>% 
                filter(rate != "Not Available") %>% 
                mutate(rate = as.numeric(rate))
        
        listData <- split(data, data$state)
        
        if(num == 'best') {
                num <- 1
        }
        
        stateList <- lapply(listData, function(i) {
                if(num == 'best') {
                        num <- 1
                } else if(num == 'worst') {
                        num = nrow(i)
                }
                
                stateData <- arrange(i, rate, hospital.name) %>% 
                        select(hospital = hospital.name, state)
                return(c(stateData$hospital[num], stateData$state[1]))
        })
        
        state <- as.data.frame(do.call('rbind', stateList))
        names(state) <- c('hospital', 'state')
        row.names(state) <- state$state
        return(state)
        
}
