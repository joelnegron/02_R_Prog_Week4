
# Best Function -----------------------------------------------------------

## Arguements:
## state:       2-character state sbbreviation (string)
## outcome:     medical condition (string)

best <- function(State, Outcome) {
        
        library(dplyr)
        library(stringr)
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        names(data) <- tolower(names(data))
        
        ## Create check for correct arguements
        listOutcome <- c('heart attack', 'heart failure', 'pneumonia')
        listStates <- unique(data$state)
        
        if(!(Outcome %in% listOutcome)) {
                stop('Invalid Outcome')
        }
        if(!(State %in% listStates)) {
                stop("Invalid State")
        }
        
        Outcome = str_replace(Outcome, pattern = ' ', replacement = '.')
        outcomeFieldName <- paste0('hospital.30.day.death..mortality..rates.from.', Outcome)
        
        output <- data %>% 
                select(state, hospital.name, mortality = starts_with(outcomeFieldName)) %>% 
                filter(state == State, mortality != "Not Available") %>% 
                mutate(mortality = as.numeric(mortality)) %>% 
                arrange(mortality, hospital.name)
        
        return(output$hospital.name[1])
        
}

best('TX', "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")
