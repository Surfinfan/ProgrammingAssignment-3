rankall <- function(outcome, num = "best") {
     ## For each state, find the hospital of the given rank
     ## Return a data frame with the hospital names and the
     ## (abbreviated) state name
     
     ## Read outcome data
     datos <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Put everything in lowercase to avoid problems with outcome Caps
     cabeceras <- names(datos)
     cabeceras <- tolower(cabeceras)
     names(datos) <- cabeceras
     outcome <- tolower(outcome)
     
     ## Check that outcome is valid
     outcome <- paste ("hospital.30.day.death..mortality..rates.from", outcome)
     outcome <- gsub(" ", ".", outcome)
     if (! outcome %in% cabeceras) {
          stop ("invalid outcome")
     }     
     
     ## Check that num is valid ("best", "worst" or numeric)
     if ((num != "best") && (num != "worst") && (class(num) != "numeric")){
          stop ("invalid num")
     }
     
     if (num == "best"){
          num <- 1
     }
     
     ## convert to numeric (and get NA instead "Not Available")
     ## And get dataframe ordered by state, outcome, and hospital name
     datos[outcome] <- as.numeric(datos[[outcome]])       
     ordenado<-datos[order( datos["state"], datos[,outcome], 
                            datos[,"hospital.name"]),]
     
     ## get the split between states, in order to enter a for loop for 
     ## each state, looking for the num-th hospital name in the state
     porestados <- split(ordenado, ordenado["state"])
     
     listafinal <- data.frame(hospital=vector(), state= vector())
     
     for (i in 1:length(porestados)){
          estado <- porestados[[i]]
          no_na <- !is.na(estado[outcome])
          hospitales <- estado[["hospital.name"]][no_na]
          ## if we need the worst, it has to be calculated in each state
          if (num == "worst"){
               numhospital <- sum(no_na)
          }
          else
          {
               numhospital <- num
          }
          # Builds a new line of the dateframe and attach it to the final list
          df <- data.frame(hospital=c(hospitales[numhospital]), 
               state = c(estado[["state"]][1]))
          row.names(df) <- estado[["state"]][1]
          listafinal <- rbind(listafinal, df)
     }

     listafinal     
}
