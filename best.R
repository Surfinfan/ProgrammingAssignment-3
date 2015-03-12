best <- function(state, outcome) {

     ## Return hospital name in that state with lowest 30-day death rate
     
     ## Read outcome data
     datos <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Check that state is valid
     estados <- names(lapply(split(datos,datos$State), nrow))
     if (! state %in% estados) {
          stop ("invalid state")
     }
     
     ## Put everything in lowercase to avoid problems with outcome Caps
     cabeceras <- names(datos)
     cabeceras <- tolower(cabeceras)
     names(datos) <- cabeceras
     outcome <- tolower(outcome)
     
     ## Check that outcome is valid
     outcome = paste ("hospital.30.day.death..mortality..rates.from", outcome)
     outcome = gsub(" ", ".", outcome)
     if (! outcome %in% cabeceras) {
          stop ("invalid outcome")
     }
     
     ## Find out the hospital with lowest 30-day death rate
     ## 1) filter by state
     datos = datos[datos["state"] == state,]     
     
     ## 2) convert to numeric (and get NA instead "Not Available")
     datos[outcome] <- as.numeric(datos[[outcome]])          
     
     ## 3) get the list ordered by the column 
     ordenado<-datos[order(datos[,outcome],  datos[,"hospital.name"]),]

     ordenado[["hospital.name"]][1]
     

     #f1 = factor("Not Available")
     #buenos = !(datos[[outcome]] == f1)
     #datosbuenos = datos[[outcome]][buenos]
     
}
