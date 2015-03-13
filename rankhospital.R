rankhospital <- function(state, outcome, num = "best") {
     ## Return hospital name in that state with the given rank 30-day death rate
     
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
     outcome <- paste ("hospital.30.day.death..mortality..rates.from", outcome)
     outcome <- gsub(" ", ".", outcome)
     if (! outcome %in% cabeceras) {
          stop ("invalid outcome")
     }
     
     ## Check that num is valid ("best", "worst" or numeric)
     if ((num != "best") && (num != "worst") && (class(num) != "numeric")){
          stop ("invalid num")
     }
     
     ## Find out the list of hospitals with lowest 30-day death rate
     ## 1) filter by state
     datos <- datos[datos["state"] == state,]     
     
     ## 2) convert to numeric (and get NA instead "Not Available")
     datos[outcome] <- as.numeric(datos[[outcome]])          
     
     ## 3) get the list ordered by the column 
     ordenado<-datos[order(datos[,outcome],  datos[,"hospital.name"]),]
     
     no_na <- !(is.na(ordenado[[outcome]]))
     
     ## if num = "worst" it is the last of the list no NA
     ## if num = "best" it is the first of the list
     if (num == "best"){
          num <- 1
     }
     if (num == "worst"){
          num <- sum(no_na)
     }
     
     ## return a NA if there is not enough hospitals in the list to reach num-th
     if (sum(no_na) < num){
          NA
     }
     else {
          ordenado[["hospital.name"]][num]
     }
     
     
}
