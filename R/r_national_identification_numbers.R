#' Generate random fake national identification numbers.
#'
#' @param n number of observations.
#' @return A character vector of \code{n} randomly generated national identification numbers.
#' @examples
#' r_national_identification_numbers(10)
#' @export

#'n' should be greater than 0. Not the most efficient but works. Generates NID for US and Slovenia

r_national_identification_numbers <- function(n) {
  b <- r_generateUSID()
  for(i in 1:(n-1)){
   rd <- sample(1000,1)
   if(rd %% 2 == 0){
    resStr <- calculateSloveniaId()
   }else{
    resStr <- r_generateUSID()
   }
   b <- paste(b,resStr, sep=" ")
  }
  #print(class(b))
  res <- strsplit(b, " ")[[1]]
  return(res)
 }



r_generateUSID <- function(){
    return(paste(sample(100:999, size = 1, replace = TRUE),
               sample(10:99, size = 1, replace = TRUE),
               sample(1000:9999, size = 1, replace = TRUE), sep = "-"))
}

r_generateDDMMYYYRRBBB <- function() {
  return(paste(formatC(sample(1:28, size = 1, replace = TRUE), width = 2, format = "d", flag = "0"),
               formatC(sample(1:12, size = 1, replace = TRUE), width = 2, format = "d", flag = "0"),
               sample(100:999, size = 1, replace = TRUE),
               "50",
               formatC(sample(0:999, size = 1, replace = TRUE), width = 3, format = "d", flag = "0"),sep = ""))
}

calculateK <- function(numList){
    summ <- 7*(as.integer(numList[[1]][1]) + as.integer(numList[[1]][7])) + 6*(as.integer(numList[[1]][1]) + as.integer(numList[[1]][7])) +  5*(as.integer(numList[[1]][1]) + as.integer(numList[[1]][7])) +  4*(as.integer(numList[[1]][1]) + as.integer(numList[[1]][7])) +  3*(as.integer(numList[[1]][1]) + as.integer(numList[[1]][7])) +  2*(as.integer(numList[[1]][1]) + as.integer(numList[[1]][7])) 
    modd <- summ%%11
    subb <- 11 - modd
    if(subb == 10 || subb == 11){
        subb <- 0
    }
    return(subb)
}

calculateSloveniaId <- function(){
    ddmmyyyrrbbb <- r_generateDDMMYYYRRBBB()
    #print(ddmmyyyrrbbb)
    numberstring_split <- strsplit(ddmmyyyrrbbb, "")
    k <- calculateK(numberstring_split)
    res <- paste(ddmmyyyrrbbb,k,sep="")
    #print(res)
    return(res)
}