makeGsTestData <- function(id){
  
  err <- paste(
    "Your ID doesn't appear to have been specified correctly.\n",
    "It should be of the form a1234567."
  )
  id <- as.character(id)[[1]]
  id <- stringr::str_remove(id, "^a")
  if (stringr::str_length(id) != 7) stop(err)
  id <- as.integer(id)
  if (is.na(id)) stop(err)
  
  # Generate the data
  set.seed(id)
  nGenes <- sample.int(2e4, 1)
  inGS <- sample.int(floor(nGenes/3), 1)
  nDE <- sample.int(floor(nGenes/5), 1)
  pi <- runif(1, 0.1, 0.9)
  df <- tibble::tibble(
    inGeneSet = c(TRUE, FALSE),
    DE = round(c(pi * nDE, (1-pi)*nDE), 0),
    notDE = c(inGS, nGenes - inGS) - DE
  )
  df <- tidyr::pivot_longer(df, cols = c("DE", "notDE"), values_to = "Total", names_to = "DE")
  df <- dplyr::mutate(df, DE = DE == "DE")
  df
  
  assign("gsTestData", df, envir = .GlobalEnv)
  
}



