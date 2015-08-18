
# *************************************************
#          Score File Loading Functions
# *************************************************

#' Load in data stored in a JSON-formatted score file and
#' optionally filter out the highest-scoring models
#'
#' @param fname Path to the score file
#' @param cutoff Percentile of scores to filter 
#' @return A data.frame representing the score file data
#' @examples
#' score.data <- getScoreDataFromFile("1abc.json", cutoff = 0.95)
#' @export
getScoreDataFromFile <- function(fname, cutoff=0.95) {
  pdb.score <- as.data.frame(do.call("rbind", lapply(fromJSON(file=fname),
                                                     unlist)),
                             stringsAsFactors=FALSE)
  
  # convert columns to appropriate data types
  pdb.score[] <- lapply(pdb.score, type.convert, as.is = TRUE)
  
  return(subset(pdb.score, pdb.score[, raw.score.col] <= 
                  quantile(pdb.score[, raw.score.col], cutoff)))
}


#' Load in the data stored in JSON-formatted score files within a single 
#' directory and collect the data in a single data.frame.
#'
#' @param directory Path to directory containing score files
#' @param label A label to apply to the entire set
#' @param pattern A regular expression to describe the file naming pattern 
#' within the directory
#' @return A data.frame containing all of the score file data
#' @examples
#' score.data <- getScoreDataFromFilesInDir("score_files", 
#'                                          label = "new_score_term_test",
#'                                          pattern = "^[[:alnum:]]{4}.json$")
#' @export
getScoreDataFromFilesInDir <- function(directory, label, 
                                       pattern = "^[[:alnum:]]{4}.json$") {
  
  # '1abc.json' contains weighted scores (including constraint scores) and RMSD
  # add scorefile data to data.frame
  list.of.files <- list.files(path=directory, pattern = pattern)
  score.file.data <- data.frame()
  
  for (score.file in list.of.files) {
    current.file <- paste(directory, score.file, sep="/")
    current_data <- getScoreDataFromFile(current.file, 1.0)
    score.file.data <- rbind(score.file.data, current_data)
  }
  
  # indicate that all of these points are from the same set
  score.file.data$model.type <- label
  return(score.file.data)
}