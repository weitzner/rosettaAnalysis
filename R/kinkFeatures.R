
# *************************************************
#             Kink Features Extraction
# *************************************************

query <- "SELECT 
  input_tag AS label, 
CASE
  WHEN (lat.tau101 BETWEEN 84.24 AND 117.64) AND (lat.alpha101 BETWEEN 3.62 AND 74.13) THEN 'KINKED'
  WHEN (lat.tau101 NOT BETWEEN 84.24 AND 117.64) AND (lat.alpha101 NOT BETWEEN 3.62 AND 74.13) THEN 'EXTENDED'
ELSE 'UNCLEAR'
  END AS base_geom
FROM
  loop_anchor_transforms AS lat, structures
WHERE
  lat.struct_id = structures.struct_id"

#' Generate a funnel plot for a rosetta simulation
#'
#' @param dbFile The full path to the SQLite database to query
#' @param label A label to apply to the entire set
#' @return A data.frame containing the results from the SQLite database
#' @examples
#' tau.alpha.data <- get.tau.alpha.from.db("someFile.db3", "antibodies")
#' @export
get.tau.alpha.from.db <- function(dbFile, label) {
  sqlite <- dbDriver("SQLite")
  tau_alpha_rows <- dbGetQuery(dbConnect(sqlite, dbFile), 
                               query)
  tau_alpha_rows$model.type <- label
  
  tau_alpha_rows$label <- sub(".pdb.gz$", "", tau_alpha_rows$label)
  tau_alpha_rows$label <- sub("/", "", tau_alpha_rows$label)
  return(tau_alpha_rows)
}