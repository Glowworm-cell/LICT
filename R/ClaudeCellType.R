#' @import reticulate
#' @import dplyr
#' @export

ClaudeCellType = function(input,topgenenumber,species,tissuename){
  Claude_input_result = Claude_input(input = input,
                               topgenenumber = topgenenumber,
                               species = species,
                               tissuename = tissuename)
  start_position <- regexpr("1:", Claude_input_result)
  result <- substring(Claude_input_result, start_position)
  rows <- strsplit(Claude_input_result, "\n")[[1]]
  data <- sapply(rows, function(row) strsplit(row, ": ")[[1]])
  df1 <- data.frame(clusters = as.integer(gsub(">", "", data[1, ]))-1, cell_type = data[2, ])
  return(df1)
}
