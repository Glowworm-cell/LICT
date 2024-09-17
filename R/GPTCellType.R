#' @import reticulate
#' @import dplyr
#' @export

GPTCellType = function(input,topgenenumber,species,tissuename){
  GPT_input_result = GPT_input(input = input,
                                     topgenenumber = topgenenumber,
                                     species = species,
                                     tissuename = tissuename)
  rows <- strsplit(GPT_input_result, "\n")[[1]]
  data <- sapply(rows, function(row) strsplit(row, ": ")[[1]])
  df1 <- data.frame(clusters = as.integer(gsub(">", "", data[1, ]))-1, cell_type = data[2, ])
  return(df1)
}
