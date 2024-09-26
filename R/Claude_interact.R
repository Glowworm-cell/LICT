#' @import reticulate
#' @import dplyr
#' @import stringr
#' @export

Claude_interact = function(positive_gene = NULL,negative_gene = NULL){
  user_input <- Claude_generate_gene_text(positive_gene = positive_gene, negative_gene = negative_gene)
  result = py$converse_with_claude(user_input)
  print(result)
  start_position <- regexpr("1:", result)
  result <- substring(result, start_position)
  rows <- strsplit(result, "\n")[[1]]
  data <- sapply(rows, function(row) strsplit(row, ": ")[[1]])
  df1 <- data.frame(clusters = as.integer(gsub(">", "", data[1, ]))-1, cell_type = data[2, ])
  return(df1)
}
