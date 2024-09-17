#' @import reticulate
#' @import dplyr
#' @export

Gemini_interact = function(positive_gene = NULL,negative_gene = NULL){
  reticulate::py_run_string("
def chat_response(user_input):
    response = chat.send_message(user_input)
    markdown=to_markdown(response.text)
    plain_text = markdown.data
    return plain_text
")

    # 如果任一不为空，执行以下代码positive_gene is expressed in the
  user_input <- Gemini_generate_gene_text(positive_gene = positive_gene, negative_gene = negative_gene)
  result = py$chat_response(user_input)
  print(result)
  rows <- strsplit(result, "\n")[[1]]
  data <- sapply(rows, function(row) strsplit(row, ": ")[[1]])
  df <- data.frame(clusters = as.integer(gsub(">", "", data[1, ]))-1, cell_type = data[2, ])
  return(df)
}
