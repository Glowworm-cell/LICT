#' @import reticulate
#' @import dplyr
#' @import stringr
#' @export

Llama3_interact = function(positive_gene = NULL,negative_gene = NULL){
  reticulate::py_run_string("
def chat_with_llama3(user_input):
    add_llama_message('user', user_input)
    headers = {'Content-Type': 'application/json'}
    json_llama_payload = json.dumps(llama_payload)
    response = requests.request('POST', url, headers=headers, data=json_llama_payload)
    result = response.json().get('result')

    add_llama_message('assistant', result)
    return result
")
    # 如果任一不为空，执行以下代码positive_gene is expressed in the
  user_input <- generate_gene_text(positive_gene = positive_gene, negative_gene = negative_gene)
  result = py$chat_with_llama3(user_input)
  print(result)
  start_position <- regexpr("1:", result)
  result <- substring(result, start_position)
  rows <- strsplit(result, "\n")[[1]]
  data <- sapply(rows, function(row) strsplit(row, ": ")[[1]])
  df1 <- data.frame(clusters = as.integer(gsub(">", "", data[1, ]))-1, cell_type = data[2, ])
  return(df1)
}

