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
  df1 <- data.frame(Number = integer(), Cell_Type = character(), stringsAsFactors = FALSE)
  rows <- strsplit(result, "\n")[[1]]
  data <- sapply(rows, function(row) strsplit(row, ": ")[[1]])
  # 遍历列表项
  for (name in names(data)) {
    # 判断是否为需要的数据项（以数字开头）
    if (grepl("^\\d+:", name)) {
      # 提取数字和细胞类型
      split_name <- unlist(strsplit(name, ": "))
      number <- as.integer(split_name[1])
      cell_type <- split_name[2]

      # 将提取的数据添加到数据框中
      df1 <- rbind(df1, data.frame(clusters = number-1, cell_type = cell_type, stringsAsFactors = FALSE))
    }
  }
  return(df1)
}

