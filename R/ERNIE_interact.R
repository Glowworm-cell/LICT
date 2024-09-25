#' @import reticulate
#' @import dplyr
#' @import stringr
#' @export
ERNIE_interact = function(positive_gene = NULL,negative_gene = NULL){
  reticulate::py_run_string("
def send_request(user_input):
    add_ERNIE_message('user', user_input)
    headers = {'Content-Type': 'application/json'}
    json_payload = json.dumps(payload)
    response = requests.request('POST', url, headers=headers, data=json_payload)
    result = response.json().get('result')

    add_ERNIE_message('assistant', result)
    return result
")
  user_input <- ERNIE_generate_gene_text(positive_gene = positive_gene, negative_gene = negative_gene)
  result1 = py$send_request(user_input)
  user_input <- paste(result1,"将上面的文本中的数字和细胞类型或者Undefined按照如下格式提取出来，1: xx
2: xx
3: Undefined
...: xx
N: xx
N为对应的行数，xx为对应的细胞类型，N和xx之间用英文的冒号加空格“: ”分隔", sep = "\n")
  result = py$send_request_2(user_input)
  print(result)
  start_position <- regexpr("1:", result)
  result <- substring(result, start_position)
  rows <- strsplit(result, "\n")[[1]]
  data <- sapply(rows, function(row) strsplit(row, ": ")[[1]])
  df1 <- data.frame(clusters = as.integer(gsub(">", "", data[1, ]))-1, cell_type = data[2, ])
  return(df1)
}
