#' @import reticulate
#' @import dplyr
#' @import stringr
#' @export

GPT_interact = function(positive_gene = NULL,negative_gene = NULL){
  reticulate::py_run_string('
import subprocess
import importlib
import openai
import json

def chat_with_gpt4(prompt):
    global chat_history  # 声明为全局变量
    try:
        # 添加用户的新消息到历史记录
        chat_history.append({"role": "user", "content": prompt})

        # 调用 ChatGPT-4 API
        response = openai.ChatCompletion.create(
            model="gpt-4-turbo-preview",  # 确保使用正确的模型名称
            messages=chat_history
        )

        # 获取 GPT-4 的回应并添加到历史记录
        gpt_response = response.choices[0].message["content"]
        chat_history.append({"role": "system", "content": gpt_response})

        # 打印聊天回复
        return gpt_response
    except Exception as e:
        return str(e)
')
  # 继续对话 (使用相同的聊天对象)
    # 如果任一不为空，执行以下代码positive_gene is expressed in the
  user_input <- Gemini_generate_gene_text(positive_gene = positive_gene, negative_gene = negative_gene)
  result = py$chat_with_gpt4(user_input)
  print(result)
  rows <- strsplit(result, "\n")[[1]]
  data <- sapply(rows, function(row) strsplit(row, ": ")[[1]])
  df <- data.frame(clusters = as.integer(gsub(">", "", data[1, ]))-1, cell_type = data[2, ])
  return(df)
}
