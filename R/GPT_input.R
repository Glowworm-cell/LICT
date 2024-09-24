#' @import reticulate
#' @import dplyr
#' @import stringr
#' @export

GPT_input <- function(input,topgenenumber, species, tissuename) {
  reticulate::py_run_string('
import subprocess
import openai
import json

chat_history = []

def chat_with_gpt4(prompt):
    global chat_history  # 声明为全局变量
    try:
        # 添加用户的新消息到历史记录
        chat_history.append({"role": "user", "content": prompt})

        # 调用 ChatGPT-4 API
        response = openai.chat.completions.create(
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
  # 开始连续对话
  top_markers <- input %>% dplyr::group_by(cluster) %>% dplyr::top_n(n = topgenenumber, wt = avg_log2FC)
  cluster_genes <- top_markers %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(genes = paste(gene, collapse=", ")) %>%
    dplyr::ungroup()
  formatted_output <- paste(cluster_genes$genes, collapse="\n")
  user_input <-paste('ldentify cell types of', species, tissuename, 'using the following markers.ldentify one celltype for each row.Just reply to the cell type, no need to reply to the reasoning section or explanation section.\n', formatted_output, '\nReply in the following format:
1: xx
2: xx
N: xx
N is the line number, xx is a phrase that only includes cell types, such as pluripotent stem cells and smooth muscle cells.')
  result = py$chat_with_gpt4(user_input)
  print(result)
  return(result)
}
