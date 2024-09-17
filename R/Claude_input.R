#' @import reticulate
#' @import dplyr
#' @import stringr
#' @export

Claude_input <- function(input,topgenenumber, species, tissuename) {
  reticulate::py_run_string('
import anthropic
import os

conversation_history = []

def converse_with_claude(prompt):
    global conversation_history
    try:
        my_api_key = os.environ.get("ANTHROPIC_API_KEY")
        client = anthropic.Anthropic(api_key=my_api_key)
        conversation_history.append({"role": "user", "content": prompt})
        response = client.messages.create(
            model="claude-3-opus-20240229",
            max_tokens=4096,
            messages=conversation_history
        )
        # 从API响应中提取Claude的回答
        claude_response = response.content[0].text

        # 将Claude的回答追加到对话历史中
        conversation_history.append({"role": "assistant", "content": claude_response})
        return claude_response
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
  user_input <-paste('ldentify cell types of', species, tissuename, 'using the following markers.ldentify one celltype for each row.Just reply to the cell type, no need to reply to the reasoning section or explanation section.\n', formatted_output, 'Reply in the following format:
1: xx
2: xx
N: xx
N is the line number, xx is a phrase that only includes cell types, such as pluripotent stem cells and smooth muscle cells.')
  result = py$converse_with_claude(user_input)
  print(result)
  return(result)
}
