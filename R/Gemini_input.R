#' @import reticulate
#' @import dplyr
#' @export

Gemini_input = function(input,topgenenumber,species,tissuename){
  reticulate::py_run_string("
import google.generativeai as genai
import pathlib
import textwrap
import subprocess
import importlib
import os
from IPython.display import display
from IPython.display import Markdown
genai.configure(api_key=GEMINI_api_key, transport='rest')
def to_markdown(text):
    text = text.replace('•', '  *')
    return Markdown(textwrap.indent(text, '> ', predicate=lambda _: True))
model = genai.GenerativeModel('gemini-1.5-pro-latest')
chat = model.start_chat(history=[])
chat
def chat_response(user_input):
    response = chat.send_message(user_input)
    markdown=to_markdown(response.text)
    plain_text = markdown.data
    return plain_text
")
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
N is the line number, xx is a phrase that only includes cell types, such as pluripotent stem cells and smooth muscle cells. Use "undefined" to replace cell names for cell types that cannot be determined.')
  result = py$chat_response(user_input)
  print(result)
  return(result)
}
