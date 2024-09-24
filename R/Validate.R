#' @import reticulate
#' @import dplyr
#' @import stringr
#' @export
#'
#'
Validate <- function(LLM_res, seurat_obj, Percent) {
  reticulate::py_run_string('
import subprocess
import openai
import json

chat_history_validate = []  # 聊天历史记录列表

def chat_with_gpt4_validate(prompt):
    global chat_history_validate  # 声明为全局变量
    try:
        # 添加用户的新消息到历史记录
        chat_history_validate.append({"role": "user", "content": prompt})

        # 限制历史记录长度为最后的两个对话：一个用户，一个系统
        if len(chat_history_validate) > 2:
            chat_history_validate = chat_history_validate[-2:]

        # 调用 ChatGPT-4 API
        response = openai.ChatCompletion.create(
            model="gpt-4-turbo-preview",  # 确保使用正确的模型名称
            messages=chat_history_validate
        )

        # 获取 GPT-4 的回应并添加到历史记录
        gpt_response = response.choices[0].message["content"]
        chat_history_validate.append({"role": "system", "content": gpt_response})

        # 打印聊天回复
        return gpt_response
    except Exception as e:
        return str(e)
')
  if (!is.data.frame(LLM_res)) {
    print('list')
    for(n in names(LLM_res)){
      input = LLM_res[[n]]
      Marker_Prompt = MarkerPrompt(result = input,
                                   species = 'human')
      result = py$chat_with_gpt4_validate(Marker_Prompt)
      print(result)
      Marker_Prompt2 = ('
  Transform the above reply into an R language dataframe code.
  Do not reply with any words other than follow format:
  markerdata <- data_frame(
  Row = c(1,2,3...),##row number
  Marker_Gene = c(
    "NCR1, KIR2DL1, KIR2DL3...",##row1 marker gene
    "CD3D, CD3E, CD3G...",##row2 marker gene
    "ACTA2, MYH11, TAGLN..."##row3 marker gene
    ...
  )
)')
      result2 = py$chat_with_gpt4_validate(Marker_Prompt2)
      print(result2)
      formatted_result2 <- gsub("\\\\n", "\n", result2)
      # 移除最开始的 "R\n"，因为它不是有效的R代码的一部分
      formatted_result2 <- sub("R\n", "", formatted_result2)
      # 移除最开始的 "r\n"，因为它不是有效的R代码的一部分
      formatted_result2 <- sub("r\n", "", formatted_result2)
      # 移除开头的Markdown代码块标记（如果存在）
      formatted_result2 <- gsub("^```", "", formatted_result2)
      # 移除末尾的Markdown代码块标记（如果存在）
      formatted_result2 <- gsub("```$", "", formatted_result2)
      # 使用parse和eval来运行处理后的字符串中的R代码
      eval(parse(text = formatted_result2))
      cluster = markerdata$Row
      unique(Idents(seurat_obj))
      levels_order <- levels(Idents(seurat_obj))
      pn_gene <- data.frame(cluters = numeric(), positive_marker = character(), negative_marker = character(), stringsAsFactors = FALSE)

      for(i in cluster){
        print(i)
        all_cells <- subset(seurat_obj, idents = levels_order[i])
        marker_gene = unlist(str_split(markerdata[markerdata$Row == i,]$Marker_Gene, ', '))
        positive_marker = character()
        negative_marker = character()
        for(j in marker_gene){
          tryCatch({
            if(any(rownames(all_cells) == j)){
              print(j)
              code <- paste0("positive_cell = subset(all_cells,", j , ">0)")
              eval(parse(text = code))
              percent = nrow(positive_cell@meta.data)/nrow(all_cells@meta.data)
              if (percent>=Percent){
                positive_marker = c(positive_marker,j)
              }else{
                negative_marker = c(negative_marker,j)
              }
            }else{
              print(j)
              print('Can not find marker gene in the data')
            }
          }, error = function(e) {
            # 打印错误消息
            print(paste("Error at marker gene ", j, ":", e$message))
          })
        }
        positive_marker = paste(positive_marker, collapse=",")
        negative_marker = paste(negative_marker, collapse=",")
        pn_gene1 <- as.data.frame(t(c(i, positive_marker,negative_marker)))
        pn_gene = rbind(pn_gene,pn_gene1)
      }
      colnames(pn_gene) = c('row', paste0(n, "_positive_marker"), paste0(n, "_negative_marker"))
      pn_gene$row = as.numeric(pn_gene$row)
      input$row = seq(1,nrow(input))
      input = input%>%
        left_join(pn_gene, by = c("row" = "row"))
      LLM_res[[n]] = input
    }
    return(LLM_res)
  } else if (is.data.frame(LLM_res)) {
    print('dataframe')
    input = LLM_res
    Marker_Prompt = MarkerPrompt(result = input,
                                 species = 'human')
    print('Marker_Prompt')
    result = py$chat_with_gpt4_validate(Marker_Prompt)
    print(result)
    Marker_Prompt2 = ('
  Transform the above reply into an R language dataframe code.
  Do not reply with any words other than follow format:
  markerdata <- data_frame(
  Row = c(1,2,3...),##row number
  Marker_Gene = c(
    "NCR1, KIR2DL1, KIR2DL3...",##row1 marker gene
    "CD3D, CD3E, CD3G...",##row2 marker gene
    "ACTA2, MYH11, TAGLN..."##row3 marker gene
    ...
  )
)')
    result2 = py$chat_with_gpt4_validate(Marker_Prompt2)
    print(result2)
    formatted_result2 <- gsub("\\\\n", "\n", result2)
    # 移除最开始的 "R\n"，因为它不是有效的R代码的一部分
    formatted_result2 <- sub("R\n", "", formatted_result2)
    # 移除最开始的 "r\n"，因为它不是有效的R代码的一部分
    formatted_result2 <- sub("r\n", "", formatted_result2)
    # 移除开头的Markdown代码块标记（如果存在）
    formatted_result2 <- gsub("^```", "", formatted_result2)
    # 移除末尾的Markdown代码块标记（如果存在）
    formatted_result2 <- gsub("```$", "", formatted_result2)
    # 使用parse和eval来运行处理后的字符串中的R代码
    eval(parse(text = formatted_result2))
    cluster = markerdata$Row
    unique(Idents(seurat_obj))
    levels_order <- levels(Idents(seurat_obj))
    pn_gene <- data.frame(cluters = numeric(), positive_marker = character(), negative_marker = character(), stringsAsFactors = FALSE)

    for(i in cluster){
      print(i)
      all_cells <- subset(seurat_obj, idents = levels_order[i])
      marker_gene = unlist(str_split(markerdata[markerdata$Row == i,]$Marker_Gene, ', '))
      positive_marker = character()
      negative_marker = character()
      for(j in marker_gene){
        tryCatch({
          if(any(rownames(all_cells) == j)){
            print(j)
            code <- paste0("positive_cell = subset(all_cells,", j , ">0)")
            eval(parse(text = code))
            percent = nrow(positive_cell@meta.data)/nrow(all_cells@meta.data)
            if (percent>=Percent){
              positive_marker = c(positive_marker,j)
            }else{
              negative_marker = c(negative_marker,j)
            }
          }else{
            print(j)
            print('Can not find marker gene in the data')
          }
        }, error = function(e) {
          # 打印错误消息
          print(paste("Error at marker gene ", j, ":", e$message))
        })
      }
      positive_marker = paste(positive_marker, collapse=",")
      negative_marker = paste(negative_marker, collapse=",")
      pn_gene1 <- as.data.frame(t(c(i, positive_marker,negative_marker)))
      pn_gene = rbind(pn_gene,pn_gene1)
    }
    colnames(pn_gene) = c('row', "positive_marker","_negative_marker")
    pn_gene$row = as.numeric(pn_gene$row)
    input$row = seq(1,nrow(input))
    input = input%>%
      left_join(pn_gene, by = c("row" = "row"))
    LLM_res = input
    return(LLM_res)
  } else {
    print("Unknown result, please enter the result of LLMcelltype()")  # 如果输入既不是列表也不是数据框
  }
}
