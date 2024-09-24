#' @import reticulate
#' @import dplyr
#' @import stringr
#' @export
#'
MarkerPrompt = function(result,species){
  mg <- character()
  cluster = numeric()
  row_number <- 1  # 初始化行号
  res1 = result
  for (i in 1:nrow(res1)) {
    ct <- as.character(res1[i,2])
    ct <- na.omit(ct)  # 排除NA值
    if (length(ct) > 0) {  # 只有在ct不为空的情况下才进行下一步
      ct_text <- paste(ct, collapse=", ")
      mg <- c(mg, paste("row", row_number, ":", ct_text))
    }
    cluster = c(cluster,i)
    row_number <- row_number + 1  # 增加行号
  }

  # 将mg的各行合并成一个长字符串，每行之间用换行符分隔
  final_text <- paste(mg, collapse="\n")

  specie = species
  # 生成最终输出
  output_text <- paste('Provide key marker genes for the following ', specie, ' cell types, with 15 key marker genes per cell type. Provide only the abbreviated gene names of key marker genes, full names are not required:\n', final_text, '\nThe format of the final response should be:\n\n1: gene1, gene2, gene3\n2: gene1, gene2, gene3\nN: gene1, gene2, gene3\n\n...where N represents the row number and gene1, gene2, gene3 represent key marker genes.', sep="")
  print(output_text)
}
