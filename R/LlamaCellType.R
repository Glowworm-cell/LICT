#' @import reticulate
#' @import dplyr
#' @export

LlamaCellType = function(input,topgenenumber,species,tissuename){
  Llama3_input_result = Llama3_input(input = input,
                               topgenenumber = topgenenumber,
                               species = species,
                               tissuename = tissuename)
  df1 <- data.frame(Number = integer(), Cell_Type = character(), stringsAsFactors = FALSE)
  rows <- strsplit(Llama3_input_result, "\n")[[1]]
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
