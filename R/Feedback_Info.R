#' @import dplyr
#' @import stringr
#' @export
#'
Feedback_Info <- function(Validate_Df, Start, End, FindAllMarkersResult) {
  Info1 <- function(Validate_Df, Start, End, FindAllMarkersResult) {
    Validate_df = Validate_Df
    # Initialize vectors to store new positive and negative genes
    new_positive_genes <- character(nrow(Validate_df))
    new_negative_genes <- character(nrow(Validate_df))

    next_top_marker_gene = FindAllMarkersResult %>%
      group_by(cluster) %>%  # 按照cluster分组
      arrange(desc(avg_log2FC)) %>%  # 按照avg_log2FC降序排列
      slice(Start:End)
    cluster_genes <- next_top_marker_gene %>%
      dplyr::group_by(cluster) %>%
      dplyr::summarise(genes_positive_marker = paste(gene, collapse=",")) %>%
      dplyr::ungroup()
    Validate_df = cbind(cluster_genes, Validate_df)
    print('more Differential expressed gene were extracted from provided DEG table..')

    # Loop through each row of Validate_df
    for (i in 1:nrow(Validate_df)) {
      # Extract positive gene columns, excluding NA values
      positive_genes <- unlist(Validate_df[i, grep("_positive_marker$", names(Validate_df), value = TRUE)])
      positive_genes <- na.omit(positive_genes)

      # Concatenate the positive genes into a single string
      concatenated_positive <- paste(positive_genes, collapse = ",")

      # Split, find unique values, and reassemble
      unique_positive <- unique(unlist(strsplit(concatenated_positive, ",")))
      new_positive_genes[i] <- paste(unique_positive, collapse = ",")

      # Extract negative gene columns, excluding NA values
      negative_genes <- unlist(Validate_df[i, grep("_negative_marker$", names(Validate_df), value = TRUE)])
      negative_genes <- na.omit(negative_genes)

      # Concatenate the negative genes into a single string
      concatenated_negative <- paste(negative_genes, collapse = ",")

      # Split, find unique values, and reassemble
      unique_negative <- unique(unlist(strsplit(concatenated_negative, ",")))
      new_negative_genes[i] <- paste(unique_negative, collapse = ",")
    }

    # Add the all positive and negative genes to Validate_df
    Validate_df$all_positive_marker <- new_positive_genes
    Validate_df$all_negative_marker <- new_negative_genes
    return(Validate_df[,29:31])
  }

  Feedback_Info1  = Info1(Validate_Df, 11, 20, markers)

  Feedback_Info1[Feedback_Info1$unrelialbe == "NO", ]$all_positive_marker = ''
  Feedback_Info1[Feedback_Info1$unrelialbe == "NO", ]$all_negative_marker = ''

  remove_commas_at_ends <- function(x) {
    gsub("^,|,$", "", x)
  }

  # 应用到特定列
  Feedback_Info1$all_positive_marker <- sapply(Feedback_Info1$all_positive_marker, remove_commas_at_ends)
  Feedback_Info1$all_negative_marker <- sapply(Feedback_Info1$all_negative_marker, remove_commas_at_ends)

  # 打印结果查看（可选）
  print('positive_marker:')
  print(Feedback_Info1$all_positive_marker)
  print('negative_marker:')
  print(Feedback_Info1$all_negative_marker)

  # 处理正面标记(all_positive_marker)
  positive_markers_list <- lapply(Feedback_Info1$all_positive_marker, function(x) {
    # 检查是否为空字符串，为空则返回空向量
    if(nchar(x) == 0) return(character(0))
    else return(strsplit(x, ',')[[1]]) # 分割字符串并返回
  })

  # 处理负面标记(all_negative_marker)
  negative_markers_list <- lapply(Feedback_Info1$all_negative_marker, function(x) {
    # 检查是否为空字符串，为空则返回空向量
    if(nchar(x) == 0) return(character(0))
    else return(strsplit(x, ',')[[1]]) # 分割字符串并返回
  })

  # 为了遵循您的命名需求（如 row1, row2...），我们可以添加对应的名称
  names(positive_markers_list) <- paste0("row", seq_along(positive_markers_list))
  names(negative_markers_list) <- paste0("row", seq_along(negative_markers_list))

  result = LLM_interect(positive_gene = positive_markers_list, negative_gene = negative_markers_list)
  return(result)
}

