#' @import dplyr
#' @import stringr
#' @export

Reliable_Df <- function(Validate_result) {
  Validateres = Validate_result
  ####Organize the results of the Validate() function into a data frame.
  Validate_df = cbind(Validateres$ERNIE,Validateres$Gemini,Validateres$GPT,Validateres$Llama,Validateres$Claude)
  ####Count the positive genes and negative genes.
  df_split <- Validate_df
  df_split[] <- lapply(df_split, function(x) {
    if (is.character(x)) {
      strsplit(x, ",")
    } else {
      x
    }
  })
  df_count <- df_split
  df_count[] <- lapply(df_count, function(x) {
    if (is.list(x)) {
      sapply(x, length)
    } else {
      x
    }
  })
  df = df_count
  df$Total_reliable = rep('NO',nrow(df))
  df$ERNIE_reliable = rep('NO',nrow(df))
  df$GPT_reliable = rep('NO',nrow(df))
  df$Gemini_reliable = rep('NO',nrow(df))
  df$Llama_reliable = rep('NO',nrow(df))
  df$Claude_reliable = rep('NO',nrow(df))
  for(i in 1:nrow(df)){
    n = df[i,]
    if(n$ERNIE_positive_marker>=4 | n$Gemini_positive_marker>=4 | n$GPT_positive_marker>=4 | n$Llama_positive_marker>=4 | n$Claude_positive_marker>=4){
      df[i,"Total_reliable"] = 'YES'
    }
  }
  ####Confirming accurate cell annotation if the expression of more than four characteristic genes aligns with expected patterns
  for(i in 1:nrow(df)){
    n = df[i,]
    for(j in c('ERNIE_positive_marker','GPT_positive_marker','Gemini_positive_marker','Llama_positive_marker','Claude_positive_marker')){
      if(n[,j]>=4){
        df[i,paste0(as.character(sapply(j, function(x) gsub('_positive_marker', "", x))), "_reliable")] = 'YES'
      }
    }
  }
  Validate_df = df[,26:31]
  return(Validate_df)
}
