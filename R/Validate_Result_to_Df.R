#' @import dplyr
#' @import stringr
#' @export

Validate_Result_to_Df <- function(Validate_result) {
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
  df$reliabe = rep('NO',nrow(df))
  df$unreliabe = rep('NO',nrow(df))
  ####Confirming accurate cell annotation if the expression of more than four characteristic genes aligns with expected patterns
  for(i in 1:nrow(df)){
    n = df[i,]
    if(n$ERNIE_positive_marker>=4 | n$Gemini_positive_marker>=4 | n$GPT_positive_marker>=4 | n$Llama_positive_marker>=4 | n$Claude_positive_marker>=4){
      df[i,"reliabe"] = 'YES'
    }else{
      df[i,"unreliabe"] = 'YES'
    }
  }
  Validate_df = cbind(Validate_df, df[,26:27])
  return(Validate_df)
}
