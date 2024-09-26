#' @import reticulate
#' @import dplyr
#' @import stringr
#' @export

intersect_result <- function(df1,df2){
  df = df1
  for(i in 1:nrow(df1)){
    print('analysis...')
    for(j in 1:ncol(df1)){
     if(df1[i,j] == df2[i,j]){
       df[i,j] = df1[i,j]
     }else if(df1[i,j] == 'YES'){
       df[i,j] = df1[i,j]
     }else if(df2[i,j] == 'YES'){
       df[i,j] = df2[i,j]
     }else{
       df[i,j] = 'NO'
     }
    }
  }
  return(df)
}
