#' @import reticulate
#' @import dplyr
#' @import stringr
#' @export

ERNIECellType = function(input,topgenenumber,species,tissuename){
  ERNIE_input_result = ERNIE_input(input = input,
                                   topgenenumber = topgenenumber,
                                   species = species,
                                   tissuename = tissuename)
  return(ERNIE_input_result)
}
