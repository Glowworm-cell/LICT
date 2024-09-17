#' @import reticulate
#' @import dplyr
#' @import stringr
#' @export


LLM_interect = function(positive_gene, negative_gene){
  tryCatch({print('ERNIE is analyzing')
    ERNIE = ERNIE_interact(positive_gene = positive_gene,negative_gene = negative_gene)})
  tryCatch({print('Llama is analyzing')
    Llama = Llama3_interact(positive_gene = positive_gene,negative_gene = negative_gene)})
  tryCatch({print('Gemini is analyzing')
    Gemini = Gemini_interact(positive_gene = positive_gene,negative_gene = negative_gene)})
  tryCatch({print('ChatGPT is analyzing')
    GPT = GPT_interact(positive_gene = positive_gene,negative_gene = negative_gene)})
  tryCatch({print('Claude is analyzing')
    Claude = Claude_interact(positive_gene = positive_gene,negative_gene = negative_gene)})
  if (exists("ERNIE") && exists("Gemini") && exists("GPT") && exists("Llama") && exists("Claude")) {
    res <- list(ERNIE = ERNIE, Gemini = Gemini, GPT = GPT, Llama = Llama, Claude = Claude)
  } else if (exists("ERNIE")) {
    res <- list(ERNIE = ERNIE)
  } else if (exists("Gemini")) {
    res <- list(Gemini = Gemini)
  } else if (exists("GPT")) {
    res <- list(GPT = GPT)
  } else if (exists("Llama")) {
    res <- list(Llama = Llama)
  } else if (exists("Claude")) {
    res <- list(Claude = Claude)
  } else {
    res <- 'error'
  }
  return(res)
}
