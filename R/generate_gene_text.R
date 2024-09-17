#' @export
generate_gene_text <- function(positive_gene, negative_gene) {
  # Calculate the maximum number of clusters
  max_length <- max(length(positive_gene), length(negative_gene))

  # Initialize a vector to hold each line of output
  result_texts <- vector("character", max_length)

  # Iterate over each cluster index
  for (i in seq_len(max_length)) {
    # Initialize parts to hold text about gene expression
    pos_part <- ""
    neg_part <- ""

    # Check if there are positive genes for this cluster and construct the string
    if (i <= length(positive_gene) && !is.null(positive_gene[[i]]) && length(positive_gene[[i]]) > 0) {
      pos_genes <- paste(positive_gene[[i]], collapse = ', ')
      pos_part <- sprintf("%s is expressed in the %d row", pos_genes, i)
    }

    # Check if there are negative genes for this cluster and construct the string
    if (i <= length(negative_gene) && !is.null(negative_gene[[i]]) && length(negative_gene[[i]]) > 0) {
      neg_genes <- paste(negative_gene[[i]], collapse = ', ')
      neg_part <- sprintf("%s is not expressed in the %d row", neg_genes, i)
    }

    # Combine the positive and negative parts with a connector if both exist
    if (nzchar(pos_part) && nzchar(neg_part)) {
      result_texts[i] <- sprintf("%s, and %s.", pos_part, neg_part)
    } else if (nzchar(pos_part)) {
      result_texts[i] <- sprintf("%s.", pos_part)
    } else if (nzchar(neg_part)) {
      result_texts[i] <- sprintf("%s.", neg_part)
    }
  }

  # Remove empty lines (in case some clusters have neither positive nor negative genes)
  result_texts <- result_texts[nzchar(result_texts)]

  # Add final instructions
  final_instruction <- "Based on the additional information above, modify my previous response and list all cell types, including those that have not been modified.Reply in the following format:\n1: xx\n2: xx\nN: xx\nN is the line number, xx is the cell type."

  # Combine all non-empty lines
  if (length(result_texts) > 0) {
    full_text <- paste(result_texts, collapse = "\n")
    full_text <- paste(final_instruction, full_text, sep = "\n")
  } else {
    full_text <- final_instruction
  }

  return(full_text)
}
