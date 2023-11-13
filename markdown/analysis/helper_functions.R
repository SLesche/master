spearman_brown_double <- function(rel){
  corrected_rel = c()
  
  for (i in seq_along(rel)){
    if (is.na(rel[i])){
      corrected_rel[i] = NA
    } else if (rel[i] > 0){
      corrected_rel[i] = (2*rel[i]) / (1 + rel[i])
    } else {
      corrected_rel[i] = -1*(2*abs(rel[i])) / (1 + abs(rel[i]))
    }
  }
  
  
  return(corrected_rel)
}

fisher_cor_mean <- function(corr_values){
  corr_values[corr_values == 1] = 0.99
  corr_values[corr_values == -1] = -0.99
  corr_values[corr_values > 1 | corr_values < -1] = NA
  z_values = DescTools::FisherZ(corr_values)
  mean_value = mean(z_values, na.rm = TRUE)
  mean_corr = DescTools::FisherZInv(mean_value)
  return(mean_corr)
}

get_homogeneity <- function(corr_matrix){
  n_entries = nrow(corr_matrix)
  homogeneity = data.frame(
    name = vector(mode = "character", length = n_entries),
    value = vector(mode = "numeric", length = n_entries)
  )
  for (i in 1:nrow(corr_matrix)){
    homogeneity$name[i] = rownames(corr_matrix)[i]
    homogeneity$value[i] = fisher_cor_mean(corr_matrix[, i][-i])
  }
  
  return(homogeneity)
}


turn_cormatrix_into_vector <- function(cormat){
  colnames = colnames(cormat)
  rownames = rownames(cormat)
  
  cormat[!upper.tri(cormat)] = NA
  size = length(colnames)
  
  vector = vector(mode = "numeric", length = size*size)
  vector_names = vector(mode = "character", length = size*size)
  
  for (col in seq_along(colnames)){
    for (row in seq_along(rownames)){
      vector[(col-1)*size + row] = cormat[row, col]
      vector_names[(col-1)*size + row] = paste0(colnames[col], "___", rownames[row])
    }
  }
  
  data = t(data.frame(vector))
  colnames(data) = vector_names
  data = t(as.data.frame(data[, colSums(is.na(data)) == 0]))
  rownames(data) = c()
  data = data.table::transpose(as.data.frame(data), keep.names = "vars")
  return(data)
}
