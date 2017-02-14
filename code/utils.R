# ====================================================================================
# utils Functions
# ====================================================================================
# Impute na values: integers: mean, factors: None
convert_na <- function(data) {
  
  for (predictor in colnames(data)){
    pred = eval(quote(predictor))
    selected_pred = data[, pred]
    
    # if column is factor, convert na value to "None"
    if (is.factor(selected_pred)) {    
      
      tmp = as.character(selected_pred)
      tmp[is.na(tmp)] = "None"
      tmp = as.factor(tmp)
      data[, pred] = tmp
    }
    # else, if coclimn is integer, convert na to mean(column)
    if (is.integer(selected_pred)) {
      
      selected_pred[is.na(selected_pred)] = mean(selected_pred, na.rm = TRUE)
      data[, pred] = selected_pred
    }
    
  }
  return(data)
  
}


# Count number of na values per feature
get_number_none <- function(data) {
  
  number_of_nones <- apply(data, 1, function(x) {
    sum(x == 'None')
  })
  
  return(number_of_nones)
}

# Find factors with less than 2 levels
less_than_two_levels <- function(col) {
  
  if (is.factor(col)) {
    return (length(levels(col)) < 2)
  } else {
    return (FALSE)
  }
  
}
