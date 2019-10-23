swap_names <- function(name, dataset_info,
                       old_col_num, new_col_num){
  for (i in seq_along(name)){
    for (j in seq(nrow(dataset_info))){
      if (name[i] == dataset_info[[old_col_num]][j]){
        name[i] <<-dataset_info[[new_col_num]][j]
      }
    }
  }
}