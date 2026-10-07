min_by_group <- function(data, id, value) {

  id_sym <- ensym(id)   # capture the column symbol
  id_str <- as.character(id_sym)  # convert to string for merge()

  val_sym <- ensym(value)   # capture the column symbol
  val_str <- as.character(val_sym)  # convert to string for merge()

  min.data <- data %>%
  group_by({{id}}) %>%
  slice(which.min({{value}}))
  min.data <- min.data[,c(id_str,val_str)]
  colnames(min.data)[2] <- paste("min",val_str,sep = ".")
  merge(min.data,data,by = paste(id_str))
}

max_by_group <- function(data, id, value) {

  id_sym <- ensym(id)   # capture the column symbol
  id_str <- as.character(id_sym)  # convert to string for merge()

  val_sym <- ensym(value)   # capture the column symbol
  val_str <- as.character(val_sym)  # convert to string for merge()

  max.data <- data %>%
    group_by({{id}}) %>%
    slice(which.max({{value}}))
  max.data <- max.data[,c(id_str,val_str)]
  colnames(max.data)[2] <- paste("max",val_str,sep = ".")
  merge(max.data,data,by = paste(id_str))
}

