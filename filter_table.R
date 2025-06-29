input_file <- "/Users/zhaomingyu/Desktop/02-12xS18-13_BSA_Seq_INDEL.table"

first_line <- readLines(input_file, n = 1)
has_header <- !all(suppressWarnings(!is.na(as.numeric(strsplit(first_line, "\t")[[1]]))))
input_data <- read.table(input_file, header = has_header, sep = "\t")

filter_pairs <- function(pair_str) {
  split_result <- strsplit(as.character(pair_str), ",")[[1]]
  if (length(split_result) == 0) {
    return(numeric(0))
  }
  parts <- as.numeric(split_result)
  return(parts)
}

filter_data <- function(data) {
  filtered_indices <- c()
  for (i in 1:nrow(data)) {
    f_parts <- filter_pairs(data[i, 6])
    k_parts <- filter_pairs(data[i, 11])
    
    if (length(f_parts) == 2 & length(k_parts) == 2) {
      if ((f_parts[1] == 0 & f_parts[2] != 0 & k_parts[1] != 0 & k_parts[2] == 0) | 
          (f_parts[1] != 0 & f_parts[2] == 0 & k_parts[1] == 0 & k_parts[2] != 0)) {
        filtered_indices <- c(filtered_indices, i)
      }
    } else if (length(f_parts) == 3 & length(k_parts) == 3) {
      f_valid <- (
        (f_parts[1] == 0 & f_parts[2] != 0 & f_parts[3] == 0) ||
        (f_parts[1] == 0 & f_parts[2] == 0 & f_parts[3] != 0) ||
        (f_parts[1] != 0 & f_parts[2] == 0 & f_parts[3] == 0)
      )
      k_valid <- (
        (k_parts[1] == 0 & k_parts[2] != 0 & k_parts[3] == 0) ||
        (k_parts[1] == 0 & k_parts[2] == 0 & k_parts[3] != 0) ||
        (k_parts[1] != 0 & k_parts[2] == 0 & k_parts[3] == 0)
      )
      if (f_valid & k_valid) {
        filtered_indices <- c(filtered_indices, i)
      }
    }
  }
  return(data[filtered_indices, ])
}

result <- filter_data(input_data)

output_file <- "/Users/zhaomingyu/Desktop/filtered_table.table"
write.table(result, output_file, sep = "\t", quote = FALSE, row.names = FALSE)