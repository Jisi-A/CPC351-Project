file_path <- "dataset/"
file_list <- list.files(file_path, full.names = TRUE)
print(file_list)

dfs <- lapply(file_list, read.csv)

lapply(dfs, summary)