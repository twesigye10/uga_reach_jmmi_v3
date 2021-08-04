# get files names and full paths into a list
get_files_metadata<-function(folder_path){
  fps<-list()
  fps$fullpath<-list.files(folder_path, full.names = T,pattern = ".csv$")
  fps$file_name<-list.files(folder_path,pattern = ".csv$")
  fps$num_files<- length(fps$file_name)
  fps
}

# replace Inf and NaN with NAs for a cleaner output
change_nan_and_inf_to_na <- function(x) {
  y <- replace(x, is.infinite(x), NA) 
  z <- replace(y, is.nan(y), NA)
  z
} 