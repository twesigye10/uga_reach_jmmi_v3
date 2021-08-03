# get files names and full paths into a list
get_files_metadata<-function(folder_path){
  fps<-list()
  fps$fullpath<-list.files(folder_path, full.names = T,pattern = ".csv$")
  fps$file_name<-list.files(folder_path,pattern = ".csv$")
  fps$num_files<- length(fps$file_name)
  fps
}