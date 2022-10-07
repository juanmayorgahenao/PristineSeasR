rename_from_parent <- function(folder_path, from_ext, to_ext){

  old_names <- list.files(folder_path, full.names = T, pattern = from_ext)

  if(length(old_names) > 0){
    new_names <- file.path(folder_path,
                           paste0(basename(folder_path),
                                  "_c",
                                  formatC(seq(1:length(old_names)), width = 2, flag = 0),
                                  to_ext))

    file.rename(old_names, new_names)

  }
}
