# Set Gd path

set_ps_paths <- function(email){

  system <- Sys.info()["sysname"]

  if(system == "Windows") {

    ps_science_path <<- "G:"

  } else {

    ps_science_path <<- paste0("~/",
                               email,
                               " - Google Drive",
                               "/My Drive/Pristine Seas/SCIENCE")
  }

}

