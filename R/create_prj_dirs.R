create_prj_dirs <- function(ps_prj_id,
                           other_dirs = NULL, ...){


  # Directory where all projects live
  prj_path <- file.path(ps_science_path,
                        "projects",
                        ps_prj_id)

  # Directory where this project will live

  dir.create(path = prj_path)

  # Standard directories
  standard_dirs <- c("data",
                     "figures",
                     "presentations")

  sapply(file.path(prj_path, standard_dirs), dir.create)

  if(!is.null(other_dirs)){
    sapply(file.path(project_path, other_dirs), dir.create)
  }

}
