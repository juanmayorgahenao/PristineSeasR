create_exp_dirs <- function(expedition_id,
                            other_dirs = NULL, ...){

  # Directory where all projects live
  exp_path <- file.path(ps_science_path,
                        "expeditions",
                        expedition_id)

  # Directory where this project will live

  dir.create(path = exp_path)

  # Standard directories
  standard_dirs <- c("data",
                     "data/secondary",
                     "data/primary",
                     "data/primary/raw/",
                     "data/primary/processed/",
                     "figures",
                     "presentations",
                     "GIS",
                     "reports")

  sapply(file.path(exp_path, standard_dirs), dir.create)

  if(!is.null(other_dirs)){
    sapply(file.path(project_path, other_dirs), dir.create)
  }

}
