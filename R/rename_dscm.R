# This function will rename .MP4 and .XML files for dropcam deployments

rename_dscm_files <- function(ps_site_id,
                              deployments_path){

  deployment_folder <- file.path(deployments_path,
                                 stringr::str_replace_all(ps_site_id, "_", "-"))

  stopifnot("Deployment directory doesn't exist" = dir.exists(deployment_folder))

  video_path <- file.path(deployment_folder, "Video")

  stopifnot("Video directory doesn't exist." = dir.exists(video_path))

  file_names <- list.files(video_path, full.names = T)

  stopifnot("Files have been renamed already" = stringr::str_detect(basename(file_names), "dscm") == FALSE)

  new_filenames <- paste(ps_site_id,
                         stringr::str_to_lower(paste(substring(basename(file_names), first = 0, last = 5),
                                                     tools::file_ext(basename(file_names)),
                                                     sep = ".")),
                         sep = "_")

  new_filenames <- file.path(video_path, new_filenames)

  file.rename(file_names, new_filenames)

}

