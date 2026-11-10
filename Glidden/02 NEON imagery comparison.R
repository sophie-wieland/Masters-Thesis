# Extract and use high resolution NEON imagery
# Nov 10, 2026
# Sophie Wieland
getwd()
library(tidyverse)
setwd("D:/NEON_images-camera-ortho/NEON_images-camera-ortho/NEON.D06.KONZ.DP1.30010.001.2023-05.basic.20260903T162148Z.RELEASE-2026")

source_dir <- "."
files_per_folder <- 15

files <- list.files(source_dir, full.names = TRUE, recursive = FALSE)

if (length(files) == 0) {
  message("No files found.")
} else {
  groups <- split(files, ceiling(seq_along(files) / files_per_folder))
  
  for (i in seq_along(groups)) {
    dest_dir <- file.path(source_dir, sprintf("part_%03d", i))
    dir.create(dest_dir, showWarnings = FALSE)
    
    moved <- file.rename(groups[[i]], file.path(dest_dir, basename(groups[[i]])))
    
    if (!all(moved)) {
      stop("Some files could not be moved. Check permissions or filename conflicts.")
    }
  }
  
  message("Moved ", length(files), " files into ", length(groups), " folders.")
}
