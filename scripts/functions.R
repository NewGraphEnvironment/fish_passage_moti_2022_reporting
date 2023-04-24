


##funciton ot find a string in your directory from https://stackoverflow.com/questions/45502010/is-there-an-r-version-of-rstudios-find-in-files

fif <- function(what, where=".", in_files="\\.[Rr]$", recursive = TRUE,
                ignore.case = TRUE) {
  fils <- list.files(path = where, pattern = in_files, recursive = recursive)
  found <- FALSE
  file_cmd <- Sys.which("file")
  for (fil in fils) {

    if (nchar(file_cmd) > 0) {
      ftype <- system2(file_cmd, fil, TRUE)
      if (!grepl("text", ftype)[1]) next
    }
    contents <- readLines(fil)
    res <- grepl(what, contents, ignore.case = ignore.case)
    res <- which(res)
    if (length(res) > 0) {
      found <-  TRUE
      cat(sprintf("%s\n", fil), sep="")
      cat(sprintf(" % 4s: %s\n", res, contents[res]), sep="")
    }
  }
  if (!found) message("(No results found)")
}

# rename the photos
dff_photo_rename <- function(dat = df_test,
                             dir_from_stub = '/Users/airvine/Projects/current/2022-061-moti-fish-passage/data/photos/mergin/',
                             dir_to_stub = 'data/photos/'){
# create new photo directories
dat %>%
  pull(site_id) %>%
  map(fpr::fpr_photo_folders)

# make a dataframe ready to rename your photos with
dat2 <- dat %>%
  pivot_longer(starts_with('photo_'),
               values_to = 'photo_og',
               names_to = 'photo_renamed',
               cols_vary = 'slowest') %>%
  # remove rows with no photo
  filter(!is.na(photo_og)) %>%
  mutate(photo_renamed = paste0(dir_to_stub,
                                site_id,
                                '/',
                                tools::file_path_sans_ext(basename(photo_og)),
                                str_extract(photo_renamed, '_.*$'),
                                '.',
                                tools::file_ext(photo_og)),
         photo_og = paste0(dir_from_stub, photo_og))
mapply(file.copy,
       from =  dat2 %>% pull(photo_og),
       to = dat2 %>% pull(photo_renamed),
       overwrite = T,
       copy.mode = TRUE)
}



