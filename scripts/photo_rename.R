# photo rename

library(tibble)
library(purrr)
library(stringr)

# Use set_names to set the column names
df <- tibble(folder_name = paste0("Folder", 1:5),
             photo_cl1 = paste0("photo_", 1:5),
             photo_cl2 = paste0("photo_", 21:25),
             photo_cl3 = paste0("photo_", 31:35),
             photo_cl4 = paste0("photo_", 41:45))



# define your new photo directories
dir_to_create <- df2 %>%
  distinct(folder_name) %>%
  pull(folder_name)

# create new photo directories
dir_to_create %>%
  map(fpr::fpr_photo_folders)

# define photo directory paths
dir_from_stub <- 'before/here/'
dir_to_stub <- 'after/there/'

# make a dataframe ready to rename your photos with
df2 <- df %>%
  pivot_longer(starts_with("photo_"),
               values_to = 'photo_og',
               # names_prefix = 'photo_',
               names_to = 'photo_renamed',
               cols_vary = 'slowest') %>%
  mutate(photo_renamed = paste0(dir_to_stub, site_id, '/', tools::file_path_sans_ext(basename(photo_og)), str_extract(photo_renamed, "_.*$"), '.JPG'),
         photo_og = paste0(dir_from_stub, photo_og))

copy_over_photos <- function(filescopy, filespaste){
  file.copy(from=filescopy, to=filespaste,
            overwrite = T,
            copy.mode = TRUE)
}

mapply(copy_over_photos, filescopy =  filestocopy_list,
       filespaste = filestopaste_list)

# ###########test--------------------------------------------------------------

df_test <- readr::read_csv('data/dff/monitoring_form_all_20230421_hand_modified.csv') %>%
  mutate(site_id = case_when(is.na(pscis_crossing_id) ~ my_crossing_reference, T ~ pscis_crossing_id))

dir_from_stub <- '/Users/airvine/Projects/current/2022-061-moti-fish-passage/data/photos/mergin/'
dir_to_stub <- 'data/photos/'

# create new photo directories
df_test %>%
  pull(site_id) %>%
  map(fpr::fpr_photo_folders)

# make a dataframe ready to rename your photos with
df_test2 <- df_test %>%
  pivot_longer(starts_with('photo_'),
               values_to = 'photo_og',
               # names_prefix = 'photo_',
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


# define files to copy
filestocopy_list <- df_test2 %>%
  pull(photo_og)

# define files to paste
filestopaste_list <- df_test2 %>%
  pull(photo_renamed)

mapply(file.copy,
       from =  filestocopy_list,
       to = filestopaste_list,
       overwrite = T,
       copy.mode = TRUE)

# here we have a bit of a glitch bc our files were so big before.  We need to resize the Bittner photos
filestoconvert <- list.files(path = 'data/photos',
                             recursive = T,
                             full.names = T)

directory_names <- dirname(filestoconvert)

mapply(fpr::fpr_photo_resize_convert,
       photo =  filestoconvert,
       path = directory_names,
       size = "1296 x 972!")




