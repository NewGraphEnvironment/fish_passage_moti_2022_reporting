source('scripts/packages.R')
# library(magrittr)
# library(dplyr)

# import a geopackage and rearrange then burn to csv

# relative path to the q project form (assuming we have same folder structure) -
# use an absolute path if you have to but it is preferable to keep our relative paths the same so we can collab

#--------------------import---------------------------

# name the project directory we are pulling from
dir_project <- 'bcfishpass_skeena_20220823-v225'

# find all the pscis forms in the file
form_names <- list.files(path = paste0('../../gis/mergin/',
                  dir_project),
           # ?glob2rx is a funky little unit
           pattern = glob2rx('form_pscis_*.gpkg'),
           full.names = T
           )

form_prep <- form_names %>%
  purrr::map(sf::st_read) %>%
  purrr::map(st_transform, crs = 3005) %>%
  purrr::map(poisspatial::ps_sfc_to_coords, X = 'long', Y = 'lat') %>%
  purrr::map(plyr::colwise(type.convert)) %>%
  # name the data.frames so we can add it later as a "source" column - we use basename to leave the filepath behind
  purrr::set_names(nm = basename(form_names)) %>%
  bind_rows(.id = 'source') %>%
  # project specific - this is pscis sites 152 and 9999 which were fake with weird coordinates. See https://github.com/NewGraphEnvironment/fish_passage_skeena_2022_reporting/issues/27
  # filter(date != '2022-08-24' & date != '2022-08-25' & date != '2022-08-26') %>%
    sf::st_as_sf(coords = c("long", "lat"),
                 crs = 3005, remove = F)
    sf::st_write('data/inputs_extracted/mergin_backups/form_pscis_v225.gpkg', append=FALSE)


# this is a table that cross references column names for pscis table and has the columns in the same order as the spreadsheet
xref_names_pscis <- fpr::fpr_xref_pscis

# get order of columns as per the excel template spreadsheet
# this can be used as a select(all_of(name_pscis_sprd_ordered)) later
# to order columns for the fieldform and/or put the field entered table inorder
name_pscis_sprd_ordered <- fpr::xref_names_pscis %>%
  filter(!is.na(spdsht)) %>%
  select(spdsht) %>%
  pull(spdsht)


# this one is the gov names
# name_pscis_bcdata_ordered <- fpr::xref_names_pscis %>%
#   filter(!is.na(bcdata)) %>%
#   select(bcdata) %>%
#   pull(bcdata)

# see names that coincide between the xref table and what we have
intersect(name_pscis_sprd_ordered, names(form_prep))

# see names that coincide between the xref table and what we have
# intersect(name_pscis_bcdata_ordered, names(form_prep))

# see which are different
setdiff(name_pscis_sprd_ordered, names(form_prep))
# order matters
setdiff(names(form_prep), name_pscis_sprd_ordered)

# if we include the site_id the above move gives us most of what we need
# lets make a xref table of it

moti_names <- setdiff(names(form_prep), name_pscis_sprd_ordered) %>%
  enframe(name = NULL, value = 'spdsht') %>%
  mutate(report = str_to_title(spdsht),
         report = stringr::str_replace_all(report, '_', ' '),
         report = stringr::str_replace_all(report, 'event affecting culvert', ''),
         report = stringr::str_replace_all(report, 'id', 'ID'),
         report = stringr::str_replace_all(report, 'Gps', 'GPS'),
         report_include = case_when(
    str_detect(spdsht,
               'photo|long|lat|mergin|surveyor|gps|width|utm|time|source|camera|aggregated|rowid|geometry') ~ F,
    T ~ T
  ),
  id_join = NA_integer_,
  id_side = NA_integer_) %>%
  filter(spdsht != 'stream_width_ratio_score')
# filter(report_include == T)

# burn out to csv so we can manually do the descriptions
moti_names %>%
  write_csv('data/inputs_raw/moti_climate.csv')



# --------------------moti climate change ---------------------------
# we want to get our climate change risk information summarized for each site.
# if we were to add a tag to the names or a xref tie that tells us if each column is part of this or not we would get ahead...

##-------------------- moti correct chris_culvert_id-----------------------
# one thing we definitely need to do it get the chris_culvert_id for each site as we used the wrong one on our forms. We should be able to cross ref the ids from bcdata
# so let's try that first


# get_this <- bcdata::bcdc_tidy_resources('ministry-of-transportation-mot-culverts') %>%
#   filter(bcdata_available == T)  %>%
#   pull(package_id)
#
# dat <- bcdata::bcdc_get_data(get_this)
#
# moti_raw <- dat %>%
#   purrr::set_names(nm = janitor::make_clean_names(names(dat)))
#
# # match our sites to ids
# moti <- left_join(
#   form_prep,
#
#   moti_raw %>% select(culvert_id, chris_culvert_id) %>% sf::st_drop_geometry(),
#
#   by = c('mot_culvert_id' = 'culvert_id')
# )

# the names must have changed so lets use the file in the mergin project as we know that one is the same
moti_raw <- sf::st_read('../../gis/mergin/bcfishpass_skeena_20220823/clipped_moti_culverts_sp.gpkg') %>%
  sf::st_drop_geometry()

moti <- left_join(
  form_prep,

  moti_raw %>% select(culvert_id, chris_culvert_id),

  by = c('mot_culvert_id' = 'culvert_id')
)

##---------------------pscis clean only--------------------------
form_prep1 <- form_prep %>%
  #split date time column into date and time
  dplyr::mutate(date = lubridate::ymd_hms(date),
                date_time_start = lubridate::ymd_hms(date_time_start),
                survey_date = lubridate::date(date_time_start),
                time = hms::as_hms(date_time_start)) %>%
  # filter out to get only the records newly created
  filter(!is.na(date_time_start)) %>%
  # when necessary split your time into a date and time - skeena only
  # note the call to any of
  select(any_of(name_pscis_sprd_ordered)) %>%
  # we are better off leaving the coordinates as per the crossings layer of bcfishpass than moving them around if
  # the switch that utm_corrected is not hit.  If it was we can pull out the coordinates from the geom or use the ones that autopopulate
  # they should be the same but we should qa it
  # see the *_template.R script to see how to convert CRS and pull out coordinates
  # geom is not a column in the spreadsheet template so remove
  sf::st_drop_geometry()


# to use all the columns from the template first we make an empty dataframe from a template
template <- fpr::fpr_import_pscis() %>%
  slice(0)

# then we join it to our populated spreadsheet
# we may as well keep all the columns that are not in the spreadsheet and append to the end
form <- bind_rows(
  template,

  form_prep1 %>%
    # we are better off leaving the coordinates as per the crossings layer of bcfishpass than moving them around if
    # the switch that utm_corrected is not hit.  If it was we can pull out the coordinates from the geom or use the ones that autopopulate
    # they should be the same but we should QA it
    # see the *_template.R script to see how to convert CRS and pull out coordinates
    # geom is not a column in the spreadsheet template so remove
    sf::st_drop_geometry()
) %>%
  # then arrange it so it is easy to see how to sort to separate spreadsheets
  arrange(crossing_type,
          continuous_embeddedment_yes_no,
          backwatered_yes_no)


# burn to a csv
form %>%

  #sort data by date
  arrange(date) %>%

  readr::write_csv(paste0(
    'data/dff/survey_form_',
    format(lubridate::now(), "%Y%m%d"),
    '.csv'))



