# this file imports our data and builds the tables we need for our reporting
pscis_list <- fpr::fpr_import_pscis_all()
pscis_phase1 <- pscis_list %>% pluck('pscis_phase1')
# pscis_phase2 <- pscis_list %>% pluck('pscis_phase2') %>%
#   arrange(pscis_crossing_id)
pscis_reassessments <- pscis_list %>% pluck('pscis_reassessments')
pscis_all_prep <- pscis_list %>%
  bind_rows()


conn <- rws_connect("data/bcfishpass.sqlite")
rws_list_tables(conn)

bcfishpass <- readwritesqlite::rws_read_table("bcfishpass", conn = conn) %>%
   mutate(downstream_route_measure = as.integer(downstream_route_measure)) %>%
   mutate(wct_network_km = round(wct_network_km,2))
wshds <- readwritesqlite::rws_read_table("wshds", conn = conn) %>%
  sf::st_drop_geometry() %>%
  mutate(across(contains('elev'), ~ replace(., . < 0, NA)))

# photo_metadata <- readwritesqlite::rws_read_table("photo_metadata", conn = conn)
# # fiss_sum <- readwritesqlite::rws_read_table("fiss_sum", conn = conn)
rws_disconnect(conn)
#
# ##build the dams table
# tab_dams_raw <- bcfishpass %>%
#   filter(aggregated_crossings_id == 1100000129 |
#            aggregated_crossings_id == 1100002016 |
#            aggregated_crossings_id == 197542) %>%
#   select(id = aggregated_crossings_id, stream = gnis_stream_name,utm_zone, utm_easting, utm_northing, dbm_mof_50k_grid) %>%
#   mutate(barrier_ind = case_when(
#     id == 1100000129 ~ 'F',
#     T ~ 'T'),
#     Notes = case_when(
#       id == 1100000129 ~ 'Remnant dam not located in main channel.',
#       id == 1100002016 ~ 'Large dam (15m  high at 55% grade) located in main channel. No fish ladder.',
#       id == 197542 ~ 'Two small dams (30cm and 40cm high) located just upstream (7m and 20m) of Dicken Road. Likely easily passable by adult WCT but barrier to fry and small juveniles. If culvert replaced these could potentially be fixed at the same time.'
#     )
#   )
#

#load priorities
habitat_confirmations_priorities <- readr::read_csv(
  file = "./data/habitat_confirmations_priorities.csv") %>%
  #filter(!alias_local_name %like% 'ef') %>% ##ditch the ef sites
  mutate(site = as.numeric(site),
         upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1)) %>%
    rename(local_name = alias_local_name) #did this to stay consistent for later

####--------------habitat and fish data------------------
habitat_confirmations <- fpr_import_hab_con(col_filter_na = T, row_empty_remove = T)

hab_site_prep <-  habitat_confirmations %>%
  purrr::pluck("step_4_stream_site_data") %>%
  # tidyr::separate(local_name, into = c('site', 'location'), remove = F) %>%
  mutate(average_gradient_percent = round(average_gradient_percent * 100, 1)) %>%
  mutate_if(is.numeric, round, 1) %>%
  select(-gazetted_names:-site_number, -feature_type:-utm_method) %>%   ##remove the feature utms so they don't conflict with the site utms
  distinct(reference_number, .keep_all = T) ##since we have features we need to filter them out


hab_loc <- habitat_confirmations %>%
  purrr::pluck("step_1_ref_and_loc_info") %>%
  dplyr::filter(!is.na(site_number))%>%
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))

hab_site <- left_join(
  hab_loc,
  hab_site_prep,
  by = 'reference_number'
) %>%
  tidyr::separate(alias_local_name, into = c('site', 'location'), remove = F) %>%
  mutate(site = as.numeric(site))
  #plyr::filter(!alias_local_name %like% '_ef') ##get rid of the ef sites

# hab_features <- left_join(
#   habitat_confirmations %>%
#     purrr::pluck("step_4_stream_site_data") %>%
#     select(reference_number,local_name, feature_type:utm_northing) %>%
#     filter(!is.na(feature_type)) %>%
#     # filter out features without utms for now, they seem a little suspect
#     filter(!is.na(utm_easting)),
#
#   fpr_xref_obstacles,
#
#   by = c('feature_type' = 'spreadsheet_feature_type')
# )


# -----------bcfishpass modelling table setup for reporting------
#this is how we line up our new column names and put things in order for reporting on the fish habitat modeling
# we need to update this sometimes.  When we do we update 02_prep_reporting/0160-load-bcfishpass-data.R,
# get the data from  rename the xref_bcfishpass_names tribble to xref_bcfishpass_names_old  and go through the following procedure
# xref_bcfishpass_names_old <- xref_bcfishpass_names


# ## join the new with the old so you can kable(xref_bcfishpass_names_prep) then run in Rmd chunk and copy paste tribble yo
# xref_bcfishpass_names_prep <- left_join(
#   bcfishpass_names_updated,
#   select(xref_bcfishpass_names_old, -column_comment),
#   by = c('bcfishpass')
# ) %>%
#     mutate(report = stringr::str_replace_all(bcfishpass, '_', ' ') %>%
#              stringr::str_to_title() %>%
#              stringr::str_replace_all('Km', '(km)') %>%
#              stringr::str_replace_all('Ha', '(ha)') %>%
#              stringr::str_replace_all('Lakereservoir', 'Lake Reservoir') %>%
#              stringr::str_replace_all('Co ', 'CO ') %>%
#              stringr::str_replace_all('Ch', 'CH ') %>%
#              stringr::str_replace_all('St ', 'ST ') %>%
#              stringr::str_replace_all('Sk ', 'SK ') %>%
#              stringr::str_replace_all('Wct ', 'WCT ') %>%
#              stringr::str_replace_all('Pscis', 'PSCIS') %>%
#              stringr::str_replace_all('Spawningrearing', 'Spawning Rearing') %>%
#              stringr::str_replace_all('Betweenbarriers', 'Between Barriers') %>%
#              stringr::str_replace_all('Belowupstrbarriers', 'Below Barriers')) %>%
#   select(bcfishpass, report, id_join, id_side, column_comment)
    # select(bcfishpass, report, id_join, id_side)

xref_bcfishpass_names <- tibble::tribble(
                                                                         ~bcfishpass,                                                 ~report, ~id_join, ~id_side,                                                                                                                                                                                                                                          ~column_comment,
                                                           "aggregated_crossings_id",                               "Aggregated Crossings Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                "stream_crossing_id",                                    "Stream Crossing Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                            "dam_id",                                                "Dam Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                     "user_barrier_anthropogenic_id",                         "User Barrier Anthropogenic Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                              "modelled_crossing_id",                                  "Modelled Crossing Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "crossing_source",                                       "Crossing Source",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                             "crossing_feature_type",                                 "Crossing Feature Type",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                      "pscis_status",                                          "PSCIS Status",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                "crossing_type_code",                                    "Crossing Type Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                             "crossing_subtype_code",                                 "Crossing Subtype Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                     "modelled_crossing_type_source",                         "Modelled Crossing Type Source",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                    "barrier_status",                                        "Barrier Status",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "pscis_road_name",                                       "PSCIS Road Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                 "pscis_stream_name",                                     "PSCIS Stream Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                          "pscis_assessment_comment",                              "PSCIS Assessment Comment",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                             "pscis_assessment_date",                                 "PSCIS Assessment Date",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                 "pscis_final_score",                                     "PSCIS Final Score",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                  "transport_line_structured_name_1",                      "Transport Line Structured Name 1",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                   "transport_line_type_description",                       "Transport Line Type Description",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                "transport_line_surface_description",                    "Transport Line Surface Description",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                               "ften_forest_file_id",                                   "Ften Forest File Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                        "ften_file_type_description",                            "Ften File Type Description",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                "ften_client_number",                                    "Ften Client Number",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                  "ften_client_name",                                      "Ften Client Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                       "ften_life_cycle_status_code",                           "Ften Life Cycle Status Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "rail_track_name",                                       "Rail Track Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "rail_owner_name",                                       "Rail Owner Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                        "rail_operator_english_name",                            "Rail Operator English Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                     "ogc_proponent",                                         "Ogc Proponent",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                          "dam_name",                                              "Dam Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                         "dam_owner",                                             "Dam Owner",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                          "utm_zone",                                              "Utm Zone",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                       "utm_easting",                                           "Utm Easting",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                      "utm_northing",                                          "Utm Northing",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                  "dbm_mof_50k_grid",                                      "Dbm Mof 50k Grid",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                 "linear_feature_id",                                     "Linear Feature Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                     "blue_line_key",                                         "Blue Line Key",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                     "watershed_key",                                         "Watershed Key",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                          "downstream_route_measure",                              "Downstream Route Measure",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                      "wscode_ltree",                                          "Wscode Ltree",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "localcode_ltree",                                       "Localcode Ltree",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                              "watershed_group_code",                                  "Watershed Group Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                  "gnis_stream_name",                                      "Gnis Stream Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "crossings_dnstr",                                       "Crossings Dnstr",       NA,       NA,                                                                                                                                          "List of the aggregated_crossings_id values of crossings downstream of the given crossing, in order downstream",
                                                      "barriers_anthropogenic_dnstr",                          "Barriers Anthropogenic Dnstr",       NA,       NA,                                                                                                                                  "List of the aggregated_crossings_id values of barrier crossings downstream of the given crossing, in order downstream",
                                                      "barriers_anthropogenic_upstr",                          "Barriers Anthropogenic Upstr",       NA,       NA,                                                                                                                                                         "List of the aggregated_crossings_id values of barrier crossings upstream of the given crossing",
                                                "barriers_anthropogenic_dnstr_count",                    "Barriers Anthropogenic Dnstr Count",       NA,       NA,                                                                                                                                                                                      "A count of the barrier crossings downstream of the given crossing",
                                                "barriers_anthropogenic_upstr_count",                    "Barriers Anthropogenic Upstr Count",       NA,       NA,                                                                                                                                                                                        "A count of the barrier crossings upstream of the given crossing",
                                                                      "stream_order",                                          "Stream Order",       NA,       NA,                                                                                                                                                                                                                           "Order of FWA stream at point",
                                                                  "stream_magnitude",                                      "Stream Magnitude",       NA,       NA,                                                                                                                                                                                                                       "Magnitude of FWA stream at point",
                                                                          "gradient",                                              "Gradient",       NA,       NA,                                                                                                                                                                                                                                  "Stream slope at point",
                                                             "access_model_ch_co_sk",                                 "Access Model CH CO Sk",       NA,       NA,                                                                                                                                                                                                             "Modelled accessibility to Salmon (15% max)",
                                                                   "access_model_st",                                       "Access Model St",       NA,       NA,                                                                                                                                                                                                          "Modelled accessibility to Steelhead (20% max)",
                                                                  "access_model_wct",                                      "Access Model Wct",       NA,       NA,                                                                                                                                                  "Modelled accessibility to West Slope Cutthroat Trout (20% max or downstream of known WCT observation)",
                                                                 "observedspp_dnstr",                                     "Observedspp Dnstr",       NA,       NA,                                                                                                                                                                           "Fish species observed downstream of point (on the same stream/blue_line_key)",
                                                                 "observedspp_upstr",                                     "Observedspp Upstr",       NA,       NA,                                                                                                                                                                                                       "Fish species observed anywhere upstream of point",
                                                                "watershed_upstr_ha",                                  "Watershed Upstr (ha)",       NA,       NA,                                                                                                                       "Total watershed area upstream of point (approximate, does not include area of the fundamental watershed in which the point lies)",
                                                                  "total_network_km",                                    "Total Network (km)",       NA,       NA,                                                                                                                                                                                                       "Total length of stream network upstream of point",
                                                                   "total_stream_km",                                     "Total Stream (km)",       NA,       NA,                                                                                                                                                "Total length of streams and rivers upstream of point (does not include network connectors in lakes etc)",
                                                            "total_lakereservoir_ha",                             "Total Lake Reservoir (ha)",       NA,       NA,                                                                                                                                                                                                      "Total area lakes and reservoirs upstream of point",
                                                                  "total_wetland_ha",                                    "Total Wetland (ha)",       NA,       NA,                                                                                                                                                                                                                  "Total area wetlands upstream of point",
                                                 "total_slopeclass03_waterbodies_km",                   "Total Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                                                            "Total length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                                             "total_slopeclass03_km",                               "Total Slopeclass03 (km)",       NA,       NA,                                                                                                                                                                        "Total length of stream potentially accessible upstream of point with slope 0-3%",
                                                             "total_slopeclass05_km",                               "Total Slopeclass05 (km)",       NA,       NA,                                                                                                                                                                        "Total length of stream potentially accessible upstream of point with slope 3-5%",
                                                             "total_slopeclass08_km",                               "Total Slopeclass08 (km)",       NA,       NA,                                                                                                                                                                        "Total length of stream potentially accessible upstream of point with slope 5-8%",
                                                             "total_slopeclass15_km",                               "Total Slopeclass15 (km)",       NA,       NA,                                                                                                                                                                       "Total length of stream potentially accessible upstream of point with slope 8-15%",
                                                             "total_slopeclass22_km",                               "Total Slopeclass22 (km)",       NA,       NA,                                                                                                                                                                      "Total length of stream potentially accessible upstream of point with slope 15-22%",
                                                             "total_slopeclass30_km",                               "Total Slopeclass30 (km)",       NA,       NA,                                                                                                                                                                      "Total length of stream potentially accessible upstream of point with slope 22-30%",
                                               "total_belowupstrbarriers_network_km",                     "Total Below Barriers Network (km)",       NA,       NA,                                                                                                                                     "Total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                                                "total_belowupstrbarriers_stream_km",                      "Total Below Barriers Stream (km)",       NA,       NA,                                                                              "Total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                                         "total_belowupstrbarriers_lakereservoir_ha",              "Total Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                                                    "Total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                                               "total_belowupstrbarriers_wetland_ha",                     "Total Below Barriers Wetland (ha)",       NA,       NA,                                                                                                                                                "Total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                              "total_belowupstrbarriers_slopeclass03_waterbodies_km",    "Total Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                "Total length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                          "total_belowupstrbarriers_slopeclass03_km",                "Total Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                                                            "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                          "total_belowupstrbarriers_slopeclass05_km",                "Total Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                                                            "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                                          "total_belowupstrbarriers_slopeclass08_km",                "Total Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                                                            "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                                          "total_belowupstrbarriers_slopeclass15_km",                "Total Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                                                           "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                                          "total_belowupstrbarriers_slopeclass22_km",                "Total Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                                                          "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                                          "total_belowupstrbarriers_slopeclass30_km",                "Total Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                                                          "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                                                               "ch_co_sk_network_km",                                 "CH CO SK Network (km)",       NA,       NA,                                                                                                                                             "Chinook/Coho/Sockeye salmon model, total length of stream network potentially accessible upstream of point",
                                                                "ch_co_sk_stream_km",                                  "CH CO SK Stream (km)",       NA,       NA,                                                                                      "Chinook/Coho/Sockeye salmon model, total length of streams and rivers potentially accessible upstream of point (does not include network connectors in lakes etc)",
                                                         "ch_co_sk_lakereservoir_ha",                          "CH CO SK Lake Reservoir (ha)",       NA,       NA,                                                                                                                                            "Chinook/Coho/Sockeye salmon model, total area lakes and reservoirs potentially accessible upstream of point",
                                                               "ch_co_sk_wetland_ha",                                 "CH CO SK Wetland (ha)",       NA,       NA,                                                                                                                                                        "Chinook/Coho/Sockeye salmon model, total area wetlands potentially accessible upstream of point",
                                              "ch_co_sk_slopeclass03_waterbodies_km",                "CH CO SK Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                               "Chinook/Coho/Sockeye salmon model, length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                                          "ch_co_sk_slopeclass03_km",                            "CH CO SK Slopeclass03 (km)",       NA,       NA,                                                                                                                                           "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 0-3%",
                                                          "ch_co_sk_slopeclass05_km",                            "CH CO SK Slopeclass05 (km)",       NA,       NA,                                                                                                                                           "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 3-5%",
                                                          "ch_co_sk_slopeclass08_km",                            "CH CO SK Slopeclass08 (km)",       NA,       NA,                                                                                                                                           "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 5-8%",
                                                          "ch_co_sk_slopeclass15_km",                            "CH CO SK Slopeclass15 (km)",       NA,       NA,                                                                                                                                          "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 8-15%",
                                                          "ch_co_sk_slopeclass22_km",                            "CH CO SK Slopeclass22 (km)",       NA,       NA,                                                                                                                                         "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 15-22%",
                                                          "ch_co_sk_slopeclass30_km",                            "CH CO SK Slopeclass30 (km)",       NA,       NA,                                                                                                                                         "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 22-30%",
                                            "ch_co_sk_belowupstrbarriers_network_km",                  "CH CO SK Below Barriers Network (km)",       NA,       NA,                                                                                                  "Chinook/Coho/Sockeye salmon model, total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                                             "ch_co_sk_belowupstrbarriers_stream_km",                   "CH CO SK Below Barriers Stream (km)",       NA,       NA,                                           "Chinook/Coho/Sockeye salmon model, total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                                      "ch_co_sk_belowupstrbarriers_lakereservoir_ha",           "CH CO SK Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                 "Chinook/Coho/Sockeye salmon model, total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                                            "ch_co_sk_belowupstrbarriers_wetland_ha",                  "CH CO SK Below Barriers Wetland (ha)",       NA,       NA,                                                                                                             "Chinook/Coho/Sockeye salmon model, total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                           "ch_co_sk_belowupstrbarriers_slopeclass03_waterbodies_km", "CH CO SK Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                   "Chinook/Coho/Sockeye salmon model, length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass03_km",             "CH CO SK Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                               "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass05_km",             "CH CO SK Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                               "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass08_km",             "CH CO SK Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                               "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass15_km",             "CH CO SK Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                              "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass22_km",             "CH CO SK Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                             "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass30_km",             "CH CO SK Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                             "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                                                                     "st_network_km",                                       "ST Network (km)",       NA,       NA,                                                                                                                                                               "Steelhead model, total length of stream network potentially accessible upstream of point",
                                                                      "st_stream_km",                                        "ST Stream (km)",       NA,       NA,                                                                                                        "Steelhead model, total length of streams and rivers potentially accessible upstream of point (does not include network connectors in lakes etc)",
                                                               "st_lakereservoir_ha",                                "ST Lake Reservoir (ha)",       NA,       NA,                                                                                                                                                              "Steelhead model, total area lakes and reservoirs potentially accessible upstream of point",
                                                                     "st_wetland_ha",                                       "ST Wetland (ha)",       NA,       NA,                                                                                                                                                                          "Steelhead model, total area wetlands potentially accessible upstream of point",
                                                    "st_slopeclass03_waterbodies_km",                      "ST Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                                                 "Steelhead model, length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                                                "st_slopeclass03_km",                                  "ST Slopeclass03 (km)",       NA,       NA,                                                                                                                                                             "Steelhead model, length of stream potentially accessible upstream of point with slope 0-3%",
                                                                "st_slopeclass05_km",                                  "ST Slopeclass05 (km)",       NA,       NA,                                                                                                                                                             "Steelhead model, length of stream potentially accessible upstream of point with slope 3-5%",
                                                                "st_slopeclass08_km",                                  "ST Slopeclass08 (km)",       NA,       NA,                                                                                                                                                             "Steelhead model, length of stream potentially accessible upstream of point with slope 5-8%",
                                                                "st_slopeclass15_km",                                  "ST Slopeclass15 (km)",       NA,       NA,                                                                                                                                                            "Steelhead model, length of stream potentially accessible upstream of point with slope 8-15%",
                                                                "st_slopeclass22_km",                                  "ST Slopeclass22 (km)",       NA,       NA,                                                                                                                                                           "Steelhead model, length of stream potentially accessible upstream of point with slope 15-22%",
                                                                "st_slopeclass30_km",                                  "ST Slopeclass30 (km)",       NA,       NA,                                                                                                                                                           "Steelhead model, length of stream potentially accessible upstream of point with slope 22-30%",
                                                  "st_belowupstrbarriers_network_km",                        "ST Below Barriers Network (km)",       NA,       NA,                                                                                                                    "Steelhead model, total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                                                   "st_belowupstrbarriers_stream_km",                         "ST Below Barriers Stream (km)",       NA,       NA,                                                             "Steelhead model, total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                                            "st_belowupstrbarriers_lakereservoir_ha",                 "ST Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                                   "Steelhead model, total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                                                  "st_belowupstrbarriers_wetland_ha",                        "ST Below Barriers Wetland (ha)",       NA,       NA,                                                                                                                               "Steelhead model, total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                                 "st_belowupstrbarriers_slopeclass03_waterbodies_km",       "ST Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                     "Steelhead model, length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                             "st_belowupstrbarriers_slopeclass03_km",                   "ST Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                                                 "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                             "st_belowupstrbarriers_slopeclass05_km",                   "ST Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                                                 "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                                             "st_belowupstrbarriers_slopeclass08_km",                   "ST Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                                                 "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                                             "st_belowupstrbarriers_slopeclass15_km",                   "ST Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                                                "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                                             "st_belowupstrbarriers_slopeclass22_km",                   "ST Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                                               "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                                             "st_belowupstrbarriers_slopeclass30_km",                   "ST Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                                               "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                                                                    "wct_network_km",                                      "WCT Network (km)",     117L,       1L,                                                                                                                                                "Westslope Cuthroat Trout model, total length of stream network potentially accessible upstream of point",
                                                                     "wct_stream_km",                                       "WCT Stream (km)",     115L,       1L,                                                                                         "Westslope Cuthroat Trout model, total length of streams and rivers potentially accessible upstream of point (does not include network connectors in lakes etc)",
                                                              "wct_lakereservoir_ha",                               "WCT Lake Reservoir (ha)",     120L,       1L,                                                                                                                                               "Westslope Cuthroat Trout model, total area lakes and reservoirs potentially accessible upstream of point",
                                                                    "wct_wetland_ha",                                      "WCT Wetland (ha)",     125L,       1L,                                                                                                                                                           "Westslope Cuthroat Trout model, total area wetlands potentially accessible upstream of point",
                                                   "wct_slopeclass03_waterbodies_km",                     "WCT Slopeclass03 Waterbodies (km)",     130L,       1L,                                                                                                                 "Westslope Cutthroat Trout model, length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                                               "wct_slopeclass03_km",                                 "WCT Slopeclass03 (km)",     140L,       1L,                                                                                                                                             "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 0-3%",
                                                               "wct_slopeclass05_km",                                 "WCT Slopeclass05 (km)",     150L,       1L,                                                                                                                                             "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 3-5%",
                                                               "wct_slopeclass08_km",                                 "WCT Slopeclass08 (km)",     160L,       1L,                                                                                                                                             "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 5-8%",
                                                               "wct_slopeclass15_km",                                 "WCT Slopeclass15 (km)",     170L,       1L,                                                                                                                                            "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 8-15%",
                                                               "wct_slopeclass22_km",                                 "WCT Slopeclass22 (km)",     180L,       1L,                                                                                                                                           "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 15-22%",
                                                               "wct_slopeclass30_km",                                 "WCT Slopeclass30 (km)",       NA,       NA,                                                                                                                                           "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 22-30%",
                                                 "wct_belowupstrbarriers_network_km",                       "WCT Below Barriers Network (km)",     117L,       2L,                                                                                                    "Westslope Cutthroat Trout model, total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                                                  "wct_belowupstrbarriers_stream_km",                        "WCT Below Barriers Stream (km)",     115L,       2L,                                              "Westslope Cuthroat Trout model, total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                                           "wct_belowupstrbarriers_lakereservoir_ha",                "WCT Below Barriers Lake Reservoir (ha)",     120L,       2L,                                                                                                   "Westslope Cutthroat Trout model, total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                                                 "wct_belowupstrbarriers_wetland_ha",                       "WCT Below Barriers Wetland (ha)",     125L,       2L,                                                                                                               "Westslope Cutthroat Trout model, total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                                "wct_belowupstrbarriers_slopeclass03_waterbodies_km",      "WCT Below Barriers Slopeclass03 Waterbodies (km)",     130L,       2L,                                                                     "Westslope Cutthroat Trout model, length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                            "wct_belowupstrbarriers_slopeclass03_km",                  "WCT Below Barriers Slopeclass03 (km)",     140L,       2L,                                                                                                 "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                            "wct_belowupstrbarriers_slopeclass05_km",                  "WCT Below Barriers Slopeclass05 (km)",     150L,       2L,                                                                                                 "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                                            "wct_belowupstrbarriers_slopeclass08_km",                  "WCT Below Barriers Slopeclass08 (km)",     160L,       2L,                                                                                                 "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                                            "wct_belowupstrbarriers_slopeclass15_km",                  "WCT Below Barriers Slopeclass15 (km)",     170L,       2L,                                                                                                "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                                            "wct_belowupstrbarriers_slopeclass22_km",                  "WCT Below Barriers Slopeclass22 (km)",     180L,       2L,                                                                                               "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                                            "wct_belowupstrbarriers_slopeclass30_km",                  "WCT Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                               "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                                                                    "ch_spawning_km",                                      "CH Spawning (km)",       NA,       NA,                                                                                                                                                                      "Length of stream upstream of point modelled as potential Chinook spawning habitat",
                                                                     "ch_rearing_km",                                       "CH Rearing (km)",       NA,       NA,                                                                                                                                                                       "Length of stream upstream of point modelled as potential Chinook rearing habitat",
                                                 "ch_spawning_belowupstrbarriers_km",                       "CH Spawning Below Barriers (km)",       NA,       NA,                                                                                                                          "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Chinook spawning habitat",
                                                  "ch_rearing_belowupstrbarriers_km",                        "CH Rearing Below Barriers (km)",       NA,       NA,                                                                                                                           "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Chinook rearing habitat",
                                                                    "co_spawning_km",                                      "CO Spawning (km)",       NA,       NA,                                                                                                                                                                         "Length of stream upstream of point modelled as potential Coho spawning habitat",
                                                                     "co_rearing_km",                                       "CO Rearing (km)",       NA,       NA,                                                                                                                                                                          "Length of stream upstream of point modelled as potential Coho rearing habitat",
                                                                     "co_rearing_ha",                                       "CO Rearing (ha)",       NA,       NA,                                                                                                                                                                          "Area of wetlands upstream of point modelled as potential Coho rearing habitat",
                                                 "co_spawning_belowupstrbarriers_km",                       "CO Spawning Below Barriers (km)",       NA,       NA,                                                                                                                             "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Coho spawning habitat",
                                                  "co_rearing_belowupstrbarriers_km",                        "CO Rearing Below Barriers (km)",       NA,       NA,                                                                                                                              "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Coho rearing habitat",
                                                  "co_rearing_belowupstrbarriers_ha",                        "CO Rearing Below Barriers (ha)",       NA,       NA,                                                                                                                              "Area of wetlands upstream of point and below any additional upstream barriers, modelled as potential Coho rearing habitat",
                                                                    "sk_spawning_km",                                      "SK Spawning (km)",       NA,       NA,                                                                                                                                                                      "Length of stream upstream of point modelled as potential Sockeye spawning habitat",
                                                                     "sk_rearing_km",                                       "SK Rearing (km)",       NA,       NA,                                                                                                                                                                       "Length of stream upstream of point modelled as potential Sockeye rearing habitat",
                                                                     "sk_rearing_ha",                                       "SK Rearing (ha)",       NA,       NA,                                                                                                                                                                          "Area of lakes upstream of point modelled as potential Sockeye rearing habitat",
                                                 "sk_spawning_belowupstrbarriers_km",                       "SK Spawning Below Barriers (km)",       NA,       NA,                                                                                                                          "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Sockeye spawning habitat",
                                                  "sk_rearing_belowupstrbarriers_km",                        "SK Rearing Below Barriers (km)",       NA,       NA,                                                                                                                           "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Sockeye rearing habitat",
                                                  "sk_rearing_belowupstrbarriers_ha",                        "SK Rearing Below Barriers (ha)",       NA,       NA,                                                                                                                              "Area of lakes upstream of point and below any additional upstream barriers, modelled as potential Sockeye rearing habitat",
                                                                    "st_spawning_km",                                      "ST Spawning (km)",       NA,       NA,                                                                                                                                                                    "Length of stream upstream of point modelled as potential Steelhead spawning habitat",
                                                                     "st_rearing_km",                                       "ST Rearing (km)",       NA,       NA,                                                                                                                                                                     "Length of stream upstream of point modelled as potential Steelhead rearing habitat",
                                                 "st_spawning_belowupstrbarriers_km",                       "ST Spawning Below Barriers (km)",       NA,       NA,                                                                                                                        "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Steelhead spawning habitat",
                                                  "st_rearing_belowupstrbarriers_km",                        "ST Rearing Below Barriers (km)",       NA,       NA,                                                                                                                         "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Steelhead rearing habitat",
                                                                   "wct_spawning_km",                                     "WCT Spawning (km)",     090L,       1L,                                                                                                                                                          "Length of stream upstream of point modelled as potential Westslope Cutthroat spawning habitat",
                                                                    "wct_rearing_km",                                      "WCT Rearing (km)",     100L,       1L,                                                                                                                                                           "Length of stream upstream of point modelled as potential Westslope Cutthroat rearing habitat",
                                                "wct_spawning_belowupstrbarriers_km",                      "WCT Spawning Below Barriers (km)",     090L,       2L,                                                                                                              "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Westslope Cutthroat spawning habitat",
                                                 "wct_rearing_belowupstrbarriers_km",                       "WCT Rearing Below Barriers (km)",     100L,       2L,                                                                                                               "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Westslope Cutthroat rearing habitat",
                                                                   "all_spawning_km",                                     "All Spawning (km)",       NA,       NA,                                                                                                                                                        "Length of stream upstream of point modelled as potential spawning habitat (all CH,CO,SK,ST,WCT)",
                                                "all_spawning_belowupstrbarriers_km",                      "All Spawning Below Barriers (km)",       NA,       NA,                                                                                                                                                         "Length of stream upstream of point modelled as potential rearing habitat (all CH,CO,SK,ST,WCT)",
                                                                    "all_rearing_km",                                      "All Rearing (km)",       NA,       NA,                                                                                                            "Length of stream upstream of point and below any additional upstream barriers, modelled as potential spawning habitat (all CH,CO,SK,ST,WCT)",
                                                 "all_rearing_belowupstrbarriers_km",                       "All Rearing Below Barriers (km)",       NA,       NA,                                                                                                             "Length of stream upstream of point and below any additional upstream barriers, modelled as potential rearing habitat (all CH,CO,SK,ST,WCT)",
                                                            "all_spawningrearing_km",                             "All Spawning Rearing (km)",       NA,       NA,                                                                                                                                                                                           "Length of all spawning and rearing habitat upstream of point",
                                         "all_spawningrearing_belowupstrbarriers_km",              "All Spawning Rearing Below Barriers (km)",       NA,       NA,                                                                                                                                                                                           "Length of all spawning and rearing habitat upstream of point",
                                                    "wct_betweenbarriers_network_km",                     "WCT Between Barriers Network (km)",       NA,       NA,                                                                                                            "Westslope Cutthroat Trout model, total length of potentially accessible stream network between crossing and all in-stream adjacent barriers",
                                                   "wct_spawning_betweenbarriers_km",                    "WCT Spawning Between Barriers (km)",       NA,       NA,                                                                                                                                 "Westslope Cutthroat Trout model, total length of spawning habitat between crossing and all in-stream adjacent barriers",
                                                    "wct_rearing_betweenbarriers_km",                     "WCT Rearing Between Barriers (km)",       NA,       NA,                                                                                                                                  "Westslope Cutthroat Trout model, total length of rearing habitat between crossing and all in-stream adjacent barriers",
                                            "wct_spawningrearing_betweenbarriers_km",            "WCT Spawning Rearing Between Barriers (km)",       NA,       NA,                                                                                                                     "Westslope Cutthroat Trout model, total length of spawning and rearing habitat between crossing and all in-stream adjacent barriers",
                                                   "all_spawningrearing_per_barrier",                      "All Spawning Rearing Per Barrier",       NA,       NA, "If the given barrier and all barriers downstream were remediated, the amount of connected spawning/rearing habitat that would be added, per barrier. (ie the sum of all_spawningrearing_belowupstrbarriers_km for all barriers, divided by n barriers)"
                           )

####-----------overview table------------

# tab_overview_prep1 <- pscis_phase2 %>%
#   select(pscis_crossing_id, stream_name, road_name, road_tenure, easting, northing, habitat_value)
#
# tab_overview_prep2 <- habitat_confirmations_priorities %>%
#   filter(location == 'us') %>%
#   select(site, species_codes, upstream_habitat_length_m, priority, comments) %>%
#   mutate(upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1))
#
# tab_overview <- left_join(
#   tab_overview_prep1,
#   tab_overview_prep2,
#   by = c('pscis_crossing_id' = 'site')
# ) %>%
#   mutate(utm = paste0(round(easting,0), ' ', round(northing,0))) %>%
#   select(`PSCIS ID` = pscis_crossing_id,
#          Stream = stream_name,
#          Road = road_name,
#          Tenure = road_tenure,
#          `UTM (11U)` = utm,
#          `Fish Species` = species_codes,
#          `Habitat Gain (km)` = upstream_habitat_length_km,
#          `Habitat Value` = habitat_value,
#          Priority = priority,
#          Comments = comments )
# # mutate(test = paste0('[', Site, ']', '(Appendix 1 - Site Assessment Data and Photos)'))##hmm.. thought this worked
# # %>%
# #   replace(., is.na(.), "-")
#
#
# rm(tab_overview_prep1, tab_overview_prep2)
#
# ####---------habitat summary--------------------------------

tab_hab_summary <- left_join(
  hab_site %>%
    select(alias_local_name,
           site,
           avg_channel_width_m,
           avg_wetted_width_m,
           average_residual_pool_depth_m,
           average_gradient_percent,
           total_cover),

  habitat_confirmations_priorities %>%
    select(local_name,
           length_surveyed,
           hab_value),

  by = c('alias_local_name' = 'local_name')
) %>%
  arrange(alias_local_name) %>%
  select(Site = alias_local_name,
         `Length Surveyed (m)` = length_surveyed,
         `Channel Width (m)` = avg_channel_width_m,
         `Wetted Width (m)` = avg_wetted_width_m,
         `Pool Depth (m)` = average_residual_pool_depth_m,
         `Gradient (%)` = average_gradient_percent,
         `Total Cover` = total_cover,
         `Habitat Value` = hab_value) %>%
  # filter out non ef sites for now, can undo later if needed
  filter(Site != "197534_us4") %>%
  filter(Site != "197534_us5")

# # -------------------------map tables
#
# ##we need an sf object with details for the interactive map
# ##prep the location data
# hab_loc_prep <- left_join(
#   hab_loc %>%
#     tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
#     filter(!alias_local_name %ilike% 'ef' &
#              alias_local_name %ilike% 'us') %>%
#     mutate(site = as.integer(site)),
#   select(filter(habitat_confirmations_priorities, location == 'us'),
#          site, priority, comments),
#   by = 'site'
# )
#
#
# ##need to populate the coordinates before this will work
# ####please note that the photos are only in those files ecause they are referenced in other parts
# ##of the document
# tab_hab_map <- left_join(
#   tab_cost_est_phase2 %>% filter(source %like% 'phase2'),
#   hab_loc_prep %>% select(site, priority, utm_easting, utm_northing, comments),
#   by = c('pscis_crossing_id' = 'site')
# )   %>%
#   sf::st_as_sf(coords = c("utm_easting", "utm_northing"),
#                crs = 26911, remove = F) %>%
#   sf::st_transform(crs = 4326) %>%
#   ##changed this to docs .html from fig .png
#   # mutate(data_link = paste0('<a href =',
#   #                           'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/docs/sum/', pscis_crossing_id,
#   #                           '.html', '>', 'data link', '</a>')) %>%
#   mutate(data_link = paste0('<a href =', 'sum/cv/', pscis_crossing_id, '.html ', 'target="_blank">Culvert Data</a>')) %>%
#   mutate(photo_link = paste0('<a href =', 'https://raw.githubusercontent.com/NewGraphEnvironment/fish_passage_elk_2021_reporting/master/data/photos/', pscis_crossing_id, '/crossing_all.JPG ',
#                              'target="_blank">Culvert Photos</a>')) %>%
#   mutate(model_link = paste0('<a href =', 'sum/bcfp/', pscis_crossing_id, '.html ', 'target="_blank">Model Data</a>'))
# # mutate(photo_link = paste0('<a href =', 'data/photos/', pscis_crossing_id,
# #                           '/crossing_all.JPG', '>', 'Photos', '>New Tab</a>'))
# # mutate(data_link = paste0('[data](fig/sum/', pscis_crossing_id, '.png)')) %>%
# # mutate(photo_link = paste0('<a href =',
# #                            'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/data/photos/', pscis_crossing_id,
# #                            '/crossing_all.JPG', '>', 'photo link', '</a>'))
#
#
# #--------------need to review if this is necessary
# tab_map_prep <- left_join(
#   pscis_all %>%
#     sf::st_as_sf(coords = c("easting", "northing"),
#                  crs = 26911, remove = F) %>% ##don't forget to put it in the right crs buds
#     sf::st_transform(crs = 4326), ##convert to match the bcfishpass format,
#   phase1_priorities %>% select(-utm_zone:utm_northing, -my_crossing_reference, priority_phase1, -habitat_value, -barrier_result), # %>% st_drop_geometry()
#   by = 'pscis_crossing_id'
# )
#
# # mutate(data_link = paste0('<a href =', 'sum/', pscis_crossing_id,
# #                           '.html', '>', 'Data', '>New Tab</a>'))
#
# tab_map <- tab_map_prep %>%
#   # mutate(pscis_crossing_id = as.character(pscis_crossing_id),
#   #        my_crossing_reference = as.character(my_crossing_reference)) %>%
#   # mutate(ID = case_when(
#   #   !is.na(pscis_crossing_id) ~ pscis_crossing_id,
#   #   T ~ paste0('*', my_crossing_reference
#   #   ))) %>%
#   # sf::st_as_sf(coords = c("utm_easting", "utm_northing"),
#   #              crs = 26911, remove = F) %>%
#   # sf::st_transform(crs = 4326) %>%
#   mutate(priority_phase1 = case_when(priority_phase1 == 'mod' ~ 'moderate',
#                                      T ~ priority_phase1)) %>%
#   mutate(data_link = paste0('<a href =', 'sum/cv/', pscis_crossing_id, '.html ', 'target="_blank">Culvert Data</a>')) %>%
#   mutate(photo_link = paste0('<a href =', 'data/photos/', amalgamated_crossing_id, '/crossing_all.JPG ',
#                              'target="_blank">Culvert Photos</a>')) %>%
#   mutate(model_link = paste0('<a href =', 'sum/bcfp/', pscis_crossing_id, '.html ', 'target="_blank">Model Data</a>'))
# # mutate(data_link = paste0('<a href =',
# #                           'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/fig/sum/', pscis_crossing_id,
# #                           '.png', '>', 'data link', '</a>')) %>%
# # dplyr::mutate(photo_link = paste0('<a href =',
# #                                   'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/data/photos/', amalgamated_crossing_id,
# #                                   '/crossing_all.JPG', '>', 'photo link', '</a>'))

##clean up the objects
rm(hab_site_prep,
   # hab_fish_indiv_prep,
   # hab_fish_indiv_prep2,
   hab_fish_collect_prep2,
   hab_loc2)
