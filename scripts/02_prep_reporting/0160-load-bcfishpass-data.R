## we can switch this to pull from our own system but we will just use simons bcfishpass for now to save time
## simons db will not have the updated pscis names so we we include workflows to see that our new pscis crossings match our old modelled crossings

source('scripts/packages.R')
#source('R/private_info.R')


conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv('PG_DB_BCBARRIERS'),
  host = Sys.getenv('PG_HOST_BCBARRIERS'),
  port = Sys.getenv('PG_PORT_BCBARRIERS'),
  user = Sys.getenv('PG_USER_BCBARRIERS'),
  password = Sys.getenv('PG_PASS_BCBARRIERS')
)

#
# ##listthe schemas in the database
# dbGetQuery(conn,
#            "SELECT schema_name
#            FROM information_schema.schemata")
# #
# # # ##list tables in a schema
dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='bcfishpass'") %>%
  filter(table_name %ilike% 'prec')
# # # # # #
# # # # # # ##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='mean_annual_precip'")


##get all the data and save it as an sqlite database as a snapshot of what is happening.  we can always hopefully update it
query <- "SELECT *
   FROM bcfishpass.crossings
   WHERE watershed_group_code IN ('TABR', 'BABL', 'SAJR')"


##import and grab the coordinates - this is already done
bcfishpass <- st_read(conn, query =  query) %>%
  # st_transform(crs = 26911) %>%  #before the coordinates were switched but now they look fine...
  # mutate(
  #        easting = sf::st_coordinates(.)[,1],
  #        northing = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()


query <- "select col_description((table_schema||'.'||table_name)::regclass::oid, ordinal_position) as column_comment,
* from information_schema.columns
WHERE table_schema = 'bcfishpass'
and table_name = 'crossings';"

bcfishpass_column_comments <- st_read(conn, query =  query) %>%
  select(column_name, column_comment)

# porphyryr <- st_read(conn, query =
# "SELECT * FROM bcfishpass.crossings
#    WHERE stream_crossing_id = '124487'")

dbDisconnect(conn = conn)


##this is how we update our local db.
##my time format format(Sys.time(), "%Y%m%d-%H%M%S")
# mydb <- DBI::dbConnect(RSQLite::SQLite(), "data/bcfishpass.sqlite")
conn <- rws_connect("data/bcfishpass.sqlite")
rws_list_tables(conn)
##archive the last version for now
# bcfishpass_archive <- readwritesqlite::rws_read_table("bcfishpass", conn = conn)
# # rws_drop_table("bcfishpass_archive", conn = conn) ##if it exists get rid of it - might be able to just change exists to T in next line
# rws_write(bcfishpass_archive, exists = F, delete = TRUE,
#           conn = conn, x_name = paste0("bcfishpass_archive_", format(Sys.time(), "%Y-%m-%d-%H%m")))
rws_drop_table("bcfishpass", conn = conn) ##now drop the table so you can replace it
rws_write(bcfishpass, exists = F, delete = TRUE,
          conn = conn, x_name = "bcfishpass")
# add the comments
# bcfishpass_column_comments_archive <- readwritesqlite::rws_read_table("bcfishpass_column_comments", conn = conn)
# rws_write(bcfishpass_column_comments_archive, exists = F, delete = TRUE,
#           conn = conn, x_name = paste0("bcfishpass_column_comments_archive_", format(Sys.time(), "%Y-%m-%d-%H%m")))
rws_drop_table("bcfishpass_column_comments", conn = conn) ##now drop the table so you can replace it
rws_write(bcfishpass_column_comments, exists = F, delete = TRUE,
          conn = conn, x_name = "bcfishpass_column_comments")
# rws_drop_table("my_pscis_modelledcrossings_streams_xref", conn = conn)
# rws_write(my_pscis_modelledcrossings_streams_xref, exists = FALSE, delete = TRUE,
#           conn = conn, x_name = "my_pscis_modelledcrossings_streams_xref")
rws_list_tables(conn)
rws_disconnect(conn)

##grab the bcfishpass spawning and rearing table and put in the database so it can be used to populate the methods and tie to the references table
urlfile="https://raw.githubusercontent.com/smnorris/bcfishpass/main/data/parameters_habitat_thresholds.csv"

bcfishpass_spawn_rear_model <- read_csv(url(urlfile))


# put it in the db
conn <- rws_connect("data/bcfishpass.sqlite")
rws_list_tables(conn)
# archive <- readwritesqlite::rws_read_table("bcfishpass_spawn_rear_model", conn = conn)
# rws_write(archive, exists = F, delete = TRUE,
#           conn = conn, x_name = paste0("bcfishpass_spawn_rear_model_archive_", format(Sys.time(), "%Y-%m-%d-%H%m")))
# rws_drop_table("bcfishpass_spawn_rear_model", conn = conn)
rws_write(bcfishpass_spawn_rear_model, exists = F, delete = TRUE,
          conn = conn, x_name = "bcfishpass_spawn_rear_model")
rws_list_tables(conn)
rws_disconnect(conn)
