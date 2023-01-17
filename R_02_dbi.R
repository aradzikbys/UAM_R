require(DBI)
require(RSQLite)
# install.packages("RMySQL")
library(RMySQL)
library(tidyverse)

# establish connection with MySQL db:
# DB info: https://docs.rfam.org/en/latest/database.html
con <- dbConnect(RMySQL::MySQL(),
                            dbname = 'Rfam',
                            host = 'mysql-rfam-public.ebi.ac.uk',
                            port = 4497,
                            user = 'rfamro')
# Check tables
dbListTables(con)
# Check fields within table
dbListFields(con, 'family')


#DBI
dbGetQuery(con, 'SELECT COUNT(rfam_acc) from family')
dbGetQuery(con, 'SELECT COUNT(*) AS cnt from family')

#DPLYR
tbl(con, 'family') %>% count()

dbDisconnect(con)




