# database.R
#
# William John Zywiec
# The George Washington University
#
# ...

# load packages
library(DBI)

con <- dbConnect(RSQLite::SQLite(), ':memory:')

dbListTables(con)

