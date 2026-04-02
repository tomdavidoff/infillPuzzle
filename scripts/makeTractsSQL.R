# makeTractsSQL.R
# R to make tracts list an SQL for fast query
# Tom Davidoff 
# 04/01/26

library(duckdb)
library(data.table)

library(duckdb)
library(data.table)

# Setup a connection to a temporary DuckDB
dt <- fread("~/DropboxExternal/dataProcessed/attomTract.tsv")
con <- dbConnect(duckdb())

# Assume your data is 'my_data'
# We 'register' it so DuckDB can see it without copying it first
duckdb_register(con, "my_df_view", dt)

# Export to Parquet with maximum compression (ZSTD)
# We set row_group_size to 100,000 for a good balance of size and speed
dbExecute(con, "
  COPY my_df_view TO 'foo.parquet'
  (FORMAT PARQUET, COMPRESSION 'ZSTD', ROW_GROUP_SIZE 100000)
")

dbDisconnect(con)

