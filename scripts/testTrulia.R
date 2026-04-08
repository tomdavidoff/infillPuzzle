# testTrulia.R
# see if can match square footage in Minneapolis
# Tom Davidoff 
# 04/07/26

library(data.table)
library(duckdb)



# enter in a table by hand with street name and number and square footage
dTrulia <- data.table(number=21,street="Russell Ave S",sqft=1948,zip=55405)
dTrulia <- rbind(dTrulia,data.table(number=3482,street="Fremont Ave N",sqft=2088,zip=55412))
dTrulia <- rbind(dTrulia,data.table(number=5036,street="11th Ave S",sqft=2646,zip=55417))
dTrulia <- rbind(dTrulia,data.table(number=5233,street="Aldrich Ave N",sqft=1736,zip=55430))
dTrulia <- rbind(dTrulia,data.table(number=1715,street="W 31st St",sqft=3154,zip=55408))
dTrulia <- rbind(dTrulia,data.table(number=3017,street="James Ave S",sqft=1961,zip=55408))
dTrulia <- rbind(dTrulia,data.table(number=3013,street="James Ave S",sqft=4232,zip=55408))
dTrulia <- rbind(dTrulia,data.table(number=1510,street="W 31st St",sqft=2173,zip=55408))
dTrulia <- rbind(dTrulia,data.table(number=2511,street="Washington St NE",sqft=1321,zip=55418))
dTrulia <- rbind(dTrulia,data.table(number=2790,street="Dean Pkwy",sqft=1646,zip=55416))
dTrulia <- rbind(dTrulia,data.table(number=4453,street="44th Ave S",sqft=2152,zip=55406))
dTrulia <- rbind(dTrulia,data.table(number=5608,street="Colfax Ave S",sqft=2736,zip=55419))
dTrulia <- rbind(dTrulia,data.table(number=5413,street="Oliver Ave S",sqft=2518,zip=55419))
dTrulia <- rbind(dTrulia,data.table(number=4534,street="Queen Ave N",sqft=588,zip=55412))
dTrulia <- rbind(dTrulia,data.table(number=5321,street="Drew Ave S",sqft=3169,zip=55410))
dTrulia <- rbind(dTrulia,data.table(number=3135,street="Pierce St NE",sqft=1440,zip=55418))
dTrulia <- rbind(dTrulia,data.table(number=205,street="W 52nd St",sqft=3157,zip=55419))
dTrulia <- rbind(dTrulia,data.table(number=5052,street="Morgan Ave S",sqft=2448,zip=55419))
dTrulia <- rbind(dTrulia,data.table(number=5204,street="Abbott Ave S",sqft=2688,zip=55410))
dTrulia <- rbind(dTrulia,data.table(number=5157,street="Washburn Ave S",sqft=1882,zip=55410))
dTrulia <- rbind(dTrulia,data.table(number=5324,street="Nokomis Ave",sqft=581,zip=55417))
dTrulia <- rbind(dTrulia,data.table(number=4017,street="44th Ave S",sqft=1756,zip=55406))
dTrulia <- rbind(dTrulia,data.table(number=3331,street="Tyler St NE",sqft=1988,zip=55418))
dTrulia <- rbind(dTrulia,data.table(number=3032,street="Longfellow Ave",sqft=2759,zip=55407))
dTrulia <- rbind(dTrulia,data.table(number=5816,street="Queen Ave S",sqft=2128,zip=55410))
dTrulia <- rbind(dTrulia,data.table(number=2867,street="Kenwood Isles Dr",sqft=1540,zip=55408))
dTrulia <- rbind(dTrulia,data.table(number=2804,street="Pillsbury Ave S",sqft=1684,zip=55408))
dTrulia <- rbind(dTrulia,data.table(number=201,street="W 48th St",sqft=2783,zip=55419))
dTrulia <- rbind(dTrulia,data.table(number=4340,street="14th Ave S",sqft=1440,zip=55407))
dTrulia <- rbind(dTrulia,data.table(number=5708,street="37th Ave S",sqft=1256,zip=55417))
dTrulia <- rbind(dTrulia,data.table(number=4008,street="Garfield Ave",sqft=2664,zip=55409))
dTrulia <- rbind(dTrulia,data.table(number=406,street="River St",sqft=5167,zip=55401))

# do fuzzy match in SQL, like number, street with any capitalization and zip match. Then grab AreaBuilding and AreaGross, too
con <- dbConnect(duckdb::duckdb())


for (i in 1:nrow(dTrulia)) {
  print(c("looking for", dTrulia[i,.(number,street,zip,sqft)]))

  # split street into words for fuzzy matching
  words <- strsplit(dTrulia$street[i], "\\s+")[[1]]
  # build ILIKE clause: number at start, each street word anywhere, zip matches
  like_clauses <- paste0("PropertyAddressFull ILIKE '%", words, "%'", collapse = " AND ")

  query <- sprintf(
    "SELECT PropertyAddressFull, PropertyAddressZip, AreaBuilding, AreaGross, RoomsBasementArea
     FROM '~/DropboxExternal/dataProcessed/by_property_states/tax_assessor_state_MN.parquet'
     WHERE CAST(PropertyAddressFull AS VARCHAR) ILIKE '%s%%'
     AND %s
     AND CAST(PropertyAddressZip AS VARCHAR) LIKE '%s%%'",
    dTrulia$number[i],
    like_clauses,
    dTrulia$zip[i]
  )

  result <- dbGetQuery(con, query)
  print(result)
}
