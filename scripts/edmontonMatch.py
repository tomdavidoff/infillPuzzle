# edmontonMatch.py
# Match building permits to assessment
# Strategy: do 2026 and late 2025 permits so old address still valid maybe
# Tom Davidoff
# 07/16/26

#library(data.table)
#library(fixest)
#library(sf)

import polars as pl
import sys
import polars as pl

path = "/Users/tdavidof/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv"

dfA = (
    pl.scan_csv(path, infer_schema_length=0)
    .with_columns(
        (pl.col("Zoning") == "RS").any().over("Account Number").alias("maxRS")
    )
    .filter(pl.col("maxRS"))
)

result = dfA.collect()
print(result.head())
sys.exit()

# convert next 3 lines to polars
dfA = pl.read_csv("~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv",infer_schema_length=0)
dfC = pl.read_csv("~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv",infer_schema_length=0)
dfP = pl.read_csv("~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv",infer_schema_length=0)


# limit to current RS zones
dfA = dfA.with_columns((pl.col("Zoning")=="RS").any().over("Account Number").alias("maxRS"))
print(dfA.head())
# print a table of maxRS by Zoning
vs_table = dfA.pivot_count(on="maxRS", index="Zoning")
print(vs_table)
sys.exit()
dfdfd

#dtA[,table(maxRS==1,Zoning)]

# header rows
#==> /Users/tdavidof/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv <==
#Row ID,PERMIT_DATE,PERMIT_NUMBER,REPORT_PERMIT_DATE,YEAR,MONTH_NUMBER,JOB_CATEGORY,JOB_DESCRIPTION,BUILDING_TYPE,WORK_TYPE,CONSTRUCTION_VALUE,FLOOR_AREA,UNITS_ADDED,ADDRESS,LEGAL_DESCRIPTION,ZONING,NEIGHBOURHOOD_NUMBER,NEIGHBOURHOOD,BIA,COUNT,LATITUDE,LONGITUDE,LOCATION,Geometry Point,Occupancy Date
#1-253374239,2019-07-04,,2019-07-04,2019,7,Commercial Final,To construct an LRT Station (Davies Station) on City Road Right-of-Way,Transportation Terminals (440),(01) New,20150000,39815,0,100 - DAVIES LRT STATION NW,"","",6170,DAVIES INDUSTRIAL WEST,"",1,,,"","",
#
#==> /Users/tdavidof/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv <==
#"Account Number","Assessment Year","Suite","House Number","Street Name","Legal Description","Latitude","Longitude","Point Location","Neighbourhood","Actual Year Built","Garage","Zoning","Lot Size","Assessed Value","Assessment Class 1","Assessment Class % 1","Assessment Class 2","Assessment Class % 2","Assessment Class 3","Assessment Class % 3"
#"1477850","2013",,"10821","150 STREET NW","Plan: 704KS  Block: 68  Lot: 5","53.5533","-113.57936","POINT (-113.57936 53.5533)","HIGH PARK","1955","Y","RF1","681.1240","$269,500.00","RESIDENTIAL","100",,,,
#
#==> /Users/tdavidof/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv <==
#"Account Number","Suite","House Number","Street Name","legal_description","zoning","lot_size","Total Gross Area","year_built","garage","Neighbourhood ID","Neighbourhood","Ward","Latitude","Longitude","Point Location"
#"11219634",,"303","29 STREET SW","Plan: 2322574  Block: 11  Lot: 42","RSF","335.364","183.4","2024","true","6669","ALCES","Sspomitapi","53.4300755348","-113.38586836927","POINT (-113.38586836927416 53.43007553480231)"

## first question: Suites in RS 2025+ completions
#print(head(dtC))
#dtCs <- dtC[zoning=="RS" & year_built>=2025 ]
#print(dtCs[,.(N=.N,hasSuite=sum(Suite!=""))])
# Answer: no suites, that's not how rowhouse for sale would be identified

# edmontonMatch_skeleton.R
# Structure for the three-file linkage. NOT runnable — stubs mark where
# real logic (canon(), knn match, demo chain, area-conservation) plugs in.
# Division of labour:
#   dtP (permits)      = CLASSIFIER  -> built form, units, suites
#   dtA (historical)   = PARENT      -> pre-redev lot_size (2023 vintage)
#   dtC (current year) = CHILD       -> new post-redev parcels (for assemblies)
#
# Column provenance (what each join produces):
#   builtForm, unitsAdded, suiteFlag   <- dtP        (never from roll)
#   parentLotSize, parentValue         <- dtA        via permit->parent join
#   n_parents, parent_area (assembly)  <- dtC<->dtA  child->parent join
#   builtForm MUST ride onto both the parent-lot number AND the assembly number

