# global.R for complex-it model

# for working on this do
#  setwd("C:/Users/Rachel Oughton/Dropbox/postdocs/COVID19/ComplexIt/App_regression")


# published at https://racheloughton.shinyapps.io/Complex-IT-RHO/

# source files and load libraries in here

require(ggplot2)
require(ggrepel)
require(dplyr)
require(tidyr)
require(timeDate)
require(tidyselect)
library(shiny)
library(markdown)
require(lmvar)
require(mvtnorm)
require(shinyWidgets)
library(Cairo)
options(shiny.usecairo=T)

options(shiny.deprecation.messages=FALSE)

###### CHANGE THIS LINE FOR NEW DATA
 inData_raw = read.csv("data/data_quadrants9.csv")
######
 
 date_format = function(x){
   as.Date(gsub("X", "", x), format="%d_%m_%Y")
 }
 

# Relevant areas
 
all_names = as.character(inData_raw$AreaName)
NEnames <- c("Barnsley", "Bradford", "Calderdale", "County Durham",
             "Darlington", "Doncaster", "East Riding of Yorkshire", "Gateshead",
             "Hartlepool", "Kingston upon Hull, City of", "Kirklees", "Leeds",
             "Middlesbrough", "Newcastle upon Tyne", "North East Lincolnshire", "North Lincolnshire",
             "North Tyneside", "North Yorkshire", "Northumberland", "Redcar and Cleveland", 
             "Rotherham", "Sheffield", "South Tyneside", "Stockton-on-Tees",
             "Sunderland", "Wakefield", "York")

 
ItalyNames = c("Abruzzo Chieti", "Abruzzo L'Aquila", "Abruzzo Pescara", "Abruzzo Teramo", 
               "Basilicata Matera", "Basilicata Potenza", "P.A. Bolzano Bolzano", 
               "Calabria Catanzaro", "Calabria Cosenza", "Calabria Crotone", 
               "Calabria Reggio di Calabria", "Calabria Vibo Valentia", "Campania Avellino", 
               "Campania Benevento", "Campania Caserta", "Campania Napoli", 
               "Campania Salerno", "Emilia-Romagna Bologna", "Emilia-Romagna Ferrara", 
               "Emilia-Romagna Forlafa-Cesena", "Emilia-Romagna Modena", "Emilia-Romagna Parma", 
               "Emilia-Romagna Piacenza", "Emilia-Romagna Ravenna", "Emilia-Romagna Reggio nell'Emilia", 
               "Emilia-Romagna Rimini", "Friuli Venezia Giulia Gorizia", "Friuli Venezia Giulia Pordenone", 
               "Friuli Venezia Giulia Trieste", "Friuli Venezia Giulia Udine", 
               "Lazio Frosinone", "Lazio Latina", "Lazio Rieti", "Lazio Roma", 
               "Lazio Viterbo", "Liguria Genova", "Liguria Imperia", "Liguria La Spezia", 
               "Liguria Savona", "Lombardia Bergamo", "Lombardia Brescia", "Lombardia Como", 
               "Lombardia Cremona", "Lombardia Lecco", "Lombardia Lodi", "Lombardia Mantova", 
               "Lombardia Milano", "Lombardia Monza e della Brianza", "Lombardia Pavia", 
               "Lombardia Sondrio", "Lombardia Varese", "Marche Ancona", "Marche Ascoli Piceno", 
               "Marche Fermo", "Marche Macerata", "Marche Pesaro e Urbino", 
               "Molise Campobasso", "Molise Isernia", "Piemonte Alessandria", 
               "Piemonte Asti", "Piemonte Biella", "Piemonte Cuneo", "Piemonte Novara", 
               "Piemonte Torino", "Piemonte Verbano-Cusio-Ossola", "Piemonte Vercelli", 
               "Puglia Bari", "Puglia Barletta-Andria-Trani", "Puglia Brindisi", 
               "Puglia Foggia", "Puglia Lecce", "Puglia Taranto", "Sardegna Cagliari", 
               "Sardegna Nuoro", "Sardegna Oristano", "Sardegna Sassari", "Sardegna Sud Sardegna", 
               "Sicilia Agrigento", "Sicilia Caltanissetta", "Sicilia Catania", 
               "Sicilia Enna", "Sicilia Messina", "Sicilia Palermo", "Sicilia Ragusa", 
               "Sicilia Siracusa", "Sicilia Trapani", "Toscana Arezzo", "Toscana Firenze", 
               "Toscana Grosseto", "Toscana Livorno", "Toscana Lucca", "Toscana Massa Carrara", 
               "Toscana Pisa", "Toscana Pistoia", "Toscana Prato", "Toscana Siena", 
               "P.A. Trento Trento", "Umbria Perugia", "Umbria Terni", "Valle d'Aosta Aosta", 
               "Veneto Belluno", "Veneto Padova", "Veneto Rovigo", "Veneto Treviso", 
               "Veneto Venezia", "Veneto Verona", "Veneto Vicenza")
UKnames_other =c("Lancashire", "Rutland", "City of London", "Blackburn with Darwen", 
                 "Bedford", "Knowsley", "Solihull", "Isle of Wight", "Peterborough", 
                 "St. Helens", "Blackpool", "Bracknell Forest", "Herefordshire, County of", 
                 "Plymouth", "Stoke-on-Trent", "Warrington", "Thurrock", "Leicester", 
                 "Luton", "Bath and North East Somerset", "Swindon", "Halton", 
                 "West Berkshire", "Southend-on-Sea", "Rochdale", "Salford", "Southampton", 
                 "Milton Keynes", "Sefton", "Torbay", "Reading", "Portsmouth", 
                 "Central Bedfordshire", "Wigan", "Cheshire West and Chester", 
                 "East Sussex", "Telford and Wrekin", "Dorset", "Bury", "Bolton", 
                 "Richmond upon Thames", "Coventry", "Derby", "Brighton and Hove", 
                 "Wirral", "Kingston upon Thames", "Suffolk", "Redbridge", "Bournemouth, Christchurch and Poole", 
                 "Havering", "Wokingham", "Cornwall and Isles of Scilly", "Cheshire East", 
                 "Bristol, City of", "Oldham", "Barking and Dagenham", "Norfolk", 
                 "Tameside", "Medway", "Dudley", "Sandwell", "Waltham Forest", 
                 "South Gloucestershire", "Stockport", "Lincolnshire", "Gloucestershire", 
                 "Somerset", "Wiltshire", "Trafford", "Liverpool", "Windsor and Maidenhead", 
                 "Cambridgeshire", "Sutton", "Newham", "North Somerset", "Nottingham", 
                 "Manchester", "Worcestershire", "Leicestershire", "West Sussex", 
                 "Walsall", "Hillingdon", "Northamptonshire", "Bexley", "Staffordshire", 
                 "Slough", "Greenwich", "Warwickshire", "Shropshire", "Hounslow", 
                 "Tower Hamlets", "Lewisham", "Hammersmith and Fulham", "Bromley", 
                 "Islington", "Enfield", "Oxfordshire", "Haringey", "Hackney", 
                 "Camden", "Nottinghamshire", "Cumbria", "Devon", "Merton", "Harrow", 
                 "Ealing", "Essex", "Croydon", "Kent", "Derbyshire", "Buckinghamshire", 
                 "Wolverhampton", "Brent", "Barnet", "Birmingham", "Hertfordshire", 
                 "Surrey", "Wandsworth", "Lambeth", "Kensington and Chelsea", 
                 "Southwark", "Westminster", "Hampshire")
  


## processing function for model

inData = select(inData_raw, AreaName, AreaID, quadrant, clus, everything()) 
names(inData)[1:4] = c("areaName", "areaID", "quadrant", "cluster")  
  #rename(areaID = AreaID, areaName = AreaName, cluster = clus)
 
 
  # variable names and Italian data for the projections

idxLastActualDay <- which(is.na(inData[1,]))[1] - 1
lastActualDay <- names(inData)[idxLastActualDay]
#projectedVars <- names(inData)[(idxLastActualDay+1):(idxLastActualDay+7)]
projectedVars <- names(inData)[(idxLastActualDay+1):(idxLastActualDay+10)]
inData_past = inData[, 1:idxLastActualDay]
date_names_past = names(inData_past)[grep("^X", names(inData_past))]
dates_past = date_format(date_names_past)
last_week = dates_past[length(dates_past)-7]

fullItaly <- inData[inData$areaName %in% ItalyNames, ]

#dataItaly <- inData[inData$areaName %in% ItalyNames, c("areaName", "quadrant", lastActualDay, projectedVars)]

fullNE = inData[inData$areaName %in% NEnames,]
fullUKother = inData[inData$areaName %in% UKnames_other, ]


 