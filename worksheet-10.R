# Documenting and Publishing your Data Worksheet

# Preparing Data for Publication

library(...)

stm_dat <- ...("data/StormEvents.csv")

# Outputting derived data
...('storm_project', showWarnings = FALSE)
...(stm_dat, "~/storm_project/StormEvents_d2006.csv")

# Creating metadata

library(...) 

template_...(path = "~", dir.name = "...") # create template files in a new directory

# move the derived data file to the new directory
file.copy("~/StormEvents_d2006.csv", "./.../data_objects/", overwrite = TRUE)
file.remove("~/StormEvents_d2006.csv")

# core metadata
template_core_metadata(path = "~/.../metadata_templates",
                       license = "CCBY",
                       file.type = "...")

# abstract
abs <- ...
# this function from the readr package writes a simple text file without columns or rows
write_file(abs, "~/storm_project/metadata_templates/abstract.txt", append = FALSE)

# methods
methd <- ...
write_file(methd, "~/storm_project/metadata_templates/methods.txt", append = FALSE)

# keywords
keyw <- read.table("~/storm_project/metadata_templates/keywords.txt", sep = "\t", header = TRUE, colClasses = rep("character", 2))
keyw <- keyw[1:3,] # create a few blank rows  
keyw$keyword <- ... # fill in a few keywords
write.table(keyw, "~/storm_project/metadata_templates/keywords.txt", row.names = FALSE, sep = "\t")

# personnel
# read in the personnel template text file
persons <- read.table("~/storm_project/metadata_templates/personnel.txt", 
                     sep = "\t", header = TRUE, colClasses = rep("character", 10)) 
persons <- persons[1:4,] # create a few blank rows 
# edit the personnel information
persons$... <- c("Jane", "Jane", "Jane", "Hank")
persons$... <- c("A", "A", "A", "O")
persons$... <- c("Doe", "Doe", "Doe", "Williams")
persons$... <- rep("University of Maryland", 4)
persons$... <- c("jadoe@umd.edu", "jadoe@umd.edu", "jadoe@umd.edu", "how@umd.edu")
persons$... <- c("PI", "contact", "creator", "Field Technician")
persons$... <- rep("Storm Events", 4)
persons$... <- rep("NSF", 4)
persons$... <- rep("000-000-0001", 4)  
# write new personnel file
write.table(persons, "~/storm_project/metadata_templates/personnel.txt", row.names = FALSE, sep = "\t")

# geographic coverage
...(path = "~/storm_project/metadata_templates", 
                             data.path = "~/storm_project/data_objects", 
                             data.table = "StormEvents_d2006.csv", 
                             lat.col = "BEGIN_LAT",
                             lon.col = "BEGIN_LON",
                             site.col = "STATE"
                             )

# data attributes
...(path = "~/storm_project/metadata_templates",
                          data.path = "~/storm_project/data_objects",
                          data.table = c(...))
# read in the attributes template text file
attrib <- read.table("~/storm_project/metadata_templates/attributes_StormEvents_d2006.txt", 
                     sep = "\t", header = TRUE)  
# define the attributes
attrib$attributeDefinition <- c(...)
attrib$dateTimeFormatString <- c(...)
# define missing values
attrib$missingValueCode <- rep(NA_character_, nrow(...))
attrib$missingValueCodeExplanation <- rep("Missing value", nrow(...))
# check classes
attrib$class <- ... %>% select(..., ...) %>% 
                mutate(class = ifelse(attributeName %in% c("DAMAGE_PROPERTY", "DAMAGE_CROPS", 
                                                           "EVENT_TYPE", "SOURCE"), 
                                      "...", class)) %>% 
                select(class) %>% unlist(use.names = FALSE)
# define units from controlled vocabulary
attrib$unit <- c("...","","...","","","","","","","...","...","...",
                 "number","","","","number","","degree",
                 "degree","degree","degree","","","")
# write revised attribute table
...(attrib, "~/storm_project/metadata_templates/attributes_StormEvents_d2006.txt", 
    row.names = FALSE, sep = "\t")

# define categorical variables
...(path = "~/storm_project/metadata_templates", 
                               data.path = "~/storm_project/data_objects")
# read in the attributes template text file
catvars <- read.table("~/storm_project/metadata_templates/catvars_StormEvents_d2006.txt", 
                      sep = "\t", header = TRUE)
# define
catvars$definition <- c("csv file", "pds file", rep("magnitude", 2), ..., ...)
# write table
write.table(..., "~/storm_project/metadata_templates/catvars_StormEvents_d2006.txt", 
            row.names = FALSE, sep = "\t")

# write EML metadata file
...(path = "~/storm_project/metadata_templates",
         data.path = "~/storm_project/data_objects",
         eml.path = "~/storm_project/eml",
         dataset.title = ...,
         temporal.coverage = c("2006-01-01", "2006-04-07"),
         geographic.description = ..., 
         geographic.coordinates = c(30, -79, 42.5, -95.5), 
         maintenance.description = "In Progress: Some updates to these data are expected",
         data.table = ...,
         data.table.name = "Storm_Events_2006",
         data.table.description = ..., 
         # other.entity = c(""),
         # other.entity.name = c(""),
         # other.entity.description = c(""),
         user.id = "my_user_id",
         user.domain = "my_user_domain", 
         package.id = "storm_events_package_id")

# Creating a data package

library(...) 
library(...)
# create empty data package
dp <- ...("DataPackage")

# add metadata file to data package
emlFile <- "~/storm_project/eml/storm_events_package_id.xml"  # define the file
emlId <- paste("urn:uuid:", UUIDgenerate(), sep = "")  # generate id
mdObj <- new("DataObject", id = ..., format = "eml://ecoinformatics.org/eml-2.1.1", file = ...)
dp <- addMember(dp, ...)  # add

# add data file to data package
datafile <- "~/storm_project/data_objects/StormEvents_d2006.csv" # define the file
dataId <- paste("urn:uuid:", UUIDgenerate(), sep = "")  # generate id
dataObj <- new("DataObject", id = ..., format = "text/csv", filename = ...) 
dp <- addMember(dp, ...) # add

# define relationship between data and metadata
dp <- insertRelationship(dp, subjectID = emlId, objectIDs = dataId)

# add R script to data package
scriptfile <- "data/storm_script.R" # define the file
scriptId <- paste("urn:uuid:", UUIDgenerate(), sep = "") # generate id
scriptObj <- ...("DataObject", id = scriptId, format = "application/R", filename = ...)
dp <- addMember(dp, ...) # add

# add Figure 1 to data package
fig1file <- "data/Storms_Fig1.png" # define the file
fig1Id <- paste("urn:uuid:", UUIDgenerate(), sep = "") # generate id
fig1Obj <- ...("DataObject", id = ..., format = "image/png", filename = fig1file)
dp <- ...(dp, fig1Obj) # add

# add Figure 2 to data package
fig2file <- "data/Storms_Fig2.png" # define the file
fig2Id <- paste("urn:uuid:", UUIDgenerate(), sep = "") # generate id
fig2Obj <- new("DataObject", id = ..., format = "image/png", filename = ...)
dp <- ...(dp, ...) # add

# Adding provenance

# create Resource Description Framework (RDF)
serializationId <- paste("resourceMap", UUIDgenerate(), sep = "")
filePath <- file.path(sprintf("%s/%s.rdf", tempdir(), serializationId))
status <- serializePackage(dp, filePath, id = serializationId, resolveURI = "")

# create conceptual diagram of provenance
library(...)
storm_diag <- grViz("digraph{
         
                     graph[rankdir = LR]
                     
                     node[shape = rectangle, style = filled]  
                     A[label = 'Storm data']
                     B[label = 'storm_script.R']
                     C[label = 'Fig. 1']
                     D[label = 'Fig. 2']

                     edge[color = black]
                     A -> B
                     B -> C
                     B -> D
                     
                     }")
storm_diag

# create provenance 
dp <- ...(dp, sources = ..., program = ..., derivations = c(fig1Obj, fig2Obj))

# look at provenance
library(...)
plotRelationships(dp)

# zip up data package!
dp_bagit <- ...(dp) # zip
file.copy(..., "~/storm_project/Storm_data_package.zip") # copy file from temp to desired location

# Upload data package to a repository

library(...)
library(...) 
library(...) 

## get your access token and paste it in the console! 

# assign a doi if desired
cn <- CNode("PROD")
mn <- getMNode(cn, "urn:node:KNB")  
doi <- generateIdentifier(mn, "DOI")
# edit your metadata file with doi
emlFile <- "~/storm_project/eml/storm_events_package_id.xml"
mdObj <- ...("DataObject", id = ..., format = "eml://ecoinformatics.org/eml-2.1.1", file = emlFile)
dp <- ...(dp, ...)

# set access rules for your data publication
dpAccessRules <- data.frame(subject="http://orcid.org/0000-0003-0847-9100", 
                            permission="changePermission") 
dpAccessRules2 <- data.frame(subject = c("http://orcid.org/0000-0003-0847-9100",
                                         "http://orcid.org/0000-0000-0000-0001"),
                             permission = c("changePermission", "read")
                             )

# set your environment and repository
d1c <- ...("STAGING2", "urn:node:mnTestKNB") 

# upload your package to the testing location
packageId <- ...(d1c, dp, public = TRUE, accessRules = ...,
                               quiet = FALSE)



