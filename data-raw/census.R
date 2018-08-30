# 2014 Census data
census_file <- "https://www12.statcan.gc.ca/datasets/OpenDataDownload.cfm?PID=109527"
download.file(census_file, destfile = "data-raw/98-400-X2016005.zip")
unzip("data-raw/98-400-X2016005.zip", exdir = "data-raw")

file_list <- function(data_table) {
  xml_file <- stringr::str_c("data-raw/98-400-X2016005/Generic_", data_table, ".xml")
  structure_file <- stringr::str_c("data-raw/98-400-X2016005/Structure_", data_table, ".xml")
  list(xml_file = xml_file, structure_file = structure_file)
}


# All Toronto census tracts start with 535
to_nodes <- "//d1:GenericData/d1:DataSet/generic:Series/generic:SeriesKey/*[@concept = 'GEO' and starts-with(@value, '535')]/../.."

data_table <- "98-400-X2016005"
files <- file_list(data_table)
census_xml <- xml2::read_xml(files$xml_file)
ns <- xml2::xml_ns(census_xml)
years <- as.integer(xml2::xml_text(xml2::xml_find_all(census_xml,
                                                      stringr::str_c(to_nodes, "/generic:Obs/generic:Time"),
                                                      ns)))
values <- as.integer(xml2::xml_attr(xml2::xml_find_all(census_xml, stringr::str_c(to_nodes, "/generic:Obs//*[@value]"), ns), "value"))
labels <- as.integer(xml2::xml_attr(xml2::xml_find_all(census_xml, stringr::str_c(to_nodes, "/generic:SeriesKey/generic:Value"), ns), "value"))
