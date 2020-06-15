## code to prepare `generate_UA_info.R` dataset goes here

# Generate names and indices of unitary authoritaries
UAuth <- as.list(covidm::cm_uk_locations("uk", 3))
names(UAuth) <- gsub("\\||UK|\\s|,", "", UAuth)

usethis::use_data(UAuth, overwrite = TRUE)

iUAuth <- as.list(1:length(UAuth))
names(iUAuth) <- names(UAuth)

usethis::use_data(iUAuth, overwrite = TRUE)
