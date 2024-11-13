library(EdSurvey)
library(dplyr)

# downloadPISA(
# 	root = 'data-raw/',
# 	years = c(2018),
# 	database = c("INT"),
# 	cache = FALSE,
# 	verbose = TRUE
# )

# https://naep-research.airprojects.org/portals/0/edsurvey_a_users_guide/_book/index.html
pisa18 <- readPISA(path = 'data-raw/PISA/2018',
				   database = 'INT',
				   countries = 'usa')

# searchSDF(string = "interest", data = pisa15, fileFormat = "student")
# searchSDF(string = "", data = pisa15, fileFormat = "student") |> View()

scale_this <- function(x) {
	(x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# https://www.air.org/sites/default/files/EdSurvey-getData.pdf
pisa2018 <- getData(
	pisa18,
	c("cnt","read","math","scie",
	  paste0("st161q0", c(1:3, 6:8) , "ha") # Reading efficacy
	))

