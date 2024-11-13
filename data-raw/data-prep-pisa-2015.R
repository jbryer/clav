library(EdSurvey)
library(dplyr)

# NOTE: downloadPISA and readPISA will take a very long time the first time.
downloadPISA(
	root = 'data-raw/',
	years = c(2015),
	database = c("INT"),
	cache = FALSE,
	verbose = TRUE
)

pisa15 <- readPISA(path = 'data-raw/PISA/2015',
				   database = 'INT',
				   countries = c('usa','can'))

# colnames(pisa15)[substr(colnames(pisa15), 1, 2) == 'st'] |> sort()
# searchSDF(string = "interest", data = pisa15, fileFormat = "student")
# searchSDF(string = "", data = pisa15, fileFormat = "student") |> View()

# 2015 questionaire: https://www.oecd.org/content/dam/oecd/en/data/datasets/pisa/pisa-2015-datasets/questionnaires/Student%20questionnaire%20PBA%20main%20PISA%202015.pdf
pisa2015 <- getData(
	pisa15,
	c("cnt","read","math","scie",
	  "st013q01ta", # How many books are there in your home?
	  paste0("st034q0", 1:5 , "ta"), # Belonging in school (reverse scored)
	  paste0("st094q0", 1:5 , "na"), # Science interest
	  paste0("st113q0", 1:4, "ta"), # Science motivation (reverse scored)
	  paste0("st129q0", 1:8, "ta"), # Science efficacy (reverse scored)
	  paste0("st094q0", 1:5, "na"), # Science Enjoyment
	  paste0("st131q", c('01','03','04','06','08','11'), "na") # Science principals
	))

pisa2015 <- do.call(rbind, pisa2015)

scale_this <- function(x) {
	(x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

pisa2015 <- pisa2015 |>
	rowwise() |>
	mutate(across(starts_with("st"), as.integer)) |>
	mutate(
		science_score = mean(c_across(ends_with("scie"))),
		belonging = 4 - mean(c_across(starts_with("st034"))),
		interest = mean(as.integer(c_across(starts_with("st094")))),
		motivation = 4 - mean(as.integer(c_across(starts_with("st113")))),
		efficacy = 4 - mean(as.integer(c_across(starts_with("st129")))),
		enjoyment = mean(as.integer(c_across(starts_with("st131")))),
		principals = mean(as.integer(c_across(starts_with("st131"))))
	) |>
	ungroup() |>
	select(cnt, science_score, principals, belonging, interest, enjoyment, motivation, efficacy) |>
	rename(country = cnt) |>
	mutate_if(is.numeric, scale_this)

usethis::use_data(pisa2015, overwrite = TRUE)
tools::resaveRdaFiles('data/')

