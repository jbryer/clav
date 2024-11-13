library(shiny)
library(readxl)
library(ggplot2)
library(GGally)
library(magrittr)
library(reshape2)
library(psych)
library(fpc)
library(cluster)

source('cluster_module.R')

se_bar_multiplier <- 1 # Could also be 1.96

# TODO: May want to compare cluster solutions between EC and WGU, for now just WGU
load('data/DAACS-WGU.rda') # WGU data
load('data/DAACS-EC.rda') # EC data
load('data/DAACS-ualbany2020.rda') # UAlbany

default_vars <- c('srl_motivation', 'srl_metacognition', 'srl_strategies')
# default_vars <- c('srl_managing_time', 'srl_help_seeking', 'srl_managing_environment',
# 				  'srl_understanding', 'srl_anxiety', 'srl_mastery_orientation',
# 				  'srl_mindset', 'srl_self_efficacy', 'srl_monitoring',
# 				  'srl_evaluation', 'srl_planning')


##### Topic Modeling
# library(tidytext)
# library(topicmodels)
# library(stopwords)
# library(ldatuning)
# # library(textmineR)
# library(tm)
# library(wordcloud)
#
# stopwords::stopwords_getsources()
# stop_words <- stopwords::stopwords(language = 'en',
# 								   source = 'snowball',
# 								   simplify = TRUE)

# For debugging
if(FALSE) {
	input <- list(variable_selection = default_vars,
				  # dependent_variable = 'mathTotal',
				  dependent_variable = 'write_summary',
				  k = 4)
	thedata <- daacs.wgu  |>
		dplyr::select(c(input$variable_selection, input$dependent_variable)) %>%
		dplyr::filter(complete.cases(.)) |>
		dplyr::mutate_if(is.numeric, scale_this)
	fit <- kmeans(thedata[,input$variable_selection], input$k)
}
