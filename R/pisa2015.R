#' Student questionnaire and science performance data from the 2015 PISA
#'
#' This is a subset of the Programme of International Student Assessment (PISA)
#' including student questionnaire data and their performance on the science
#' assessment. Data from the United States and Canada are included.
#'
#' @details
#' There are seven variables:
#'
#' * `country.` The country the student is from (USA or Canada).
#' * `science_score.` Average science score over the 10 plausible values.
#' * `principals.` Average scale score for students self-reported understanding of scientific principals.
#' * `belonging.` Average scale score for belonging.
#' * `interest.` Average scale score for interest in science.
#' * `enjoyment.` Average scale score for enjoyment doing science.
#' * `motivation.` Average scale score for motivation in science.
#' * `efficacy.` Average scale score for students self-efficacy in science.
#'
#' All numeric scores have been converted to standard scores (z-scores).
#'
#' Below are the Likert response questions students completed that are used to
#' calculate the belonging, interest, motivation, efficacy, and principals
#' variables. The values in parentheses correspond to the variable names in the
#' original PISA database. The questionnaire can be downloaded from
#' [the OECD PISA website](https://www.oecd.org/content/dam/oecd/en/data/datasets/pisa/pisa-2015-datasets/questionnaires/Student%20questionnaire%20PBA%20main%20PISA%202015.pdf).
#'
#' **Belonging:** Thinking about your school: to what extent do you agree with the following statements? (`ST034`)
#' * I feel like an outsider (or left out of things) at school.
#' * I make friends easily at school.
#' * I feel like I belong at school.
#' * I feel awkward and out of place in my school.
#' * Other students seem to like me.
#' * I feel lonely at school.
#'
#' **Interest:** How much do you disagree or agree with the statements about yourself below? (`ST094`)
#' * I generally have fun when I am learning *broad science* topics.
#' * I like reading about *broad science*.
#' * I am happy working on *broad science* topics.
#' * I enjoy acquiring new knowledge in *broad science*.
#' * I am interested in learning about *broad science*.
#'
#' **Enjoyment:** How much do you disagree or agree with the statements about yourself below? (`ST094`)
#' * I generally have fun when I am learning *broad science* topics.
#' * I like reading about *broad science*.
#' * I am happy working on *broad science* topics.
#' * I enjoy acquiring new knowledge in *broad science*.
#' * I am interested in learning about *broad science*.
#'
#' **Motivation:** How much do you agree with the statements below? (`ST113`)
#' * Making an effort in my *school science* subject(s) is worth it because this will help me in the work I want to do later on.
#' * What I learn in my *school science* subject(s) is important for me because I need this for what I want to do later on.
#' * Studying my *school science* subject(s) is worthwhile for me because what I learn will improve my career prospects.
#' * Many things I learn in my *school science* subject(s) will help me to get a job.
#'
#' **Efficacy:** How easy do you think it would be for you to perform the following tasks on your own? (`ST129`)
#' * Recognise the science question that underlies a newspaper report on a health issue.
#' * Explain why earthquakes occur more frequently in some areas than in others.
#' * Describe the role of antibiotics in the treatment of disease.
#' * Identify the science question associated with the disposal of garbage.
#' * Predict how changes to an environment will affect the survival of certain species.
#' * Interpret the scientific information provided on the labelling of food items.
#' * Discuss how new evidence can lead you to change your understanding about the possibility of life on Mars.
#' * Identify the better of two explanations for the formation of acid rain.
#'
#' **Principals:** How much do you disagree or agree with the statements below? (`ST131`)
#' * A good way to know if something is true is to do an experiment.
#' * Ideas in *broad science* sometimes change.
#' * Good answers are based on evidence from many different experiments.
#' * It is good to try experiments more than once to make sure of your findings.
#' * Sometimes *broad science* scientists change their minds about what is true in science.
#' * The ideas in *broad science* science books sometimes change.
#'
#' @docType data
#' @keywords datasets
#' @name pisa2015
#' @usage data(pisa2015)
#' @source https://www.oecd.org/en/data/datasets/pisa-2015-database.html
#' @format A data frame with 20,571 rows and 8 variables
"pisa2015"
