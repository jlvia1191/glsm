#' hsbdemo: School data for testing.
#'
#' Entering high school students make program choices among general program, vocational program and academic program. Their choice might be modeled using their writing score and their social economic status.The data set contains variables on 200 students. The outcome variable is prog, program type. The predictor variables are social economic status, ses, a three-level categorical variable and writing score, write, a continuous variable.
#'
#' @format A data frame with 200 rows and 17 columns:
#' \describe{
#'   \item{Student}{Categorical. Student identification code.}
#'   \item{id}{Categorical. Unique identifier for each student.}
#'   \item{gender}{Categorical. Student gender: "female" or "male".}
#'   \item{ses}{Categorical. Socioeconomic status: "low", "middle", "high".}
#'   \item{schtyp}{Categorical. Type of school: "private" or "public".} # corregido
#'   \item{prog}{Categorical. Program of study chosen: 0 = General, 1 = Vocational, 2 = Academic.}
#'   \item{read}{Continuous. Reading test score.}
#'   \item{write}{Continuous. Writing test score.}
#'   \item{math}{Continuous. Math test score.}
#'   \item{science}{Continuous. Science test score.}
#'   \item{socst}{Continuous. Social studies test score.}
#'   \item{honors}{Categorical. Honors enrollment status: "enrolled" or "not enrolled".}
#'   \item{awards}{Integer. Number of awards received, ranging from 0 to 9.}
#'   \item{cid}{Categorical. Unspecified score, ranging from 0 to 20.}
#'   \item{prog0}{Binary. 1 if prog = General, 0 otherwise.}
#'   \item{prog1}{Binary. 1 if prog = Vocational, 0 otherwise.}
#'   \item{prog2}{Binary. 1 if prog = Academic, 0 otherwise.}
#' }
#' @source Simulated dataset inspired by high school program choices.
"hsbdemo"




