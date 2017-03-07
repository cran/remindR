#'Insert, extract, and print text "reminders"
#'
#'remindR is a simple package to insert, extract, and print text
#' "reminders" either in source code comments or as the "comment" attribute of
#' any R object.
#'
#'The primary intent is to allow addition and retrieval of informal
#' text notes to oneself or collaborators during code development. These could
#' be reminders about argument requirements, changes or additions to make, the
#' structure of the function's returned value, required options settings, etc.
#' -- basically anything that one wants to note. Generally such reminders will
#' be ephemeral and brief. They are not intended as detailed development
#' specifications.
#'
#' It is also possible to use such notes as minimal help "tooltips" for users.
#' To facilitate this, the \code{"remind<-"} insertion and \code{remind}
#' extraction functions wrap R's \code{comment} functionality to use the
#' \code{"comment"} attribute of any R object as a list of reminders.
"_PACKAGE"
