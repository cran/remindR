#' @name remind
#' @title Insert, extract, and print "reminders" for functions and objects
#' @description Insert, extract, and print text (i.e. character vector)
#' "reminders" either as the "comment" attribute of objects or as delimited
#' comments within function source code.
#'
#' @details Extracts text between the 'begin' and 'end' delimiting character
#' strings within source code comments. This is mostly intended to enable easy
#' addition and retrieval of informal text notes during the course of
#' script/function development, aka 'reminders', e.g. to fix something, add a
#' validity check, note additional argument requirements etc. Multiple such
#' separately delimited reminders can be included.
#'
#' As a slight convenience, the function wraps R's existing \code{comment}
#' function to attach and extract reminders as the 'comment' attribute of any R
#' object. These can be used to provide information (provenance, context) on
#' objects and even serve as a kind of minimal informal Help documentation for
#' functions, i.e. a sort of simple manual "tooltip" functionality.
#'
#' @note Because there is no fixed syntax in source code comments, extracting
#'   reminders cannot be guaranteed to always work correctly. Some simple checks
#'   have been included to warn when they may not be properly extracted, but
#'   this may not succeed in all cases.
#'
#'   Also, retrieving reminders from functions depends on whether
#'  \code{options(keep.source = TRUE)} is in use when they are saved.
#'
#'
#' @return  A list of S3 class "reminder".
#'
#' @seealso  \code{\link{comment}}
#'
#' @param x An R object/function for \code{remind}; a class "reminder" object
#'   for \code{print}
#' @param begin Character string delimiting reminder beginning
#' @param end   Character string delimiting reminder ending
#' @param value Character vector of text to attach as '\code{comment}' attribute
#' @param ...  Additional arguments to methods
#'
#' @examples
#' x <- 1:3
#' remind(x)
#' remind(x) <- c("first comment","and a second")
#' remind(x)
#' remind(x) <- NULL ## removes reminder
#' remind(x)
#' f <- function(x){
#'    y <- x
#'    ## Some miscellaneous comments
#'    ## Now <<This is reminder 1>> and next
#'    ## This is
#'    ## yet another <<reminder 2 is this>>
#'    ## some more stuff
#'    y
#' }
#' f(5)
#' remind(f)
#' remind(f)<- "something else"
#' remind(f)
#' ## "tooltip" type example:
#' my.summary<- function(x, fun = mean,...)fun(x)
#' remind(my.summary)<-
#' "Don't forget to include the na.rm argument if missing values might be present"
#' remind(my.summary)
#' rm(f,x,my.summary)
#
remind <- function(x,...)UseMethod("remind")

#' @rdname remind
remind.default <- function(x,...) {
   structure(list(inFunc = NULL, inAttr = comment(x), type = "default")
             ,class = c("reminder","list")) ## for printing
 ## <<In code test reminder >>
}
comment(remind.default) <- "A comment reminder"


#' @rdname remind
remind.function <- function(x,
            begin = "<<",## opening reminder symbol
            end =">>"  ## closing reminder symbol
            ,...)
{

   ## check begin and end delimiters
   assertthat::assert_that(is.string(begin), is.string(end))
   if(begin == end)stop("begin and end delimiters must be different"
                        ,call. = FALSE)
   ## stringify f and chop off any non reminders at beginning and end
   z <- paste(deparse(x,control = "useSource",width.cutoff = 132L),collapse= " ")
         ## strip off "function(...)" part at beginning
   z <- stripFun(z)
   ## extract all text between delimiters
   begin.posn <- gregexpr(begin,z, fixed = TRUE)[[1]]
   end.posn <- gregexpr(end,z, fixed = TRUE)[[1]]
   ## Check for balanced delimiters and issue warning if unbalanced
   msg <- "Some reminders may not be extracted.\n"
   if(length(begin.posn) != length(end.posn)){
      warning("Unbalanced delimiters. ", msg,
              call. = FALSE, immediate. = TRUE) } else {
         ## check for matching delimiters
         if(any(begin.posn > end.posn))
            warning("Unmatched delimiters. ",msg,
                    call. = FALSE, immediate. = TRUE)
         }
   pat <- paste0("^.*?(",begin,".*",end,").*?$")
   if(!grepl(pat,z)) inFunc <- NULL  else
      {
         z <- gsub(pat,"\\1",z) ## chop!
         ## extract portions fenced by begin and end strings
         ## and then strsplit on end
         pat <- paste0(".*?",begin,"(.*?",end,")")
         zvec <- strsplit(gsub(pat,"\\1",z),end)[[1]]
         ## remove extra spaces at beginning and end and all #'s
         zvec <- gsub("^ *| *$|#+","",zvec)
         ## remove remaining extra spaces
         inFunc <- gsub(" +"," ",zvec)
      }
   structure(list(inFunc = hasContent(inFunc),
                  inAttr = hasContent(comment(x)), type = "function"),
             class = c("reminder","list"))
}

#' @rdname remind
"remind<-" <- function(x,value)
{
   if(is.null(value)) comment(x)<- NULL else {
      if(!is.character(value))
         stop("Replacement must be a character vector")
      else value <- hasContent(value)
      if(is.null(value))
            stop("No meaningful content in replacement")
      else comment(x) <- value
   }
   x
}

#' @rdname remind
print.reminder <- function(x,...)
{
   if(all(sapply(x[-3],is.null)))cat("No reminders\n\n") else
   {
      g <- function(z){
         if(is.null(z)) cat("No reminders\n\n") else {
            z <- paste0("[",seq_along(z),"] ",z)
            z <- strwrap(z,simplify = FALSE, width = getOption("width")-2)
            invisible(sapply(z,function(x)cat(paste0(x,"\n"),"\n")))
         }
      }
      if(x$type == "default") g(x[[2]]) else {
         lab <- paste0(c("In function", "In comment"),":\n")
         for(i in (1:2)){ cat(lab[i]); g(x[[i]])}
      }
   }
}
