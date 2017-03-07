
## unexported utility functions
##
hasContent <- function(y){
   # << Returns printable character strings of a character vector if any, else NULL>>
   #
   ok <- nzchar(sub("[^[:graph:]]+","",y), keepNA = TRUE)
   ok[is.na(ok)] <- FALSE
   y <- y[ok]
   if(!length(y)) NULL else y
}

stripFun<- function(string,open = "(", close =")"){
   ##  <<Strips out everything from beginning of a character string through
   ## all characters from first open substring to matching closing substring.>>
   ## <<Primarily intended to strip out function call from source code character string returned by
   ## paste(deparse(f,control = "useSource",width = 132L),collapse= " ")  >>
   ## get substring from first match to open on
   string <- substring(string,regexpr(open,string,fixed =TRUE))
   ## vectorize substring from first match
   string_vec <- strsplit(string,"")[[1]]
   substring(string,which.min(cumsum(
      (string_vec == open) - (string_vec == close)))+1)
}
