
## insulting error messages

## function to insult your intelligence

enter_any_word <- function(x) {
  if(!is.numeric(x)){
    message <- sample(c("SUX 2 SUCK. ","You must be the least favorite child. ","Why are you so stupid!? ", "8====D. ", "ERROR FIGURE OUT LIFE BEFORE TRYING THIS FUNCTION! ", "The input obviously has to be a number, dummy. ", "Please enter a f*cking number! ", "Please add 5 to your IQ, did you make it into the double digits?  "), 2)
    stop(message)
  }
  x+2
}



enter_any_word("help")
enter_any_word("word")



## updated kruskal-wallis test

kruskal_insulting <- function (x, g, ...) 
{
  if (is.list(x)) {
    if (length(x) < 2L) 
      stop("'x' must be a list with at least 2 elements, how the fuck else are we supposed to know if SAMPLES ORIGINATE FROM THE SAME DISTRIBUTION OR WHAT")
    if (!missing(g)) 
      warning("'x' is a list, so ignoring argument 'g' cuz yer dumbass didn't include that argument")
    DNAME <- deparse(substitute(x))
    x <- lapply(x, function(u) u <- u[complete.cases(u)])
    if (!all(sapply(x, is.numeric))) 
      warning("DID YOU CHECK THE FUCKING DATA STRUCTURE??!!!! some elements of 'x' are not numeric and will be coerced to numeric")
    k <- length(x)
    l <- lengths(x)
    if (any(l == 0L)) 
      stop("all groups must contain data")
    g <- factor(rep.int(seq_len(k), l))
    x <- unlist(x)
  }
  else {
    if (length(x) != length(g)) 
      stop("'x' and 'g' OBVIOUSLY HAVE TO have the same length")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    OK <- complete.cases(x, g)
    x <- x[OK]
    g <- g[OK]
    g <- factor(g)
    k <- nlevels(g)
    if (k < 2L) 
      stop("eff off and put more observations in, biiitch")
  }
  n <- length(x)
  if (n < 2L) 
    stop("not enough observations BEYOTCH")
  r <- rank(x)
  TIES <- table(x)
  STATISTIC <- sum(tapply(r, g, "sum")^2/tapply(r, g, 
                                                "length"))
  STATISTIC <- ((12 * STATISTIC/(n * (n + 1)) - 3 * (n + 1))/(1 - 
                                                                sum(TIES^3 - TIES)/(n^3 - n)))
  PARAMETER <- k - 1L
  PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  names(STATISTIC) <- "Kruskal-Wallis chi-squared"
  names(PARAMETER) <- "df"
  RVAL <- list(statistic = STATISTIC, parameter = PARAMETER, 
               p.value = PVAL, method = "Kruskal-Wallis rank sum test", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}



mygroups <- c("a")
mynums <- c(2)
df <- data.frame(mygroups, mynums)

## a dataframe with only one observation should give you an error message
kruskal_insulting(x = df$mynums, g= df$mygroups)
