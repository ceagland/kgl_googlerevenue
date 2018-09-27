

# Works like is.na, but for NaN values
is.nan.df <- function(x) {
  do.call(cbind, lapply(x, is.nan))
} ##END function is.nan.df

#Rounds all numeric variables in a data frame or data.table to the specified number of digits
#High digit default is to avoid losing precision when forgetting to set the argument;
#prefer this to verifying argument, particularly given the data.table update by reference
round_num <- function(df, digits = 10) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  if ("data.table" %in% class(df)) {
    nums.dt <- names(nums)[nums]
    df <- 
    for (k in nums.dt) set(df, j = k, value = round(df[[k]], digits = digits))
  } else {
  df[,nums] <- round(df[,nums], digits = digits)
  return(df)
  }
}

#These two name functions are created separately rather than with an argument to identify class because...
#data.table will set by reference, which can potentially be faster, but doesn't require an assignment
#Doing them both together just got weird, though they have the same regexs


#data.table name clean
nameclean_dt <- function(DT) {
  require(rebus)
  data.table::setnames(DT, old = names(DT), new = tolower(make.names(names(DT))))
  data.table::setnames(DT, old = names(DT), new = stringr::str_replace_all(names(DT), "_", "."))
  data.table::setnames(DT, old = names(DT), new = stringr::str_replace_all(names(DT), one_or_more(DOT), "."))
  data.table::setnames(DT, old = names(DT), new = stringr::str_replace_all(names(DT), DOT %R% END, ""))
} ##END name.clean

#data.frame name clean
nameclean_df <- function(DF) {
  names(DF) <- tolower(names(DF))
  names(DF) <- gsub(" ",".",names(DF))
  names(DF) <- gsub("[.]{2,}",".",names(DF))
  names(DF) <- gsub("\\.$","",names(DF))
  return(DF)
}

name.clean <- function(this.df, is.nielsen = FALSE) {
  names(this.df) <- tolower(names(this.df))
  names(this.df) <- gsub(" ",".",names(this.df))
  names(this.df) <- gsub("[.]{2,}",".",names(this.df))
  names(this.df) <- gsub("\\.$","",names(this.df))
  
  if (is.nielsen) names(this.df) <- gsub("^x\\.","",names(this.df))
  
  return(this.df)
}##END name.clean


#For looking at irregular lags (so, periods 14:26, for example - also works like normal lag/shift functions)
#lagvec - the vector/variable we want to lag
#lag.periods - can be a single number, but the function is really meant for combining periods: 1:4 or 5:8, for example
#default - how to pad values that wouldn't otherwise have a lag (for the first few observations, generally)
lag_prev <- function(lagvec, lag.periods = 1, default = NA) {
  lag.divide <- length(lag.periods)
  tmpvar <- Reduce("+", data.table::shift(lagvec, n = lag.periods, fill = default)) / lag.divide
  return(tmpvar)
}

#identifies columns with NA values in a data.frame or data.table
#I had written this interactively to output a table - rather than making this nice I just added a couple of lines
#Ain't pretty but it doesn't really have to be
identify_NAcols <- function(data, tbl.out = FALSE) {
  require(dplyr)
  out <- sapply(data, function(x) any(is.na(x))) %>%
    data_frame(var = names(.), hasNA = .) %>%
    filter(hasNA)
  if (tbl.out == TRUE) {
    return(out)
  } else {
    out <- out %>%
    select(var) %>%
    unlist(use.names = FALSE)
    return(out)
  }
}

#The last day of the newell week is Saturday. In lubridate, this corresponds to the 7th wday.
#This function moves the WE date to the previous Saturday (virtually all WE dates in data are Sat-Mon)
date_to_prev_saturday <- function(datevar) {
  datevar <- data.table(date = as.Date(datevar))
  datevar[, weekday := lubridate::wday(date)]
  datevar[, date := ifelse(weekday == 7,
                           date,
                           date - weekday)]
  return(as.Date(datevar$date))
} # End date_to_prev_saturday function#


#For checking whether a name is a valid variable name. Generally I use with sapply on the names of a df. Big help when I have
#thousands of variable names that were created based on observations that I may not have read directly
#https://www.r-bloggers.com/testing-for-valid-variable-names/

is_valid_variable_name <- function(x, allow_reserved = TRUE)
{
  ok <- rep.int(TRUE, length(x))
  
  #is name too long?
  max_name_length <- if (getRversion() < "2.13.0") 256L else 10000L
  ok[nchar(x) > max_name_length] <- FALSE
  
  #is it a reserved variable, i.e.
  #an ellipsis or two dots then a number?
  if (!allow_reserved)
  {
    ok[x == "..."] <- FALSE
    ok[grepl("^\\.{2}[[:digit:]]+$", x)] <- FALSE
  }
  
  #is it a reserved word?
  reserved_words <- c("if", "else", "repeat", "while", "function", "for", "in", "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
  ok[grepl(paste(reserved_words, collapse = "|"), x)]
  
  #are there any illegal characters?
  ok[!grepl("^[[:alnum:]_.]+$", x)] <- FALSE
  
  #does it start with underscore?
  ok[grepl("^_", x)] <- FALSE
  
  #does it start with dot then a number?
  ok[grepl("^\\.[[:digit:]]", x)] <- FALSE
  
  ok
}

# save <- sapply(names(train.data), is_valid_variable_name)
