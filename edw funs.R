# Create quoted char vectors on the fly
Cs <- function(...) {as.character(sys.call())[-1]}


####
# Various simple functions to manipulate academic dates
####

# Increment quarters (4 should wrap to 1)
next.qtr <- function(x){
  ifelse(x == 4, 1, x + 1)
}

# decrement quarters (1 -> 4) (could be combined into 1 fun with next.qtr but I'm not compelled)
last.qtr <- function(x){
  ifelse(x == 1, 4, x - 1)
}

# Increment yrq (YYYYQ)
next.yrq <- function(x){
  ifelse((x %% 10) == 4, x + 7, x + 1)
}

# Functions to get yr or quarter from a YYYYQ (also helpers for next.fall)
get.q <- function(yrq) { yrq %% 10 }
get.y <- function(yrq) { yrq %/% 10 }

# Calculate the next fall quarter from a YYYYQ (could be generic/switched but others are rarely necessary)
next.fall <- function(x){
  ifelse(get.q(x) < 4,
         get.y(x) * 10 + 4,
         x+10)
}

# calc difference, in qtrs, between two yrqs
# ex: 20152 <-> 20123 should be 2 yrs, 3 quarters or 11 quarters
qtr.diff <- function(yrq1, yrq2){
  ((get.y(yrq1) - get.y(yrq2)) * 4) + (get.q(yrq1) - get.q(yrq2))
}

fix.sdb.names <- function(x = "EDWSampleVarName"){
  # convert UpperStyle edw/sdb names to r style
  tolower(gsub("([[:lower:]])([[:upper:]])", "\\1.\\2", x, fixed = F))
}
