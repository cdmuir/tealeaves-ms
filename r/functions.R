# Logit and inverse logit functions
logit <- function(x) qlogis(x)
inv_logit <- function(x) plogis(x)

# Carry forward object for downstream use
carry_forward <- function(object, what, where) {

  write_rds(object, str_c(where, "/", what, ".rds"))
  
}

# Bring forward object from previous analysis
bring_forward <- function(what, where) {
  
  object <- read_rds(str_c(where, "/", what, ".rds"))
  object 
  
}

# Export objects to ms
export2ms <- function(x, path_export = getOption("path_export", "/export")) {
  
  for (i in 1:length(x)) {
    stopifnot(is.character(x[i]))
    tmp <- eval(parse(text = x[i]))
    path <- normalizePath(str_c("ms/", path_export)) %>%
      dewindowsify_path()
    if (!dir.exists(path)) dir.create(path)
    write_rds(tmp, str_c(path, "/", x[i], ".rds"))
  }
  
}

# Import objects to ms
import2ms <- function(path_export = getOption("path_export", "/export")) {
  
  path <- str_c("ms/", path_export)
  files <- list.files(path, ".rds$")
  object_names <- str_replace(files, ".rds$", "")
  eval(parse(text = str_c(object_names, " <- read_rds('", path, "/", files, "')")),
       envir = .GlobalEnv)

}
  
# Make statistical significance asterisks
sigStar <- function(pvalue)
{
  if (pvalue >= 0.05) return("n.s.")
  if (pvalue < 0.05 & pvalue >= 0.01) return("*")
  if (pvalue < 0.01 & pvalue >= 0.001) return("**")
  if (pvalue < 0.001) return("***")
}

# Determine a number's order of magnitude
oom <- function(x)
{
  # x should be a vector of numbers
  floor(log10(abs(x)))
}

# Determine number of significant digits to use for rounding in tables
sigDig <- function(x)
{
  # x should be a vector of numeric elements to be rounded to same significant digit
  ret <- round(x, -min(oom(x)))
  return(ret)
}

# Text string for p-value in tables
pval2latex <- function(x)
{
  stem <- x * 10 ^ -oom(x)
  ret <- ifelse(oom(x) > -3, 
                sprintf("%.*f", 3, x), 
                sprintf("%.*f $\\\\times10^{%s}$", 1, stem, oom(x)))
  return(ret)
}


