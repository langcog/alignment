#' Non-parametric bootstrap for numeric vector data
#' 
#' Computes arbitrary bootstrap statistics on univariate data.
#' 
#' @param data A numeric vector of data to bootstrap over.
#' @param summary_function A string that is the name of a function to be 
#'   computed over each set of samples. This function needs to take a vector and
#'   return a single number (defaults to \code{"mean"}).
#' @param statistics_functions A vector of strings that are names of functions to be 
#'   computed over the set of summary values from all samples.
#' @param nboot The number of bootstrap samples to take (defaults to \code{1000}).
#' @param replace Logical indicating whether to sample with replacement (defaults to \code{TRUE}).
#' @param ... Other arguments passed from generic.
#'
#' @examples
#' ## Mean and 95% confidence interval for 1000 samples from a normal distribution
#' x <- rnorm(1000, mean = 0, sd = 1)
#' ci_lower <- function(x) {quantile(x, 0.025)}
#' ci_upper <- function(x) {quantile(x, 0.975)}
#' multi_boot(x, statistics_functions = c("ci_lower", "mean", "ci_upper"))
## S3 method for class 'numeric'
multi_boot.numeric <- function(data, summary_function = "mean", statistics_functions,
                               nboot = 1000, replace = TRUE, ...) {
  
  formulas <- sapply(statistics_functions, function(x) lazyeval::interp(~fun, fun = x))
  
  one_sample <- function() {
    do.call(summary_function, list(sample(data, replace = replace)))
  }
  
  all_samples <- data.frame(sample = replicate(nboot, one_sample())) %>%
    summarise_each(funs_(formulas), sample)
  
  if(length(statistics_functions) == 1) {
    all_samples <- all_samples %>%
      rename_(.dots = setNames("sample", statistics_functions))
  }
  
  return(all_samples)
  
}

#' Non-parametric bootstrap for logical vector data
#' 
#' Computes arbitrary bootstrap statistics on univariate data.
#' 
#' @param data A logical vector of data to bootstrap over.
#' @inheritParams multi_boot.numeric
#'   
#' @examples
#' ## Mean and 95% confidence interval for 1000 samples from a binomial distribution
#' x <- as.logical(rbinom(1000, 1, 0.5))
#' ci_lower <- function(x) {quantile(x, 0.025)}
#' ci_upper <- function(x) {quantile(x, 0.975)}
#' multi_boot(x, statistics_functions = c("ci_lower", "mean", "ci_upper"))
## S3 method for class 'logical'
multi_boot.logical <- function(data, summary_function = "mean", statistics_functions,
                               nboot = 1000, replace = TRUE, ...) {
  multi_boot(as.numeric(data), summary_function = "mean", statistics_functions,
             nboot = 1000, replace = TRUE, ...)
}

#' Non-parametric bootstrap for data frames
#' 
#' Computes arbitrary bootstrap statistics on univariate data.
#' 
#' @param data A data frame.
#' @param summary_function A function to be computed over each set of samples as a data frame, or a
#'   string that is the name of the function to be computed over each set of samples as a single column of
#'   a data frame indicated by \code{column} (defaults to \code{mean}).
#' @param column A string indicating the column of \code{data} to bootstrap over (only necessary if
#'   \code{summary_function} is a string).
#' @param summary_groups A vector of strings that are column names of \code{data} indicating how it should
#'   be grouped before applying \code{summary_function}.
#' @param statistics_functions A function to be computed over the data frame of summary values from all
#'   samples, or a vector of strings that are names of functions to be computed over the vector of
#'   summary values from all samples.
#' @param statistics_groups A vector of strings that are column names of \code{data} indicating how it should
#'   be grouped before applying \code{statistics_functions} (defaults to \code{summary_groups}).
#' @param nboot The number of bootstrap samples to take (defaults to \code{1000}).
#' @param replace Logical indicating whether to sample with replacement (defaults to \code{TRUE}).
#' @param ... Other arguments passed from generic.
#'  
#' @examples
#' ## Mean and 95% confidence interval for 1000 samples from two different normal distributions
#' require(dplyr)
#' gauss1 <- data.frame(value = rnorm(1000, mean = 0, sd = 1), condition = 1)
#' gauss2 <- data.frame(value = rnorm(1000, mean = 2, sd = 3), condition = 2)
#' ci_lower <- function(x) {quantile(x, 0.025)}
#' ci_upper <- function(x) {quantile(x, 0.975)}
#' multi_boot(data = bind_rows(gauss1, gauss2),
#'            summary_function = "mean", column = "value", summary_groups = "condition",
#'            statistics_functions = c("ci_lower", "mean", "ci_upper"))
#' multi_boot(data = bind_rows(gauss1, gauss2),
#'            summary_function = function(df) summarise(df, mean = mean(value)),
#'            summary_groups = c("condition"),
#'            statistics_functions = function(df) summarise_each(df,
#'                                                               funs("ci_upper", "mean", "ci_lower"), 
#'                                                               mean),
#'            statistics_groups = c("condition"),
#'            nboot = 100, replace = TRUE)
## S3 method for class 'data.frame'
multi_boot.data.frame <- function(data, summary_function = "mean", column = NULL, summary_groups = NULL,
                                  statistics_functions, statistics_groups = summary_groups, 
                                  nboot = 1000, replace = TRUE, ...) {
  
  assertthat::assert_that(typeof(summary_function) %in% c("closure", "character"))
  assertthat::assert_that(typeof(statistics_functions) %in% c("closure", "character"))
  assertthat::assert_that(all(statistics_groups %in% summary_groups))
  
  original_groups <- groups(data)

  if(typeof(summary_function) == "closure") { # function
    call_summary_function <- summary_function
  } else { # string
    assertthat::assert_that(!is.null(column))
    summary_dots <- list(lazyeval::interp(~fun(arg), fun = as.name(summary_function), arg = as.name(column)))
    call_summary_function <- function(df) summarise_(df, .dots = setNames(summary_dots, "summary"))
  }
  
  if(typeof(statistics_functions) == "closure") { # function
    call_statistics_functions <- statistics_functions
  } else { # string
    statistics_formulas <- sapply(statistics_functions, function(x) lazyeval::interp(~fun, fun = x))
    call_statistics_functions <- function(df) summarise_each(df, funs_(statistics_formulas), summary)
  }

  one_sample <- function(df, call_summary_function, summary_groups, replace) {
    function(k) {
      if (!is.null(summary_groups)) {
        df <- df %>%
          group_by_(.dots = summary_groups)
      }
      df %>%
        sample_frac(replace = replace) %>%
        call_summary_function() %>%
        mutate(sample = k)
    }
  }
  
  all_samples <- sapply(1:nboot, one_sample(data, call_summary_function, summary_groups, replace),
                        simplify = FALSE) %>%
    bind_rows()
  
  if(!is.null(original_groups)) all_samples %<>% group_by_(.dots = original_groups) 
  
  
  if (!is.null(statistics_groups)) {
    all_samples <- all_samples %>% group_by_(.dots = statistics_groups)
  }
  
  booted_vals <- all_samples %>% call_statistics_functions()

  if(typeof(statistics_functions) == "character" & length(statistics_functions) == 1) {
    booted_vals <- booted_vals %>% rename_(.dots = setNames("summary", statistics_functions))
  }
  
  return(booted_vals)
}

#' Non-parametric bootstrap with multiple sample statistics
#' 
#' \code{multi_boot} is a generic function for bootstrapping on various data
#' structures. The function invokes particular methods which depend on the class
#' of the first argument.
#' 
#' @param data A data structure containg the data to bootstrap.
#' @param ... Additional arguments passed to particular methods.
#' 
#' @examples
#' ## List of available methods
#' methods(multi_boot)
multi_boot <- function(data, ...) UseMethod("multi_boot")
