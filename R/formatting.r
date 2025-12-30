#' Format array as quoted string list
#'
#' Takes a character array and returns a character vector of length 1
#' containing a quoted list of the input array.
#' 
#' @export 
#' 
#' @param x character vector of items to be quoted
#' 
#' @return a character vector of length 1 containing a quoted comma-separated list
#' 
#' @examples 
#' quoted_list_string(c('a','b','c'))
#' 
#' @tests
#' expect_equal(
#'  quoted_list_string(c('a','b','c')),
#'  '"a", "b", "c"'
#' )
#' 
quoted_list_string <- function(x) {
    paste0(paste0('"', x, '"'), collapse = ', ')
}

#' Get Indefinite Article
#' 
#' Get the correct indefinite article for a given word. Returns
#' 'an' if word starts with starts with a vowel and 'a' otherwise.
#' 
#' @export 
#' 
#' @param word the word to which the indefinite article is applied
#' 
#' @return either 'a' or 'an' depending on the word
#' 
#' @examples 
#' get_indefinite_article('fruit')
#' get_indefinite_article('apple')
#' 
#' @tests
#' 
#' expect_equal(
#'  get_indefinite_article('fruit'),
#'  'a'
#' )
#' 
#' expect_equal(
#'  get_indefinite_article('apple'),
#'  'an'
#' )
get_indefinite_article <- function(word) {
    if(substr(word, 0, 1) %in% word_start_vowels) {
        return('an')
    }

    'a'
}

#' Create Parameter Formatter
#' 
#' Create formatter function to be used for a set of parameters.
#' 
#' @param ... arguments to be passed to format
#' 
#' @return a function that takes a vector of numbers and returns a
#' formatted character vector
#' 
#' @export
#' 
#' @examples 
#' formatter <- create_param_formatter(digits = 3)
#' formatter(mtcars$mpg)
#' 
#' @tests
#' 
#' expect_equal(
#'  create_param_formatter(digits = 4)(0.1234567),
#'  "0.1235"
#' )
create_param_formatter <- function(...) {
    args <- list(...)
    args$trim <- TRUE
    if (is.null(args$digits)) {
        args$digits <- 3
    }
    function(x) {
        format_args <- append(list(x), args)
        do.call(format, format_args)
    }
}

#' Format to Print as List Item
#'
#' Takes an object and returns a character vector representing how
#' the object should be printed as a list item.
#'
#' @param x the object to be printed
#' @param n the number of levels to indent output
#' @param skip the number of lines of output to skip
#'
#' @return a character vector of output as list item
#'
#' @importFrom utils capture.output
#' @export 
#' 
#' @examples 
#' to_list_item_output(mtcars)
#' 
#' @tests
#' expect_equal(
#'  to_list_item_output(mtcars[1:2,1:3]),
#'  "\n                      mpg cyl disp\n        Mazda RX4      21   6  160\n        Mazda RX4 Wag  21   6  160"
#' )
#' expect_equal(
#'  to_list_item_output(mtcars[1:2,1:3], n = 2),
#'  "\n                  mpg cyl disp\n    Mazda RX4      21   6  160\n    Mazda RX4 Wag  21   6  160"
#' )
#' expect_equal(
#'  to_list_item_output(mtcars[1:2,1:3], skip = 1, n = 2),
#'  "\n    Mazda RX4      21   6  160\n    Mazda RX4 Wag  21   6  160"
#' )
to_list_item_output <- function(x, n = 6, skip = 0) {
  output <- capture.output(print(x))
  if (skip > 0) {
    output <- output[-seq_len(skip)]
  }
  n_lines <- length(output)
  if (n_lines == 1) {
    return(output)
  }
  indents <- paste0(rep(' ', n + 2), collapse = '')
  indented_output <- paste0('\n', paste0(indents, output, collapse = '\n'))

  indented_output
}