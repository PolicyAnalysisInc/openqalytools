#' Create a List Object with Custom Class
#'
#' Creates a structured list object with a specified class attribute.
#'
#' @param class Character vector specifying the class(es) to assign to the object.
#' @param ... Named elements to include in the list.
#'
#' @return A list with the specified class attribute.
#'
#' @examples
#' obj <- create_list_object(c("myclass", "list"), a = 1, b = 2)
#' class(obj)
#'
#' @tests
#' expect_equal(
#'  class(create_list_object(c('a','b'),
#'  list())), c('a','b')
#' )
#'
#' @export
create_list_object <- function(class, ...) {
    structure(list(...), class = class)
}