#' Adresses
#'
#'
#' @format A data frame with 1 rows and 2 variables:
#' \describe{
#'   \item{address_id}{}
#'   \item{address_name}{}
#'   \item{address_surface}{}
#' }
"address"


#' Persons of interest
#'
#'
#' @format A data frame with 1 rows and 4 variables:
#' \describe{
#'   \item{person_id}{}
#'   \item{address_id}{}
#'   \item{birth_data}{}
#'   \item{sex}{}
#' }
"person"

#' Residents of the address
#'
#'
#' @format A data frame with 2 rows and 4 variables:
#' \describe{
#'   \item{person_id}{}
#'   \item{address_id}{}
#'   \item{birth_data}{}
#'   \item{sex}{}
#' }
"resident"
