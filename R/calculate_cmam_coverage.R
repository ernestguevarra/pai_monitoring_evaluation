################################################################################
#
#'
#' Calculate recovering out of the programme
#'
#
################################################################################

calculate_rec_out <- function(k, sam_in, sam_out, rec_in) {
  floor(
    (1 / k) * (rec_in * ((sam_in + sam_out + 1) / (sam_in + 1)) - rec_in)
  )
}


################################################################################
#
#'
#' Calculate case-finding effectiveness
#'
#
################################################################################

calculate_case_finding <- function(sam_in, sam_out) {
  sam_in / (sam_in + sam_out)
}


################################################################################
#
#'
#' Calculate treatment coverage
#'
#
################################################################################

calculate_treatment <- function(sam_in, sam_out, rec_in, k) {
  rec_out <- calculate_rec_out(
    k = k, sam_in = sam_in, sam_out = sam_out, rec_in = rec_in
  )
  
  (sam_in + rec_in) / (sam_in + rec_in + sam_out + rec_out)
}

