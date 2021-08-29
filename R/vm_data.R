#' Vaginal microbiome data from "Replication and refinement of a vaginal microbial signature of preterm birth in two racially distinct cohorts of US women" by Calahan et al., 2017, PNAS.
#'
#' A dataset containing the 16S rRNA sequencing counts from longitudinal samples
#' of pregnant subjects throughout their pregnancy.
#'
#' @format A list of three data frames
#' \describe{
#'   \item{counts}{The number of times a given ASV (columns) was sequenced in a given sample (rows) material.}
#'   \item{sample_info}{Information about each sample including sequencing info (e.g. primers) and information about subjects and individual samples such as collection time.}
#'   \item{taxonomy}{Taxonomy table providing taxonomic information about each ASV.}
#' }
#' @source \url{https://stacks.stanford.edu/file/druid:yb681vm1809/RepRefine_Scripts.tar.gz}
"vm_data"
