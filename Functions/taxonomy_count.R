#Calculates the number of unique taxa at each taxonomic level for an
#individual dive

taxonomy_count <- function(x) { 
  x |> 
    dplyr::summarize(
      across(phylum:species, \(x) dplyr::n_distinct(x, na.rm = TRUE))
    )
}