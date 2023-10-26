# Fetch the biochemical properties 

fetch_biochemical_structure <- function(tert_bioc, ref, position) {
  # Filter the dictionary based on criteria (Residue, Position) and return the desired value
  filtered_dict <- tert_bioc %>%
    filter(Residue == ref, Position == position)
  
  # Return the specific value (e.g., Ref_amino_acid_properties, median_RASP, Secondary_structure, etc.)
  if (nrow(filtered_dict) > 0) {
    return(filtered_dict$Ref_amino_acid_properties, 
           filtered_dict$median_RASP, filtered_dict$Secondary_structure)  # Replace with the desired column name
  } else {
    return(NA)  # Return NA if no match is found in the dictionary
  }
}
