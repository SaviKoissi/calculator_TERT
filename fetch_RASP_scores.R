# Fetch the RASP score 

fetch_RASP_scores <- function(tert_rasp, ref, position, mut) {
  # Filter the dictionary based on criteria (Ref, Position, Mut) and return the desired value
  filtered_dict <- tert_rasp %>%
    filter(Ref == ref, Position == position, Mut == mut)
  
  # Return the specific value (e.g., RSAP_score, median_RASP, etc.)
  if (nrow(filtered_dict) > 0) {
    return(filtered_dict$RSAP_score)  # Replace with the desired column name
  } else {
    return(NA)  # Return NA if no match is found in the dictionary
  }
}