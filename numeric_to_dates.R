numeric_to_dates = function(vector=NULL) {
  
  # This will create a vector with the number-characters as numbers, while the rest will be just NA's
  numeric_part <- suppressWarnings(as.numeric(vector))   
  
  #Convert the numbers into dates - the positions of the entries already in date format will be saved
  converted_numeric_dates <- as.Date(numeric_part, origin = "1899-12-30")
  
  #Create an ifelse cycle that saves either the dates - if the numeric transformation yielded a NA - or the date-from-number that was previously transformed.
  final_dates <- ifelse(!is.na(numeric_part), 
                        as.character(converted_numeric_dates), 
                        vector)
  
  # Finally, take the final object and transform every entry as Date
  final_dates <- as.Date(final_dates, format = "%Y-%m-%d")
  
  return(final_dates)
  
}
