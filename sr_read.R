sr_read <- function() {
      # Function Usage----
      # To create a dataframe from folder contains collections Service Request Files downloaded fro
  
      # Pilih Folder
      pth <- choose.dir(default = "", caption = "Select folder Collection of SR")
      
      # Make All (246 Cols) Column as text
      listcoltype_SR <- vector()
      for(i in 1:246) {
        listcoltype_SR <- append(listcoltype_SR , "text")
      }
      
      # Open Excel Files in Folder then Combine them to dataframe (Require purrr library)
      SR_SEP <-
        list.files(path = pth,
                   pattern = "*.xlsx", 
                   full.names = T,
                   recursive = T) %>% 
        map_df(~read_xlsx(., sheet = 1,  col_types = listcoltype_SR)) 
      
      # Remove Trailing space character on Header names
      names(SR_SEP) <- str_trim(names(SR_SEP))

      
      # Make string to datetime data type (Require lubridate library)
      SR_SEP <- SR_SEP %>%  
        mutate(`Created Date` = as.POSIXct(strptime(`Created Date`, "%d %b %Y %H:%M")) ) %>% 
        mutate(`Complaint Due Date time` = as.POSIXct(strptime(`Complaint Due Date time`, "%d %b %Y %H:%M:%S")) ) %>% 
        mutate(`Due Date` = as.POSIXct(strptime(`Due Date`, "%d %b %Y %H:%M:%S")) ) %>% 
        mutate(`Closed Date` = as.POSIXct(strptime(`Closed Date`, "%d %b %Y %H:%M:%S")) ) %>% 
        mutate(`Last Updated Date` = as.POSIXct(strptime(`Last Updated Date`, "%d %b %Y %H:%M:%S")) )
      return(SR_SEP)
}

