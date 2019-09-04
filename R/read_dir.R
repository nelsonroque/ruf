#' @name read_dir read all files in directory into one dataframe
#' @param dir class: string; directory path to location of files
#' @param recursive class: boolean; directory path to location of files
#' @param filetype class: string; current options: csv; coming soon: txt, spss, sas, matlab
#' @param sanitize_cols class: boolean; whether or not to sanitize column names (all lowercase)
read_dir <- function(dir, recursive=T, filetype='csv', sanitize_cols=F, comment=NA) {

	# list files in dir
  fl <- list.files(dir,
                   full.names = T, 
                   recursive = recursive)
  
  # print confirmation of start message
  print(paste("Preparing to read:", length(fl), "files..."))
  
  # create blank metadata df
  meta.df <- data.frame()
  
  # start timer
  stime <- Sys.time()

  # create binded data frame
  read_metadata <- list()
  result <- do.call(dplyr::bind_rows, lapply(fl, function(path) {
    if(tolower(filetype) == 'csv') {
      df <- readr::read_csv(path, comment=comment)
      read_metadata[[path]] <- data.frame(nrow=nrow(df), ncol=ncol(df), colnames = paste(names(df),collapse=","))
      df <- readr::read_csv(path, comment = comment)
    }
    
    # calculate meta features
    ffp <- rep(path, nrow(df))
    fs <- file.size(path)
    fmts <- file.mtime(path)
    export.df <- data.frame(full_file_path = ffp,
                            file_size = fs,
                            file_mod_ts = fmts)
    
    # add meta-data for each file
    df[["meta_full_path"]] <- ffp
    df[["meta_file_size"]] <- fs
    df[["meta_file_modified_timestamp"]] <- fmts
    
    # save meta data in df also
    meta.df <- rbind(meta.df, export.df)
    
    # need this or else no DF returned
    df 
  }))
  
  # santizie column names
  if(sanitize_cols) {
    names(result) <- tolower(gsub("__","_",gsub("[[:punct:]]","_",gsub(" ","_",names(result)))))
  }
  
  # get time difference in seconds
  print(difftime(Sys.time(),stime,units='secs'))
  
  # standardize column headings
  if(sanitize_cols) {
    names(result) <- gsub("__", "_" , gsub("[[:punct:]]","_", gsub(" ", "_", tolower(names(result)))))
  }

	# return DF
  return(list(result=result,metadata=read_metadata))

}