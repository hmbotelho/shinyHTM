

# ADD SETTINGS HERE:
settingNames <- c("imagePathColumns")

# Convenience function which simplifies subsetting the UI list
subsetUI <- function( LS, type = "input", name = "", field = "selected"){
  
  print( paste0("Fetching setting: ", name) )
  
  if( name == "" ) 
  {
    print("No name => return setting: NULL")
    return ( NULL )
  }
  
  if( is.null( LS ) ) 
  {
    print("Settings object is NULL => return setting: NULL")
    return ( NULL )
  }
  

  # Let's take this list 'UI' as an example
  # [[1]]
  # [[1]]$type
  # [1] "input"
  # 
  # [[1]]$name
  # [1] "colTreatment"
  # 
  # [[1]]$choices
  # [1] "NOT SUPPORTED"
  # 
  # [[1]]$selected
  # [1] "X1_Treatment"
  # 
  # [[1]]$comment
  # [1] ""
  #
  # 'subsetUI(UI)' would return 'X1_Treatment'
  # 'subsetUI(UI, field = "choices")' would return 'NOT SUPPORTED'
  
  if(is.reactive(LS)){
    LS <- isolate(LS())
  }

  types <- sapply(LS, function(x) x$type)
  names <- sapply(LS, function(x) x$name)
  okelement <- which(types == type & names == name)
  
  if(length(okelement) > 1) stop(paste0("There are multiple list elements matching the criteria: ", paste(okelement, collapse = ", ")))

  setting = LS[[okelement]][[field]]

  print( paste0("Found setting: ", setting) )

  return ( setting )
}


getSetting <- function( settings, settingName ) {
  print( paste0("# Fetching setting: ", settingName ) )
  print( paste0("Settings: ") )
  str( settings )
  if ( exists("settings") && !is.null( settings ) ) {
    print("Return setting: ");
    str( settings[settingName][[1]] )
    return( settings[settingName][[1]] )
  } else {
    print( "Setting could not be found!!" )
    return(NULL)
  }
}



extractSettingsFromInputsList <- function( inputsList )
{
  print( 'Extracting settings from inputs list...' )
  
  settings = list()
  for ( settingName in settingNames )
  {
    settings[settingName] <- inputsList[settingName]
    str( settings[settingName] )
  }
 return( settings )
}

saveSettings <- function( settings, path ) {
  print('Saving settings...')
  write( toJSON( settings ), path, append = FALSE)
  print('...done!')
}

readSettings <- function( path ){
  print("Reading settings...")
  return( fromJSON( file = path ) )
  print("...done.")
}

testSettingsIO <- function()
{
  path = '/Users/tischer/Downloads/settings.json'
  
  settings = list();
  settings[["test"]] <- c("a","b")
  
  saveSettings( settings, path)
  
  readSettings <- read_settings(path)
  
  identical( settings, readSettings )
}