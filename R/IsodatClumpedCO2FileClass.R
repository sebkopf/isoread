#' @include IsodatDualInletFileClass.R
NULL

#' Clumped dual inlet data class
#' 
#' 
#' @name IsodatClumpedCO2File
#' @exportClass IsodatClumpedCO2File
#' @seealso \link{IrmsDualInletData}, \link{BinaryFile}, \link{IsodatFile}, \link{IsodatDualInletFile}
IsodatClumpedCO2File <- setRefClass(
  "IsodatClumpedCO2File",
  contains = c("IsodatDualInletFile"),
  fields = list (),
  methods = list(

    #' initialize irms data container
    init_irms_data = function(){
      callSuper()
      
      # specifically define the data table columns of CO2 dual inlet files
      dataTableColumns <<- 
        data.frame(
          data = c("cycle", "d 45CO2/44CO2 ", "d 46CO2/44CO2 ", 
                   "d 13C/12C ", "d 18O/16O ", "d 17O/16O ", 
                   "AT% 13C/12C ", "AT% 18O/16O "),
          column = c("cycle", "d 45CO2/44CO2", "d 46CO2/44CO2",
                     "d13C", "d18O", "d17O", 
                     "at% 13C", "at% 18O"),
          units = c("", rep("permil", 5), "", ""),
          type = c("integer",  rep("numeric", 7)),
          show = TRUE, stringsAsFactors = FALSE)
    }
    
  )   
)