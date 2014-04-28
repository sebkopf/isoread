#' @include IrmsDataClass.R
NULL

#' IrmsDualInletData reference class
#' @note not implemented yet for any actual data reading
#' @name IrmsDualInletData
#' @seealso \link{IrmsData}, \link{IrmsContinuousFlowData}
IrmsDualInletData <- setRefClass(
  "IrmsDualInletData",
  contains = "IrmsData",
  fields = list (),
  methods = list()
)