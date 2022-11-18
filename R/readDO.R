#' Fast and friendly data logger file finagler
#' @description Load and concatenate files from dissolved oxygen loggers, may work for other parameters from supported loggers
#' @import data.table
#' @param path File name in working directory or path to logger files
#' @param type Type of data logger, currently supports PME miniDOT, YSI EXO, or the AIMS pipeline
#'
#' @return a data.table of logger measurements with nice column names
#' @export
#'
readDO <- function(path, type = c("PME", "YSI", "AIMS")) {
  tempFiles <- Sys.glob(paste0(path, "/*.TXT"))
  if(type == "PME"){
    tempLog <- sapply(tempFiles, fread, skip = 7, check.names = TRUE,
                      na.strings = c("(Second)", "(none)", "(Volt)", "(deg C)","(mg/l)", "(%)"),
                      simplify = FALSE)
    tempLog <- rbindlist(tempLog)
    tempLog <- stats::na.omit(tempLog)
    newCol <- c("UnixTimestamp", "DateTime_UTC", "DateTime_CST", "BatteryVoltage", "Temperature", "Dissolved.Oxygen", "Q")
    oldCol <- c("Unix.Timestamp", "UTC_Date_._Time", "Central.Standard.Time", "Battery", "Temperature", "Dissolved.Oxygen", "Q")
    setnames(tempLog, oldCol, newCol)
  } else if(type == "YSI"){
      stop("Still in development")
  } else if(type == "AIMS"){
      stop("Still in development")
  } else if(!(type %in% c("PME", "YSI", "AIMS")))
      stop("Currently only supports files from PME miniDOTs, YSI EXOs, or AIMS")
}
