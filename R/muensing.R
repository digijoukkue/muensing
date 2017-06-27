#' Calculates fontsize of value labels for barplots
#'
#' Calculates fontsize of value labels for barplots. Values the function gives were found using trial and error.
#'
#' @param nbars Number of bars in barplot
#' @export
#' @examples
#' CalculateFontSizeForBars()

CalculateFontSizeForBars <- function(nbars) {
  
  if (nbars > 60) {
    return(2.5)
  } 
  if (nbars > 40) {
    return(3.1)
  } 
  if (nbars > 30) {
    return(3.7)
  } 
  if (nbars > 15) {
    return(4.2)
  } 
  if (nbars > 9) {
    return(4.8)
  } 
  if (nbars > 7) {
    return(6)
  } 
  if (nbars > 4) {
    return(7)
  } 
  return(8)
}

#' Calculate the sample size of dataFrame for each subset
#'
#' Calculate the sample size of dataFrame, i.e. How many times each unique value appears in columName. 
#' E.g. a column called gender would contain Male / Female text. Counts number of males and number of 
#' females. Returns result as a dataFrame.
#'
#' @param dataFrame Contains filtered data, i.e. the data rows that will be used for plot.
#' @param columnName Name of column containing subsets.
#' @return dataFrame containing new column called rowcount that has counts for each unique value in original dataFrame.
#' @export
#' @examples
#' CalculateNs()

CalculateNs <- function(dataFrame, columnName) {
  
  colnames(dataFrame)[colnames(dataFrame) == columnName] <- "HeadingColumn"         #Give temporary name so dplyr code works
  
  dataFrame$rowcount <- 1
  dataFrame          <- dataFrame %>%
    dplyr::group_by(HeadingColumn) %>%
    dplyr::mutate(rowcount = sum(rowcount)) %>%
    dplyr::ungroup(HeadingColumn) %>%
    dplyr::mutate(HeadingColumn = ifelse(HeadingColumn != "", paste0(HeadingColumn, " (n= ", rowcount, ")"),""))
  
  colnames(dataFrame)[colnames(dataFrame) == "HeadingColumn"] <- columnName         #Replace nome with original
  
  return(dataFrame)
}

#' Calculates the sum of all positive answers of a question with a scale.
#' 
#' Scale value have to be in a column called "Scale". The x value labels have to be in a colmun called "XLabels". 
#' Answer percentages have to be in column called "pct". Verified that this function works with scale that has 
#' an odd number of choices. May not work with a scale that has an even number of choices.
#' @return tidyDataPos A dataFrame containing top3 summary data
#' @param dataFrame Contains columns Scale, XLabels, pct
#' @export
#' @examples
#' CalculateTop3()

CalculateTop3 <- function(dataFrame) {
  
  tidyDataPos  <- dataFrame                                 %>% #top 3 summary
    dplyr::filter(as.numeric(Scale) < nlevels(Scale)/2+0.5) %>% #we'll filter positive values from data
    dplyr::group_by(XLabels)                                %>%
    dplyr::summarise(posvalues = sum(pct))                      #count amount of positive values
  
  return(tidyDataPos)
}

#' Summarizes percentage of answers according to a demography
#' 
#' Summarizes percentage of answers according to a demography. E.g. Demography column is called gender, 
#' containing male/female answers. Brand column contains different brands of beers. We get out a dataFrame 
#' with beer brands: how many males knew the beer brand and how many females knew the beer brand.
#' @export
#' @examples
#' CalculatePctsForDemog()

CalculatePctsForDemog    <- function(weightedData, dataFrame, demography) {
  
  if (demography == "none") {
    return(NULL)
  }
  
  weightedData$Demography   <- dataFrame[, demography]
  weightedData$weightsdemog <- dataFrame$weight
  
  weightedData              <- weightedData %>%
    CalculateNs("Demography")   %>%
    dplyr::group_by(Demography) %>%
    dplyr::mutate(weightsdemog = sum(weightsdemog)) %>%
    tidyr::gather(Brand, weight, -Demography, -weightsdemog, -rowcount, na.rm = TRUE)    %>% #make data long
    dplyr::group_by(weightsdemog, Demography, Brand, rowcount) %>%
    dplyr::summarise(n = sum(weight)) %>%
    dplyr::mutate(pct = n / weightsdemog) %>%
    dplyr::filter(Demography != "")
  
  weightedData$DemographyCategory <- demography
  return(weightedData)
}

#' Changes values in a dataFrame
#'
#' Goes through all elements in a dataFrame and replaces what is being searched for with a replacement.
#'
#' @param dataFrame The dataframe to be edited
#' @param searchfor The value we want to replace
#' @param replacement The value that will be used as replacement
#' @return dataFrame in a form that is ready for plotting
#' @export
#' @examples
#' ChangeValue()

ChangeValue              <- function(dataFrame, searchfor, replacement) {
  
  for (x in 1:ncol(dataFrame)) {
    for (y in 1:nrow(dataFrame)) {
      if (dataFrame[y, x] == searchfor) {
        dataFrame[y, x] = replacement
      }
    }
  }
  return(dataFrame)
}

#' Checks if first column and row contains zeroes and ones
#' 
#' Checks if first column and first row of dataFrame contains zeroes and ones. If yes, it can be assumed that all of the data is zeroes and ones.
#' Excludes column "weight".
#' @export
#' @examples
#' CheckZeroesAndOnes()

CheckZeroesAndOnes       <- function(dataFrame) {
  
  data <- dplyr::select(dataFrame, -weight)
  for (i in 1:ncol(data)) {
    if(data[1, i] != 0 & data[1, i] != 1) {
      return(FALSE)
    }
  }
  for (i in 1:nrow(data)) {
    if(data[i, 1] != 0 & data[i, 1] != 1) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Converts a dataFrame containing text into Zeroes and Ones
#
#' Converts a dataFrame containing text into Zeroes and Ones. Dataframe should contain one column with text and another column with weights.
#'
#' @param dataFrame Two colmuns: first containing text, second containing values/weights
#' @return zeroesAndOnesdf DataFrame in zeroes and ones form.
#' @export
#' @examples
#' ConvertToZeroesAndOnes()

ConvertToZeroesAndOnes   <- function(dataFrame) {
  
  colnames(dataFrame)[1] <- "HeadingColumn" #Give temporary name so dplyr code works
  
  dataFrame$HeadingColumn <- as.character(dataFrame$HeadingColumn)
  uniqueValues <- dplyr::distinct(dataFrame, HeadingColumn)
  
  zeroesAndOnesdf <- data.frame(matrix(data = 0, nrow = nrow(dataFrame), ncol = nrow(uniqueValues)))
  names(zeroesAndOnesdf) <- uniqueValues$HeadingColumn
  
  
  for (i in 1:nrow(dataFrame)) {            #for loop moves values from dataFrame to appropriate spot in zeroesAndOnesdf
    rowText  <- dataFrame[i, 1]
    rowValue <- dataFrame[i, 2]
    zeroesAndOnesdf[i, rowText] <- rowValue
    
  }
  
  return(zeroesAndOnesdf)
}

#' Checks if dataFrame contains column weight and adds rowcount column
#' 
#' Checks if dataFrame contains column weight. If not, it will add weight column containing 1 for each row. This function will also add a rowcount column containing ones to be used for nrow calculations.
#' @export
#' @examples
#' CleanDataFrame()

CleanDataFrame           <- function(dataFrame) {
  
  if(!("weight" %in% names(dataFrame))) {
    warning("Column [weight] not found. Check that column heading is not [Weight] or [WEIGHT] instead of [weight].
              Code will continue by applying weight of 1 to each row.")
    dataFrame$weight <- 1
  }
  
  dataFrame$rowcount <- 1
  
  return(dataFrame)
  
}