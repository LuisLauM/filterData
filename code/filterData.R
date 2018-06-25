if(file.exists("code/filterData.Rout")) unlink(x = "code/filterData.Rout")
rm(list = ls()); gc(reset = TRUE)

# Parameters --------------------------------------------------------------
# Path for input
filename <- choose.files(default = getwd(), multi = FALSE, 
                         caption = "Select the directory for the input")

# Path for message table
messageCodeFile <- "data/messageCodes.csv"

# Path for output
outDir <- choose.dir(caption = "Select the directory for the output", 
                     default = getwd())

# Analysis ----------------------------------------------------------------

require(openxlsx)

# Function for generating warning messages using meesageList as a dictionary of issues
warningText <- function(..., messageList, code, prevText = NULL, language = "english", 
                        atStart = "", atEnd = "", times = 1){
  languageCol <- paste0("message_", substr(language, 1, 3))
  
  msg <- unlist(strsplit(x = messageList[code, languageCol], split = "---"))
  msg <- c(sprintf(fmt = msg[1], ...), msg[2])
  msg <- c(code, paste0(atStart, msg[1], atEnd), msg[2]) 
  
  msg <- matrix(data = msg, nrow = times, ncol = length(msg), byrow = TRUE)
  
  if(!is.null(prevText)){
    for(i in seq(ncol(msg))){
      msg[,i] <- paste(prevText[,i], msg[,i], sep = " & ")
    }
  }
  
  return(msg)
}

# Funtion to find a vector inside a vector
vectorINvector <- function(x, pattern){
  lenPatt <- length(pattern)
  
  out <- NULL
  for(i in seq(1, length(x) - lenPatt + 1)){
    if(isTRUE(all.equal(x[seq(i, i + lenPatt - 1)], pattern))){
      out <- c(out, i)
    }
  }
  
  return(out)
}

# Read extra arguments
extraArguments <- readLines(con = "filterArguments.txt", warn = FALSE, encoding = "UTF-8")
extraArguments <- gsub(x = extraArguments, pattern = " ", replacement = "")

# Get language
language <- extraArguments[grepl(x = extraArguments, pattern = "language=")]
language <- gsub(x = language, pattern = "language=", replacement = "")

# Get a_b and tolerance
ab_tolerance <- sapply(c("a=", "b=", "tolerance="), grepl, x = extraArguments)
ab_tolerance <- apply(ab_tolerance, 2, function(x, y) x[y], x = extraArguments)
ab_tolerance <- mapply(gsub, pattern = names(ab_tolerance), x = ab_tolerance, 
                       MoreArgs = list(replacement = ""))
ab_tolerance <- as.list(as.numeric(ab_tolerance))
names(ab_tolerance) <- c("a", "b", "tolerance")

# Read data
originalData <- read.xlsx(xlsxFile = filename, sheet = 1, check.names = TRUE)

# Correct column names
colnames(originalData) <- tolower(colnames(originalData))
colnames(originalData) <- chartr(old = "αινσϊόρ", new = "aeiouun", x = colnames(originalData))

# Add number of row for data
originalData$n_orig <- seq(nrow(originalData))

# Generates dates' values
originalData$date <- with(originalData, as.Date(paste(ano, mes, dia, sep = "-")))

# Round catch weight (p.captura) and sample weight (p.muestra)
originalData$p.captura <- round(originalData$p.captura, 2)
originalData$p.muestra <- round(originalData$p.muestra, 2)

# Add a column for storaging filter observations
originalData$Filter_Sug <- originalData$Filter_Obs <- originalData$Filter_IssueCode <- ""
obsSugIndex <- c("Filter_IssueCode", "Filter_Obs", "Filter_Sug")

# Read message codes file
messageList <- read.csv(file = messageCodeFile, stringsAsFactors = FALSE)

# FILTER 1:::::CHECK NA ON IMPORFTANT VARIABLES
# 1. Define names of columns used for define travel code
criteriaList <- c("recurso", "date", "laboratorio", "embarcacion", "zona.de.pesca", "p.captura", "p.muestra")

# 2. Check NA and put warnings by row
naMatrix <- is.na(originalData[,criteriaList])
for(i in which(rowSums(naMatrix) > 0)){
  # Add warning and suggestion texts
  originalData[i, obsSugIndex] <- warningText(paste(criteriaList[is.na(originalData[i, criteriaList])], collapse = ", "), 
                                              messageList = messageList, code = 1, 
                                              language = language)
}

cat("\nEnd Filter 1 without errors.\n")

# FILTER 2:::::CHECK (strange) SIMILAR DISTRIBUTIONS
# 1. Define names of columns used for define travel code
criteriaList <- c("recurso", "date", "laboratorio", "embarcacion", "zona.de.pesca", "p.captura", "p.muestra")

# Sort data and add n index for sorted data
index <- do.call(order, originalData[,criteriaList])
originalData <- originalData[index,]

index <- match("n_orig", colnames(originalData))
originalData <- data.frame(originalData[,seq(index)], n_filter = seq(nrow(originalData)), 
                           originalData[,seq(index + 1, ncol(originalData))], stringsAsFactors = FALSE)

# Generates code by pasting columns specified on 'filter' vector of criteriaList
originalData$travel_code <- apply(originalData[,criteriaList], 1, paste, collapse = "-")
originalData$travel_code <- cumsum(!duplicated(originalData$travel_code))

# 2. Get a table with relevant info (mean and variance) for each travel
allTravels <- data.frame(travel_code = unique(originalData$travel_code),
                         rowRange = NA, mean = NA, var = NA, wCatch = NA, wSample = NA,
                         stringsAsFactors = FALSE)

# 3. Get relevant info by travel in order to identify duplicated data
for(i in seq(nrow(allTravels))){
  # Subset data by travels
  index <- originalData$travel_code == allTravels$travel_code[i]
  tempData <- originalData[index,]
  
  # Get row ranges
  allTravels$rowRange[i] <- paste(range(which(index)), collapse = "-")
  
  # Get relevant info (mean, var and n_row)
  tempValues <- rep(tempData$long..cm., tempData$fcia..absoluta)
  allTravels$mean[i] <- mean(tempValues, na.rm = TRUE)
  allTravels$var[i] <- var(tempValues, na.rm = TRUE)
  allTravels$n_row[i] <- nrow(tempData)
  
  # Get weights info
  allTravels$wCatch[i] <- unique(tempData$p.captura)
  allTravels$wSample[i] <- unique(tempData$p.muestra)
}

# 4. Check which travels have the same mean, variance and n_row
allTravels <- allTravels[do.call(order, allTravels[,c("mean", "var", "n_row", "wCatch", "wSample")]),]
dupIndex <- which(duplicated(allTravels[,c("mean", "var", "n_row", "wCatch", "wSample")]))

# Each duplicated case generates an entry on Filter_Obs column
for(i in seq_along(dupIndex)){
  index1 <- originalData$travel_code == allTravels$travel_code[dupIndex[i]]
  index2 <- originalData$travel_code == allTravels$travel_code[dupIndex[i] - 1]
  
  # Add warning and suggestion texts
  originalData[index1, obsSugIndex] <- warningText(unique(originalData$travel_code[index2]),
                                                   prevText = originalData[index1, obsSugIndex], 
                                                   messageList = messageList, code = 2, 
                                                   language = language, times = sum(index1))
}

cat("\nEnd Filter 2 without errors.\n")

# FILTER 3:::::CHECK DUPLICATED VALUES FOR LENGTH COLUMN INTO SAME TRAVELS
allTravels <- unique(originalData$travel_code)
for(i in seq_along(allTravels)){
  # Subset data by travel code
  index1 <- originalData$travel_code == allTravels[i]
  tempData <- originalData[index1,]
  
  # Check duplicated values for length categories
  index2 <- duplicated(tempData$long..cm.)
  if(sum(index2) > 0){
    originalData[index1, obsSugIndex][index2,] <- warningText(messageList = messageList, code = 3,
                                                              prevText = originalData[index1, obsSugIndex][index2,], 
                                                              language = language, times = sum(index2))
  }
}

cat("\nEnd Filter 3 without errors.\n")

# FILTER 4:::::CHECK WRONG VALUES DUE TO DRAG & DROP IN EXCEL
# 1. Define names of columns used for define travel codes by different criteria
criteriaList <- list(c1 = c("recurso", "date", "laboratorio", "zona.de.pesca", "p.captura"    , "p.muestra"),
                     c2 = c("recurso", "date", "laboratorio", "embarcacion"  , "zona.de.pesca", "p.muestra"),
                     c3 = c("recurso", "date", "laboratorio", "embarcacion"  , "zona.de.pesca", "p.captura"),
                     c4 = c("recurso", "date", "embarcacion", "zona.de.pesca", "p.captura"    , "p.muestra"),
                     c5 = c("recurso", "date", "laboratorio", "embarcacion"  , "p.captura"    , "p.muestra"))

# 2. Build a matrix as a multicriteria index
criteriaCols <- matrix(data = NA, nrow = nrow(originalData), ncol = length(criteriaList))
for(i in seq_along(criteriaList)){
  # Generates code by pasting columns specified on 'filter' vector of criteriaList
  tempVals <- apply(originalData[,criteriaList[[i]]], 1, paste, collapse = "-")
  criteriaCols[,i] <- cumsum(!duplicated(tempVals))
}

# 3. Check unusual 1-row-travels

# Check diff travel codes along each variable
diffMatrixV <- apply(criteriaCols, 2, function(x) c(0, diff(x)))

# Find 1-row-travels by finding repeated changes, pattern = c(1, 1)
oneRowTravelIndex <- apply(diffMatrixV, 2, vectorINvector, pattern = c(1, 1))

# Get all issues
allIssues <- unique(do.call(c, oneRowTravelIndex))

# Get common issues for all variables and report them
commonIssues <- Reduce(intersect, oneRowTravelIndex)
for(i in seq_along(commonIssues)){
  index <- commonIssues[i]
  originalData[index, obsSugIndex] <- warningText(prevText = originalData[index, obsSugIndex],
                                                  messageList = messageList, code = 4, language = language)
}

# Remove commonIssues from allIssues
allIssues <- allIssues[!is.element(allIssues, commonIssues)]

# Check issues by variable
varNames <- as.character(sapply(criteriaList, setdiff, x = unique(do.call(c, criteriaList))))
for(i in seq_along(allIssues)){
  detectVariable <- sapply(oneRowTravelIndex, function(x) !any(is.element(x, allIssues[i])))
  index <- allIssues[i]

  originalData[index, obsSugIndex] <- warningText(paste(varNames[detectVariable], collapse = ", "),
                                                  prevText = originalData[index, obsSugIndex], 
                                                  messageList = messageList, code = 5, language = language)
}

cat("\nEnd Filter 4 without errors.\n")

# FILTER 5:::::CHECK (STRANGELY) HIGH DIFFERENCES ON SAMPLE WEIGHTS
# Calculate teoric weight for each length category
originalData$WbyFreqs <- with(ab_tolerance, a*originalData$long..cm.^b*originalData$fcia..absoluta)*1e-3
WbyTravel <- with(originalData, tapply(WbyFreqs, travel_code, sum, na.rm = TRUE))

# Calculate diff between teorical and observed values
index <- match(originalData$travel_code, names(WbyTravel))
originalData$WbyTravel <- WbyTravel[index]
originalData$diffWeights <- with(originalData, abs(WbyTravel - p.muestra))

# Add warning values
index <- !is.na(originalData$diffWeights) & originalData$diffWeights > ab_tolerance$tolerance
originalData[index, obsSugIndex] <- warningText(ab_tolerance$tolerance, prevText = originalData[index, obsSugIndex], 
                                                messageList = messageList, code = 6, language = language, times = sum(index))

# Remove temporal columns created for diff filter
originalData <- originalData[,-match(c("WbyFreqs", "WbyTravel", "diffWeights"), colnames(originalData))]

cat("\nEnd Filter 5 without errors.\n")

# Correct warning labels
for(i in seq_along(obsSugIndex)){
  originalData[,obsSugIndex[i]] <- gsub(x = originalData[,obsSugIndex[i]], replacement = "", pattern = "^( \\& )", perl = TRUE)
}

# Save worksheet file
if(!dir.exists(paths = outDir)) dir.create(path = outDir, showWarnings = FALSE)
write.xlsx(x = originalData, file = paste0("outputs/output_", format(Sys.time(), format = "%Y%m%d-%H%M%S"), ".xlsx"))

cat("\nEverything was OK!\n")
if(file.exists("code/filterData.Rout")) unlink(x = "code/filterData.Rout")