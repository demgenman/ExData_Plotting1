# Import
debugPrint <- FALSE

debug.print <- function(...) {
    if (debugPrint) {
        message(...)
    } 
}

# Patterns to match selected rows
patternRowBegin <- "1/2/2007"
patternRowEnd <- "3/2/2007"

## Download and unzip dataset
zippedFile <- "household_power_consumption.zip"
if (!file.exists(zippedFile)) {
    url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    debug.print("Download ", url)
    download.file(url, destfile=zippedFile, method="internal")
    rm(url)
}
# Get list of names contained in zip file
fileName <- unzip(zippedFile, list=TRUE)
# Unzip
debug.print("Unzip ", zippedFile)
unzip(zippedFile)

## Read dataset
f <-  paste0("", fileName[1, "Name"])
debug.print("Open connection to ", f)
df <- data.frame(character(0))
con1 <- file(f, open="rt")
debug.print("Header row")
h <- readLines(con1, 1)
debug.print("Skip data rows until begin date ...")
repeat {
    d <- readLines(con1, 1)
    if (length(d) == 0 || grepl(paste0("^", patternRowBegin), d)) {
        break
    }
}
debug.print("Read data rows until end date ...")
d2 <- vector()
while (length(d) > 0 && !grepl(paste0("^", patternRowEnd), d)) {
    # Convert missing values ? to NA
    d <- gsub("?", "NA", d, fixed=TRUE)
    d2 <- c(d2, d)
    d <- readLines(con1, 1)
}
debug.print("Close connection.")
close(con1)

## Split into columns and convert to data frame
debug.print("Convert to data frame")
d3 <- sapply(d2, strsplit, split=";", fixed=TRUE)
#df <- do.call(rbind.data.frame, d3)
df <- data.frame(do.call(rbind, d3), stringsAsFactors=FALSE)

rownames(df) <- NULL
colnames(df) <- unlist(strsplit(h, ";", fixed=TRUE))

## Format conversions
# Convert values to numeric
for (i in 3:9) {
    df[,i] <- as.numeric(df[,i])
}
# Generate combined date/time variable
df[,"DT"] <- as.POSIXct(strptime(paste(df$Date, df$Time, sep=" "), format="%d/%m/%Y %H:%M:%S"))

if (debugPrint) {
    # Show a few stats
    message("Nr of NA values in dataset:")
    print(sapply(df, function(x) sum(is.na(x))))
    message("Dataset summary:")
    print(summary(df))
}

# remove zip file
debug.print("Clean up")
rm(con1, f)
rm(h, d, d2, d3)
if (file.exists(zippedFile)) {
    #file.remove(zippedFile)
}
debug.print("Import ready")

# Create plot 2
# Use cairo to create bit depth 32 (windows creates png with bit depth 8)
png(filename="plot2.png", width=480, height=480, pointsize=12, type="cairo", bg="transparent")

# approach off-black color of original
mainColor <- "gray0"

with (df, {
    plot(Global_active_power ~ DT, ylab="Global Active Power (kilowatts)", xlab="", main="", pch="")
    lines(x=DT, y=Global_active_power, col=mainColor)
})

dev.off()
