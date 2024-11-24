library(datawizard)

data(efc)

cb <- datawizard::data_codebook(df)

datawizard::to_factor(mydata, "VG_Frequency", dummy_factors = FALSE)

mynewdata <- factor(mydata$VG_Frequency, levels = c("Less than once a month", "About once a month", ))

mylevels <- c(
  "Less than once a month",
  "About once a month",
  "A couple of times a month",
  "About once a week",
  "About 2 or 3 times a week",
  "About 4 or 5 times a week",
  "Almost every day"
)

# mynewdata <- datawizard::assign_labels(mynewdata, values = myvals)
# levels(mynewdata)
# labels(mynewdata)
#
# as.factor(mydata$VG_Frequency)
#
# head(to_factor(mydata$VG_Frequency))


df <- read.csv("C:/Users/Danny Squire/OneDrive/Documents/Projects/test-datasetx/mills_allen.csv",
               header = TRUE,
               na.strings = "NA")

myfreq <- factor(df$VG_Frequency, levels = mylevels, ordered = TRUE)

library(correlation)

df$Ethnicity_Elab <- NULL

mycors <- correlation::correlation(dfnew, method = "auto", redundant = FALSE, include_factors = TRUE)

datawizard::empty_columns(df)
datawizard::empty_rows(df)
datawizard::data_peek(df)
df$VG_Frequency <- myfreq

dfnew <- datawizard::data_extract(df, c("VG_Frequency", "Intrinsic", "Integrated"))
correlation::cor_lower(dfnew, diag = FALSE)

blah <- as.data.frame(sapply(df, is.character))

blah <- datawizard::data_peek(df)

datawizard::empty_columns(df)

anyNA(df)

print(names(which(vapply(df, is.character, FUN.VALUE = logical(1)))))

names(df)
length(names(datawizard::empty_columns(df)))
typeof(datawizard::empty_columns(df))
datawizard::empty_rows(df)
replace
df$Male <- NA
df$EnglishNativeLang <- NA

ignore = c("Age", "Gender", "Ethnicity", "Male")
colorder <- colnames(df)

ignorecolumns <- datawizard::get_columns(df, ignore)
df <- datawizard::data_remove(df, select = ignore)
newdf <- cbind(ignorecolumns, df)

newdf <- data_reorder(newdf, colorder)
identical(df, newdf)

unique(df$Ethnicity)
