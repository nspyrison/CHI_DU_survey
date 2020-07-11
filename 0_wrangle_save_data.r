##### Wrangle and save data for CHI_DU survey responses
## NS 09/07/2020

library("dplyr")
library("ggplot2")
library("readxl")
library("stringr")


## Decoding original column names
.read_colnames <- readxl::read_xlsx(
  path = "./data/Perceptions+on+a+Virtual+Conference_July+8,+2020_17.08.xlsx",
  skip = 0, n_max = 1)
.raw_short_cn <- colnames(.read_colnames)
.raw_long_cn  <- as.character(.read_colnames[1, ])
colnames_tbl  <- data.frame(ShortColNames = .raw_short_cn,
                            LongColNames = .raw_long_cn)

## Read data
raw <- readxl::read_xlsx(
  path = "./data/Perceptions+on+a+Virtual+Conference_July+8,+2020_17.08.xlsx",
  skip = 1)
colnames(raw) <- .raw_short_cn
skimr::skim(raw)
## Wrangling -----
df <- raw
df <- df[df$`Status` == "IP Address", ]  ## Remove 8 "preview" surveys, 112 rows remain
df <- select(df, !c(RecipientLastName,   ## Remove 6 all NaN columns:
                    RecipientFirstName,
                    RecipientEmail,
                    ExternalReference,
                    Q3.1_4_TEXT,
                    Q4.7_8,
                    DistributionChannel, ## Remove DistributionChannel, all == "anonymous"
                    UserLanguage))       ## Remove UserLanguage, all == "EN-GB"
## Parse emails to look at domain
.at_nchar <- stringr::str_locate(df$Q2.4, "@")[, 1]
df$Q2.4_names   <- substr(df$Q2.4, 1,             .at_nchar - 1)
df$Q2.4_domains <- substr(df$Q2.4, .at_nchar + 1, nchar(df$Q2.4))
skimr::skim(df)


### Decodes the column names
ns_add_LongColNames <- function(df){
  ## Grab all same names
  .LongColNames <- colnames_tbl[which(colnames_tbl[, 1] %in% colnames(df)), 2]
  ## Fix first 2
  .LongColNames[1:2] <- .LongColNames[2:1]
  ## Fix last 2; email broke to name, domain
  .LongColNames <- c(.LongColNames, "email_name", "email_domain")
  ## Convert df to char df, append LongColNames as top row.
  .rest <- as.data.frame(as.matrix(df, ncol = ncol(df)))
  ret <- as.data.frame(rbind(.LongColNames, .rest))
  colnames(ret) <- colnames(df)
  rownames(ret) <- NULL
  #str(ret, nchar.max = 30) ## Reviewing correctness
  ret
}

##### df_text and df_qaunt -----
.cols_contain_TEXT <- grep("_TEXT", colnames(df))
.cols_gt_6_unique <- NULL
for (i in .cols_Qs){
  if(length(unique(df[, i])) > 6)
    .cols_gt_6_unique <- c(.cols_gt_6_unique, i)
}
## ResponseId, RecordedDate, (cols with > 5 unique) & (*_TEXT columns)
.cols_Qs_text <- unique(c(9, 8, .cols_contain_TEXT, .cols_gt_5_unique))
df_text <- df[, .cols_Qs_text]


### df_quant
.cols_Qs_notText <- .cols_Qs[!(.cols_Qs %in% .cols_Qs_text)]
.cols_quant <- c(9, 8, .cols_Qs_notText)
df_quant <- df[, .cols_quant]


### Save cleaned datasets to .csv files -----
if(F){
  .decoded_df_full  <- ns_add_LongColNames(df)
  .decoded_df_text  <- ns_add_LongColNames(df_text)
  .decoded_df_quant <- ns_add_LongColNames(df_quant)
  .fn_full  <- paste0("./data/cleaned_full_survey_responses_ns",  Sys.Date(), ".csv")
  .fn_text  <- paste0("./data/cleaned_text_survey_responses_ns",  Sys.Date(), ".csv")
  .fn_quant <- paste0("./data/cleaned_quant_survey_responses_ns", Sys.Date(), ".csv")
  write.table(x = .decoded_df_full,  file = .fn_full,  row.names = FALSE, sep = ",")
  write.table(x = .decoded_df_text,  file = .fn_text,  row.names = FALSE, sep = ",")
  write.table(x = .decoded_df_quant, file = .fn_quant, row.names = FALSE, sep = ",")
  cat(paste0("NS: Saved cleaned FULL data to ",  .fn_full,  ". \n"))
  cat(paste0("NS: Saved cleaned TEXT data to ",  .fn_text,  ". \n"))
  cat(paste0("NS: Saved cleaned QUANT data to ", .fn_quant, ". \n"))
}
