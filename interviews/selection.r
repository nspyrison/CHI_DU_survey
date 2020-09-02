library("dplyr")
library("ggplot2")
library("knitr")
# do_write_xlsx <- FALSE

prefix <- c("full", "quant", "text")
do_shorten_col_nms <- c(T, F, F)
i_s <- 1:length(prefix)
for(i in i_s) {
  xFilepath = paste0("../data/cleaned_", prefix[i], "_survey_responses_ns2020-07-11.csv")
  .df <- read.csv2(xFilepath, 
                   skip = 1, sep = ",", check.names = F)
  .header <- read.csv2(xFilepath, 
                       skip = 0, sep = ",", nrows = 1)
  .ShortColNames <- colnames(.header)
  if(do_shorten_col_nms[i] == T)
    colnames(.df) <-  .ShortColNames
  .LongColNames  <- as.character(.header)
  .colname_tbl <- data.frame(cbind(.ShortColNames,
                                   .LongColNames,
                                   1:ncol(.header)))
  colnames(.colname_tbl) <- c("ShortName", "LongName", "ColNumber")
  df_nm <- paste0("df_", prefix[i])
  assign(df_nm, .df)
  col_nm <- paste0("colname_tbl_", prefix[i])
  assign(col_nm, .colname_tbl)
  cat(paste0("Assigned ", df_nm, " and ", col_nm, " to the global env. \n"))
}
## EXPECTED init 
f <- df_full
q <- df_quant
t <- df_text

### Geom bar helper
ns_geom_bar_ord <- function(col_num, ## NUMBER of the column
                            df_obj = q,
                            top_n = 8,
                            do_ord_decr = TRUE) {
  ## Decoded long name
  .col_nm <- colnames(df_obj)[col_num]
  ## Vector of values
  .x <- df_obj[, col_num]
  .x_alt <- .x
  .x_alt[is.na(.x_alt)] <- "<NA>"
  .x_alt[.x_alt == ""] <- "<blank>"
  .cnt_tbl <- sort(table(.x_alt), decreasing = !do_ord_decr)
  .df_cnt <- data.frame(Options = names(.cnt_tbl), 
                        Count = as.numeric(.cnt_tbl))
  
  ## Sum loower options
  .n_rows <- nrow(.df_cnt)
  if(.n_rows > top_n) {
    .df_cnt <- data.frame(
      rbind(.df_cnt[1:top_n, ],
            c("<all other options>", sum(.df_cnt[(top_n + 1):.n_rows, 2]))
      )
    )
  }
  ## Fix disp order after appending <all others>
  .df_cnt$Options <- factor(.df_cnt$Options, levels = unique(.df_cnt$Options))
  
  ## numerator and denominator for response rate
  .n <- length(.x[!is.na(.x)])
  .d <- length(.x)
  
  ## Create bar chart
  ggplot(.df_cnt, aes(x = Options, y = Count, fill = as.numeric(Count))) + 
    geom_col(stat = "identity") + 
    coord_flip() + 
    ggtitle(paste0("Top ", top_n, " reponses of: ", .col_nm),
            paste0(.n, " non-na obs of ", .d, " respones (", round(100 * .n/.d, 1),"%)")) +
    #scale_x_discrete(limits = rev(levels(.df_cnt$Options))) +
    labs(fill = "Count")
}

library("wordcloud")
library("RColorBrewer")
library("tm")
set.seed(1234) # for reproducibility 
ns_make_wordcloud <- function(col_num, ## NUMBER of the column
                              df_obj = t) {
  ## following: https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
  ## STEP 1: Retrieving the data and uploading the packages
  .x <-  df_obj[, col_num]
  .x_alt <- .x[!is.na(.x)]
  .x_alt <- tolower(.x_alt)
  docs <- Corpus(VectorSource(.x_alt))
  
  ## STEP 2: Clean the text data
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  ## STEP 3: Create a document-term-matrix
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix), decreasing = TRUE) 
  df <- data.frame(word = names(words), freq = words)
  
  ## STEP 4: Generate the word cloud
  ## (set seed outside of for loop)
  .col_nm <- colnames(df_obj)[col_num]
  .title <- paste0(nrow(df), " responses, for: " , .col_nm)
  
  # layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  # par(mar=rep(0, 4))
  # plot.new()
  # text(x=0.5, y=0.5, .title, cex = 1)
  cat(.title)
  try(
    wordcloud(words = df$word, freq = df$freq, min.freq = 1,
              max.words=200, random.order = FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"), main = "Title")
  )
}


library(DT)
### SUBSET COLUMNS
sub <- df_full %>% select(Q2.2, Q2.3, Q2.4, Q3.1, Q3.2, Q3.3, Q3.4_1, Q3.5, 
                          Q4.1, Q4.2, Q4.3, StartDate, Finished,
                          Duration..in.seconds., LocationLatitude, LocationLongitude, Q6.8:Q6.14)
### SUBSET ROWS
sub2 <- sub
sub2 <- sub2[sub2[, 1] == "Yes" &
               is.na(sub2[, 1]) == FALSE, ] ## Aggree to be interviewed
sub2 <- sub2[nchar(sub2[, 3]) > 5 &         ## Have follow up email
               is.na(sub2[, 3]) == FALSE, ] 
sub2 <- sub2[sub2$Finished == "True", ]     ## Did finish, remove 2 low info rows
dim(sub2)

### HUMAN READABLE COLNAMES
sub_colnames_lookup <- data.frame(ShortName = colnames(sub)) %>% 
  left_join(colname_tbl_full, by = "ShortName")
sub_colnames <- paste0(sub_colnames_lookup$ShortName,"--",sub_colnames_lookup$LongName)
sub_colnames[12] <- "StartDate--Start Date"
colnames(sub) <- sub_colnames

str(sub)



DT::datatable(sub2, options = 
                list(
                  order = list(list(1, 'desc'), list(2, 'asc'))
                )
)


if (do_write_xlsx == TRUE){
  library("openxlsx")
  writexl::write_xlsx(sub, path = "./output/CHI_DU_InterviewCanidates_raw.xlsx")
}