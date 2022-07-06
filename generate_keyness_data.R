# generate_keyness_data.R
# To generate keyness data:
# - Install "tm" and "RWeka" packages.
# - Set working directory to the project directory.
# - Make sure the directories "corpustxts" and "rdata" exist, the former filled with novel txts.
# - Read all functions into R (i.e. run all code)
# - Run make_data_chunks()
# E.g.:
# setwd("/Users/phb514/mygit/memo-keywords-across-time")
# install.packages(c("tm", "RWeka"))
# Run all code (e.g. opt+cmd+R)
# make_data_chunks()

suppressMessages(library("tm"))
options(java.parameters = "-Xmx1024m")
suppressMessages(library("RWeka"))
options(scipen=999)

years <- 1870:1899


make_data_chunks <- function(chunksize=5) {
  # Split year range into chunks of <chunksize> years each
  yearchunks <- split(years, ceiling(seq_along(years)/chunksize))
  resultlist <- list()
  for (chunk in yearchunks) {
    print(paste0('NEW DATASET (', paste(chunk, collapse = ', '), ') -------------------------------------'))
    target_signif <- make_data(chunk)
    dataname <- paste(c(head(chunk, 1), tail(chunk, 1)), collapse = "-")
    resultlist[[dataname]] <- target_signif
  }
  assign("signifdata", resultlist, envir = .GlobalEnv)
  save(signifdata, file = "rdata/signifdata.Rda")
}


make_data <- function(yearvector) {
  complementyears = years[!years %in% yearvector]
  
  # Target corpus freqlist
  target.aggregated.freqlist <- make_aggregated_freqlist(yearvector)
  #print(head(target.aggregated.freqlist))
  #print(nrow(target.aggregated.freqlist))
  
  # Reference corpus freqlist
  ref.aggregated.freqlist <- make_aggregated_freqlist(complementyears)
  #print(head(ref.aggregated.freqlist))
  #print(nrow(ref.aggregated.freqlist))
  
  merged.corpusdata <- merge_corpusdata(target.aggregated.freqlist, ref.aggregated.freqlist)
  merged.corpusdata <- add_stats(merged.corpusdata, target.aggregated.freqlist, ref.aggregated.freqlist)
  #print(head(merged.corpusdata, 100))
  
  # Mønstre der er overrepræsenteret i enten LANCHART eller NEWS.
  signif.df <- sort_by(merged.corpusdata[merged.corpusdata[, 'pval'] <= .001, ], 'loglik')
  signif.df <- signif.df[signif.df$T.count >= 4, ]
  #print(head(signif.df, 100))
  
  # Signifikante LANCHART-ngrammer.
  target.signif <- signif.df[signif.df$loglik > 0,]
  # Signifikante NEWS-ngrammer.
  ref.signif <- signif.df[signif.df$loglik < 0,]
  
  print('target.signif -------------------------')
  print(head(target.signif, 100))
  print('nrow target.signif')
  print(nrow(target.signif))
  print('ref.signif --------------------------')
  print(head(ref.signif, 100))
  print('nrow ref.signif')
  print(nrow(ref.signif))
  return(target.signif)
}


blaha <- function() {
  # Reference corpus ------------------------------------------------------------------
  # Linjer med en avisnarrativ i hver.
  newspaper.narr <- filter_newspaper_lines('data/newspaper/output1.txt.ord.pos.lem.SOLIDUS.txt')
  # Korpus med opmærkede tokens.
  newspaper.narr.corp <- get_corpus(newspaper.narr)
  
  # Term-doc matrix over rå avistokens.
  newspaper.words.tdm <- make_tdm(newspaper.narr.corp, min_n=1, max_n=1)
  inspect(newspaper.words.tdm)
  # Frekvensliste af rå tokens i NEWS.
  news.words.freq <- make_freqlist(newspaper.words.tdm)
  
  # Term-doc matrix over ikke-hybridiserede n-grammer i NEWS.
  news.token.ngrams.tdm <- make_tdm(newspaper.narr.corp, min_n=4, max_n=4)
  inspect(news.token.ngrams.tdm)
  # Dataframe med n-gram-frekvens m.m. Tager ca. 3 min.
  news.token.ngrams.freq <- make_corpusdata(news.token.ngrams.tdm, news.words.freq)
  
  # Det endelige datasæt -------------------------------------------------------
  lanch.news.df <- merge_corpusdata(lanchart.token.ngrams.freq, news.token.ngrams.freq)
  # Tilføj statistikker.
  lanch.news.df <- add_stats(lanch.news.df, lanchart.words.freq, news.words.freq)
  
  data <- list(lanch=lanchart.narr, lwt=lanchart.words.tdm, lwf=lanchart.words.freq,
               news=newspaper.narr, nwt=newspaper.words.tdm, nwf=news.words.freq,
               lanch.news=lanch.news.df)
  return(data)
}




add_stats <- function(mergedDf, freqDfA, freqDfB) {
  # Tilføj forskellige mål.
  #print(head(mergedDf))
  pre <- paste0(min_diff_str(deparse(substitute(freqDfA)), deparse(substitute(freqDfB))), '.')
  countA <- paste0(pre[1], 'count')
  countB <- paste0(pre[2], 'count')

  #print(head(mergedDf))
  #print(sum(mergedDf[countA]))
  #print(sum(mergedDf[countB]))
  
  
  # Loglik
  mergedDf['loglik'] <- loglik(mergedDf[countA], mergedDf[countB], sum(mergedDf[countA]), sum(mergedDf[countB]))
  mergedDf['pval'] <- loglik_p(mergedDf['loglik'][[1]])
  
  mergedDf
}

merge_corpusdata <- function(a, b) {
  # Sammenstil data fra to forskellige korpusser.
  pre <- min_diff_str(deparse(substitute(a)), deparse(substitute(b)))
  df <- merge_freqdata(a, b, pre)
  return(df)
}

min_diff_str <- function(s1, s2) {
  # Returner n første bogstaver af s1 og s2 hvor n er det mindste tal hvor s1 og s2 er forskellige.
  equal_length <- toupper(gsub(' ', '_', format(c(s1, s2), width=max(nchar(c(s1, s2))))))
  splits <- strsplit(equal_length, '')
  equal_v <- splits[[1]] == splits[[2]]
  firstfalse <- which(equal_v == FALSE)[1]
  return(c(paste(splits[[1]][1:firstfalse], collapse=''), paste(splits[[2]][1:firstfalse], collapse='')))
}

merge_freqdata <- function(a, b, pre='') {
  # Sæt frekvensdata fra to korpusser sammen til én dataframe.
  if (length(pre) != 2) pre <- min_diff_str(deparse(substitute(a)), deparse(substitute(b)))
  names(a)[sapply(a, is.numeric)] <- paste0(pre[1], '.', names(a)[sapply(a, is.numeric)])
  names(b)[sapply(b, is.numeric)] <- paste0(pre[2], '.', names(b)[sapply(b, is.numeric)])
  #print(c(names(a), names(b)))
  # Full merge - alle termer fra a og alle termer fra b.
  df <- merge(a, b, all=TRUE)
  df[is.na(df)] <- 0
  # Remove 
  hybridcols <- names(df)[grepl("hybrid.|uif", names(df))]
  # TODO! Tilføj N.hybridcount, N.uif, N.sc og L. ... EFTER merge.
  #for (col in hybridcols) df[col] <- ave(df[col], df$hybrid, FUN=max)
  #print('head of merged df')
  #print(head(df))
  return(df)
}





loglik <- function(a, b, a_total, b_total) {
  # Lav log-likelihood-værdier for antal forekomster i a og b.
  a <- a + .1  # Eliminer 0'er så log() ikke giver Inf.
  b <- b + .1
  
  # Lav relative frekvenser og expected values.
  a_rel <- a / a_total
  b_rel <- b / b_total
  e1 <- a_total * (a + b) / (a_total + b_total)
  e2 <- b_total * (a + b) / (a_total + b_total)
  
  # Plusminus: Signal der angiver om a eller b er størst.
  rel_diff  <- a_rel - b_rel
  plusminus <- rel_diff / abs(rel_diff)
  
  # Log Likelihood-værdier. Eliminer na. Rund af til 2 decimaler.
  loglik <- 2 * (a * log(a/e1) + b * log(b/e2)) * plusminus
  loglik[is.na(loglik)] <- 0
  return(round(loglik, 2))
}


loglik_p <- function(loglik) {
  # Afrundet p-værdi af log-likelihood-værdi med df = 1.
  round(1 - pchisq(abs(loglik), df=1), 3)
}

make_aggregated_freqlist <- function(yearvector) {
  freqlists.rbound <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(freqlists.rbound) <- c('term', 'count')
  for (year in yearvector) {
    freqlist <- get_year_freqlist(year)
    freqlists.rbound <- rbind(freqlists.rbound, freqlist)
  }
  aggregated.freqlist <- aggregate(. ~ term, data = freqlists.rbound, sum)
  aggregated.freqlist
}

get_year_freqlist <- function(year) {
  rdatadir <- 'rdata/'
  freqfilename <- paste0(rdatadir, year, '.freqs.Rda')
  if (file.exists(freqfilename)) {
    load(freqfilename)
  } else {
    searchpattern <- paste0('^', year)
    corpus <- get_corpus(searchpattern)
    corpus_tdm <- make_tdm(corpus, 1, 1)
    freqlist <- make_freqlist(corpus_tdm)
    save(freqlist, file=freqfilename)
  }
  freqlist
}

get_corpus <- function(pattern) {
  srcdir = 'corpustxts'
  corpus_source <- tm::DirSource(directory = srcdir, pattern = pattern)
  tm::VCorpus(corpus_source, readerControl = list(language = "da"))
}

make_tdm <- function(corpus, min_n, max_n) {
  # Lav en term-document matrix med mine specifikationer.
  dlm <- ' \r\n\t.,;:"„“«»()?!—'
  tok <- function(x) RWeka::NGramTokenizer(x, Weka_control(min=min_n, max=max_n, delimiters=dlm))
  tdm.params <- list(tokenize=tok, tolower=FALSE, wordLengths=c(1, Inf))
  tm::TermDocumentMatrix(corpus, control = tdm.params)
}

make_freqlist <- function(tdm, ignore="") {
  # Lav en frekvensliste af en term-doc matrix.
  mat <- as.matrix(tdm)
  freqlist <- rowSums(mat)
  df <- data.frame(term=row.names(mat), count=freqlist, row.names = row.names(mat))
  if (nchar(ignore) > 0) {
    df <- df[!grepl(ignore, df$term), ]
  }
  df[order(-df$count), ]
}


sort_by <- function(df, col, decreasing=TRUE) {
  # Sorter df på col.
  return(df[order(df[col], decreasing=decreasing),])
}