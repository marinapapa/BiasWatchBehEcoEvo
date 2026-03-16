#############################################
## Analysis on abstracts' words to identify
## similarity with abstracts in Animal Behaviour

library(dplyr)
library(ggplot2)
library(stopwords)

ddf <- read.csv('../data/all_data.csv')
anbeh <- ddf$abstr[ddf$journal == "animal behaviour" ]

stopwords_regex = paste(stopwords::stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')

special_stops = c('studi', 'associ', 'differ', 'may', 'result',
                  'suggest','test', 'respons', 'increas', 'use',
                  'show', 'can' , 'effect', 'relat', 'howev', 
                  'also', 'found', 'find', 'low', 'whether', 'signific', 'high', 
                  'within', 'like', 'affect', 'one', 'two', 'indic',
                  'examin', 'less', 'number', 'evid', 'higher', 'among',
                  'often', 'reduc', 'previous', 'thus', 'appear', 'low',
                  'first', 'ltd', 'well', 'predict','lower', 'either',
                  'approach', 'decreas', 'across', 'greater', 'author', 'press',
                  'support', 'present', 'hypothesi', 'mani', 'springer', 'licenc',
                  'provid', 'larg', 'data', 'understand', 'new', 'part', 'demonstr',
                  'identifi', 'key', 'measir','yet', 'declin', 'repres','depend', 'contribut',
                  'right', 'et', 'al','analysi', 'specific', 'larg', 'understand', 'allow', 'non',
                  'known', 'licens', 'reveal', 'possibl', 'three',
                  'oxford', 'publish')

publisher_statements <- c(
  'Evolution Letters published by Wiley Periodicals, Inc. on behalf of Society for the Study of Evolution (SSE) and European Society for Evolutionary Biology (ESEB).',
  'British Ecological Society.',
  'Macmillan Publishers Limited, part of Springer Nature.',
  'All rights reserved.',
  'Elsevier Ltd.',
  'Published by',
  'Springer-Verlag',
  'Elsevier Science Ltd.',
  'Elsevier Science Inc.',
  'The Royal Society.',
  'licensee BioMed Central Ltd.',
  'The Ecological Society of America.',
  'The Fisheries Society of the British Isles.',
  'Taylor & Francis Group, LLC.',
  ' Methods in Ecology and Evolution',
  'Academic Press.',
  'The Association for the Study of Animal Behaviour.'
  
)


for (x in publisher_statements){
  ddf$abstr <- gsub(x,' ', ddf$abstr)
}

allcoms <- list()
k <- 1
for (i in unique(ddf$journal)){
  print(k)
  
  allcoms[[k]] <- ddf[ddf$journal == i, ] %>%
    ## keep first column only and name it 'keywords':
    dplyr::select('keywords' = 6) %>%

    mutate(keywords = stringr::str_replace_all(tolower(keywords), stopwords_regex, '')) %>%
    mutate(keywords = gsub('[[:punct:] ]+',' ', keywords)) %>%

    mutate(keywords = tm::stemDocument(keywords)) %>%
    mutate(keywords = tm::removeNumbers(keywords)) %>%
    ## multiple cell values (as separated by a blank)
    ## into separate rows:
    tidyr::separate_longer_delim(keywords, delim = " ") %>%
    filter(keywords != "" ) %>%
    filter(!(keywords %in% special_stops)) %>%
    filter(!(keywords %in% special_stops)) %>%
    filter(nchar(keywords) > 1) %>%
    group_by(keywords) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  names(allcoms)[k] <- i
  k <- k + 1
}

pw_comp <- lapply(allcoms, 
                  function(x) {
                    thel <- lapply(allcoms,
                                   function(y) data.frame(sim = length(intersect(x$keywords[1:100], y$keywords[1:100]))/100))
                    thel <- data.table::rbindlist(thel, idcol = TRUE)
                    colnames(thel) <- c('jj', 'perc')
                    return(thel)}
)
pw_comp <- data.table::rbindlist(pw_comp, idcol = TRUE)
colnames(pw_comp)[1] <- 'ji'

tp <- pw_comp[pw_comp$ji == 'animal behaviour',]
write.csv(tp, '../data/abstr_100w_sim.csv')