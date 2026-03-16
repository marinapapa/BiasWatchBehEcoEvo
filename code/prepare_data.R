###################################################################################
## Reads json files that should be saved in the data repository in a 'jsons' folder

rm(list = ls())

library(parallel)
library(dplyr)

## Path to where the jsons files are saved:
raw_data_path <- '../data/jsons/'
allf <- list.files(raw_data_path)

############
## Function
read_jsons <- function(x, path2data){
  
  tjs <- rjson::fromJSON(file = paste0(path2data, x))
  
  if ( length(tjs$eid) < 1|
       length(tjs$publication_name) < 1 |
       length(tjs$authors) < 1|
       length(tjs$cover_date) < 1|
       length(tjs$abstract) < 1|
       length(tjs$reference_count) < 1|
       length(tjs$citation_count) < 1)
  {
    return(NA)
  }
  
  maindf <- data.frame(
    id = tjs$eid,
    journal =  tjs$publication_name,
    N = length(tjs$authors),
    date = sub("\\-.*", "", tjs$cover_date),
    cit = tjs$citation_count,
    abstr = tjs$abstract,
    Nrefs = tjs$reference_count)
  
  if (maindf$N < 1) {
    return(NA)
  }
  
  fnames <- c()
  k <- 1
  for (i in tjs$authors){
    fname <- strsplit(i, "[ ]")[[1]][1]
    fnames[k] <- stringi::stri_trans_general(str = fname, 
                                             id = "Latin-ASCII")
    k <- k + 1
  }
  
  all_gs <- gender::gender(fnames)
  if (nrow(all_gs) < 1) {
    maindf$missing_genders <- NA
    maindf$g_f <- NA
    maindf$g_f_p <- NA
    maindf$g_l <- NA
    maindf$g_l_p <- NA
    maindf$g_idx <- NA
    maindf$g_idx_p <- NA
    return(maindf)
  }
  
  all_gs$ismale <- all_gs$gender == 'male'
  
  maindf$missing_genders <- maindf$N - nrow(all_gs)
  
  gidx <- sum(all_gs$ismale)/nrow(all_gs)
  gidx_av_prop <- mean(ifelse(all_gs$gender == 'male', all_gs$proportion_male, all_gs$proportion_female) )
  
  maindf$g_f <- all_gs$gender[1] 
  maindf$g_f_p <- ifelse(all_gs$gender[1] == 'male', all_gs$proportion_male[1], all_gs$proportion_female[1]) 
  
  if (maindf$N > 1){
    maindf$g_l <- all_gs$gender[nrow(all_gs)] 
    maindf$g_l_p <- ifelse(all_gs$gender[nrow(all_gs)] == 'male', all_gs$proportion_male[nrow(all_gs)], all_gs$proportion_female[nrow(all_gs)]) 
  } else {
    maindf$g_l <- NA
    maindf$g_l_p <- NA
  }
  maindf$g_idx <- gidx
  maindf$g_idx_p <- gidx_av_prop
  
  return(maindf)
}

###############
## Get all data

cl <- makeCluster(8)
alldt <- parSapply(cl, allf, read_jsons, path2data = raw_data_path)
stopCluster(cl)

## remove mistakes from missing values
alldt <- alldt[!is.na(alldt)]
alldt <- data.table::rbindlist(alldt)
alldt <- distinct(alldt) # in case mistake duplicates in jsons

## remove papers from journals downloaded by accident
thejourn <- read.csv('data/journals_list.csv', header = F)
thejourn <- thejourn$V1
thejourn <- tolower(thejourn) 

alldt$journal <- gsub(pattern = '&', replacement = 'and', alldt$journal) 
alldt$journal <- tolower(alldt$journal) 
ddf <- alldt[alldt$journal %in% thejourn,]

## remove papers with missing genders
noauthornames <- sum(is.na(ddf$missing_genders))
print(paste0('missing author gender data for ', noauthornames, ' papers.'))

ddf <- ddf[!is.na(ddf$missing_genders),]

ddf$perc_unknown <- ddf$missing_genders/ddf$N
noallauthorgender <- sum(ddf$perc_unknown > 0.3)

print(paste0('missing gender data for at least 30% of authors in ', noallauthorgender, ' papers.'))
ddf <- ddf[ddf$perc_unknown <= 0.3,]

## remove papers with uncertain gender definition
lowconf <- nrow(ddf[ddf$g_f_p < 0.8 | ddf$g_idx_p < 0.8,])
print(paste0('confidence less than 80% in gender estimation in ', lowconf, ' papers'))

ddf <- ddf[ddf$g_f_p >= 0.8 &  ddf$g_idx_p >= 0.8,]

## remove papers with very few references (corrections or responses)
lowrefs <- nrow(ddf[ddf$Nrefs <= quantile(ddf$Nrefs, 0.015), ])
print(paste0('less than 8 references in ', lowrefs, ' papers'))

ddf <- ddf[ddf$Nrefs > quantile(ddf$Nrefs, 0.015), ]

################################################################
## Cut categories in data

## Categorize author groups
levs <- seq(0, 1, 0.2)
ddf$g_cat <- cut(ddf$g_idx, include.lowest = T,
                 breaks= levs, 
                 labels=c("HF","LF","B", "LM", "HM"))

## Tranform distributions of references 
ddf$refs_log <- log(ddf$Nrefs, base = 10)

## Add diversity index from Female ratio
ddf$fr <- 1 - ddf$g_idx # GBI to Female ratio
ddf$div_idx <- ddf$fr
ddf$div_idx <- sapply(ddf$fr, function(x){
  if(x > 0.5) return(abs(x - 1))
  return(x)})
ddf$div_idx <- (ddf$div_idx / 0.5) # normalization, 0.5 max value, 0 lower

## Save clean dataset 
write.csv(ddf, '../data/all_data.csv', row.names = F)

########################################################################
## Prepare data for statistics

ddf <- read.csv('../data/all_data.csv')

## Remove outliers
ddf <- ddf[ddf$cit < quantile(ddf$cit, 0.99),]
ddf <- ddf[ddf$Nrefs < quantile(ddf$Nrefs, 0.99),]
ddf <- ddf[ddf$N < quantile(ddf$N, 0.99),]

## Remove journals with missing data in many years
ddf <- ddf[!(ddf$journal %in% c("ecosphere")),]
jc <- table(ddf$journal)
tokeep <- names(jc)[jc > 1000] # remove if less than 1000 papers in dataset
ddf <- ddf[ddf$journal %in% tokeep,]

# Create Citation Performance quantiles
quants <- ddf %>% group_by(date, journal) %>%
  reframe(qs = quantile(cit, c(0.25, 0.5, 0.75, 0.9, 1)),
          q = c(1, 2, 3, 4, 5),
          N = n())

## if less than 50 papers per year, remove the year from analysis
ddf$journ_year <- paste(ddf$journal, ddf$date, sep = '_')
quants$journ_year <- paste(quants$journal, quants$date, sep = '_')
ddf <- ddf[!(ddf$journ_year %in% quants$journ_year[quants$N < 50]),]

quants <- ddf %>% group_by(date, journal) %>%
  reframe(qs = quantile(cit, c(0.25, 0.5, 0.75, 0.9, 1)),
          q = c(1, 2, 3, 4, 5),
          N = n())

ddf <- ddf %>% group_by(date, journal) %>%
  mutate(cit_q = cut(cit,
                     breaks = c(-0.5, quants$qs[quants$date == date[1] &
                                                  quants$journal == journal[1]]),
                     labels = quants$q[quants$date == date[1] &
                                         quants$journal == journal[1]]))

## Save data for stats
write.csv(ddf, '../data/data_for_stats.csv', row.names = F)

################################################
## Get the pool of all authors' gender per year

## Get all authors pool per year
all_authors <- vector(mode = "list", length = 21)
all_authors <- sapply(c(1:21), function(x) all_authors[[x]] <- vector() )
names(all_authors) <- seq(2000, 2020, 1)

allf <- paste0(raw_data_path, ddf$id, '.json')
k <- 0
pb = txtProgressBar(min = 0, max = length(allf), style = 3) 
for (x in allf){
  
  tjs <- rjson::fromJSON(file = x)
  date <- sub("\\-.*", "", tjs$cover_date)
  authors <- tjs$authors
  all_authors[[date]] <- unique(c(all_authors[[date]], authors))
  k <- k+1
  setTxtProgressBar(pb,k)
}
close(pb)

## same names of all authors per year
##save(all_authors, file = 'data/all_authors.RData')

## Get all authors gender per year
cl <- makeCluster(8)
gidx_per_year <- parLapply(cl, all_authors, function(x){
  
  gs <- sapply(x, function(i){
    
    fname <- strsplit(i, "[ ]")[[1]][1]
    fname<- stringi::stri_trans_general(str = fname, 
                                        id = "Latin-ASCII")
    gn <- gender::gender(fname)
    if ( nrow(gn) < 1){ return(NA) }
    if (gn$proportion_female > 0.2 & gn$proportion_female < 0.8){
      return(NA) 
    }
    return(ifelse(gn$gender == 'male', 0, 1))
  })
  
  return(mean(gs, na.rm = T))
})

stopCluster(cl)

gbdf <- data.frame(date = names(gidx_per_year), gidx = unlist(gidx_per_year) )
write.csv(gbdf, file ='../data/baseline_gbi_per_year.csv')


## The end