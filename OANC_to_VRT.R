library(tidyverse)

# list of txt files
f <- list.files("data/", pattern = "txt", full.names = T, recursive = T)

# list of pos tag files
f2 <- list.files("data/", pattern = "hepple", full.names = T, recursive = T)

# list of sentence annotation files
f3 <- list.files("data/", pattern = "\\-s.xml", full.names = T, recursive = T)

# find files that don't have POS tags
f_without_pos <- setdiff(gsub(".txt", "", f), gsub("-hepple.xml", "", f2))

# exclude those files
f1 <- f[-which(gsub(".txt", "", f) %in% f_without_pos)]
f3 <- f3[-which(gsub(".txt", "", f) %in% f_without_pos)]

# IDs for files
set.seed(1985)
ids <- sample(1:length(f1), length(f1))


# get VRT from all files

for(j in 1:length(f1)) {
  
  if(j == 1) {
    
    # create XML header
    xml_header1 <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    write.table(xml_header1, file = "oanc.vrt", quote = F, row.names = F, col.names = F, fileEncoding = "UTF-8")
    write.table("<corpus>", file = "oanc.vrt", quote = F, row.names = F, col.names = F, fileEncoding = "UTF-8", append = T)
    
  }
  
  
  
  # header for metadata
  # metadata
  modality <- gsub("_[12]", "", unlist(strsplit(f1[j], "/+"))[2])
  genre <- unlist(strsplit(f1[j], "/+"))[3]
  textname <- f1[j]
  id <- ids[j]
  header <- paste0("<text id=\"", id, "\" modality=\"", modality, "\" genre=\"", genre, "\" file=\"", textname, "\">", sep = "", collapse = "")
  write.table(header, file = "oanc.vrt", quote = F, row.names = F, col.names = F, fileEncoding = "UTF-8", append = T)
  
  
  # words with inline annotation
  # read txt file
  t <- readr::read_file(file = f1[j])
  
  # read metadata file
  m <- readLines(f2[j])
  
  # find anchors for POS tags and lemmas
  anchors <- grep("<region xml\\:id", m)
  anchors_both <- gsub(".*anchors=\"", "", m[anchors])
  anchor1 <- as.numeric(gsub(" .*", "", anchors_both))
  anchor2 <- as.numeric(gsub(".* |\"/>", "", anchors_both))
  
  # find anchors for sentence annotation
  sentenceAnno <- readLines(f3[j])
  s_anchors <- grep("region xml:id", sentenceAnno, value = T)
  s_anchors_start <- as.numeric(gsub(" .*", "", gsub(".*anchors=\"", "", s_anchors)))
  s_anchors_end <- as.numeric(gsub("\"/>", "", gsub(".* ", "", s_anchors)))
  s_id <- gsub("\".*", "", gsub(".*:id=\"", "", s_anchors))
  s_anno <- tibble(anchor_start = s_anchors_start,
                   anchor_end   = s_anchors_end,
                   s_id         = s_id)
  
  # split into individual characters
  wc <- unlist(strsplit(t, ""))
  
  # get individual words
  w <- sapply(1:length(anchor1), function(i) paste0(wc[(anchor1[i]+1):(anchor2[i])], collapse = ""))

  # get individual annotations
  bases <- gsub(".* value=\"|\"/>", "", grep("<f name=\"base\"", m, value = T))
  pos   <- gsub(".* value=\"|\"/>", "", grep("<f name=\"msd\"", m, value = T))
  
  # table
  df <- tibble(word = w,
               lemma = bases,
               pos = pos,
               anchor1 = anchor1,
               anchor2 = anchor2)
  
  # insert syntactic annotation
  for(i in 1:nrow(s_anno)) {
    
    # find start anchor
    start_here <- which(df$anchor1==s_anno$anchor_start[i])
    
    # if the anchor is not in the dataframe,
    # look for the closest match by looking for the smallest
    # anchor that is bigger than the last anchor_end
    # and smaller than the next one
    
    if(length(start_here)==0) {
      
      if(i == 1) {
        start_here <- 1
      } else  {
        
        # omit "sentences" spanning only 1 character
        if(s_anno$anchor_end[(i-1)] != (s_anno$anchor_end[i]-1)) {
          start_here <- min(which(df$anchor1 > s_anno$anchor_end[(i-1)] & df$anchor1 < s_anno$anchor_end[i]))
          
        }
        
      }
      
    }
    
    # if start_here is still 0, don't proceed
    
    
    
    if(length(start_here) > 0) {
      
      if( start_here != Inf) {
        # define row to insert
        insert_this <- tibble(word = "<s>",
                              lemma = "",
                              pos = "",
                              anchor1 = numeric(length = 1),
                              anchor2 = numeric(length = 1))
        
        if(start_here==1) {
          df <- bind_rows(insert_this, df)
        } else {
          df <- bind_rows(df[1:(start_here - 1),],
                          insert_this,
                          df[start_here:nrow(df),])
        }
        
        # find end anchor
        end_here <- which(df$anchor2==s_anno$anchor_end[i])
        
        # if the anchor isn't in the dataframe,
        # look for the largest number that is 
        # bigger than the start anchor & smaller
        # than the subsequent start anchor
        
        if(length(end_here)==0) {
          if(i < nrow(s_anno)) {
            end_here <- max(which(df$anchor2 > s_anno$anchor_start[i] & df$anchor2 < s_anno$anchor_start[i+1]))
          } else {
            end_here <- max(which(df$anchor2 > s_anno$anchor_start[i]))
          }
          
        }
        
        # define row to insert
        insert_that <- tibble(word = "</s>",
                              lemma = "",
                              pos = "",
                              anchor1 = numeric(length = 1),
                              anchor2 = numeric(length = 1))
        
        if(end_here == nrow(df)) {
          df <- bind_rows(df, insert_that)
        } else {
          df <- bind_rows(df[1:end_here,],
                          insert_that,
                          df[(end_here+1):nrow(df),])
        }
      }
      
      }
      
      
    
    
    print(paste0("File ", j, " of ", length(f1), " row ", i, sep = "", collapse = ""))
  }
  
  # export
  write.table(df, file = "oanc.vrt", sep = "\t", quote = F, row.names = F, col.names = F, fileEncoding = "UTF-8", append = T)
  
  # close text tag
  write.table("</text>", file = "oanc.vrt", sep = "\t", quote = F, row.names = F, col.names = F, fileEncoding = "UTF-8", append = T)
  
  # close corpus text after last file has been processed
  if(j == length(f1)) {
    write.table("</corpus>", file = "oanc.vrt", quote = F, row.names = F, col.names = F, fileEncoding = "UTF-8", append = T)
  }
  
  print(j)
  
}


# then replace:
# <s>.*\n by <s>\n
# </s>.*\n by </s.>\n
# <\t by \\<\t
# >\t by \\>\t
# & by \\&

