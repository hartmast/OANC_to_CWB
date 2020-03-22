library(tidyverse)


# list of txt files
f1 <- list.files("data/spoken/", pattern = "txt", full.names = T, recursive = T)

# list of pos tag files
# f2b <- list.files("data/", pattern = "hepple", full.names = T, recursive = T)
f2 <- list.files("../OANC-data/data/spoken/", pattern = "hepple", full.names = T, recursive = T)

# list of sentence annotation files
f3 <- list.files("data/spoken/", pattern = "\\-s.xml", full.names = T, recursive = T)

# list of turn-taking annotation files
f4 <- list.files("data/spoken/", pattern = "\\-logical.xml", full.names = T, recursive = T)

# list of .anc metadata
f5 <- list.files("data/spoken/", pattern = "\\.anc", full.names = T, recursive = T)


# IDs for files
set.seed(1985)
ids <- sample(1:length(f1), length(f1))


# get VRT from all files

for(j in 1:length(f1)) {
  
  if(j == 1) {
    
    # create XML header
    xml_header1 <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    write.table(xml_header1, file = "oanc_spoken.vrt", quote = F, row.names = F, col.names = F, fileEncoding = "UTF-8")
    write.table("<corpus>", file = "oanc_spoken.vrt", quote = F, row.names = F, col.names = F, fileEncoding = "UTF-8", append = T)
    
  }
  
  
  
  # header for metadata
  # metadata
  modality <- gsub("_[12]", "", unlist(strsplit(f1[j], "/+"))[2])
  genre <- unlist(strsplit(f1[j], "/+"))[3]
  textname <- f1[j]
  id <- ids[j]
  header <- paste0("<text id=\"", id, "\" modality=\"", modality, "\" genre=\"", genre, "\" file=\"", textname, "\">", sep = "", collapse = "")
  write.table(header, file = "oanc_spoken.vrt", quote = F, row.names = F, col.names = F, fileEncoding = "UTF-8", append = T)
  
  
  # words with inline annotation
  # read txt file
  t <- readr::read_file(file = f1[j])
  
  # read metadata file
  m <- readLines(f2[j])
  
  # find anchors for POS tags and lemmas
  anchors <- grep("<struct type=\"tok\"", m)
  anchors_both <- gsub(".*from=\"", "", m[anchors])
  anchor1 <- as.numeric(gsub("\".*", "", anchors_both))
  anchor2 <- as.numeric(gsub(".*to=\"|\">", "", anchors_both))
  
  
  # find anchors for sentence annotation
  sentenceAnno <- scan(f3[j], what = "character", sep = "\n")
  s_anchors <- grep("region xml:id", sentenceAnno, value = T)
  s_anchors_start <- as.numeric(gsub(" .*", "", gsub(".*anchors=\"", "", s_anchors)))
  s_anchors_end <- as.numeric(gsub("\"/>", "", gsub(".* ", "", s_anchors)))
  s_id <- gsub("\".*", "", gsub(".*:id=\"", "", s_anchors))
  s_anno <- tibble(anchor_start = s_anchors_start,
                   anchor_end   = s_anchors_end,
                   s_id         = s_id)
  
  # find anchors for turn annotation
  turnAnno <- readLines(f4[j])
  turn_anchors <- grep("region xml:id", turnAnno, value = T)
  turn_anchors_position <- grep("region xml:id", turnAnno)
  turn_anchors_start <- as.numeric(gsub(" .*", "", gsub(".*anchors=\"", "", turn_anchors)))
  turn_anchors_end <- as.numeric(gsub("\"/>", "", gsub(".* ", "", turn_anchors)))
  t_id <- gsub("\".*", "", gsub(".*:id=\"", "", turn_anchors))
  t_anno <- tibble(anchor_start = turn_anchors_start,
                   anchor_end   = turn_anchors_end,
                   t_id         = t_id)
  
  
  # get IDs of participants with age etc.
  md <- readLines(f5[j])
  partic_start <- grep("<particDesc>", md)
  partic_end   <- grep("</particDesc>", md)
  partic <- md[partic_start:partic_end]
  
  ps <- grep("id=", partic, value = T) # participant IDs and data
  p_id <- gsub("\".*", "", gsub(".*id=\"", "", ps))
  p_sex <- gsub("\".*", "", gsub(".*sex=\"", "", ps))
  p_age <- gsub("\".*", "", gsub(".*age=\"", "", ps))
  
  p <- tibble(id = p_id,
              sex = p_sex,
              age = p_age)
  
  # align turns with t_id --------
  
  # find turns
  turns <- grep("<a label=\"turn\"", turnAnno)
  turns_end <- grep("</a", turnAnno)
  turns_end <- sapply(1:length(turns), function(i) turns_end[min(which(turns_end > turns[i]))])
  
  # get "who" and "id"s
  turns_who <- sapply(1:length(turns), function(i)   gsub(".*value=\"|\"/>", "", grep("f name=\"who\"", turnAnno[turns[i]:turns_end[i]], value = T)))
  turns_ids <- sapply(1:length(turns), function(i)   gsub(".*value=\"|\"/>", "", grep("f name=\"id\"", turnAnno[turns[i]:turns_end[i]], value = T)))
  
  # check if p_id is identical with IDs from
  # metadata file (sometimes they're just labeled A and B)
  if(unique(turns_who) %in% p$id) {
    # merge with participant metadata dataframe
    p <- left_join(tibble(id = turns_who,
                          turn_id = turns_ids),
                   p, by = "id", all.x = T)
    
  } else {
    p$id_orig <- p$id
    
    # get sides
    sides <- gsub(",.*", "", gsub(".* Side : ", "", ps))
    p$id <- sides
    
    p <- left_join(tibble(id = turns_who,
                          turn_id = turns_ids),
                   p, by = "id", all.x = T)
  }
  
  
  # anchors of turns: the last preceding position argument
  turns_find_anchors <- sapply(1:length(turns), function(i) max(which(turn_anchors_position < turns[i])))
  
  # add to p
  p$t_anchor_start <- turn_anchors_start[turns_find_anchors]
  p$t_anchor_end   <- turn_anchors_end[turns_find_anchors]
  
  
  # split into individual characters
  wc <- unlist(strsplit(t, ""))
  
  # get individual words
  w <- sapply(1:length(anchor1), function(i) paste0(wc[(anchor1[i]+1):(anchor2[i])], collapse = ""))

  # get individual annotations
  bases <- gsub(".* value=\"|\"/>", "", grep("<feat name=\"base\"", m, value = T))
  pos   <- gsub(".* value=\"|\"/>", "", grep("<feat name=\"msd\"", m, value = T))
  
  
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
  
  
  # insert turn annotation
  
  # omit turns that are not actually covered by the text
  # (those sometimes occur in Switchboard, for some reason...)
  if(length(which(p$t_anchor_end < min(df$anchor1[which(df$anchor1>0)]))) > 0) {
    p <- p[-which(p$t_anchor_end < min(df$anchor1[which(df$anchor1>0)])),]
  }
  
  if(length(which(p$t_anchor_start > max(df$anchor2))) > 0) {
    p <- p[-which(p$t_anchor_start > max(df$anchor2)),]
  }
  
  # loop over all turns 
  for(i in 1:nrow(p)) {
    
    # find start anchor
    start_here <- which(df$anchor1==p$t_anchor_start[i])
    
    # if the anchor is not in the dataframe,
    # look for the closest match by looking for the smallest
    # anchor that is bigger than the last anchor_end
    # and smaller than the next one
    
    if(length(start_here)==0) {
      
      if(i == 1) {
        start_here <- 1
      } else  {
        
        # omit "turns" spanning only 1 character
        if(p$t_anchor_end[(i-1)] != (p$t_anchor_end[i]-1)) {
          start_here <- min(which(df$anchor1 > p$t_anchor_end[(i-1)] & df$anchor1 < p$t_anchor_end[i]))
          
        }
        
      }
      
    }
    
    # if start_here is still 0, don't proceed
    
    
    if(length(start_here) > 0) {
      
      if( start_here != Inf) {
        # define row to insert
        insert_this <- tibble(word = paste0("<turn id=\"", p$turn_id[i],"\" sex=\"", p$sex[i], "\" age=\"", p$age[i], "\">", sep=""),
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
        end_here <- which(df$anchor2==p$t_anchor_end[i])
        
        # if the anchor isn't in the dataframe,
        # look for the largest number that is 
        # bigger than the start anchor & smaller
        # than the subsequent start anchor
        
        if(length(end_here)==0) {
          if(i < nrow(p)) {
            end_here <- max(which(df$anchor2 > p$t_anchor_start[i] & df$anchor2 < p$t_anchor_start[i+1]))
          } else {
            end_here <- max(which(df$anchor2 > p$t_anchor_start[i]))
          }
          
        }
        
        # define row to insert
        insert_that <- tibble(word = "</turn>",
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
  write.table(df, file = "oanc_spoken.vrt", sep = "\t", quote = F, row.names = F, col.names = F, fileEncoding = "UTF-8", append = T)
  
  # close text tag
  write.table("</text>", file = "oanc_spoken.vrt", sep = "\t", quote = F, row.names = F, col.names = F, fileEncoding = "UTF-8", append = T)
  
  # close corpus text after last file has been processed
  if(j == length(f1)) {
    write.table("</corpus>", file = "oanc_spoken.vrt", quote = F, row.names = F, col.names = F, fileEncoding = "UTF-8", append = T)
  }
  
  print(j)
  
}


# replace: <s>.*\n by <s>\n;
#          </s>.*\n by </s.>\n;
#          <\t by \\<\t;
#          >\t by \\>\t;
#          & by \\&.


tx <- readLines("oanc_spoken.vrt")
tx <- gsub("<s>.*", "<s>", tx)
tx <- gsub("</s>.*", "</s>", tx)
tx <- gsub("</turn>.*", "</turn>", tx)
tx[grep("<turn id.*", tx)] <- gsub("\t.*", "", tx[grep("<turn id.*", tx)])
tx <- gsub("<\t", "\\<\t", tx)
tx <- gsub(">\t", "\\>\t", tx)
tx <- gsub("&", "\\\\\\&", tx)

# export again
write.table(tx, "oanc_spoken.vrt", quote = F, col.names = F, row.names = F)
