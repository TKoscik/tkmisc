bt_summarizer <- function(data.dir,
                          scratch,
                          label.csv,
                          save.dir,
                          file.name) {
  
  fls <- list.files(data.dir, 
                    recursive = T,
                    full.names = T, 
                    pattern = "dustCleaned")
  
  temp.id <- as.numeric(na.omit(unname(as.numeric(unlist(strsplit(fls, "/"))))))
  ursi <- temp.id[seq(1,length(temp.id),2)]
  mrqid <- temp.id[seq(2,length(temp.id),2)]
  
  # copy to scratch to unzip
  for (i in 1:length(mrqid)) {
    fname <- paste0(scratch, "/", mrqid[i], ".nii.gz")
    fname.gunzip <- paste0(scratch, "/", mrqid[i], ".nii")
    if (!file.exists(fname.gunzip)) {
      file.copy(from=fls[i], to=fname)
      gunzip(fname, remove=TRUE)
    }
    fls[i] <- fname.gunzip
  }
  df <- label.summary(label.sjx = fls,
                      id.ls = mrqid,
                      label.csv = label.csv,
                      return.df = TRUE)
  df <- data.frame(ursi, df)
  write.table(df,
              file=paste0(save.dir, "/", file.name),
              col.names=T, row.names=F, quote=F, sep=",")
  
}
