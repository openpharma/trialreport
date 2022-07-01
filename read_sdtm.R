# read cdiscpilot01 study sdtm data

library(haven)
library(zip)

dir.create(tmp <- tempfile(pattern = 'tmpdir'))
tmp_file <- tempfile()

download.file("https://github.com/openpharma/phuse-scripts/raw/master/data/sdtm/TDF_SDTM_v1.0%20.zip", destfile = tmp_file)
unzip(tmp_file, exdir = tmp)

sdtm <- sapply(list.files(path=tmp, pattern = '.xpt$', full.names = TRUE), read_xpt, simplify = FALSE)
names(sdtm) <- sub('.xpt$', '', basename(names(sdtm)))

# clean up
unlink(c(tmp, tmp_file), recursive = TRUE)
rm(tmp, tmp_file)
