library(data.table)
library(arrow)

source("R/leitura.r")

# Leitura dos ensembles --------------------------------------------------------

# A raiz dos diretorios de ensemble deve ser configurada no arquivo .Renviron
root   <- Sys.getenv("ROOT_DIRS_ENSEMBLE")
outdir <- "data/ensemble"

dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)

dats <- lapply(head(dirs, 10), le_diretorio_ensemble)
dats <- rbindlist(dats)
write_dataset(as.data.frame(dats), outdir, "parquet", partitioning = c("posto", "h"))
