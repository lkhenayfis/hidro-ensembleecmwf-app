library(data.table)
library(arrow)

source("R/leitura.r")

outdir <- "data/ensemble"
dir.create(outdir, recursive = TRUE)

root <- Sys.getenv("ROOT_DIRS_ENSEMBLE")
dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)

# Arquivo de postos -----------------------------------------------------------

arq_dummy <- list.files(dirs[1], full.names = TRUE)[1]
dat_dummy <- fread(arq_dummy)

postos <- dat_dummy[, 1:3]
postos[, id := seq_len(.N)]
colnames(postos)[1:3] <- c("codigo", "latitude", "longitude")
setcolorder(postos, c("id", "codigo", "latitude", "longitude"))

write_parquet(postos, file.path(outdir, "usinas.parquet"))

# Arquivo de modelos ----------------------------------------------------------

modelos <- data.table(id = 1, nome = "ECMWF", horizonte_previsao = 45)
write_parquet(modelos, file.path(outdir, "modelos_previsao.parquet"))

# Ensembles previstos ---------------------------------------------------------

# como nao da pra appendar em parquet, precisa ler tudo de uma vez so e fazer as escritas depois
ensemble <- lapply(dirs, function(dir) {
    print(dir)

    dat <- le_diretorio_ensemble(dir)
    dat <- merge(dat, postos[, .(id, codigo)], by.x = "posto", by.y = "codigo")
    dat <- dat[, .(id, data_simulacao, h, membro, prec)]

    # muda os nomes das colunas aqui p compat com o dbrenovaveis. Quando ele for generalizado
    # para quaisquer campos isso nao sera mais necessario
    colnames(dat)[1:3] <- c("id_usina", "data_hora_previsao", "dia_previsao")

    return(dat)
})
ensemble <- rbindlist(ensemble)
ensemble[, id_modelo := 1]
setorder(ensemble, id_usina, data_hora_previsao, dia_previsao)

previstos <- unique(ensemble[, .(id_usina, dia_previsao)])
previstos[, tabela := paste0("previstos_", id_usina, "_", dia_previsao)]
setorder(previstos, id_usina, dia_previsao)

write_parquet(previstos, file.path(outdir, "previstos.parquet"))

for(i in seq_len(nrow(previstos))) {

    id_i <- previstos[i, id_usina]
    dia_i <- previstos[i, dia_previsao]

    prev_i <- ensemble[(id_usina == id_i) & (dia_previsao == dia_i)]
    arq_out <- file.path(outdir, paste0("previstos_", id_i, "_", dia_i, ".parquet"))

    write_parquet(prev_i, arq_out)
}

# Verificados -----------------------------------------------------------------

verif <- fread(Sys.getenv("ARQ_PSAT"))
verif <- verif[complete.cases(verif[, -1])]
verif <- melt(verif, id.vars = "V1", variable.name = "codigo", value.name = "prec")
verif[, c("data_hora", "V1") := .(as.Date(V1, format = "%d/%m/%Y"), NULL)]
verif <- merge(verif, postos[, .(id, codigo)])
colnames(verif)[4] <- "id_usina"

verif[, codigo := NULL]
setcolorder(verif, c("id_usina", "data_hora", "prec"))
setorder(verif, id_usina, data_hora)

write_parquet(verif, "data/ensemble/verificados.parquet")
