# LEITURA DE ARQUIVOS BRUTOS -----------------------------------------------------------------------

library(data.table)

# LEITURA DOS DADOS DE ENSEMBLE BRUTOS -------------------------------------------------------------

#' Leitor De Arquivo Individual
#' 
#' Le arquivo correspondente a apenas um membro, contendo os 45 passos de simulacao para cada posto
#' 
#' @param arq o arquivo a ser lido
#' @param membro inteiro indicando indice do membro. Se NULL, sera advinhado a partir de \code{arq}
#' 
#' @return data.table com as colunas
#'     \itemize{
#'     \item{posto}{nome do posto}
#'     \item{membro}{indice do membro}
#'     \item{prec}{valor de precipitacao simulada}
#'     }

le_arquivo_ensemble <- function(arq, membro = NULL) {

    if(is.null(membro)) membro <- as.numeric(regmatches(arq, regexpr("(?<=_p)[[:digit:]]+", arq, perl = TRUE)))

    dat <- fread(arq, stringsAsFactors = FALSE)
    dat[, c("V2", "V3") := .(NULL, NULL)]
    dat <- melt(dat, id.vars = "V1", variable.name = "h", value.name = "prec")
    colnames(dat)[1] <- "posto"
    dat[, h := as.numeric(sub("V", "", h)) - 3]
    dat[, membro := membro]
    dat[, prec := as.numeric(prec)]

    return(dat)
}

#' Leitor De Ensemble Completo
#' 
#' Le todos os membros de ensemble gerado em um determinado dia
#' 
#' @param dir diretorio contendo os membros de ensemble gerados em um determinado dia
#' @param data data de geracao do ensemble. Se NULL, sera advinhado a partir de \code{dir}
#' 
#' @return data.table contendo as colunas:
#'     \itemize{
#'     \item{data_simulacao}{a data alvo da simulacao (data de execucao mais horizonte)}
#'     \item{posto}{nome do posto}
#'     \item{h}{horizonte de simulacao, inteiro de 1 a 45}
#'     \item{membro}{indice do membro}
#'     \item{prec}{valor de precipitacao simulada}
#'     }

le_diretorio_ensemble <- function(dir, data = NULL) {

    arqs <- list.files(dir, "p[[:digit:]]+.dat$", full.names = TRUE)

    if(is.null(data)) {
        data <- regmatches(arqs[1], regexpr("(?<=_m_)[[:digit:]]{6}", arqs[1], perl = TRUE))
        data <- as.Date(data, format = "%d%m%y")
    }

    dats <- lapply(arqs, le_arquivo_ensemble)
    dats <- rbindlist(dats)
    dats[, data_simulacao := data + h]

    setorder(dats, posto, h, membro)
    setcolorder(dats, c("data_simulacao", "posto", "h", "membro", "prec"))

    return(dats)
}

# LEITURA DO DADO VERIFICADO -----------------------------------------------------------------------

#' Leitor De Arquivo De Verificado
#' 
#' Le arquivo de dados verificados, espera uma certa estrutura.
#' 
#' Esta funcao espera um arquivo da forma
#' 
#' | data | POSTO1 | POSTO2 | ... | POSTON |
#' | --- | --- | --- | --- | --- |
#' | 01/01/2020 | XXX | XXX | ... | XXX |
#' | 02/01/2020 | XXX | XXX | ... | XXX |
#' | 02/01/2020 | XXX | XXX | ... | XXX |
#' 
#' O nome da primeira coluna e irrelevante, pois sera trocado para "data" internamente. Os demais
#' serao utilizados como identificadores de cada registro
#' 
#' @param arq o arquivo a ser lido
#' 
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{prec}{valor de precipitacao simulada}
#'     }

le_arquivo_verificado <- function(arq) {

    dat <- fread(arq)
    dat <- dat[complete.cases(dat[, -1])]
    dat[, V1 := as.Date(V1, format = "%d/%m/%Y")]

    dat <- melt(dat, id.vars = "V1", variable.name = "posto", value.name = "prec")
    colnames(dat)[1] <- "data"

    setorder(dat, posto, data)

    return(dat)
}