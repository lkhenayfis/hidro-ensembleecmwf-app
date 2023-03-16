########################################## CLASSE DADOPREC #########################################

library(dbrenovaveis)
library(data.table)

# Redefine os defaults das funcoes de dbrenovaveis para rodar com o banco de prec
# Isso so e necessario enquanto o backend nao e generalizado para lidar com campos arbitrarios

formals(getverificado)$campos <- "prec"
formals(getprevisto)$campos <- c("prec", "membro")
formals(getprevisto)$modelos <- "ECMWF"
formals(getprevisto)$horizontes <- 1

conector_default <- conectabucket("s3://ons-pem-historico", "hidro/ensemble-ecmwf", "parquet")

# LEITURA DOS ARQUIVOS PROCESSADOS -----------------------------------------------------------------

#' Leitor Do Dado Para Um Posto
#' 
#' Le dados verificados e previstos para um determinado posto, combinando em um objeto
#' 
#' @param posto string indicando um nome de posto
#' @param horizonte o horizonte de previsao do ensemble
#' @param datas string indicando uma faixa de datas do tipo "AAAA-MM-DD/AAAA-MM-DD". "*" pega todas
#'     as datas disponiveis
#' @param conexao uma conexao com o banco no padrao do \code{dbrenovaveis}. Por padrao o diretorio
#'     "data/ensemble" na raiz do projeto
#' 
#' @value objeto \code{dadoprec}, contendo o verificado e previsto nas datas comuns dentro da faixa
#'     \code{datas} do posto \code{posto} e horizonte \code{horizonte}

ledadoprec <- function(posto, horizonte = 1, datas = "*", conexao = conector_default) {

    if(length(horizonte) > 1) {
        warning("'horizonte' tem comprimento maior que 1, apenas o primeiro elemento sera usado")
        horizonte <- horizonte[1]
    }

    if(length(posto) > 1) {
        warning("'posto' tem comprimento maior que 1, apenas o primeiro elemento sera usado")
        posto <- posto[1]
    }

    verif <- getverificado(conexao, posto)
    prev  <- getprevisto(conexao, posto, datas, horizontes = horizonte)

    verifINprev <- (verif$data_hora %in% prev$data_hora_previsao)
    prevINverif <- (prev$data_hora_previsao %in% verif$data_hora)

    verif <- verif[verifINprev]
    prev <- prev[prevINverif]

    new_dadoprec(verif, prev, posto, horizonte)
}

#' Construtor Interno De \code{dadoprec}
#' 
#' Funcao interna, nao deve ser chamada diretamente pelo usuario
#' 
#' @param verif,prev dados verificados e previstos, padronizados
#' @param posto o posto ao qual o dado se refere
#' @param horizonte horizonte de previsao do ensemble
#' 
#' @return objeto \code{dadoprec}, uma lista de dois elmentos

new_dadoprec <- function(verif, prev, posto, horizonte) {

    new <- list(verificado = verif, previsto = prev)
    attr(new, "posto") <- posto
    attr(new, "horizonte") <- horizonte
    class(new) <- "dadoprec"

    return(new)
}

# METODOS ------------------------------------------------------------------------------------------
