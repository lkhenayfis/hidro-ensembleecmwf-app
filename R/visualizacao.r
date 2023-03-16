############################# FUNCOES DE VISUALIZACAO DOS DADOS DE PREC ############################

library(ggplot2)

plot.dadosprec <- function(x, datas = "*", confs = c(90, 95)) {

    if(datas != "*") {
        datas <- dbrenovaveis:::parsedatas(datas, "", FALSE)
        datas <- sapply(1:2, function(i) datas[[i]][i])

        x$verificado <- x$verificado[(data_hora >= datas[1]) & (data_hora < datas[2])]
        x$previsto <- x$previsto[(data_hora_previsao >= datas[1]) & (data_hora_previsao < datas[2])]
    }

    confs <- sort(confs)

    ff <- function(v, cc = confs) {

        v_confs <- lapply(cc, function(v) {
            delta <- (100 - v) / 2
            c(delta, 100 - delta) * 1e-2
        })
        v_confs <- unlist(v_confs)

        nconfs <- length(cc)

        out <- as.list(quantile(v, v_confs))
        names(out) <- paste0(c("lower_", "upper_"), rep(seq_len(nconfs), each = nconfs))

        return(out)
    }

    dshade <- x$previsto[, ff(prec), by = data_hora_previsao]
    dshade <- melt(dshade, id.vars = 1, measure = patterns(lower = "^lower_", upper = "^upper_"))
    dshade[, variable := confs[variable]]
    dshade[, variable := paste0(variable, "%")]
    dshade[, variable := factor(variable, levels = paste0(confs, "%"))]

    dmedprev <- x$previsto[, .(prev = mean(prec)), by = data_hora_previsao]
    dlines <- merge(x$verificado, dmedprev, by.x = "data_hora", by.y = "data_hora_previsao")
    dlines <- melt(dlines, id.vars = 1, variable.name = "tipo", value.name = "prec")

    gg <- ggplot() +
        geom_line(data = dlines, aes(data_hora, prec, linetype = tipo)) +
        geom_ribbon(data = dshade, aes(data_hora_previsao, ymin = lower, ymax = upper, fill = variable), alpha = .3) +
        labs(title = paste0(attr(x, "posto"), " (h = ", attr(x, "horizonte"), ")")) +
        scale_fill_discrete(name = "Intervalo") +
        scale_linetype_discrete(name = "Tipo", labels = c("Observado", "Previsto")) +
        theme_bw()

    return(gg)
}

