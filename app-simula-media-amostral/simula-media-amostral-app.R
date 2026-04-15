library(shiny)

ui <- fluidPage(
  titlePanel("Simulação de amostras, médias amostrais e estatísticas t"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "dist",
        label = "Família da distribuição de X:",
        choices = c(
          "Uniforme" = "unif",
          "Normal" = "norm",
          "Exponencial" = "exp",
          "Beta" = "beta",
          "Poisson" = "pois",
          "Gamma" = "gamma",
          "Binomial" = "binom",
          "Binomial Negativa" = "nbinom",
          "Qui-quadrado" = "chisq",
          "F" = "f",
          "t-Student" = "t",
          "Geométrica" = "geom"
        ),
        selected = "unif"
      ),
      uiOutput("parametros_ui"),
      numericInput("m_sim", "Número de amostras (m_sim):", value = 1000, min = 1, step = 1),
      numericInput("n_obs", "Tamanho de cada amostra (n_obs):", value = 30, min = 2, step = 1),
      actionButton("simular", "Simular")
    ),
    mainPanel(
      plotOutput("hist_amostra", height = "360px"),
      br(),
      plotOutput("hist_medias", height = "360px"),
      br(),
      plotOutput("hist_tstats", height = "360px"),
      br(),
      verbatimTextOutput("resumo")
    )
  )
)

server <- function(input, output, session) {
  
  output$parametros_ui <- renderUI({
    switch(
      input$dist,
      unif = tagList(
        numericInput("unif_min", "Mínimo (a):", value = 0),
        numericInput("unif_max", "Máximo (b):", value = 1)
      ),
      norm = tagList(
        numericInput("norm_mean", "Média (mu):", value = 0),
        numericInput("norm_sd", "Desvio-padrão (sigma):", value = 1, min = 0.0001)
      ),
      exp = tagList(
        numericInput("exp_rate", "Taxa (lambda):", value = 1, min = 0.0001)
      ),
      beta = tagList(
        numericInput("beta_shape1", "Shape 1 (alpha):", value = 2, min = 0.0001),
        numericInput("beta_shape2", "Shape 2 (beta):", value = 5, min = 0.0001)
      ),
      pois = tagList(
        numericInput("pois_lambda", "Lambda:", value = 3, min = 0)
      ),
      gamma = tagList(
        numericInput("gamma_shape", "Shape (alpha):", value = 2, min = 0.0001),
        numericInput("gamma_rate", "Rate (beta):", value = 1, min = 0.0001)
      ),
      binom = tagList(
        numericInput("binom_size", "Número de ensaios (size):", value = 10, min = 1, step = 1),
        numericInput("binom_prob", "Probabilidade de sucesso (p):", value = 0.4, min = 0, max = 1, step = 0.01)
      ),
      nbinom = tagList(
        numericInput("nbinom_size", "Número de sucessos (size):", value = 5, min = 0.0001),
        numericInput("nbinom_prob", "Probabilidade de sucesso (p):", value = 0.4, min = 0.0001, max = 1, step = 0.01)
      ),
      chisq = tagList(
        numericInput("chisq_df", "Graus de liberdade:", value = 5, min = 0.0001)
      ),
      f = tagList(
        numericInput("f_df1", "Graus de liberdade 1:", value = 5, min = 0.0001),
        numericInput("f_df2", "Graus de liberdade 2:", value = 10, min = 0.0001)
      ),
      t = tagList(
        numericInput("t_df", "Graus de liberdade:", value = 10, min = 0.0001)
      ),
      geom = tagList(
        numericInput("geom_prob", "Probabilidade de sucesso (p):", value = 0.4, min = 0.0001, max = 1, step = 0.01)
      )
    )
  })
  
  parametros <- reactive({
    switch(
      input$dist,
      unif = {
        validate(need(input$unif_min < input$unif_max,
                      "Na distribuição Uniforme, deve valer a < b."))
        list(min = input$unif_min, max = input$unif_max)
      },
      norm = {
        validate(need(input$norm_sd > 0,
                      "Na distribuição Normal, o desvio-padrão deve ser positivo."))
        list(mean = input$norm_mean, sd = input$norm_sd)
      },
      exp = {
        validate(need(input$exp_rate > 0,
                      "Na distribuição Exponencial, a taxa deve ser positiva."))
        list(rate = input$exp_rate)
      },
      beta = {
        validate(need(input$beta_shape1 > 0 && input$beta_shape2 > 0,
                      "Na distribuição Beta, os dois parâmetros devem ser positivos."))
        list(shape1 = input$beta_shape1, shape2 = input$beta_shape2)
      },
      pois = {
        validate(need(input$pois_lambda >= 0,
                      "Na distribuição Poisson, lambda deve ser não negativo."))
        list(lambda = input$pois_lambda)
      },
      gamma = {
        validate(need(input$gamma_shape > 0 && input$gamma_rate > 0,
                      "Na distribuição Gamma, shape e rate devem ser positivos."))
        list(shape = input$gamma_shape, rate = input$gamma_rate)
      },
      binom = {
        validate(need(input$binom_size >= 1 && input$binom_size == round(input$binom_size),
                      "Na distribuição Binomial, size deve ser um inteiro positivo."))
        validate(need(input$binom_prob >= 0 && input$binom_prob <= 1,
                      "Na distribuição Binomial, p deve estar entre 0 e 1."))
        list(size = as.integer(round(input$binom_size)), prob = input$binom_prob)
      },
      nbinom = {
        validate(need(input$nbinom_size > 0,
                      "Na Binomial Negativa, size deve ser positivo."))
        validate(need(input$nbinom_prob > 0 && input$nbinom_prob <= 1,
                      "Na Binomial Negativa, p deve estar em (0, 1]."))
        list(size = input$nbinom_size, prob = input$nbinom_prob)
      },
      chisq = {
        validate(need(input$chisq_df > 0,
                      "Na Qui-quadrado, os graus de liberdade devem ser positivos."))
        list(df = input$chisq_df)
      },
      f = {
        validate(need(input$f_df1 > 0 && input$f_df2 > 0,
                      "Na distribuição F, ambos os graus de liberdade devem ser positivos."))
        list(df1 = input$f_df1, df2 = input$f_df2)
      },
      t = {
        validate(need(input$t_df > 0,
                      "Na t-Student, os graus de liberdade devem ser positivos."))
        list(df = input$t_df)
      },
      geom = {
        validate(need(input$geom_prob > 0 && input$geom_prob <= 1,
                      "Na Geométrica, p deve estar em (0, 1]."))
        list(prob = input$geom_prob)
      }
    )
  })
  
  is_discrete <- reactive({
    input$dist %in% c("pois", "binom", "nbinom", "geom")
  })
  
  nome_distribuicao <- reactive({
    pars <- parametros()
    switch(
      input$dist,
      unif = paste0("Uniforme(", pars$min, ", ", pars$max, ")"),
      norm = paste0("Normal(", pars$mean, ", ", pars$sd, ")"),
      exp = paste0("Exponencial(rate = ", pars$rate, ")"),
      beta = paste0("Beta(", pars$shape1, ", ", pars$shape2, ")"),
      pois = paste0("Poisson(", pars$lambda, ")"),
      gamma = paste0("Gamma(shape = ", pars$shape, ", rate = ", pars$rate, ")"),
      binom = paste0("Binomial(size = ", pars$size, ", p = ", pars$prob, ")"),
      nbinom = paste0("Binomial Negativa(size = ", pars$size, ", p = ", pars$prob, ")"),
      chisq = paste0("Qui-quadrado(df = ", pars$df, ")"),
      f = paste0("F(df1 = ", pars$df1, ", df2 = ", pars$df2, ")"),
      t = paste0("t-Student(df = ", pars$df, ")"),
      geom = paste0("Geométrica(p = ", pars$prob, ")")
    )
  })
  
  rdist <- function(n, dist, pars) {
    switch(
      dist,
      unif = runif(n, min = pars$min, max = pars$max),
      norm = rnorm(n, mean = pars$mean, sd = pars$sd),
      exp = rexp(n, rate = pars$rate),
      beta = rbeta(n, shape1 = pars$shape1, shape2 = pars$shape2),
      pois = rpois(n, lambda = pars$lambda),
      gamma = rgamma(n, shape = pars$shape, rate = pars$rate),
      binom = rbinom(n, size = pars$size, prob = pars$prob),
      nbinom = rnbinom(n, size = pars$size, prob = pars$prob),
      chisq = rchisq(n, df = pars$df),
      f = rf(n, df1 = pars$df1, df2 = pars$df2),
      t = rt(n, df = pars$df),
      geom = rgeom(n, prob = pars$prob)
    )
  }
  
  ddist <- function(x, dist, pars) {
    switch(
      dist,
      unif = dunif(x, min = pars$min, max = pars$max),
      norm = dnorm(x, mean = pars$mean, sd = pars$sd),
      exp = dexp(x, rate = pars$rate),
      beta = dbeta(x, shape1 = pars$shape1, shape2 = pars$shape2),
      pois = dpois(x, lambda = pars$lambda),
      gamma = dgamma(x, shape = pars$shape, rate = pars$rate),
      binom = dbinom(x, size = pars$size, prob = pars$prob),
      nbinom = dnbinom(x, size = pars$size, prob = pars$prob),
      chisq = dchisq(x, df = pars$df),
      f = df(x, df1 = pars$df1, df2 = pars$df2),
      t = dt(x, df = pars$df),
      geom = dgeom(x, prob = pars$prob)
    )
  }
  
  momentos_teoricos <- reactive({
    pars <- parametros()
    switch(
      input$dist,
      unif = list(media = (pars$min + pars$max) / 2,
                  variancia = (pars$max - pars$min)^2 / 12),
      norm = list(media = pars$mean,
                  variancia = pars$sd^2),
      exp = list(media = 1 / pars$rate,
                 variancia = 1 / pars$rate^2),
      beta = list(media = pars$shape1 / (pars$shape1 + pars$shape2),
                  variancia = (pars$shape1 * pars$shape2) /
                    (((pars$shape1 + pars$shape2)^2) * (pars$shape1 + pars$shape2 + 1))),
      pois = list(media = pars$lambda,
                  variancia = pars$lambda),
      gamma = list(media = pars$shape / pars$rate,
                   variancia = pars$shape / pars$rate^2),
      binom = list(media = pars$size * pars$prob,
                   variancia = pars$size * pars$prob * (1 - pars$prob)),
      nbinom = list(media = pars$size * (1 - pars$prob) / pars$prob,
                    variancia = pars$size * (1 - pars$prob) / pars$prob^2),
      chisq = list(media = pars$df,
                   variancia = 2 * pars$df),
      f = list(
        media = if (pars$df2 > 2) pars$df2 / (pars$df2 - 2) else NA_real_,
        variancia = if (pars$df2 > 4) {
          2 * pars$df2^2 * (pars$df1 + pars$df2 - 2) /
            (pars$df1 * (pars$df2 - 2)^2 * (pars$df2 - 4))
        } else {
          NA_real_
        }
      ),
      t = list(
        media = if (pars$df > 1) 0 else NA_real_,
        variancia = if (pars$df > 2) pars$df / (pars$df - 2) else if (pars$df > 1) Inf else NA_real_
      ),
      geom = list(media = (1 - pars$prob) / pars$prob,
                  variancia = (1 - pars$prob) / pars$prob^2)
    )
  })
  
  formato_numero <- function(x) {
    if (is.na(x)) return("indefinida")
    if (is.infinite(x)) return("infinita")
    formatC(x, digits = 4, format = "f")
  }
  
  n_classes_hist <- function(n) {
    max(7, ceiling(sqrt(n)))
  }
  
  calc_breaks_continuous <- function(x) {
    x <- x[is.finite(x)]
    alvo <- n_classes_hist(length(x))
    rx <- range(x)
    
    if (!all(is.finite(rx))) {
      return(seq(-0.5, 0.5, length.out = alvo + 1))
    }
    
    if (rx[1] == rx[2]) {
      largura <- max(0.5, abs(rx[1]) * 0.1)
      return(seq(rx[1] - largura, rx[2] + largura, length.out = alvo + 1))
    }
    
    br <- pretty(rx, n = alvo)
    if ((length(br) - 1) < alvo) {
      br <- seq(rx[1], rx[2], length.out = alvo + 1)
    }
    br
  }
  
  calc_breaks_discrete <- function(x) {
    x <- x[is.finite(x)]
    alvo <- n_classes_hist(length(x))
    
    if (length(x) == 0) {
      return(seq(-0.5, alvo - 0.5, by = 1))
    }
    
    lo <- floor(min(x))
    hi <- ceiling(max(x))
    n_bins <- hi - lo + 1
    
    if (n_bins < alvo) {
      extra <- alvo - n_bins
      lo <- lo - floor(extra / 2)
      hi <- hi + ceiling(extra / 2)
    }
    
    seq(lo - 0.5, hi + 0.5, by = 1)
  }
  
  suporte_continuo <- function(dist, pars, dados) {
    switch(
      dist,
      unif = c(pars$min, pars$max),
      norm = c(
        min(min(dados), qnorm(0.001, mean = pars$mean, sd = pars$sd)),
        max(max(dados), qnorm(0.999, mean = pars$mean, sd = pars$sd))
      ),
      exp = c(0, max(max(dados), qexp(0.999, rate = pars$rate))),
      beta = c(0, 1),
      gamma = c(0, max(max(dados), qgamma(0.999, shape = pars$shape, rate = pars$rate))),
      chisq = c(0, max(max(dados), qchisq(0.999, df = pars$df))),
      f = c(0, max(max(dados), qf(0.999, df1 = pars$df1, df2 = pars$df2))),
      t = c(
        min(min(dados), qt(0.001, df = pars$df)),
        max(max(dados), qt(0.999, df = pars$df))
      )
    )
  }
  
  suporte_discreto <- function(dist, pars, dados) {
    switch(
      dist,
      pois = {
        hi <- max(max(dados), qpois(0.999, lambda = pars$lambda))
        0:ceiling(hi)
      },
      binom = 0:pars$size,
      nbinom = {
        hi <- max(max(dados), qnbinom(0.999, size = pars$size, prob = pars$prob))
        0:ceiling(hi)
      },
      geom = {
        hi <- max(max(dados), qgeom(0.999, prob = pars$prob))
        0:ceiling(hi)
      }
    )
  }
  
  simulacao <- eventReactive(input$simular, {
    pars <- parametros()
    mom <- momentos_teoricos()
    
    amostras <- replicate(
      n = input$m_sim,
      expr = rdist(input$n_obs, input$dist, pars)
    )
    
    medias <- colMeans(amostras)
    
    t_stats <- apply(amostras, 2, function(x) {
      s <- sd(x)
      if (!is.finite(s) || s <= 0 || !is.finite(mom$media)) {
        return(NA_real_)
      }
      (mean(x) - mom$media) / (s / sqrt(length(x)))
    })
    
    idx <- sample(seq_len(input$m_sim), size = 1)
    amostra_escolhida <- amostras[, idx]
    
    list(
      amostras = amostras,
      medias = medias,
      t_stats = t_stats,
      idx = idx,
      amostra_escolhida = amostra_escolhida,
      pars = pars,
      media_teorica = mom$media
    )
  }, ignoreInit = FALSE)
  
  output$hist_amostra <- renderPlot({
    sim <- simulacao()
    req(sim)
    
    x <- sim$amostra_escolhida
    
    if (is_discrete()) {
      breaks_plot <- calc_breaks_discrete(x)
      suporte <- suporte_discreto(input$dist, sim$pars, x)
      ymax <- max(hist(x, breaks = breaks_plot, plot = FALSE, probability = TRUE)$density,
                  ddist(suporte, input$dist, sim$pars), na.rm = TRUE)
      
      hist(
        x,
        probability = TRUE,
        breaks = breaks_plot,
        col = rgb(0.2, 0.4, 0.8, 0.45),
        border = "white",
        ylim = c(0, 1.1 * ymax),
        main = paste("Histograma de uma amostra e distribuição teórica -", nome_distribuicao()),
        xlab = "Valores observados"
      )
      
      segments(suporte, 0, suporte, ddist(suporte, input$dist, sim$pars), lwd = 3, col = 2)
      points(suporte, ddist(suporte, input$dist, sim$pars), pch = 16, col = 2)
      
      legend(
        "topright",
        legend = c(paste("Amostra", sim$idx), "Lei teórica"),
        fill = c(rgb(0.2, 0.4, 0.8, 0.45), NA),
        border = c("white", NA),
        lty = c(NA, 1),
        lwd = c(NA, 3),
        pch = c(NA, 16),
        col = c(NA, 2),
        bty = "n"
      )
    } else {
      xlim_plot <- suporte_continuo(input$dist, sim$pars, x)
      breaks_plot <- calc_breaks_continuous(x)
      x_grid <- seq(xlim_plot[1], xlim_plot[2], length.out = 500)
      ymax <- max(hist(x, breaks = breaks_plot, plot = FALSE, probability = TRUE)$density,
                  ddist(x_grid, input$dist, sim$pars), na.rm = TRUE)
      
      hist(
        x,
        probability = TRUE,
        breaks = breaks_plot,
        col = rgb(0.2, 0.4, 0.8, 0.45),
        border = "white",
        xlim = xlim_plot,
        ylim = c(0, 1.1 * ymax),
        main = paste("Histograma de uma amostra e fdp teórica -", nome_distribuicao()),
        xlab = "Valores observados"
      )
      
      lines(x_grid, ddist(x_grid, input$dist, sim$pars), lwd = 3, col = 2)
      
      legend(
        "topright",
        legend = c(paste("Amostra", sim$idx), "fdp teórica"),
        fill = c(rgb(0.2, 0.4, 0.8, 0.45), NA),
        border = c("white", NA),
        lty = c(NA, 1),
        lwd = c(NA, 3),
        col = c(NA, 2),
        bty = "n"
      )
    }
  })
  
  output$hist_medias <- renderPlot({
    sim <- simulacao()
    req(sim)
    
    breaks_medias <- calc_breaks_continuous(sim$medias)
    
    hist(
      sim$medias,
      probability = TRUE,
      breaks = breaks_medias,
      col = "gray80",
      border = "white",
      main = "Histograma das médias amostrais",
      xlab = "Médias amostrais"
    )
    
    abline(v = mean(sim$medias), lwd = 2, lty = 2, col = 2)
    
    legend(
      "topright",
      legend = c("Histograma das médias", "Média empírica das médias"),
      fill = c("gray80", NA),
      border = c("white", NA),
      lty = c(NA, 2),
      lwd = c(NA, 2),
      col = c(NA, 2),
      bty = "n"
    )
  })
  
  output$hist_tstats <- renderPlot({
    sim <- simulacao()
    req(sim)
    
    t_validos <- sim$t_stats[is.finite(sim$t_stats)]
    validate(need(length(t_validos) >= 2,
                  "Não foi possível calcular estatísticas t suficientes com s > 0 nas amostras simuladas."))
    
    breaks_t <- calc_breaks_continuous(t_validos)
    x_grid <- seq(min(breaks_t), max(breaks_t), length.out = 500)
    h_t <- hist(t_validos, breaks = breaks_t, plot = FALSE, probability = TRUE)
    ymax <- max(h_t$density, dt(x_grid, df = input$n_obs - 1), na.rm = TRUE)
    
    hist(
      t_validos,
      probability = TRUE,
      breaks = breaks_t,
      col = rgb(0.6, 0.6, 0.6, 0.7),
      border = "white",
      ylim = c(0, 1.1 * ymax),
      main = paste("Histograma das estatísticas t e densidade teórica t(", input$n_obs - 1, ")", sep = ""),
      xlab = "Estatísticas t"
    )
    
    lines(x_grid, dt(x_grid, df = input$n_obs - 1), lwd = 3, col = 4)
    abline(v = mean(t_validos), lwd = 2, lty = 2, col = 2)
    
    legend(
      "topright",
      legend = c("Histograma das estatísticas t", paste0("fdp teórica t(", input$n_obs - 1, ")"), "Média empírica das estatísticas t"),
      fill = c(rgb(0.6, 0.6, 0.6, 0.7), NA, NA),
      border = c("white", NA, NA),
      lty = c(NA, 1, 2),
      lwd = c(NA, 3, 2),
      col = c(NA, 4, 2),
      bty = "n"
    )
  })
  
  output$resumo <- renderPrint({
    sim <- simulacao()
    req(sim)
    
    mom <- momentos_teoricos()
    xbar_amostra <- mean(sim$amostra_escolhida)
    s2_amostra <- var(sim$amostra_escolhida)
    t_validos <- sim$t_stats[is.finite(sim$t_stats)]
    
    cat("Distribuição escolhida:", nome_distribuicao(), "\n")
    cat("m_sim =", input$m_sim, "\n")
    cat("n_obs =", input$n_obs, "\n")
    cat("Amostra exibida no 1º gráfico:", sim$idx, "\n\n")
    
    cat("Média teórica de X =", formato_numero(mom$media), "\n")
    cat("Variância teórica de X =", formato_numero(mom$variancia), "\n")
    cat("Média teórica de Xbarra =", formato_numero(mom$media), "\n")
    cat("Variância teórica de Xbarra =", formato_numero(mom$variancia / input$n_obs), "\n\n")
    
    cat("xbarra da amostra exibida =", formato_numero(xbar_amostra), "\n")
    cat("s^2 da amostra exibida =", formato_numero(s2_amostra), "\n\n")
    
    cat("Média empírica das médias amostrais =", formato_numero(mean(sim$medias)), "\n")
    cat("Variância empírica das médias amostrais =", formato_numero(var(sim$medias)), "\n\n")
    
    cat("Estatística t calculada em cada amostra: t = (xbarra - mu) / (s / sqrt(n))\n")
    cat("com mu igual à média teórica da distribuição escolhida.\n")
    cat("Número de estatísticas t válidas =", length(t_validos), "de", input$m_sim, "\n")
    cat("Média empírica das estatísticas t =", formato_numero(mean(t_validos)), "\n")
    cat("Variância empírica das estatísticas t =", formato_numero(var(t_validos)), "\n\n")
    
    cat("Regra de classes dos histogramas: número de classes = max(7, teto(sqrt(n))).\n")
    cat("No 1º gráfico, n = n_obs; no 2º, n = m_sim; no 3º, n = número de estatísticas t válidas.\n")
  })
}

shinyApp(ui = ui, server = server)