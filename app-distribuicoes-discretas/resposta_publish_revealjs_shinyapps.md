# Diagnóstico do erro ao publicar um Revealjs em `.qmd` no shinyapps.io

O erro não parece estar no conteúdo dos slides em si. Ele indica que o **bundle foi classificado como conteúdo estático** (`static`) na hora do deploy, e o **shinyapps.io espera conteúdo Shiny** quando você publica um documento Quarto interativo.

No arquivo que você colou, a estrutura principal está coerente para um **Quarto + Shiny**:

- há `server: shiny` no YAML;
- há um chunk com `#| context: server`.

Então, à primeira vista, o problema está mais no **modo de publicação** do que no seu código-fonte.

## O que fazer

### 1. Atualize os pacotes e teste localmente

```r
install.packages(c("quarto", "rsconnect", "rmarkdown"))
```

### 2. Rode localmente antes de publicar

```r
quarto::quarto_serve("seuarquivo.qmd")
```

ou use **Run Document** no RStudio.

### 3. Publique como app, não como documento estático

```r
library(quarto)

quarto_publish_app(
  input = "seuarquivo.qmd",
  server = "shinyapps.io"
)
```

Esse é o caminho recomendado para documentos interativos com Shiny.

### 4. Se estiver usando `rsconnect::deployApp()`, force o modo correto

```r
rsconnect::deployApp(
  appDir = ".",
  appPrimaryDoc = "seuarquivo.qmd",
  appMode = "quarto-shiny",
  quarto = TRUE
)
```

Isso é útil quando a inferência automática do tipo da aplicação falha.

## Pontos importantes

- **Não precisa trocar para `runtime: shiny`**; em Quarto, o padrão esperado é `server: shiny`.
- Como seus slides são `revealjs`, vale a pena usar uma versão recente do Quarto.

## Resumo

O seu `.qmd` parece estar montado corretamente; o que provavelmente está errado é que ele está sendo enviado ao shinyapps como **static**.

A solução mais provável é uma destas:

- publicar com `quarto_publish_app()`;
- ou forçar `appMode = "quarto-shiny"` no `deployApp()`.

Se o problema continuar, vale conferir também:

- versão do Quarto;
- versão do `rsconnect`;
- comando exato usado no deploy.
