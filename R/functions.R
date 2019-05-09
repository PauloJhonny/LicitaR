#' Scraper para extrair dados do Portal de Informações para Todos (PIT) - Dados Abertos - do Tribunal
#' de Contas do Estado do Paraná (TCE/PR).
#'
#' Recebe um ano, um tema relacionado a licitações e contratos, os códigos IBGE dos municípios do PR desejados
#' e um caminho para salvar os arquivos. Retorna arquivos XML individuais por ano e município.
#'
#' @importFrom magrittr %>%
#'
#' @param ano ano da licitação/contrato. Default: 2013 a 2017.
#' @param tema Licitacao, Contrato ou Relacionamentos.
#' @param cod_ibge_mun códigos dos municípios do PR. Default: "Todos".
#' @param path caminho para salvar os arquivos.
#'
#' @examples
#' scraper_licitacao(ano = c(2013, 2014), tema = "Licitacao",
#'                   cod_ibge_mun = 410010, path = "C:/")
#'
#' @export
scraper_licitacao <- function(ano = c(2013:2017),
                              tema = c("Licitacao", "Contrato", "Relacionamentos"),
                              cod_ibge_mun = "Todos",
                              path){

  # verificações
  if(length(ano) == 0) stop("É necessário indicar pelo menos um ano entre 2013 e 2017!")
  if(tema %in% c("Licitacao", "Contrato", "Relacionamentos") == FALSE) stop("O tema indicado não está disponível. Por favor, indique um dos seguintes temas: Licitacao, Contrato ou Relacionamentos!")
  if(length(tema) > 1) stop(paste0("Não é possível realizar o procedimento para ",length(tema)," temas ao mesmo tempo. Por favor, indique apenas um tema!"))
  if(cod_ibge_mun == "") stop("É necessário indicar pelo menos um município paranaense!")
  if(path == "") stop("É necessário indicar um caminho para salvar os arquivos!")

  # cria os diretórios temáticos para armazenamento dos arquivos XML
  ifelse(!dir.exists(file.path(sprintf(paste(path, "/%s", sep = ""), tema))),
         dir.create(file.path(sprintf(paste(path, "/%s", sep = ""), tema))), FALSE)

  # estabelece o diretório de trabalho
  setwd(sprintf(paste(path, "/%s", sep = ""), tema))

  # processo de download e armazenamentos dos arquivos
  for(ano in ano[1]:ano[length(ano)]){

    # cria a url para acesso aos arquivos
    url <- sprintf("http://servicos.tce.pr.gov.br/TCEPR/Tribunal/Relacon/Arquivos/%02d/", ano)

    # cria os diretórios para os arquivos compactados e anos
    dir_ano <- sprintf("%02d", ano) # especifica o diretório referente ao ano onde o arquivo será salvo
    ifelse(!dir.exists(file.path("./arq_compact/")), dir.create(file.path("./arq_compact/")), FALSE)
    ifelse(!dir.exists(file.path("./arq_compact/", dir_ano)), dir.create(file.path("./arq_compact/", dir_ano)), FALSE)

    # filtra o arquivo data(cod_mun_ibge) pelo cod_ibge_mun passado na função
    data(cod_mun_ibge); if("Todos" %in% cod_ibge_mun == FALSE) {cod_mun_ibge <- cod_mun_ibge %>%
      dplyr::filter(cdIBGE %in% cod_ibge_mun)} else {NULL}

    # faz o download do arquivo .zip e extrai os arquivos XML para cada município
    for(i in 1:nrow(cod_mun_ibge)){

      zip <- sprintf("%02d_%02d_%s.zip", ano, cod_mun_ibge[i, 1], tema) # .zip com ano, códido do município e tema
      url_principal <- paste0(url, zip) # cria a url principal

      if(stringr::str_detect(httr::GET(url_principal)$headers$`content-type`, "x-zip-compressed") == TRUE) {

        dir <- sprintf("./arq_compact/%02d/%s", ano, zip) # especifica o diretório onde o arquivo será salvo
        downloader::download(url_principal, dest=dir, mode="wb") # faz o download do arquivo .zip
        unzip(dir, exdir = paste0(".")) # extrai os arquivos em uma pasta específica

      } else {print(paste("Os dados do município", cod_mun_ibge[i, 1],"não estão disponíveis para o ano de",ano))}
    }
  }
}

#' Scraper para extrair dados de empresas da API ReceitaWS (https://www.receitaws.com.br).
#'
#' Recebe um dataframe com apenas uma coluna de CNPJ's e retorna uma lista com quatro elementos: dados gerais das empresas,
#' atividades principais, atividades secundárias e quadros societários. Obs.: a função pode demorar para retornar os
#' resultados devido ao fato de a API ter uma limitação de apenas 3 CNPJ's por minuto. Por isso, a cada 3 CNPJ's
#' consultados, é necessário aguardar 60 segundos para realizar nova consulta, senão ocorre timeout.
#'
#' @importFrom magrittr %>%
#'
#' @param cnpj_data_frame dataframe de CNPJ's.
#'
#' @examples
#' scraper_empresas(data.frame(cnpj = "00543679000165"))
#'
#' @export
scraper_empresas <- function(cnpj_data_frame){

  # verificações
  if(is.data.frame(cnpj_data_frame) == FALSE) stop("É necessário inserir um data frame de cnpj's válido!")

  # função para coletar o dados da API da ReceitaWS
  scraper_empresas_ind <- function(cnpj_data_frame){

    # cria listas para receber os dados
    dados_gerais <- rep(list(""), nrow(cnpj_data_frame))
    atividade_principal <- rep(list(""), nrow(cnpj_data_frame))
    atividade_secundaria <- rep(list(""), nrow(cnpj_data_frame))
    quadro_societario <- rep(list(""), nrow(cnpj_data_frame))
    dados_ind <- rep(list(""), 4)

    # extrai os dados para cada um dos CNPJ's do data frame inserido
    for(i in 1:nrow(cnpj_data_frame)){

      # url da API da ReceitaWS para cada uma das empresas
      url_empresa <- sprintf("https://www.receitaws.com.br/v1/cnpj/%s", cnpj_data_frame$cnpj[i])

      # faz uma requisição para a URL criada
      req <- try(url_empresa %>%
                   httr::GET())

      # verifica se a requisição foi realizada ou se ocorreu um erro. Se ocorrer erro, significa que a requisição
      # não foi bem sucedida devido a não existência de dados para o CNPJ ou a requisição demorou para retornar
      # os dados. Como resultado, são inseridos nas listas os números dos CNPJ's que deram problemas.
      if("try-error" %in% class(req) == TRUE){

        dados_gerais[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i])
        atividade_principal[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i])
        atividade_secundaria[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i])
        quadro_societario[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i])

      } else {

        # extrai o conteúdo da requisição
        cont <- httr::content(req)

        # verifica se o status para o CNPJ é "OK". Se não o for, são inseridos nas listas os números dos CNPJ's
        # que deram problemas.
        if(cont$status %in% "OK" == FALSE){

          dados_gerais[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i])
          atividade_principal[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i])
          atividade_secundaria[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i])
          quadro_societario[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i])

        } else {

          # # extrai os dados do CNPJ e salva em uma lista
          # dados <-  url_empresa %>%
          #   jsonlite::fromJSON()

          atvp <- as.data.frame(cont$atividade_principal)
          atvs <- data.table::rbindlist(cont$atividades_secundarias, fill = TRUE)
          qsa <- data.table::rbindlist(cont$qsa, fill = TRUE)

          # dados da atividade principal da empresa
          if(nrow(atvp) == 0){
            atividade_principal[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i], text = "", code = "")
          } else {
            atividade_principal[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i], atvp)
          }

          # dados das atividades secundárias da empresa
          if(nrow(atvs) == 0){
            atividade_secundaria[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i], text = "", code = "")
          } else {
            atividade_secundaria[[i]] <- data.frame(cnpj = rep(cnpj_data_frame$cnpj[i], nrow(atvs)), atvs)
          }

          # dados do quadro societário da empresa
          if(nrow(qsa) == 0){
            quadro_societario[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i], qual = "", nome = "")
          } else {
            quadro_societario[[i]] <- data.frame(cnpj = rep(cnpj_data_frame$cnpj[i], nrow(qsa)), qsa)
          }

          # exclui os data frames da lista para poder tranformá-la em data frame e criar os dados gerais da empresa
          cont$atividade_principal <- NULL
          cont$atividades_secundarias <- NULL
          cont$qsa <- NULL
          cont$extra <- NULL
          cont$billing <- NULL

          # dados gerais da empresa
          dados_gerais[[i]] <- data.frame(cnpj = cnpj_data_frame$cnpj[i], as.data.frame(cont))
        }
      }
    }

    # empilha os elementos das listas, transformando-as em data frames
    dados_ind[[1]] <- data.table::rbindlist(dados_gerais, fill = TRUE)
    dados_ind[[2]] <- data.table::rbindlist(atividade_principal, fill = TRUE)
    dados_ind[[3]] <- data.table::rbindlist(atividade_secundaria, fill = TRUE)
    dados_ind[[4]] <- data.table::rbindlist(quadro_societario, fill = TRUE)

    return(dados_ind)

  }

  # a função pode demorar para retornar os resultados devido ao fato de a API ter uma limitação de apenas
  # 3 CNPJ's por minuto. Por isso, a cada 3 CNPJ's consultados, é necessário aguardar 60 segundos para
  # realizar nova consulta, senão ocorre timeout.

  # marcadores
  j=1
  w=3

  # verifica a nomenclatura dos CNPJ's
  cnpj_data_frame <- cnpj_data_frame %>%
    dplyr::mutate(cnpj = cnpj_data_frame[,names(cnpj_data_frame)[which(stringr::str_detect(names(cnpj_data_frame), ".*cnpj.*|.*CNPJ.*|.*Cnpj.*") == TRUE)]]) %>%
    dplyr::select(cnpj)

  # cria listas para receber os resultados
  cnpj_dados_gerais_list <- rep(list(""), ceiling(nrow(cnpj_data_frame)/3))
  cnpj_atividade_principal_list <- rep(list(""), ceiling(nrow(cnpj_data_frame)/3))
  cnpj_atividade_secundaria_list <- rep(list(""), ceiling(nrow(cnpj_data_frame)/3))
  cnpj_quadro_societario_list <- rep(list(""), ceiling(nrow(cnpj_data_frame)/3))

  # aplica a função scraper_empresas_ind para cada data frame de tamanho 3 e salva os resultados em cada lista
  # criada acima
  for(i in 1:ceiling(nrow(cnpj_data_frame)/3)){

    data <- data.frame(cnpj = as.character(cnpj_data_frame[j:w, "cnpj"])) %>% dplyr::filter(!is.na(cnpj))
    cnpj_data_frame_all <- scraper_empresas_ind(data)
    cnpj_dados_gerais_list[[i]] <- cnpj_data_frame_all[[1]]
    cnpj_atividade_principal_list[[i]] <- cnpj_data_frame_all[[2]]
    cnpj_atividade_secundaria_list[[i]] <- cnpj_data_frame_all[[3]]
    cnpj_quadro_societario_list[[i]] <- cnpj_data_frame_all[[4]]

    j = j+3
    w = w+3

    # devido à limitação da API, foi setado para aguardar 60 segundos para fazer nova requisição de 3 CNPJ's
    if(ceiling(nrow(cnpj_data_frame)/3)>1){

      Sys.sleep(60)

    } else {NULL}

  }

  # cria lista vazia e salva os resultados
  dados_all <- rep(list(""), 4)
  names(dados_all) <- c("dados-gerais", "atividade-principal", "atividade-secundaria", "quadro-societario")

  dados_all[[1]] <- data.table::rbindlist(cnpj_dados_gerais_list, fill = TRUE)
  dados_all[[2]] <- data.table::rbindlist(cnpj_atividade_principal_list, fill = TRUE)
  dados_all[[3]] <- data.table::rbindlist(cnpj_atividade_secundaria_list, fill = TRUE)
  dados_all[[4]] <- data.table::rbindlist(cnpj_quadro_societario_list, fill = TRUE)

  return(dados_all)

}

#' Transformação de arquivos XML obtidos via função 'scraper_licitacao' em data frames.
#'
#' Transforma um conjunto de arquivos XML em data frame e salva os resultados em uma base de dados. Além disso,
#' é salvo um arquivo de possíveis erros na tranformação. Obs.: Arquivos que excedem 500MB são desconsiderados
#' do processo. Assim, é necessário utilizar o arquivo de saída relativo aos 'erros' para identificá-los e
#' realizar o procedimento de transformação de XML em data frame separadamente!
#'
#' @importFrom magrittr %>%
#'
#' @param ano ano da licitação/contrato. Default: 2013 a 2017.
#' @param tema Licitacao, LicitacaoVencedor, LicitacaoParticipante, Contrato, ContratoAditivo ou Relacionamentos.
#' @param cod_ibge_mun códigos dos municípios do PR. Default: "Todos".
#' @param path_xml local de armazenamento dos arquivos XML.
#' @param size_xml tamanho máximo de arquivo XML a ser considerado na tranformação (em MB). Como default foi setado o
#' valor de 500MB. É possível aumentar esse valor, entretanto pode sobrecarregar o processamento e ocorrer timeout.
#' @param path_df local para salvar a base de dados e o arquivo de erros.
#' @param file_name nome da base de dados a ser salva.
#'
#' @examples
#' prep_xml_licitacao(ano = c(2013, 2014), tema = "Licitacao",
#'                    cod_ibge_mun = 410010, path_xml = "C:/Licitacao",
#'                    path_df = "C:/Licitacao")
#'
#' @export
prep_xml_licitacao <- function(ano = c(2013:2017),
                               tema = c("Licitacao", "LicitacaoVencedor", "LicitacaoParticipante", "Contrato", "ContratoAditivo", "Relacionamentos"),
                               cod_ibge_mun = "Todos",
                               path_xml,
                               size_xml = 500,
                               path_df,
                               file_name = paste(ifelse(length(ano) > 1, c(paste(ano[1], ano[length(ano)], sep = "_")),
                                                        ano[1]), tolower(tema), Sys.Date(), sep = "_")){

  # verificações
  print("Arquivos que excedem 500MB são desconsiderados do processo para não sobrecarregar a memória. Favor utilizar o arquivo de saída relativo aos 'erros' para identificá-los e realizar o procedimento de transformação de XML em data frame separadamente!")
  if(length(ano) == 0) stop("É necessário selecionar pelo menos um ano entre 2013 e 2017!")
  if(length(tema) > 1) stop(paste0("Não é possível realizar o procedimento para ",length(tema)," temas ao mesmo tempo. Por favor, selecione apenas um tema!"))
  if(tema %in% c("Licitacao", "LicitacaoVencedor", "LicitacaoParticipante", "Contrato", "ContratoAditivo", "Relacionamentos") == FALSE) stop("O tema indicado não está disponível. Por favor, indique um dos seguintes temas: Licitacao, LicitacaoVencedor, LicitacaoParticipante, Contrato, ContratoAditivo ou Relacionamentos!")
  if(path_xml == "") stop("É necessário indicar um caminho para os arquivos XML!")
  if(path_df == "") stop("É necessário indicar um caminho para salvar os arquivos!")
  if(file_name == "") stop("É necessário indicar um nome para o arquivo final!")
  if(cod_ibge_mun == "") stop("É necessário indicar pelo menos um município paranaense!")
  if(file.exists(paste0(path_df, "/", file_name, ".txt"))) stop(paste0("O arquivo ", paste0(path_df, "/", file_name, ".txt"), " já existe!"))

  # estabelece o diretório de trabalho
  setwd(path_xml)

  # lista os arquivos XML a serem transformados em data frame
  if(tema != "Relacionamentos"){

    files <- list.files() %>%
      stringr::str_subset(paste0("^", ano, ".*_", tema, ".xml$", collapse = "|"))

    if("Todos" %in% cod_ibge_mun == FALSE) {files <- files %>%
      stringr::str_subset(paste0("^", ano, "_", cod_ibge_mun, "_", tema, ".xml$", collapse = "|"))} else {NULL}

  } else {

    files <- list.files() %>%
      stringr::str_subset(paste0("^", ano, ".*_", "LicitacaoXContrato.xml$", collapse = "|"))

    if("Todos" %in% cod_ibge_mun == FALSE) {files <- files %>%
      stringr::str_subset(paste0("^", ano, "_", cod_ibge_mun, "_", "LicitacaoXContrato.xml$", collapse = "|"))} else {NULL}

  }

  # verifica a quantidade de arquivos XML. Se for 0, a função é interrompida.
  if(length(files) == 0) stop(paste("Não há arquivos XML do tema", tema, "no diretório", path_xml))

  # tranforma cada arquivo XML em data frame e salva em uma lista
  transf <- function(FileName) {

    result <- xml2::read_xml(FileName) %>%
      xml2::xml_contents() %>%
      purrr::map(xml2::xml_attrs) %>%
      purrr::map_df(~as.list(.))

    return(result)

  }

  # cria objetos para armazenar os resultados
  dados <- as.list(rep(NULL, length(files)))
  erros <- data.frame(erro = NA, tamanho_exc = NA)

  # faz a tranformação dos XML em data frames
  for(i in 1:length(files)) {

    if(file.info(files[i])$size/1024 <= size_xml*1000) {

      teste <- try(transf(files[i]), silent = TRUE)

      if("try-error" %in% class(teste) == TRUE) {

        print(paste("Erro no arquivo XML:", files[i]))
        erros[i, "erro"] <- files[i]

      } else {

        dados[[i]] <- teste
      }

    } else {

      print(paste("O arquivo XML excedeu o tamanho máximo de arquivo:", files[i]))
      erros[i, "tamanho_exc"] <- files[i]

    }
  }

  # arquivo de erros
  erros <- dplyr::filter(erros, !(is.na(erro) & is.na(tamanho_exc)))

  # empilha a lista de data frames
  df_final <- data.table::rbindlist(dados, fill = TRUE)

  # exclui NA's
  if(tema != "Relacionamentos" & tema != "ContratoAditivo") df_final <- df_final %>% dplyr::filter(!is.na(cdIBGE))

  # renomeia variável
  if("t.xml_data..i..." %in% colnames(df_final)) df_final <- df_final %>% dplyr::select(-t.xml_data..i...)

  # retira múltiplos espaços
  if("nmMunicipio" %in% colnames(df_final) & "nmEntidade" %in% colnames(df_final))
    df_final[, c("nmMunicipio", "nmEntidade")] <- lapply(df_final[, c("nmMunicipio", "nmEntidade")],
                                                         function(x) stringr::str_trim(x))

  if("sgDocParticipanteLicitacao" %in% colnames(df_final) & "DataReferencia" %in% colnames(df_final))
    df_final[, c("sgDocParticipanteLicitacao", "DataReferencia")] <- lapply(df_final[, c("sgDocParticipanteLicitacao", "DataReferencia")],
                                                                            function(x) stringr::str_trim(x))

  # salva os data frames em extensão .txt
  write.table(x = df_final, file = paste0(path_df, "/", file_name, ".txt"),
              row.names = FALSE, sep = ";", qmethod = "double")

  write.table(x = erros, file = paste0(path_df, "/", file_name, "_erros.txt"),
              row.names = FALSE, sep = ";", qmethod = "double")

}

#' Transformação de grandes arquivos XML do tema LicitacaoVencedor obtidos via função 'scraper_licitacao' em data frames.
#'
#' @importFrom magrittr %>%
#'
#' @param ano ano da licitação/contrato. Default: 2013.
#' @param cod_ibge_mun códigos dos municípios do PR. Default: "411520".
#' @param path_xml local de armazenamento dos arquivos XML.
#' @param path_df local para salvar a base de dados.
#' @param encoding codificação do arquivo. Default: "latin1".
#'
#' @examples
#' prep_large_xml_venc(ano = 2013, cod_ibge_mun = 411520, path_xml = "C:/Licitacao",
#'                     path_df = "C:/Licitacao")
#'
#' @export
prep_large_xml_venc <- function(ano = 2013,
                                cod_mun_ibge = "411520",
                                path_xml,
                                path_df) {

  if(length(ano) == 0 | length(ano) > 1) stop("É necessário selecionar um dos anos entre 2013 e 2017!")
  if(length(cod_mun_ibge) == 0 | length(cod_mun_ibge) > 1) stop("É necessário selecionar um código de município entre os 399 disponíveis!")
  if(path_xml == "") stop("É necessário indicar um caminho para os arquivos XML!")
  if(path_df == "") stop("É necessário indicar um caminho para salvar os arquivos!")
  #if(length(encoding) == 0 | length(encoding) > 1) stop("É necessário selecionar um tipo de codificação para os dados!")

  path <- paste(system.file(package="LicitaR"), "prep_large_xml_venc.py", sep="/")
  command <- paste("python", path, ano, cod_mun_ibge, path_xml, path_df)
  system(command, intern = T)

}




