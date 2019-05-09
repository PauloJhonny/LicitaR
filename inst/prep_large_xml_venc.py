#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys

def transf_large_xml(ano,
                     cod_ibge_mun,
                     path_xml,
                     path_df):

    import xml.etree.cElementTree as et
    import pandas as pd
    import numpy as np

    xml_data = str(path_xml) + "/" + str("%s_%s_LicitacaoVencedor.xml" % (ano, cod_ibge_mun))

    dfcols = ['cdIBGE', 'nmMunicipio', 'idPessoa', 'nmEntidade', 'idlicitacao', 'nrAnoLicitacao', 'nrLicitacao',
              'dsModalidadeLicitacao', 'nmPessoa', 'nrDocumento', 'nrLote', 'nrItem', 'nrQuantidade', 'idUnidadeMedida',
              'dsUnidadeMedida', 'vlMinimoUnitarioItem', 'vlMinimoTotal', 'vlMaximoUnitarioitem', 'vlMaximoTotal',
              'dsItem', 'dsFormaPagamento', 'nrPrazoLimiteEntrega', 'idTipoEntregaProduto', 'dsTipoEntregaProduto',
              'nrQuantidadePropostaLicitacao', 'vlPropostaItem', 'dtValidadeProposta', 'dtPrazoEntregaPropostaLicitacao',
              'nrQuantidadeVencedorLicitacao', 'vlLicitacaoVencedorLicitacao', 'nrClassificacao', 'dtHomologacao',
              'ultimoEnvioSIMAMNesteExercicio', 'DataReferencia']

    root = et.parse(xml_data)
    rows = root.findall('.//LicitacaoVencedor')

    xml_data = [[row.get('cdIBGE'), row.get('nmMunicipio'), row.get('idPessoa'), row.get('nmEntidade'), row.get('idlicitacao'), row.get('nrAnoLicitacao'), row.get('nrLicitacao'),
                 row.get('dsModalidadeLicitacao'), row.get('nmPessoa'), row.get('nrDocumento'), row.get('nrLote'), row.get('nrItem'), row.get('nrQuantidade'), row.get('idUnidadeMedida'),
                 row.get('dsUnidadeMedida'), row.get('vlMinimoUnitarioItem'), row.get('vlMinimoTotal'), row.get('vlMaximoUnitarioitem'), row.get('vlMaximoTotal'),
                 row.get('dsItem'), row.get('dsFormaPagamento'), row.get('nrPrazoLimiteEntrega'), row.get('idTipoEntregaProduto'), row.get('dsTipoEntregaProduto'),
                 row.get('nrQuantidadePropostaLicitacao'), row.get('vlPropostaItem'), row.get('dtValidadeProposta'), row.get('dtPrazoEntregaPropostaLicitacao'),
                 row.get('nrQuantidadeVencedorLicitacao'), row.get('vlLicitacaoVencedorLicitacao'), row.get('nrClassificacao'), row.get('dtHomologacao'),
                 row.get('ultimoEnvioSIMAMNesteExercicio'), row.get('DataReferencia')]
                for row in rows]

    df_xml = pd.DataFrame(xml_data, columns=dfcols)

    df_xml.to_csv(str(path_df) + "/" + str(ano) + '_licitacaovencedor_' + str(cod_ibge_mun) + '.txt', header=True, index=False, sep=';', mode='w')

if __name__ == "__main__":
    transf_large_xml(ano = int(sys.argv[1]),
                     cod_ibge_mun = str(sys.argv[2]),
                     path_xml = str(sys.argv[3]),
                     path_df = str(sys.argv[4]))
