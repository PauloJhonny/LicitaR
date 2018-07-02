#!/bin/bash
#set -x

# esse script serve para enviar arquivos do projeto 033-proj-licitacao para o repositório no git

# parâmetros

email=$1
nome=$2
dir_file=$3
mensagem=$4

if [ ! -e $dir_file ]; then

	  echo "Erro: O diretório ou arquivo $dir_file não existe. Favor indicar um diretório ou arquivo existente!"
	  exit

fi

if [ ! -w $dir_file ]; then

	  echo "Erro: você não tem permissão para alterar o diretório ou arquivo $dir_file!"
	  exit

fi

exec 2>/dev/null
index=./.git/index.lock

#if [ -e $index ]; then
	
#	rm $index
#	exit

#fi

git config --global user.name "$nome"
git config --global user.email "$email"

git init

git add ./$dir_file

git commit -m "$mensagem"

git push origin master
