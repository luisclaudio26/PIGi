# PIGi
[![All Contributors](https://img.shields.io/badge/all_contributors-4-orange.svg?style=flat-square)](#contributors)

Interpretador utilizando a
linguagem de programação funcional Haskell da linguagem [PIG (Parallel Image &amp; Geometry)](https://raquel-oliveira.gitbooks.io/pig/content/), linguagem imperativa focada em problemas de geometria computacional, computação gráfica e processamento de imagens. Trabalho da matéria Linguagens de Programação - Conceitos e Paradigmas, UFRN 2016.

## Configuração do Ambiente ##
---


### Dependências (pré-requisitos) ###
---
[Haskell](https://www.haskell.org) versão 4.8+ e [Cabal](https://wiki.haskell.org/Cabal-Install) são necessários. 

Instale as seguintes dependências (caso já não estejam instaladas), através dos seguintes comandos: 

Execute `cabal install parsec`

Execute `cabal install parsec-numbers`

## Build & development ##
---
Execute `cabal run` para usar o interpretador.

Execute `cabal run -- -a` para verificar se o programa pig o passa pela análise estática.

Execute `cabal run -- -s` para verificar a árvore sintática.

Há exemplos disponíveis de códigos .pig. Para executa-los utilize os comandos:

Problema1 : (avaliação de uma expressão) :     
`cabal run Problems/problem1.pig`
    
Problema2 : (faixa de valores, leitura de valores) :     
`cabal run Problems/problem2.pig`
    
Problema5 : (mdc, uso de recursão) :      
`cabal run Problems/problem5.pig`


## Contribuidores ##
---
<!-- Contributors table START -->
| [![Fernanda Isabel](https://avatars.githubusercontent.com/feisabel?s=100)<br /><sub>Fernanda Isabel</sub>](https://github.com/feisabel)<br />[👀](https://github.com/luisclaudio26/PIGi/commits?author=feisabel) | [![Lucas Torres](https://avatars.githubusercontent.com/ltrr?s=100)<br /><sub>Lucas Torres</sub>](https://github.com/ltrr)<br />[👀](https://github.com/luisclaudio26/PIGi/commits?author=ltrr)  | [![Luis Rocha](https://avatars.githubusercontent.com/luisclaudio26?s=100)<br /><sub>Luís Rocha</sub>](https://github.com/luisclaudio26)<br />[👀](https://github.com/luisclaudio26/PIGi/commits?author=luisclaudio26) | [![Raquel Oliveira](https://avatars.githubusercontent.com/raquel-oliveira?s=100)<br /><sub>Raquel Oliveira</sub>](http://raquel-oliveira.github.io/cv/CV-Oliveira.html)<br />[👀](https://github.com/luisclaudio26/PIGi/commits?author=raquel-oliveira)
| :---: | :---: | :---: | :---: |



