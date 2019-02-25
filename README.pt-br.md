# skeleton-creator

### _Crie projetos a partir de um diretório esqueleto._

Leia isto em outros idiomas: [English](https://github.com/noloop/skeleton-creator/blob/master/README.md), [Portuguese-br](https://github.com/noloop/skeleton-creator/blob/master/README.pt-br.md)

## Começando no skeleton-creator

### Portabilidade

Testei apenas no Linux usando o SBCL, não sei qual é o comportamento em outros, mas acredito que funcionará.

### Dependências

[:conf](https://github.com/noloop/conf)
[:cl-fad](https://github.com/edicl/cl-fad)
[:cl-ppcre](https://github.com/edicl/cl-ppcre)

### Instalação

**1 - Download skeleton-creator system**

No quicklisp:

```
IN PROGRESS...
```

ou diretamente do github:

```
git clone https://github.com/noloop/skeleton-creator.git
```
**2 - Instale o skeleton-creator**

By quicklisp:

```
IN PROGRESS...
```

ou diretamente do asdf:

```lisp
(asdf:load-system :skeleton-creator)
```

_**Note: Remember to configure asdf to find your directory where you downloaded the libraries (asdf call them "systems") above, if you do not know how to make a read at: https://common-lisp.net/project/asdf/asdf/Configuring-ASDF-to-find-your-systems.html or https://lisp-lang.org/learn/writing-libraries.**_

## Como skeleton-creator funciona?

É necessário que exista um diretório de configuração para onde o skeleton-creator irá olhar, você poderá configurar o caminho desse diretório como verá nos tópicos abaixo, a árvore do diretório de configuração deve ser assim:

```lisp
skeleton-creator-conf/     ; ou o nome que quiser
    skeleton/              ; o esqueleto clonado ao criar seus projetos
    license/               ; seus arquivos de licença, os arquivos devem ter o tipo .txt
        notices/           ; seus arquivos de aviso de licença, os arquivos devem ter o tipo .txt
    skeleton-creator.conf  ; o arquivo de configuração do skeleton-creator
```

caso não queira criar um diretório de condfiguração manualmente, você poderá também poderá optar por usar o diretório de configuração padrão do skeleton-creator, você pode ver ele neste mesmo respositório, o diretório **default-conf/**.

O arquivo de configuração é uma lista lisp seguindo o seguinte padrão:

```lisp
(:SK-AUTHOR "you"
:SK-EMAIL "you@mail.com" 
:SK-MAINTAINER "your" 
:SK-LICENSE "GPLv3" 
:SK-VERSION "0.1.0" 
:SK-GIT-SERVICE "github")
```

Como funciona?

Você configura uma marcação no seu arquivo de configuração, não precisam ser as acima, e em seus arquivos dentro do diretório skeleton/ você coloca elas onde quer ter o texto cnfigurado em sua marcação. Um exemplo, utilizando do arquivo de configuração acima para um arquivo README.md:

```lisp
# SK-PROJECT-NAME in vSK-VERSION by SK-AUTHOR

### SK-PROJECT-DESCRIPTION

## Contact

SK-EMAIL

## LICENSE

SK-LICENSE

Copyright (C) SK-DATE-YEAR SK-AUTHOR
```

Perceba que há um `SK-PROJECT-NAME, SK-PROJECT-DESCRIPTION, SK-DATE-YEAR` qual não está em meu arquivo de configuração, isso acontece porque há 3 marcações que são padrão do skeleton configuração, duas delas: `SK-PROJECT-NAME` e `SK-PROJECT-DESCRIPTION` serão configuradas por você ao chamar a função `create-project`. A outra é `SK-DATE-YEAR` que armazena o ano atual e é configurada automáticamente, `SK-DATE-YEAR` é muito útil para arquivos de avisos de licensa.

Então ao chamar `create-project` e inserir o nome de projeto "my-project" e a descrição "My description project...", estando no ano de 2019, seu arquivo README.md no diretório de projeto criado, deverá estar assim:

```lisp
# create-project in v0.1.0 by you

### My description project...

## Contact

you@mail.com

## LICENSE

GPLv3

Copyright (C) 2019 you
```

Recomendo utilizar um prefixo antes de suas marcações para evitar de sobrepor palavras que possam ser communs em outros arquivos, como a palavra LICENSE, a qual normalmente há diversas aparições nos arquivos de aviso de licença.

## set-configure-directory e get-configure-directory

Você pode configurar o diretório de configuração manualmente, skeleton-creator irá olhar para o que aqui estiver configurado, você usa set-configure-directory para cnfigurar, e get-configure-directory para obter o caminho que o skeleton-creator está olhando atualmente.

```lisp
SKELETON-CREATOR> (set-configure-directory "/tmp/.config/skeleton-creator-conf/")
"/tmp/.config/skeleton-creator-conf/"
SKELETON-CREATOR> (get-configure-directory)
"/tmp/.config/skeleton-creator-conf/"
```

## configure-skeleton-creator

Para configurar interativamente o arquivo de configuração `skeleton-creator.conf`.

```lisp
SKELETON-CREATOR> (configure-skeleton-creator)
```

## create-new-project

Criar um novo projeto utilizando o diretório `skeleton/`, ao chamar a função `create-new-project` você irá ter que preencher alguns dados, como diretório de destino onde o projeto irá ser criado, nome do projeto, descrição do projeto, e caso ainda não tenha configurado um diretório de configuração padrão válido, poderá optar por clonar o diretório de configuração padrão do skeleton-creator para um diretório destino de sua escolha. Fique atento ao texto e as perguntas que aparecerão no REPL.

Você também pode chamar `(create-new-project :force t)` que irá sobrepor o diretório de projeto existente caso ele já exista.

```lisp
SKELETON-CREATOR> (create-new-project)
SKELETON-CREATOR> (create-new-project :force t)
```

## delete-project-directory

Você pode deletar seus diretório de projetos criados, como qualquer outro diretório com essa função, por isso, use-a com cuidado, ela tem o poder de excluir qualquer diretório passado para ela recursivamente.

Você deve chamá-la assim `(delete-project-directory "/tmp/new-project-test/")`, imaginando que você tenha já criado um diretório "new-project-test/" em seu diretório "/tmp/", lembrando que com isso você deletará todo o projeto "new-project-test/" recursivamente.

```lisp
SKELETON-CREATOR> (delete-project-directory "/tmp/new-project-test/")
```

## license-project


## API

function **(set-configure-directory new-directory)**

function **(get-configure-directory)**

function **(configure-skeleton-creator)**

function **(create-new-project &key force use-default-conf-p)**

function **(delete-project-directory project-directory)**

function **(license-project)**

## LICENSE

Copyright (C) 2019 noloop

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

Contact author:

noloop@zoho.com
