# skeleton-creator

### _Create projects from a skeleton directory._

Read this in other languages: [English](https://github.com/noloop/skeleton-creator/README.md), [Portuguese-br](https://github.com/noloop/skeleton-creator/README.pt-br.md)

## Getting Started in skeleton-creator

### Portability

I just tested on Linux using SBCL, I'm not sure what the behavior is in others, but I believe it will work.

### Dependencies

[:conf](https://github.com/noloop/conf)
[:cl-fad](https://github.com/edicl/cl-fad)
[:cl-ppcre](https://github.com/edicl/cl-ppcre)

### Instalation

**1 - Download skeleton-creator system**

By quicklisp:

```
IN PROGRESS...
```

or directly from github:

```
git clone https://github.com/noloop/skeleton-creator.git
```
**2 - Install skeleton-creator**

By quicklisp:

```
IN PROGRESS...
```

or directly from asdf:

```lisp
(asdf:load-system :skeleton-creator)
```

_**Note: Remember to configure asdf to find your directory where you downloaded the libraries (asdf call them "systems") above, if you do not know how to make a read at: https://common-lisp.net/project/asdf/asdf/Configuring-ASDF-to-find-your-systems.html or https://lisp-lang.org/learn/writing-libraries.**_

## How does skeleton-creator work?

It is necessary that there is a configuration directory where the skeleton-creator will look, you can configure the path of this directory as you will see in the topics below, the configuration directory tree should look like this:

```lisp
skeleton-creator-conf/     ; or whatever name you want
    skeleton/              ; the skeleton cloned while creating your projects
    license/               ; your license files, the files should have type .txt
        notices/           ; your notices files, the files should have type .txt
    skeleton-creator.conf  ; skeleton-creator configuration file
```

if you do not want to create a configuration directory manually, you may also choose to use the skeleton-creator default configuration directory, you can see it in the same repository, the **default-conf/** directory.

The configuration file is a lisp list following the following pattern:

```lisp
(:SK-AUTHOR "you"
:SK-EMAIL "you@mail.com" 
:SK-MAINTAINER "your" 
:SK-LICENSE "GPLv3" 
:SK-VERSION "0.1.0" 
:SK-GIT-SERVICE "github")
```

How it works?

You configure a markup in your configuration file, they do not need to be the same as configured above. And in your files inside the "skeleton/ " directory you write it where you want to have the text set in your markup. An example, using the above configuration file for a README.md file:

```lisp
# SK-PROJECT-NAME in vSK-VERSION by SK-AUTHOR

### SK-PROJECT-DESCRIPTION

## Contact

SK-EMAIL

## LICENSE

SK-LICENSE

Copyright (C) SK-DATE-YEAR SK-AUTHOR
```

Note that there is a `SK-PROJECT-NAME, SK-PROJECT-DESCRIPTION, SK-DATE-YEAR` which is not in my configuration file, this is because there are 3 markings that are skeleton-creator default, `SK-PROJECT-NAME` and `SK-PROJECT-DESCRIPTION` will be configured by you when calling the `create-project` function. The other is `SK-DATE-YEAR` which stores the current year and is automatically configured, `SK-DATE-YEAR` is very useful for license notices files.

Then when calling `create-project` and entering the project name 'my-project` and the description `My description project ...`, being in the year `2019`, your README.md file in the created project directory should be like this:

```lisp
# create-project in v0.1.0 by you

### My description project...

## Contact

you@mail.com

## LICENSE

GPLv3

Copyright (C) 2019 you
```

I recommend using a prefix(how SK-) before your markup to avoid overlapping words that might be common in other files, such as the word LICENSE, which usually has several appearances in the license warning files.

## set-configure-directory and get-configure-directory

You can configure the configuration directory manually, skeleton-creator will look at what you configure, you use `set-configure-directory` to configure, and `get-configure-directory` to get the path that the skeleton-creator is currently looking at.

```lisp
SKELETON-CREATOR> (set-configure-directory "/tmp/.config/skeleton-creator-conf/")
"/tmp/.config/skeleton-creator-conf/"
SKELETON-CREATOR> (get-configure-directory)
"/tmp/.config/skeleton-creator-conf/"
```
## configure-skeleton-creator

To interactively configure the `skeleton-creator.conf` configuration file.

```lisp
SKELETON-CREATOR> (configure-skeleton-creator)
```

## create-new-project

Criar um novo projeto utilizando o diretório `skeleton/`, ao chamar a função `create-new-project` você irá ter que preencher alguns dados, como diretório de destino onde o projeto irá ser criado, nome do projeto, descrição do projeto, e caso ainda não tenha configurado um diretório de configuração padrão válido, poderá optar por clonar o diretório de configuração padrão do skeleton-creator para um diretório destino de sua escolha. Fique atento ao texto e as perguntas que aparecerão no REPL.

Você também pode chamar `(create-new-project :force t)` que irá sobrepor o diretório de projeto existente caso ele já exista. E `(create-new-project :use-default-conf-p t)` que irá utilizar o diretório de configuração padrão já indo para o momento em que você deve dar ao REPL o diretório de destino onde quer que o diretório de configuração padrão seja clonado.

```lisp
SKELETON-CREATOR> (create-new-project)
SKELETON-CREATOR> (create-new-project :force t)
SKELETON-CREATOR> (create-new-project :use-default-conf-p t)
SKELETON-CREATOR> (create-new-project :force t :use-default-conf-p t)
```

## delete-project-directory

Você pode deletar seus diretório de projetos criados, como qualquer outro diretório com essa função, por isso, use-a com cuidado, ela tem o poder de excluir qualquer diretório passado para ela recursivamente. vocêr deve chamá-la assim `(delete-project-directory "/tmp/new-project-test/")`, imaginando que você tenha já criado um diretório "new-project-test/" em seu diretório "/tmp/", lembrando que com isso você deletará todo o projeto "new-project-test/" recusirvamente.

```lisp
SKELETON-CREATOR> (delete-project-directory "/tmp/new-project-test/")
```


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
