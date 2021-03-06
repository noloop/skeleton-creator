# skeleton-creator

### _Create projects from a skeleton directory._

Read this in other languages: [English](https://github.com/noloop/skeleton-creator/blob/master/README.md), [Portuguese-br](https://github.com/noloop/skeleton-creator/blob/master/README.pt-br.md)

## Getting Started in skeleton-creator

### Portability

I just tested on Linux using SBCL, I'm not sure what the behavior is in others, but I believe it will work.

### Dependencies

[:conf](https://github.com/noloop/conf)
[:cl-fad](https://github.com/edicl/cl-fad)
[:cl-ppcre](https://github.com/edicl/cl-ppcre)


### Download and load

**1 - Load skeleton-creator system by quicklisp**

```
(ql:quickload :skeleton-creator)
```

**2 - Download and load skeleton-creator system by github and asdf**

Download directly from github:

```
git clone https://github.com/noloop/skeleton-creator.git
```

and load by ASDF:

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

Then when calling `create-project` and entering the project name `my-project` and the description `My description project...`, being in the year `2019`, your README.md file in the created project directory should be like this:

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

Create a new project using the `skeleton/` directory, when calling the `create-new-project` function you will have to fill in some data such as the destination directory where the project will be created, project name, and if you want to if not already set up a valid default configuration directory, you can choose to clone the skeleton-creator default configuration directory to a destination directory of your choice. Stay tuned for the text of questions that will appear in REPL.

You can also call `(create-new-project: force t)` which will override the  project directory if it already exists.

```lisp
SKELETON-CREATOR> (create-new-project)
SKELETON-CREATOR> (create-new-project :force t)
```

## delete-project-directory

You can delete your created project directory, like any other directory with this function, so use it carefully, it has the power to delete any directory passed to it recursively. 

You must call it `(delete-project-directory "/tmp/new-project-test/")`, imagining that you have already created a "new-project-test/" in your in your directory "/tmp/", remembering again that with this you will delete the entire project "new-project-test/".

```lisp
SKELETON-CREATOR> (delete-project-directory "/tmp/new-project-test/")
```

## license-project

To license your code automatically, you can use the `license-project` function, it can do 3 things:

1. Create a license file named LICENSE in the root of the last project directory.
2. Create license warnings at the beginning of all files in your project, and you can ignore files or directories that you do not want to have the warning.
3. Create a LICENSE topic with a license warning at the end of the README.md file, following the Markdown syntax.

When calling the function as shown below, you will have to answer some questions, and choose which of the 3 options you will want to perform.

```lisp
SKELETON-CREATOR> (license-project)
```

You need to have in your configuration directory a directory called licenses/ and a directory called noticies/ to use this function. The files in the licenses/ directory should follow the default: "\<license-name\>.txt", while the files in the notices/ directory should follow the default: "\<license-name\>-notice.txt".

If you have not set up a configuration directory, skeleton-creator is aware and will ask if you want to use the cloned default configuration directory for a destination directory which it will ask you to do. Currently the licenses/ default directory gives you two license alternatives: GPLv3 and CC0.

## API

function **(set-configure-directory new-directory)**

function **(get-configure-directory)**

function **(configure-skeleton-creator)**

function **(create-new-project &key force)**

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
