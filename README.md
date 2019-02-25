# skeleton-creator

### _Create projects from a skeleton directory._

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

## Topics...

## API
#:set-configure-directory
                #:get-configure-directory
                #:configure-skeleton-creator
                #:create-new-project
                #:delete-project-directory
                #:license-project
function **(suite description &rest tests)**


