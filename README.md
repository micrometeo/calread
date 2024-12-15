# calread - Program for comparing the outputs from two same-grid, same-time Calpuff runs

by Patrizia Favaron (patti.favaron@gmail.com)

## Purpose

One often needs, in research and professional activities, to compare the outputs from a same dispersion model. Reasons for this necessity may be many: among them, for example,

- Changing one or more modelling parameters.
- Using different emission patterns.
- Adopting different meteorological inputs.
- ...

All these decisions may affect the modelled outputs, and the question arises about how large the change was.

One approach of course is to carry out the two simulations, then post-processing them using the same method, and last to compare the bulk results (for example the time-average 2D concentration field) by analyzing the difference.

This is often satisfactory as a first step, but in case a difference is found it elicits a second question, namely "Ok, but _where_ this change occurred? When? Why?"

Designed to check whether bulk modelling results comply to some regulatory requirements, the post-processing tools commonly associated with dispaersion models do not usually allow the degree of "microscopic" within-the-numbers magnification necessary to address the questions, thus hampering the used dispersion model capabilities as a research tool.

It is important to notice this is ot only a pure research concern: this may also have important consequences in professional activities. For example it happens quite frequently some evaluating party asks the modeler to justify their numbers (on which important decisions are taken), and then a detailed analysis of the result should be made - would only this possible.

All these things happened to me, the initial author, involved in both research and professional activities. Being more-or-less able to write programs in modern Fortran, the same language the vast majority of dispersion models are written in, I developed the first version of this code as a by-product of a paper revision I and three friends of mine were involved in, and decided to make the result of my task available to the community (and the paper reviewers!).

Of course, a one-woman-band piece is far less, well, varied and attracting that a full blues group: if you are interested to this work to the point of using it, ok bro or sis, cite me, but also consider to contribute some work, by reviewing the code, or extending it. And do not forget starring it if you are a github user: it may change in the next future.


## Locality

This code works for Calpuff version 6 and 7, and specifically for _gridded_ receptors.

All the code depending on this specific model is stored in a unique source file (**cal_get.f90**), and in principle could be replaced with procedures to read outputs from other model(s). I have not done, my time being dedicated on the specific problem I had to solve.



## How to compile

The source code consists in four files: the main program, **main.f90**; the model-independent calculations, **commands.f90**; the model-dependent data read procedures **cal_get.f90**; and an auxiliary set of date-and-time manipulation routines, **calendar.f90**.

Compilation may be accomplished using **gfortran** (any decently recent version suffices - see next section however) and compiling from the bottom of dependency tree up, namely:
1. calendar.f90
2. cal_get.f90
3. commands.f90
4. main.f90

Intentionally I did not provide any makefile: you can use the one you prefer, would you need it. In case, the writing and use of the makefile is upon you. Anyway, a single-line compile command could be just enough - code is very simple.

One last word: in the following I assume the compiled executable name is **calread** (or **calread.exe** under Microsoft Windows). All instructions will be based on this assumption, using the command line conventions in use among the various Linux-estates and UNIX-es (I know a "Linux extension" is also available for free under Windows at the Microsoft Store, so my choice hopes to be as inclusive as possible).


## Which Fortran version?

The code is written using a plain style, my main concern being readability. I made some use of array processing statements, of the type one may find in Fortran 95, and uses (at quite a primitive level) the object-oriented extensions of Fortran 2003.

So any Fortran compiler understanding Fortran 2003 code (the vast majority, I suppose) will do the trick. As mentioned, I've used **gfortran**, but imagine the code would work also using other compilers without modification - provided these compilers, of course, know Fortran 2003 constructs.


## Why Fortran, by the way?

One could answer by detailing the many qualities the Fortran language has, especially in its most recent incarnations, and these would be all true to the writer.

I tell _my_ reason: the Calpuff manual does not contain, as far as I know, a detailed and up-to-date specification of "*.CON" Calpuff output format, and I had to reverse-engineer it from the (available) sources.

It is worth mentioning Calpuff is _not_ open-source, its owner, Exponent Inc, having released it under a proprietary license. This license is kind enough to allow anyone inspect the source code, but does not entitles anyone changing it and using, or selling, their derived work with the same or a different name.

The license does not prohibit, however, to develop independent software based on the knowledge of sources: you may imagine the Calpuff source code as a sort of "last-instance manual": if you need, read it.

Reading and understanding sources demands knowledge of the programming language the code is written in. I was lucky enough Calpuff being written in quite a modern Fortran. And once you see how data are written by a model developed using a language, chances are better you will more easily write your own reading code if you use the same. The Calpuff designers adopted Fortran, and so did I.

In principle nothing prevents one can write a model post-processing tool in C++, Rust, Swift, Python, or whatever they like. But the effort would have beel _to me_ excessive (any language has their own conventions to read binary streams, often inclining to the tricky side). This does mean you could succeed easily - in case, please let me know how you did.


## Using 'calread'

The way I use the program is by a terminal session.

The command is:



