# **rfmt**: A code formatter for R

The **rfmt** package is intended to improve the formatting of R code to aid readability (in the same mold as [**gofmt**](https://golang.org/cmd/gofmt/) tool
for Go, and [**clang-format**](http://clang.llvm.org/docs/ClangFormat.html) for
C/C++). It shares many of the objectives of Yihui Xie's [**formatR**](https://cran.r-project.org/web/packages/formatR/index.html) package, though with its more eleborate layout algorithm (documented in [this technical report](http://research.google.com/pubs/pub44667.html), also included in the package documentation) and general approach to code formatting, it aims to produce more ''aesthetically appealing'' results.

To use **rfmt**, you'll need to have a Python (v. 2.7 or later) installation available. If this is not the case, you can download one yourself from [python.org](https://www.python.org/downloads/release/python-2711/).

The implementation of the formatter relies heavily on David Beazley's [`ply` package](http://www.dabeaz.com/ply/), a copy of which is supplied with the package.
