# ------------------------------------------------------------------------------
# rfmt functions
# ------------------------------------------------------------------------------

.rfmt.defaults = list(
  backup = TRUE, margin0 = 0, margin1 = 80, cost0 = .05, 
  cost1 = 100, costb = 2, indent = 2, adj.comment = .5, 
  adj.flow = .3, adj.call = .5, adj.arg = 5, cpack = 1e-3, 
  force.brace = FALSE, space.arg.eq = TRUE, quiet = TRUE)

.onLoad <- function(lib, pkg) {
  options(rfmt = .rfmt.defaults)
}

.rfmt.opts <- function() {
  getOption("rfmt")
}

.new.rfmt.opts <- function(opts) {
  nm.opts <- names(opts)
  i.opts <- pmatch(nm.opts, names(.rfmt.defaults))
  if (!all(is.finite(i.opts)))
    stop("Invalid rfmt options:", paste(nm.opts[!is.finite(i.opts)]))
  replace(.rfmt.opts(), i.opts, opts)
}

.path.rfmt <- function() {
  system.file("python", "rfmt.py", package = "rfmt")
}

.call.rfmt <- function(opts, source.files) {
  all.opts <- .new.rfmt.opts(opts)
  py.opts <- sprintf("--%s=%s", 
      gsub(".", "_", names(all.opts), fixed = TRUE), unlist(all.opts))
  system2("python", c(shQuote(.path.rfmt()), py.opts, source.files))
}

#' Set or retrieve options relating to the \code{rfmt} formatter.
#'
#' Though they are specific to the \code{rfmt} code formatter, these functions 
#' mirror \code{options} and \code{getOption} in base \R in their behavior.
#'
#' @param \ldots named arguments used to set option values. See below for the
#' available options. A single named list may also be provided.
#'
#' @note For a description of the layout algorithm used in \code{rfmt}, look in
#' the \dQuote{User guides, package vignettes and other documentation} section
#' of the package documentation for the technical report 
#' \dQuote{A New Approach to Optimal Code Formatting}.
#'
#' @section Options and default values:
#' \describe{
#'   \item{\code{backup}}{
#'     If \code{TRUE}, the formatter copies the source file \var{fn} to the
#'     file \var{fn}\code{.bak}. 
#'     Default: \code{TRUE}.
#'   }
#'   \item{\code{margin0}}{
#'     The offset (from the left-most column) of the \dQuote{soft} right margin
#'     (where the layout algorithm is encouraged to break lines).
#'     Default: \code{0}.
#'   }
#'   \item{\code{margin1}}{
#'     The offset (from the left-most column) of the \dQuote{hard} right margin
#'     (where the layout algorithm is mandated to break lines, if at all 
#'     possible).
#'     Default: \code{80}.
#'   }
#'   \item{\code{cost0}}{
#'     The cost of each character beyond the soft right margin, \code{margin0}.
#'     Default: \code{0.05}.
#'   }
#'   \item{\code{cost1}}{
#'     The cost of each character beyond the hard right margin, \code{margin1}.
#'     Default: \code{100}.
#'   }
#'   \item{\code{costb}}{
#'     The cost associated with the introduction of a newline.
#'     Default: \code{2}.
#'   }
#'   \item{\code{indent}}{
#'     The number of spaces introduced by each successive indent.
#'     Default: \code{2}.
#'   }
#'   \item{\code{force.brace}}{
#'     Whether to insert braces in \dQuote{control flow} constructs, such as 
#'     \code{if} or \code{while}, that lack them.
#'     Default: \code{FALSE}.
#'   }
#'   \item{\code{space.arg.eq}}{
#'     Whether to surround \dQuote{\code{=}} signs in argument lists with 
#'     spaces.
#'     Default: \code{TRUE}.
#'   }
#'   \item{\code{quiet}}{
#'     Whether to suppress output of all diagnostic information.
#'     Default: \code{TRUE}.
#'   }
#'   \item{\code{adj.comment}}{
#'     Multiplier applied to the cost of a newline when formatting comments.
#'     Default: \code{0.5}.
#'   }
#'   \item{\code{adj.flow}}{
#'     Multiplier applied to the cost of a newline when formatting 
#'     \dQuote{control flow} constructs, such as \code{if} or \code{while}.
#'     Default: \code{0.3}.
#'   }
#'   \item{\code{adj.call}}{
#'     Multiplier applied to the cost of a newline when formatting function
#'     calls.
#'     Default: \code{0.5}.
#'   }
#'   \item{\code{adj.arg}}{
#'     Multiplier applied to the cost of a newline when formatting argument
#'     expressions in function calls.
#'     Default: \code{5}.
#'   }
#'   \item{\code{cpack}}{
#'     Small penalty favoring justified layouts with elements packed onto
#'     earlier lines.
#'     Default: \code{1e-3}.
#'   }
#' }
#' @export
options_rfmt <- function(...) {
  opts <- list(...)
  if (!length(opts)) return(.rfmt.opts())
  if (length(opts) == 1) {
    if (is.character(opt1 <- opts[[1]])) 
      return(structure(list(getOption_rfmt(opt1, NULL)), names = opt1))
    opts <- opts[[1]]
  }   
  options(rfmt = .new.rfmt.opts(opts))
}

#' @rdname options_rfmt
#' @param x a character string naming an \code{rfmt} option.
#' @param default value supplied if the specified option is not set (for
#' compatibility; not particularly applicable in this instance, because all 
#' \code{rfmt} options are set).
#' @export
getOption_rfmt <- function(x, default = NULL) {
  if (x %in% names(.rfmt.opts())) .rfmt.opts()[[x]] else default
}

#' Format R code using the \code{rfmt} formatter
#' 
#' This function allows the \code{rfmt} \R source code formatting tool to be
#' invoked from \R. It applies \code{rfmt} to an \R source file, the contents
#' of the system clipboard, or to a character string.
#' 
#' @param source.file a character string giving the location of a file 
#' containing \R source code. Not supplied if parameter \code{text} is
#' given instead.
#' @param text a character string containing \R source code. Not supplied
#' if \code{source.file} is given instead.
#' @param \ldots options to \code{rfmt}. See \code{\link{options_rfmt}} for
#' options and their defaults.
#' @param opts options to \code{rfmt}, supplied as a named list.
#' @return If formatting a file, nothing (\code{invisible(NULL)}). If formatting
#' a string, a \code{character} vector comprising the formatted code. If
#' formatting the contents of the clipboard, a similar \code{character} vector,
#' but rendered invisible; the formatted code is also output to the terminal
#' (using \code{cat}).
#' @note Input is taken from the clipboard if neither \code{source.file} nor 
#' \code{text} is supplied.
#' @export
rfmt <- function(source.file, text, ..., opts = list(...)) {
  if (m.src <- missing(source.file)) {
    if (m.text <- missing(text)) {
      clip <- if (Sys.info()["sysname"] == "Darwin")
          pipe("pbpaste", "r") else file("clipboard", "r")
      text <- readLines(clip, warn = FALSE)
      on.exit(close(clip))
    }
    source.file = tempfile("rfmt")
    writeLines(text, source.file) # file(source.file, "w")
    on.exit(unlink(source.file))
  } else {
    if (!length(exp.path <- Sys.glob(source.file)))
      stop(sprintf("File '%s' not found", source.file))
    source.file <- exp.path
  }
  .call.rfmt(opts, source.file)
  if (m.src) {
    fmt.text <- readLines(source.file)
    if (m.text) {
      cat(fmt.text, sep = "\n")
      invisible(fmt.text)
    } else
      fmt.text
  } else
    invisible(NULL)
}

#' Format R code in a directory tree using the \code{rfmt} formatter
#' 
#' This function applies the the \code{rfmt} \R source code formatting
#' tool the \R source file(s) located in a specified directory and (optionally)
#' its descendents.
#' 
#' @param directory a character string giving the location of the directory
#' containing \R source code.
#' @param pattern a character string comprising a regular expression used to
#' distinguish \R source code files. By default, looks for files with an
#' extension "\code{r}" or "\code{s}" (lower- or upper case).
#' @param all.files logical, \code{TRUE} iff invisible files, as well as visible
#' files, are to be examined.
#' @param recursive logical, \code{TRUE} iff descendent directories of that 
#' specified by the parameter \code{directory} are to be examined.
#' @param opts options to \code{rfmt}, supplied as a named list.
#' @export
rfmt_dir <- function(directory, pattern = "\\.[RrSs]$", all.files = FALSE, 
    recursive = FALSE, opts = list()) {
  source.files <- list.files(directory, pattern = pattern, 
                             all.files = all.files, full.names = TRUE, 
                             recursive = recursive)
  if (!("quiet" %in% names(opts)))
    opts$quiet <- FALSE
  .call.rfmt(opts, source.files)
}

.edit.init.file <- function(paths, addend, cygwin.path = NULL) {
  if ((info <- Sys.info())["sysname"] == "Windows") {
    # Work around peculiarities of "missing" argument handling.
    if (is.null(cygwin.path)) cygwin.path <- "C:/cygwin"
    if (!file.exists(cygwin.path)) stop("Can't find cygwin root directory")
    home <- file.path(cygwin.path, "home", info["user"])
  } else
    home <- Sys.glob("~")
  full.paths <- sapply(paths, function(p) file.path(home, p))
  if (any(fe <- file.exists(full.paths))) {
    target.path <- full.paths[min(which(fe))]
    lines <- readLines(target.path, warn = FALSE)
  } else {
    target.path <- full.paths[1]
    lines <- character()
  }
  if (any(grepl(addend[1], lines, fixed = TRUE))) {
    warning("rfmt already installed")
    return(invisible())
  }
  if (file.exists(target.path)) {
    bak0 <- bak <- paste0(target.path, "-bak")
    i <- 0
    while (file.exists(bak))
      bak <- sprintf("%s-%d", bak0, i <- i + 1)
    file.copy(target.path, bak)
  } else
    bak <- NULL
  # Open binary connection to ensure that '\n' is not expanded under Windows.
  conn <- file(target.path, "wb")
  on.exit(close(conn))
  writeLines(c(lines, "", addend, ""), conn)
  cat(sprintf("rfmt added to '%s'.\n", target.path))
  if (!is.null(bak)) cat(sprintf("Backup in '%s'.\n", bak))
  invisible()
}

cyg.munge <- function(fn) {
  if (Sys.info()["sysname"] == "Windows")
    fn <- sub("^(.):", "/cygdrive/\\1", fn)
  fn
}

#' Make the \dQuote{\code{rfmt}} command available to the (\code{csh}-style) 
#' *nix shell.
#' 
#' @param shellrc a string indicating shell initialization file in the user's 
#' home directory (by default, \code{.bashrc}, the initialization file for the
#' \code{bash} shell).
#' @param cygwin.path if running \href{https://www.cygwin.com}{Cygwin} under 
#' Windows, the (Windows) path of the Cygwin root directory (set to 
#' \dQuote{\code{C:/Cygwin}} if not provided).
#' @export
install_rfmt_shell <- function(shellrc = ".bashrc", cygwin.path = NULL) {
  .edit.init.file(shellrc, c("# rfmt: R source code formatter",
                             sprintf("alias rfmt=\"python '%s'\"",
                                     cyg.munge(.path.rfmt()))), cygwin.path)
}

#' Make the formatter available to Emacs, running under *nix, or 
#' Cygwin (on Windows).
#' 
#' @param emacsrc a string indicating Emacs initialization file in the user's 
#' home directory (by default, \code{.emacs}).
#' @param key the key combination used to invoke the formatter from Emacs
#' (by default, \dQuote{\code{Ctrl-x Ctrl-i}}).
#' @param cygwin.path if running Cygwin under Windows, the (Windows) path of the
#' Cygwin root directory (set to \dQuote{\code{C:/Cygwin}} if not provided).
#' @export
install_rfmt_emacs <- function(emacsrc = ".emacs", key = "C-x C-i", 
    cygwin.path = NULL) {
  emacs.addend <- c(
    ";;; rfmt: R source code formatter",
    sprintf('(add-to-list \'load-path "%s")', 
        cyg.munge(system.file("elisp", package = "rfmt"))),
    sprintf('(setq rfmt-executable "%s")', cyg.munge(.path.rfmt())),
    "(require 'rfmt-emacs)",
    sprintf('(global-set-key (kbd "%s") \'rfmt-buffer)', key))
  .edit.init.file(c(emacsrc, ".emacs.d/init.el"), emacs.addend, cygwin.path)
}

#' Make the formatter available to \code{vim}, running under *nix, or 
#' Cygwin (on Windows).
#' 
#' @param vimrc a string indicating Emacs initialization file in the user's 
#' home directory (by default, \code{.vimrc}).
#' @param key the key combination used to invoke the formatter from \code{vim}
#' (by default, \dQuote{\code{Ctrl-I}}).
#' @param cygwin.path if running Cygwin under Windows, the (Windows) path of the
#' Cygwin root directory (set to \dQuote{\code{C:/Cygwin}} if not provided).
#' @export
install_rfmt_vim <- function(vimrc = ".vimrc", key = "<C-I>", 
    cygwin.path = NULL) {
  rfmt.vim <- system.file("python", "rfmt_vim.py", package = "rfmt")
  if ((info <- Sys.info())["sysname"] == "Windows") {
    # On Windows, unfortunately, vim has inordinate problems dealing with
    # spaces in file paths; make a local copy of the rfmt executiong script as a
    # workaround.
    if (is.null(cygwin.path)) cygwin.path <- "C:/cygwin"
    file.copy(rfmt.vim, file.path(cygwin.path, "home", info["user"], 
                                  ".rfmt_vim.py"), overwrite = TRUE)
    rfmt.vim <- file.path("/home", info["user"], ".rfmt_vim.py")
  }
  vi.addend <- c(
    '" rfmt: R source code formatter',
    sprintf("let rfmt_executable = '%s'", cyg.munge(.path.rfmt())),
    sprintf("map %s :pyf %s<cr>", key, rfmt.vim),
    sprintf("imap %s <c-o>:pyf %s<cr>", key, rfmt.vim))
  .edit.init.file(vimrc, vi.addend)
}

