# Internal: resolve a preferred/legacy argument pair, warning if the legacy one
# is used. Preferred wins if both supplied. Returns the value to use.
.deprecate_alias <- function(preferred, legacy, what, with) {
  if (!is.null(legacy)) {
    if (requireNamespace("lifecycle", quietly = TRUE)) {
      lifecycle::deprecate_warn("0.1.1", what, with)
    } else {
      warning(what, " is deprecated; use ", with, " instead.", call. = FALSE)
    }
    if (is.null(preferred)) {
      return(legacy)
    }
  }
  preferred
}
