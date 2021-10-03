#!/usr/bin/env Rscript
description <- desc::description$new()
description_old <- description$clone(deep = TRUE)
deps <- description$get_deps()
deps <- deps[order(deps$type, deps$package), , drop = FALSE]
description$del_deps()
description$set_deps(deps)
remotes <- description$get_remotes()
if (length(remotes) > 0) {
  description$set_remotes(sort(remotes))
}
description$set(Encoding = "UTF-8")
try(description$normalize(), silent = TRUE)
if (!all(capture.output(description) == capture.output(description_old))) {
  description$write()
}
