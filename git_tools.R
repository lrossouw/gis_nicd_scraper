
git_add <- function(path, path_to_add) {
  command = paste0("git ",
                   "-C ",
                   "\"",
                   path,
                   "\" ",
                   "add ",
                   path_to_add)
  system(command)
}

git_log <- function(path) {
  command = paste0("git ",
                   "-C ",
                   "\"",
                   path,
                   "\" ",
                   "log")
  system(command)
}

git_commit <- function(path, message) {
  command = paste0("git ",
                   "-C ",
                   "\"",
                   path,
                   "\" ",
                   "commit ",
                   "-m ",
                   "\"",
                   message,
                   "\"")
  system(command)
}

git_push <- function(path, repo="", branch="") {
  command = paste0("git ",
                   "-C ",
                   "\"",
                   path,
                   "\" ",
                   "push ",
                   repo,
                   " ",
                   branch)
  system(command)
}

git_pull <- function(path, repo="", branch="") {
  command = paste0("git ",
                   "-C ",
                   "\"",
                   path,
                   "\" ",
                   "pull ",
                   repo,
                   " ",
                   branch)
  system(command)
}

git_checkout <- function(path, branch) {
  command = paste0("git ",
                   "-C ",
                   "\"",
                   path,
                   "\" ",
                   "checkout ",
                   branch)
  system(command)
}

