library(future)
#plan(sequential)
plan(multisession)
pid <- Sys.getpid()
a %<-% {
       pid <- Sys.getpid()
       cat("Future 'a' ...\n")
       3.14
  }
b %<-% {
       rm(pid)
       cat("Future 'b' ...\n")
       Sys.getpid()
  }
c %<-% {
       cat("Future 'c' ...\n")
       2 * a
  }
