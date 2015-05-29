# Another set of utilities for emailing maintainers (mtrs) of 
# broken packages. Uses slightly different utility functions for
# sending email than email.R. 


.getMasterBuilders <- function()
{
    yaml <- content(GET("http://bioconductor.org/config.yaml"))
    config <- yaml.load(yaml)
    ret <- list()
    for (version in c("release", "devel"))
    {
        ret[[version]] <- config[[paste0("active_", version, "_builders")]]$linux
    }
    ret
}

.getPackageFails <- function(package, software=TRUE)
{
    if (software)
        repos = "bioc"
    else
        repos = "data-experiment"
    ret <- list()
    for (version in c("release", "devel"))
    {
        url <- paste0("http://bioconductor.org/checkResults/",
            version, "/", repos, "-LATEST/STATUS_DB.txt")
        status_txt <- content(GET(url))
        lines <- strsplit(status_txt, "\n")[[1]]
        raw <- lines[grep(paste0("^", package, "#"), lines)]
        j <- unlist(strsplit(raw, " ")) 
        results <- sort(unique(j[c(rep(FALSE,TRUE), TRUE)]))
        results <-  results[!grepl("NotNeeded|skipped", results)]
        ret[[version]] <- results
    }
    if (all(unlist(lapply(x, function(y) length(y) == 1 && y == "OK"))))
        stop("This package has no issues!")
    ret
}


emailMtr <- function(package, software=TRUE, from=getOption("fromEmail"),
  subject="%s build problem")
{

}