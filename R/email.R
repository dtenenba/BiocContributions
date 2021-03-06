
########################################################
##
## TODO: deal with multiple maintainers more elegantly!
## TODO: write email function to request svn account from Carl.
##
########################################################



## I need to send emails to Carl, new/older authors, as well as
## prospective authors.  These should all be functions if for no other
## reason than to allow their contents to be edited by all team
## members via svn.

####################################################################
## lets start with a function to send an email when the user already
## has a svn account.

## This just gets the contents for the Maintainer field
## (There might be multiple emails returned from here)
.extractEmails <- function(dir){
    dirPath <- file.path(dir, "DESCRIPTION")
    DESC <- read.dcf(dirPath)
    rawEmail <- DESC[,grepl("Maintainer",colnames(DESC)),drop=FALSE]
    ## if there are multiples, clean them up
    emails <- unlist(strsplit(rawEmail, "> ?"))
    ## remove newlines and reattach ">'s", then return character() 
    emails <- paste(sub("\n"," ", emails),">",sep="")
    ## remove commas with whitespace after, and also lonely commas,
    emails <- sub("^\\s+?","",sub(",","",emails),perl=TRUE)
    ## and make sure we have only one whitespace followed by < before email 
    sub("<"," <",sub("\\s+?<","<",emails))
}

## This just extracts email adresses from a Maintainer field.
.scrubOutEmailAddresses <-function(rawEmail){
    as.character(sub(">$","",sub("^.*<","", rawEmail)))
}

## This just extracts names from a Maintainer field
.scrubOutNamesFromEmails <-function(rawEmail){
    names <- sub("\\s.?<.*$","", rawEmail, perl=TRUE)
    sub(",","",sub(", ","", names))
}

## Email wrapper so that I don't have to do this more than once
## NOTE: for sendmailR (or even command line mail) to run, you must
## have set /etc/mailname.  Mine was set to: gamay.fhcrc.org
.sendEmailMessage <- function(email, msg, subject){
    require("sendmailR")
    fromEmail = getOption("fromEmail")
    sendmail(from=fromEmail, to=email,
             subject=subject, msg=msg)
}

## And for when we want to send multiple messages:
.sendEmailMessages <- function(emails, msgs, subject){
    for(i in seq_along(emails)){
        .sendEmailMessage(emails[i], msgs[i], subject)
    }
}

## and code to define a single message template
.makeExistingUserMsg <- function(authorName, packageName){
    existingMsg <- paste(
                         "Hi ",
                         authorName,
                     ",
Your package has been added to the Bioconductor repository. 
Some helpful notes to help you as a package maintainer: 

a) svn information 
Your Subversion account information is the same as before, but
permissions have now been extended to your new package
You now have read/write access to the ",packageName," package:
URL: https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/",packageName,"
Please let me know if you have any question or problem with your svn
access.
svn docs at: http://bioconductor.org/developers/how-to/source-control/

b) Build report: 
Please  keep an eye on the build/check daily reports for the 
Bioconductor devel packages:  http://bioconductor.org/checkResults/
and get rid of any warnings and errors for your package. 

c) RSS Feeds:
You can find the rss feed for your software packages at:
http://bioconductor.org/rss/build/packages/",packageName,".rss

d) Stay connected using Bioc-devel mailing list and support site
http://bioconductor.org/help/support/#bioc-devel

e) Bioconductor Git mirrors
if you prefer to use git and Github instead of Subversion, you can
use the Bioconductor Git mirrors which are documented at:
http://bioconductor.org/developers/how-to/git-mirror/

f) add maintainer or removal of package from Bioconductor:
Please email us at packages NEAR bioconductor POINT org with your requests
for new mainatiners, please clearly state their name and email address
and mark them a cc on the request email sento us. 

g) Permanent URL of your package.
Your package has a permanent URL:

http://bioconductor.org/packages/",packageName,"/

This will redirect to the release landing page of your package
(and until it's released, the devel landing page). Therefore this
is the URL that should be used (in publications, etc.) to refer
to your package. For convenience, you can also refer specifically to 
the devel version, the release version, or a specific numbered
version of Bioconductor:

http://bioconductor.org/packages/devel/",packageName,"/
http://bioconductor.org/packages/release/",packageName,"/
http://bioconductor.org/packages/",BiocInstaller:::BIOC_VERSION,"/",packageName,"/


Thanks for contributing to the Bioconductor project!

 Sonali", sep="")
  
    ## then return
    existingMsg 
}

## General purpose multiplier for functions that take authorName, packageName and that also have a function to define the message based on that.
.makeMessages <- function(authorNames, packageName, FUN){
    msgs <- character()
    for(i in seq_along(authorNames)){
        msgs[i] <- FUN(authorName=authorNames[i], packageName)
    }
    msgs
}

emailExistingUser <- function(tarball, sendMail=FALSE){
    ## untar
    untar(tarball)
    ## get dir
    dir <- .getShortPkgName(tarball)
    ## extract email from DESCRIPTION file
    emails <- .extractEmails(dir)
    ## clean the email out
    cleanEmails <- .scrubOutEmailAddresses(emails)    
    ## extract name
    names <- .scrubOutNamesFromEmails(emails)
    ## format msgs
    msgs <- .makeMessages(authorName=names, packageName=dir,
                          FUN=.makeExistingUserMsg)
    ## subject
    subject <- paste("Congratulations.  Package",dir,
                     "has been added to the repository.")
    ## either send emails OR write out messages
    if(sendMail){
        ## send an email at this time.
        .sendEmailMessages(email=cleanEmails, msg=msgs, subject=subject)
    }else{
        paths <- paste(dir,"_cngrtsEml_existing_<",cleanEmails,">_.txt",sep="")
        ## now make connections and write results out.
        .writeOutEmailTemplates(paths, msgs)
    }
    ## cleanup
    unlink(dir, recursive=TRUE)
}


##############################################
##  example
##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## emailExistingUser(tarball)








##########################################################################
##########################################################################
## email for NEW users.  This one will also create an email from the
## tarball, but this time we can't email them since we have to still
## put the email credentials in...


## 1st we need our new user greeting:
.makeNewUserMsg <- function(authorName, packageName){
    newUserMsg <- 

paste("Hi ", authorName, "," ,                   
"


Congrats on your package being accepted to Bioconductor.  The following 
information is to help you in your new role as a package maintainer.

Every package in Bioconductor gets its own landing page. Contents from your 
DESCRIPTION file are pulled out to populate this page. Your package's 
permanent URL is:  http://bioconductor.org/packages/<pkgname>/

This will redirect to the release landing page of your package (and 
until it's released, the devel landing page). Therefore this is the 
URL that should be used (in publications, etc.) to refer to your package. 
For convenience, you can also refer specifically to the devel version, 
the release version, or a specific numbered version of Bioconductor:

http://bioconductor.org/packages/devel/<packageName>/
http://bioconductor.org/packages/release/<packageName>/
http://bioconductor.org/packages/3.2/<packageName>/

Maintaining your package: 

1) Git- svn bridge 
If you prefer to use git and Github instead of Subversion, you can use the 
Bioconductor Git-svn bridge which is documented at: 
http://www.bioconductor.org/developers/how-to/git-svn/

2) Build report:
Please  keep an eye on the build/check daily reports for the Bioconductor 
devel packages:  http://bioconductor.org/checkResults/ and get rid of any 
warnings or errors from your packages build report. 

3) Rss feeds:
You can find the rss feed for software packages at: 
http://bioconductor.org/rss/build/packages/<pkgname>.rss

4) Bioc-devel mailing list 
Our primary channel is the Bioc-devel mailing list which we use for 
communication between developers and for important announcements like 
release schedules, build system news, etc.. which you have signed up for. 
Please check this email on a regular basis. 

Also, when your package will pass the CHECK test for the first time, we 
strongly encourage you to send a note to Bioc-devel to announce its 
public availability (with a short description) so other people  can 
start to test it.

5) Adding Maintainers for your package:
If for some reason, your email address changes, please update the 
maintainer field in your description file. We may need to reach you 
if there are issues building your package (this could happen as a 
result of changes to R or to packages you depend on). If we can't 
reach you, we may have to drop your package from Bioconductor.  

If you want to add a new mainatiner or transfer responsibility to a
someone else,  please email us at packages NEAR bioconductor POINT org
and clearly state the new maintainers name and email address. Also 
mark him a cc on this email 

6) Support Site:
Please also subscribe to the bioconductor support site so that you can 
answer questions from users of your package and respond to bug reports 
promptly. https://support.bioconductor.org/ 
We recommend that you 'follow' tags that match your own package (such 
as your package name) so that you will know be able to know when someone is 
asking a question about your package or that relates to your work.  You 
can edit your profile on the support site to be notified when certain tags 
are used to describe your package.

7) Removal of your package:
If you no longer want to maintain your package, please let us know and we 
will remove it from Bioconductor, or (with your permission) find a new 
maintainer for it.

8) Updating your package in Bioconductor: 
You will need to use subversion to update your package inside Bioconductor. 
Your subversion account credentials are . 



Subversion user ID:
Password:



These credentials give you read access to the whole Bioconductor 
repository and WRITE permissions only to your package.
To update your package you need to do the following steps:
a) install subversion(svn) on your machine, if you dont have that already
You can learn about svn from here: http://bioconductor.org/developers/how-to/source-control/
b) svn co --username your_name 
https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/pkgName
You will be prompted for a password  - This will create a folder for your package.
c) make the changes you need to make to your package.
d) bump the version from x.y.z to x.y.(z+1) in the DESCRIPTION file (DON'T FORGET TO
BUMP Z! or your changes will not be pushed to the public repository.)
e) R CMD build packageName
f) R cmd check pkganme_x.y.(z+1).tar.gz
g) Fix any warnings/ errors from step (e) and (f)
h) svn ci pkgname
Please let me know if you have any question or problem with your svn access.


9) Helpful things to know about Bioconductor: 

Bioconductor Newsletter: http://bioconductor.org/help/newsletters/
Upcoming Courses & Course Material: http://bioconductor.org/help/events/
YouTube channel : https://www.youtube.com/user/bioconductor/
Twitter: https://twitter.com/Bioconductor

Thanks for contributing to the Bioconductor project!

 Sonali", sep="")
    ## then return
    newUserMsg 
}

.writeOutEmailTemplates <- function(paths, msgs){
    for(i in seq_along(paths)){
        con <- file(paths[i])
        writeLines(text=msgs[i], con=con)
        close(con)
    }
}

emailNewUser <- function(tarball){
    ## untar
    untar(tarball)
    ## get dir
    dir <- .getShortPkgName(tarball)
    ## extract email from DESCRIPTION file
    emails <- .extractEmails(dir)
    ## clean the email out
    cleanEmails <- .scrubOutEmailAddresses(emails)
    ## extract name
    names <- .scrubOutNamesFromEmails(emails)
    ## format msg
    msgs <- .makeMessages(authorName=names, packageName=dir,
                          FUN=.makeNewUserMsg)
    ## write the result to a file for convenience.
    paths <- paste(dir,"_cngrtsEml_<",cleanEmails,">_.txt",sep="")
    ## now make connections and write results out.
    .writeOutEmailTemplates(paths, msgs)
    ## cleanup
    unlink(dir, recursive=TRUE)
}


##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## emailNewUser(tarball)

## works





##########################################################################
##########################################################################
## email for new svn accounts.  This one takes a tarball and sends an
## email to Carl at scicomp regarding new accounts.

.makeNewSvnUserRequestMsg <- function(emailsAndUserNames){
    msg <- paste("Hi Carl,

Could you please create a new svn account on hedgehog for

",emailsAndUserNames,"

Thanks!

    Marc", sep="")
    ## then return
    msg 
}

.generateProposedUsername <- function(names){
    res <- character()
    for(i in seq_along(names)){
        firstName <- unlist(strsplit(names[i]," "))[1]
        init <- tolower(substr(firstName,1,1))
        numNames <- length(unlist(strsplit(names[i]," ")))
        lastName <- tolower(unlist(strsplit(names[i]," "))[numNames])
        res[i] <- paste(init, lastName, sep=".") 
    }
    res
}

## TODO: maybe I should modify this to take a SERIES of tarballs...
## BUT 1st I need to refactor my functions that access svn logs.
requestNewSvnAccountFromScicomp <- function(tarball, sendMail=FALSE){
    ## untar
    untar(tarball)
    ## get dir
    dir <- .getShortPkgName(tarball)
    ## extract email from DESCRIPTION file
    emails <- .extractEmails(dir)
    ## clean the email out
    cleanEmails <- .scrubOutEmailAddresses(emails)    
    ## extract name
    names <- .scrubOutNamesFromEmails(emails)
    ## make a proposed username.
    usernames <- .generateProposedUsername(names)

    ## generate emails and UserNames
    emailsAndUserNames <- paste(
                                paste(emails,
                                      "\n\n  proposed username:",
                                      usernames,
                                      "\n"),
                                collapse="\n\n AND \n\n")
    
    ## format msgs
    msg <- .makeNewSvnUserRequestMsg(emailsAndUserNames)
    if(sendMail){
        ## send an email at this time.
        ## .sendEmailMessage(email="scicomp@fhcrc.org", msg=msg,
        ##                   subject="new svn account")
        email = getOption("fromEmail")
        .sendEmailMessage(email=email, msg=msg,
                          subject="new svn account")
    }else{
        con <- file(paste(dir,"_svnRequest_<scicomp@fhcrc.org>_.txt",sep=""))
        writeLines(text=msg, con=con)
        close(con)
    }
    ## cleanup
    unlink(dir, recursive=TRUE)
}



##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## requestNewSvnAccountFromSciComp(tarball)

## works


## requestNewSvnAccountFromSciComp(tarball, sendMail=FALSE)





##############################################################################
## I need a tool for getting latest svn perms 

## Problem: the above requires a passphrase to access the content.
## I am going to email scicomp to see if they can help me square that away.


## this (old) extractor is for when you only want to know if someone has
## access to bioconductor or not. ## TODO; if we ever start to use
## this we will want to also load it to the zzz.R file etc.
.extractUsernamesFromAuthz <- function(){
    if(.Platform$OS.type != "unix"){
        stop("Sorry this function is only available from Unix")}    
    ## Just get the latest file  (this will require you
    ## to enter your passphrase
    permFile = getOption("permFile")
    cmd <- paste0('rsync ',permFile,' .')
    system(cmd)
    
    if(file.exists('bioconductor.authz')){
        con <- file('bioconductor.authz')
        res <- readLines(con)
        close(con)
        cats <- c("^bioconductor-readers =","^bioconductor-write0 =")
        res <- res[ grepl(cats[1], res) | grepl(cats[2], res)  ]
        res <- unlist(strsplit(res, ","))
        res <- unique(sub(" ","",sub(cats[2],"",sub(cats[1],"",res))))
    }
    unlink("bioconductor.authz")
    res
}

## This extractor is final word for knowing if an svn account exists at all...
## This is generally the conservative choice for most testing.
.extractUsernamesFromUsers <- function(){
    if(.Platform$OS.type != "unix"){
        stop("Sorry this function is only available from Unix")}    
    ## Just get the latest file  (this will require you
    ## to enter your passphrase
    ## usersFile = getOption("usersFile")
    ## cmd <- paste0('rsync ',usersFile,' .')
    ## system(cmd)
    tempDir <- get('tempDir', BiocContributions:::stash)
    usersFile <- file.path(tempDir, 'users') 
    
    if(file.exists(usersFile)){
        con <- file(usersFile)
        res <- readLines(con)
        close(con)
        res <- strsplit(res, ":")
        res <- unique(unlist(lapply(res, function(x){x[1]})))
    }
    ## unlink("users")
    res
}

## TODO/Bug fix: change the arrangement so that the file above is
## extracted ONCE per call of the highest level function (and then the
## file handle is passed down).  This will get rid of the bug where we
## have to type in the passphrase every time that we have a new user
## name...  Once call per functions should really be more than enough.  In fact,
## better would be to call it only once when we first load the package!

##
## TODO: make helper for extracting data from getOption("userDbFile")
## This will allow checking to see if the email in the package is the
## same as the one we have on record.
##


####################################################################
## Check if a username exists in svn
## I need this to be a public and private way of looking at whether an
## svn user exists for Bioconductor.
## So all the above emails should use this check 1) make sure that a user exists


## These return TRUE or FALSE
.svnUserExists <- function(name){
    names <- .extractUsernamesFromUsers()
    ## now grep
    any(grepl(name, names))
}

.svnUsersExist <- function(names){
    unlist(lapply(names, .svnUserExists))
}


## these returns matches (so you can think about it better)
.svnUserMatcher <- function(name){
    names <- .extractUsernamesFromUsers()
    ## now grep
    names[grepl(name, names)]
}

svnUserMatches <- function(names){
    unlist(lapply(names, .svnUserMatcher))
}




## Check if a tarball is in svn yet or not.
## (for quickly assessing - a standalone function)
.existingSvnUsers <- function(tarball){
    ## untar
    untar(tarball)
    ## get dir
    dir <- .getShortPkgName(tarball)
    ## extract email from DESCRIPTION file
    emails <- .extractEmails(dir)
    finalEmail <- paste(emails, collapse=", ")
    ## extract names
    names <- .scrubOutNamesFromEmails(emails)
    ## make a proposed username.
    usernames <- .generateProposedUsername(names)
    finalUserNames <- paste(usernames, collapse=", ")
    ## get the answer
    res <- svnUserMatches(usernames)
    finalMatches <-  paste(res, collapse=", ")
    ## cleanup
    unlink(dir, recursive=TRUE)
    if(length(res) == 0){
        message("For package: ",tarball, " :\n",
                "No matching users found...  Please consider: \n\n",
                finalEmail,"\n\n",
                "proposed usernames: ", finalUserNames, "\n\n")
    }else{
        message("For package: ",tarball, " :\n",
                "FOUND THE FOLLOWING MATCHES: ", finalMatches,"\n",
                "MATCHES HAVE THESE EMAILS: ", finalEmail, "\n\n")
    }
}

existingSvnUsers <- function(tarballsPath=".", suffix=".tar.gz$"){
    tarballs <- .getTars(path=tarballsPath, suffix=suffix)
    message("\n")
    res <- lapply(tarballs, .existingSvnUsers)
    ifelse(length(res)>0, TRUE, FALSE)
}


##############################################
##  example
##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## existingSvnUsers()




## TODO: make use of the above helpers in the other email functions (but ONLY after we get better access to the .authz file)






##############################################################################
## helper for generating the stuff we put into the permissions file
## 1st: lets do the really annoying part at the end.
## then do the middle part, but don't worry about the 1st part.

## things to bear in mind:
## This will tell you what version you are using
## biocVersion()  ## BiocInstaller:::BIOC_VERSION
## You need to use this string to format the tedious part later

## You need to also make sure we are using devel in order to even try
## to use this function.  (non-devel is not permitted)
## this will tell if you are using devel or not
## isDevel <- function(){packageVersion("BiocInstaller")$minor %% 2 == 1}

## Helper to retrieve userName and packageName
.getPkgNameAndUser <- function(tarball){
    ## untar
    untar(tarball)
    ## get pkgName
    pkgName <- .getShortPkgName(tarball)
    ## extract email from DESCRIPTION file
    emails <- .extractEmails(pkgName)
    finalEmail <- paste(emails, collapse=", ")
    ## extract names
    names <- .scrubOutNamesFromEmails(emails)
    ## make a proposed username.
    usernames <- .generateProposedUsername(names)
    finalUserNames <- paste(usernames, collapse=", ")
    ## get the answer
    res <- svnUserMatches(usernames)
    finalUserNames <-  paste(res, collapse=", ")
    ## Combine and return
    names(finalUserNames) <- pkgName
    finalUserNames
}

## helper for ONLY getting tarballs (used instead of dir())
.getTars <- function(path=".",suffix=".tar.gz$"){
    if(grepl(suffix,path)){
        stop("You need to supply a path that contains tarballs: not an actual tarball...")
    }
    res <- dir(path)
    res[grepl(suffix,res)]
}

.printAssociations <- function(elem){
    paste0(names(elem), " = " , elem, "\n")
}

.printTediousStuff <- function(elem){
    pkg <- names(elem)
#    version <- biocVersion() ## re-enable this in fall 
    version <- "3.1" ## Till just before release (b/c we want 'version before')
    part1 <- strsplit(as.character(version),split='[.]')[[1]][1]
    part2 <- strsplit(as.character(version),split='[.]')[[1]][2]
#    part2 <- as.character(as.integer(part2) - 1) ## no longer needed?
    version <- paste0(part1,"_",part2) 
    paste0("[/trunk/madman/Rpacks/",pkg,"]\n@",pkg,
           " = rw\n\n",
           "[/branches/RELEASE_",version,"/madman/Rpacks/",pkg,"]\n@",pkg,
           " = rw\n\n")
}

## helper to test if we are in devel
.isDevel <- function(){packageVersion("BiocInstaller")$minor %% 2 == 1}

## tarballs is a character vector of tarball paths.
generatePermissionEdits <- function(tarballsPath=".", suffix=".tar.gz$"){
    if(!.isDevel()){
        stop("TROUBLE!  You should only run this function if you are using an approved devel version of R.")
    }
    ## start with tarballs in whatever dir we have here...
    tarballs <- .getTars(path=tarballsPath, suffix=suffix)
    ## store the above in a list object
    data <- lapply(tarballs, .getPkgNameAndUser)
    
    ### For all packages in that list:

    ## write out association part (for each - helper2)
    message(paste(sapply(data, .printAssociations), collapse=""))
    
    ## write out the tedious part (for each - helper3)
    message(paste(sapply(data, .printTediousStuff), collapse=""))
    
}











## Output should look like:
## , y.shen, t.carroll, w.yang, f.zhang, j.schumann, a.waardenberg

## ASSIGN = y.shen
## ChIPQC = t.carrol, r.stark
## ABSSeq = w.yang
## FRGEpistasis = f.zhang
## flowCyBar = j.schumann
## CompGO = a.waardenberg
## Rariant = j.gehring

## [/trunk/madman/Rpacks/ASSIGN]
## @ASSIGN = rw

## [/branches/RELEASE_2_13/madman/Rpacks/ASSIGN]
## @ASSIGN = rw

## [/trunk/madman/Rpacks/ChIPQC]
## @ChIPQC = rw

## [/branches/RELEASE_2_13/madman/Rpacks/ChIPQC]
## @ChIPQC = rw

## [/trunk/madman/Rpacks/ABSSeq]
## @ABSSeq = rw

## [/branches/RELEASE_2_13/madman/Rpacks/ABSSeq]
## @ABSSeq = rw

## [/trunk/madman/Rpacks/FRGEpistasis]
## @FRGEpistasis = rw

## [/branches/RELEASE_2_13/madman/Rpacks/FRGEpistasis]
## @FRGEpistasis = rw

## [/trunk/madman/Rpacks/flowCyBar]
## @flowCyBar = rw

## [/branches/RELEASE_2_13/madman/Rpacks/flowCyBar]
## @flowCyBar = rw

## [/trunk/madman/Rpacks/CompGO]
## @CompGO = rw

## [/branches/RELEASE_2_13/madman/Rpacks/CompGO]
## @CompGO = rw

## [/trunk/madman/Rpacks/Rariant]
## @Rariant = rw

## [/branches/RELEASE_2_13/madman/Rpacks/Rariant]
## @Rariant = rw
