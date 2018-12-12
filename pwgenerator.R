genPass <- function(nChar = 20, nSpchar = 2, ncLet = 5, nLet = 10, nNum = 5, style = 'rando', nWord = 4, replace = T) {
  specChar <- c('!','?',':','-','_','@','#','$','%','&','*','+','/','^','(',')','[',']','{','}','~',"'",',','.')
  if(style == 'rando') {
    if(nChar > 6) {
    repeat {pw1 <- sample(c(specChar, 0:9, LETTERS, letters), nChar) #works, magically.
    if(!all(!all(is.na(pmatch(pw1, specChar))), 
            !all(is.na(pmatch(pw1, 0:9))), 
            !all(is.na(pmatch(pw1, LETTERS))),
            !all(is.na(pmatch(pw1, letters)))) == F) break
    }
    pw2 <- toString(pw1)
    pw <- gsub(',', '', gsub("\\s", "",pw2))
    return(pw)} else if(nChar > 3) {
      repeat {pw1 <- sample(c(specChar, 0:9, LETTERS, letters), nChar) #works, magically.
    if(!all(!all(is.na(pmatch(pw1, specChar))), 
            !all(is.na(pmatch(pw1, 0:9))), 
            !all(is.na(pmatch(pw1, LETTERS))),
            !all(is.na(pmatch(pw1, letters)))) == F) break
    }
      pw2 <- toString(pw1)
      pw <- gsub(',', '', gsub("\\s", "",pw2))
      return(c('Warning! A secure password requires at least 8 characters. 20 is recommended.', pw))} else if(nChar < 4) {return('Error! At least 4 characters are required to work.')}
  } else
  if(style == 'words') {
    if(Sys.info()['sysname'] == 'Darwin')
    {dic <- read.table("/usr/share/dict/words")} else
      if(Sys.info()['sysname'] == 'Windows') {dic <- read.table('%AppData%/Microsoft/Spelling/default.acl')} else 
        if('try-error' %in% calss(read.table(file = 'https://users.cs.duke.edu/~ola/ap/linuxwords'))) {return('Cannot find local words list. Please connect to the internet.')}
    else {dic <- read.table(file = 'https://users.cs.duke.edu/~ola/ap/linuxwords')}
    wrd <- sample(levels(dic$V1), nWord)
    if (nWord > 1) {
      repeat {c1 <- sample(c(specChar, 0:9, LETTERS), 3)
      if(!all(!all(is.na(pmatch(c1, specChar))), 
            !all(is.na(pmatch(c1, 0:9))), 
            !all(is.na(pmatch(c1, LETTERS)))) == F) break
    }
    } else {return('Error: must contain at least 2 words!')}
    wpw1 <- sample(c(c1, wrd))
    wpw2 <- toString(wpw1)
    wpw <- gsub(',', '', gsub("\\s", "",wpw2))
    if(nchar(wpw) > 127) {return(c(substr(wpw, 1, 127), 'Notice: normal character string length reached. Password has been truncated to 127 characters.'))} else return(wpw)
  } else {return("Error: style must be 'rando' or 'words'")}
  
}

genPass(style = 'words', nWord = 2)
genPass(nChar = 4)
