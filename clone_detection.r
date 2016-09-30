
JS_KEYWORDS = c("abstract","arguments","boolean","break", "byte", "case", "catch", "char", "class", "const", "continue", "debugger", "default", "delete", "do", "double", "else", "enum", "evail", "export", "extends", "false", "final", "finally", "float", "for", "function", "goto", "if","implements", "import","in", "instanceof", "int", "interface", "let", "long", "native", "new", "null", "package", "private", "protected", "public", "return", "short", "static", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "true", "try", "typeof", "var", "void", "volatile", "while", "with", "yield")

CHARACTER_START = c("'", "\"", "`")

TOKEN_START = c("_", LETTERS, letters, "$")

NUMBER_START = c(".", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")

read.fullStats <- function(filename) {
    read.table(filename, header = F, sep = ",", col.names = c(
        "fid", # TODO this has changed in the new format
        "pid",
        "projectPath",
        "filePath",
        "bytes",
        "commentBytes",
        "whitespaceBytes",
        "tokenBytes",
        "separatorBytes",
        "loc",
        "commentLoc",
        "emptyLoc",
        "totalTokens",
        "uniqueTokens",
        "errors",
        "fileHash",
        "tokensHash"
    ))
}


isCharacter <- function(what) {
    x <- substring(what, 1, 1)
    x %in% CHARACTER_START
}

isNumber <- function(what) {
    x <- substring(what, 1, 1)
    x %in% NUMBER_START
}

isRegExpr <- function(what) {
    x <- substring(what, 1, 1)
    x == "/"
}

isKeyword <- function(what) {
    what %in% JS_KEYWORDS
}

isIdent <- function(what) {
    x <- substring(what, 1, 1)
    !isKeyword(what) && x %in% TOKEN_START
}

tokenType <- function(what) {
    if (isCharacter(what))
        "character"
    else if (isNumber(what))
        "number"
    else if (isRegExpr(what))
        "regexpr"
    else if (isKeyword(what))
        "keyword"
    else if (isIdent(what))
        "ident"
    else
        "???"
}

# crude function that unescapes paths and tokens (i.e. converts %XX to the respective characters)
unescape <- function(what, maxLength = 0) {
    what = strsplit(what, "")[[1]]
    e = length(what)
    if (maxLength == 0)
        maxLength = e
    i = 1
    result = ""
    while (i <= e) {
        if (what[[i]] == "%") {
            result <- paste(result, intToUtf8(strtoi(paste("0x", what[[i+1]], what[[i+2]], sep = ""))), sep = "")
            i = i + 3
        } else {
            result <- paste(result, what[[i]], sep = "")
            i = i + 1
        }
        if (nchar(result) >= maxLength)
            break
    }
    result
}

# unescapes first 100 characters of given string
unescape50 <- function(what) {
    unescape(what, 50)
}

read.tokens <- function(filename, compress = T) {
    t = read.table(filename, header = F, sep = ",", col.names = c("id", "count", "text"))
    #t$text = sapply(t$text, unescape)
    t$text = as.character(t$text)
    t$textSize = nchar(t$text)
    if (compress)
        t$text = substr(t$text, 1, 52)
    t
}
