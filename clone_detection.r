library(RMySQL)

OVERWRITE = F

DB_CONNECTION_ = NULL

# time of the last command
LAST_SQL_TIME_ = NULL

println <- function(...) {
    cat(paste(..., "\n", sep = ""))
}



sql.connect <- function(username, password, dbname, host = "localhost") {
    # disconnect first, if we have existing connection
    sql.disconnect()
    tryCatch({
        # now connect to the database
        DB_CONNECTION_ <<- dbConnect(MySQL(), user = username, password = password, host = host, dbname = dbname)
    }, error = function(e) {
        # if the error is the databse does not exist, create it
        if (length(grep("Failed to connect to database: Error: Unknown database", e$message)) > 0) {
            DB_CONNECTION_ <<- dbConnect(MySQL(), user = username, password = password, host = host)
            sql.query("CREATE DATABASE ", dbname)
            println("Creating database ", dbname)
            sql.disconnect()
            sql.connect(username, password, dbname, host)
        } else {
            stop(e)
        }
    })
}

sql.disconnect <- function() {
    if (! is.null(DB_CONNECTION_)) {
        dbDisconnect(DB_CONNECTION_)
        DB_CONNECTION_ <<- NULL
    }
}


sql.loadTokenizerData <- function(outputRoot, overwrite = OVERWRITE) {
    # now create the tables, if they do not exist
    sql.createTokenizerTables()
    # now load the data, check the timestamps where necessary
    sql.load.table("bookkeeping_proj", paste(outputRoot, "bookkeeping_projs/bookkeeping-proj-0.txt", sep = "/"), overwrite)
    sql.load.table("tokenizer_clones", paste(outputRoot, "clones/clones-0.txt", sep = "/"), overwrite)
    sql.load.table("files_stats", paste(outputRoot, "files_stats/files-stats-0.txt", sep = "/"), overwrite)
    sql.load.table("files_full_stats", paste(outputRoot, "files_full_stats/stats-full-0.txt", sep = "/"), overwrite)
    sql.load.table("tokens", paste(outputRoot, "tokens.txt", sep = "/"), overwrite)
    invisible(NULL)
}


sql.table.status <- function(table) {
    x <- sql.query("SHOW TABLE STATUS WHERE NAME='",table,"'")
    list(name = x$Name, length = x$Rows, bytes = x$Data_length)
}

sql.query <- function(...) {
    result <- 0
    f <- function() {
        res <- dbSendQuery(DB_CONNECTION_, paste(..., sep = ""))         
        result <<- dbFetch(res, n = -1)
        dbClearResult(res)
    }
    LAST_SQL_TIME_ <<- system.time({
        f()
    })
    result
}

sql.last.time <- function() {
    if (is.null(LAST_SQL_TIME_))
        0
    else
        LAST_SQL_TIME_[["elapsed"]]
}

# Creates clone groups. The clone groups table 
calculateCloneGroups <- function(table.from, table.clone_groups, table.clone_info, overwrite = OVERWRITE) {
    ts = sql.timestamp(table.from)
    if (sql.timestamp.shouldOverwrite(ts, table.clone_groups, overwrite) || sql.timestamp.shouldOverwrite(ts, table.clone_info, overwrite)) {
        println("Calculating clone groups from clone pairs in ", table.from)
        sql.query("DELETE FROM ", table.clone_groups)
        sql.query("DELETE FROM ", table.clone_info)
        t = system.time({
            sql.query("DROP TABLE IF EXISTS tmp_clonepairs")
            println("  cloning clone pairs info")
            sql.query("CREATE TABLE tmp_clonepairs (SELECT * FROM ", table.from, ")")
            i = 1
            println("  building clone group membership, unaccounted ",sql.table.status("tmp_clonepairs")$length, ", iteration ", i)
            # first add all files that are non-recursively defined, i.e. where the second file is first file in any other comparison (first files are clone groups)
            sql.query("INSERT INTO ", table.clone_info, " SELECT fileId2, fileId1, projectId2 FROM tmp_clonepairs WHERE fileId2 NOT IN (SELECT fileId1 FROM tmp_clonepairs)")
            # for each clone group, the first file (the one that is the group id as well) must also belong to the group
            sql.query("INSERT IGNORE INTO ", table.clone_info, " SELECT fileId1, fileId1, projectId1 FROM tmp_clonepairs WHERE fileId1 NOT IN (SELECT fileId2 FROM tmp_clonepairs)")
            # now repeatedly add further clones (i.e. those who are reported as clones of someone already having a clonegroup)
            repeat {
                ++i
                # remove all files already accounted for 
                # TODO this can be faster and use deltas
                sql.query("DELETE FROM tmp_clonepairs WHERE fileId2 IN (SELECT fileId FROM ",table.clone_info,")") 
                # if there are no more clone pairs to process, we are done
                l <- sql.table.status("tmp_clonepairs")$length
                if (l == 0)
                    break
                println("  building clone group membership, unaccounted ",sql.table.status("tmp_clonepairs")$length, ", iteration ", i)
                # now get all unmatched, where the first file has already been accounted for and add the second into the same group
                sql.query("INSERT INTO ", table.clone_info," SELECT fileId2, groupId, projectId2 FROM tmp_clonepairs JOIN ", table.clone_info," ON fileId1 = fileId")
                # we will repeat this until we have no files left
            }
            # cleanup the temp table
            sql.query("DROP TABLE tmp_clonepairs")
            # build the clone groups summaries
            println("  building clone groups info")
            sql.query("INSERT INTO ", table.clone_groups," SELECT groupId, COUNT(fileId), COUNT(DISTINCT projectId) FROM ", table.clone_info ," GROUP BY groupId")
        })
        println("  done in ", t[[3]], " [s]")
        sql.setTimestamp(table.clone_groups, ts)
        sql.setTimestamp(table.clone_info, ts)
        
    } else {
        println("Skipping clone groups preprocessing into ", table.from," - already created, timestamp: ", timestamp.to.date(ts))
    }
    x <- sql.table.status(table.clone_info)
    println("  ", x$length, " entries [", x$bytes, " bytes]")
    x <- sql.table.status(table.clone_groups)
    println("  ", x$length, " clone groups [", x$bytes, " bytes]")
}

# preprocessing - calculates non-empty files
calculateNonEmptyFiles <- function(overwrite = OVERWRITE) {
    ts = sql.timestamp("files_full_stats")
    if (sql.timestamp.shouldOverwrite(ts, "non_empty_files", overwrite)) {
        println("Calculating non-empty files")
        sql.query("DELETE FROM non_empty_files")        
        sql.query("INSERT INTO non_empty_files SELECT id FROM files_full_stats WHERE bytes > 0")
        println("  done in ", sql.last.time(), "[s]")
        sql.setTimestamp("non_empty_files", ts)
    } else {
        println("Skipping preprocessing non-empty files - already done, timestamp: ", timestamp.to.date(ts))
    }
    x <- sql.table.status("non_empty_files")
    println("  ", x$length, " entries [", x$bytes, " bytes]")
}

# calculates project summaries 
calculateProjectStats <- function(overwrite = OVERWRITE) {
    ts = sql.timestamp("files_full_stats")
    if (sql.timestamp.shouldOverwrite(ts, "project_stats", overwrite)) {
        println("Calculating project statistics")
        sql.query("DELETE FROM project_stats")
        sql.query("INSERT INTO project_stats SELECT projectId, COUNT(*), SUM(bytes), SUM(commentBytes), SUM(whitespaceBytes), SUM(tokenBytes), SUM(separatorBytes), SUM(loc), SUM(commentLoc), SUM(emptyLoc), SUM(errors) FROM files_full_stats GROUP BY projectId")
        println("  done in ", sql.last.time(), "[s]")
        sql.setTimestamp("project_stats", ts)
    } else {
        println("Skipping calculating project statistics - already done, timestamp: ", timestamp.to.date(ts))
    }
    x <- sql.table.status("project_stats")
    println("  ", x$length, " entries [", x$bytes, " bytes]")
    
    ts = sql.timestamp("files_full_stats")
    if (sql.timestamp.shouldOverwrite(ts, "tokenizer_project_stats_original", overwrite)) {
        println("Calculating project statistics from only original files")
        sql.query("DELETE FROM tokenizer_project_stats_original")
        sql.query("INSERT INTO tokenizer_project_stats_original SELECT projectId, COUNT(*), SUM(bytes), SUM(commentBytes), SUM(whitespaceBytes), SUM(tokenBytes), SUM(separatorBytes), SUM(loc), SUM(commentLoc), SUM(emptyLoc), SUM(errors) FROM files_full_stats WHERE id NOT IN (SELECT fileId FROM tokenizer_clone_info) GROUP BY projectId")
        # if there are any projects that have *no* original files, we will insert them as well
        sql.query("INSERT INTO tokenizer_project_stats_original SELECT id, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 FROM bookkeeping_proj WHERE id NOT IN (SELECT projectId FROM tokenizer_project_stats_original)")
        
        
        println("  done in ", sql.last.time(), "[s]")
        sql.setTimestamp("tokenizer_project_stats_original", ts)
    } else {
        println("Skipping calculating project statistics from original files - already done, timestamp: ", timestamp.to.date(ts))
    }
    x <- sql.table.status("tokenizer_project_stats_original")
    println("  ", x$length, " entries [", x$bytes, " bytes]")
}

calculateTokenSizes <- function(overwrite = OVERWRITE) {
    ts = sql.timestamp("tokens")
    if (sql.timestamp.shouldOverwrite(ts, "tokens_size", overwrite)) {
        println("Calculating token sizes")
        sql.query("DELETE FROM tokens_size")
        sql.query("INSERT INTO tokens_size SELECT id, CHAR_LENGTH(text) FROM tokens")
        println("  done in ", sql.last.time(), "[s]")
        sql.setTimestamp("tokens_size", ts)
    } else {
        println("Skipping calculating token sizes - already done, timestamp: ", timestamp.to.date(ts))
    }
    x <- sql.table.status("tokens_size")
    println("  ", x$length, " entries [", x$bytes, " bytes]")
}


timestamp.to.date <- function(timestamp) {
    as.POSIXct(timestamp, origin="1970-01-01")    
}

sql.setTimestamp <- function(what, timestamp = as.integer(Sys.time())) {
    sql.query("DELETE FROM timestamp WHERE what='",what,"'")
    sql.query("INSERT INTO timestamp VALUES ('",what,"', ", timestamp, ")")
}

sql.timestamp <- function(what) {
    x <- sql.query("SELECT timestamp FROM timestamp WHERE what='", what, "'")$timestamp
    # if not found, assume UTC 0, otherwise return the integer
    if (length(x) == 0)
        0 
    else 
        x[[1]]
}

sql.timestamp.shouldOverwrite <- function(expected, tableName, overwrite) {
    ts = sql.timestamp(tableName)
    if (ts == expected) {
        # if the timestamp is what we expect, only overwrite if forced to
        return(overwrite == "force")
    } else if (ts == 0) {
        # if timestamp is 0, always overwrite, as it is the first time we are doing so
        return(T)
    } else {
        # different timestamps, error is overwrite not allowed
        if (overwrite == F)
            stop(paste("Table", tableName,"not in sync with its input. Either call with overwrite=T to overwrite the old data, or use a different database"))
        return(T)
    }
}

# fills the given table from specified file. 
sql.load.table <- function(tableName, fromFile, overwrite = OVERWRITE) {
    if (! file.exists(fromFile))
        stop(paste("Unable to load table", tableName,"from file", fromFile,"- file not found"))
    modified = as.integer(file.info(fromFile)$mtime)
    if (sql.timestamp.shouldOverwrite(modified, tableName, overwrite)) {
        sql.query("DELETE FROM ", tableName) # delete existing content
        println("Loading table ", tableName," from file ", fromFile)
        sql.query("LOAD DATA LOCAL INFILE '", fromFile, "' INTO TABLE ", tableName, " FIELDS TERMINATED BY ',' ENCLOSED BY '\"'")
        println("  done in ", sql.last.time(), " [s]")
        sql.setTimestamp(tableName, modified)
    } else {
        println("Skipping loading table ", tableName," - already loaded, timestamp: ", timestamp.to.date(modified))
    }
    x <- sql.table.status(tableName)
    println("  ", x$length, " entries [", x$bytes, " bytes]")
}


# Creates the tables for the tokenizer and timestamps the `created` entry if the tables are created as new. 
sql.createTokenizerTables <- function() {
    sql.query("CREATE TABLE IF NOT EXISTS bookkeeping_proj(
              id INT NOT NULL,
              path VARCHAR(1000) NOT NULL,
              PRIMARY KEY(id))")
    println("bookkeeping_proj - project to path mapping")
    sql.query("CREATE TABLE IF NOT EXISTS tokenizer_clones (
              projectId1 INT NOT NULL,
              fileId1 INT NOT NULL,
              projectId2 INT NOT NULL,
              fileId2 INT NOT NULL,
              PRIMARY KEY(fileId1, fileId2))")
    println("tokenizer_clones - clone pairs as reported by the tokenizer")
    sql.query("CREATE TABLE IF NOT EXISTS files_stats(
              projectId INT NOT NULL,
              id INT NOT NULL,
              filePath VARCHAR(1000) NOT NULL,
              url VARCHAR(1000) NOT NULL,
              fileHash VARCHAR(32) NOT NULL,
              bytes INT NOT NULL,
              f_lines INT NOT NULL,
              loc INT NOT NULL,
              sloc INT NOT NULL,
              PRIMARY KEY(id))")
    println("files_stats - file statistics in sourcererCC's format")
    sql.query("CREATE TABLE IF NOT EXISTS files_full_stats(
              id INT NOT NULL,
              projectId INT NOT NULL,
              projectPath VARCHAR(1000) NOT NULL,
              relPath VARCHAR(1000) NOT NULL,
              bytes INT NOT NULL,
              commentBytes INT NOT NULL,
              whitespaceBytes INT NOT NULL,
              tokenBytes INT NOT NULL,
              separatorBytes INT NOT NULL,
              loc INT NOT NULL,
              commentLoc INT NOT NULL,
              emptyLoc INT NOT NULL,
              totalTokens INT NOT NULL,
              uniqueTokens INT NOT NULL,
              errors INT NOT NULL,
              fileHash VARCHAR(32) NOT NULL,
              tokensHash VARCHAR(32) NOT NULL,
              PRIMARY KEY(id));")
    println("files_full_stats - file statistics in our format (extra info our tokenizer generates)")
    sql.query("CREATE TABLE IF NOT EXISTS tokens(
              id INT NOT NULL,
              count INT NOT NULL,
              text LONGTEXT NOT NULL,
              PRIMARY KEY(id));")
    println("tokens - tokens and their frequencies as reported by the tokenizer for all files")
    sql.query("CREATE TABLE IF NOT EXISTS timestamp (
              what VARCHAR(100) NOT NULL,
              timestamp INT NOT NULL,
              PRIMARY KEY (what))")
    println("timestamp - bookkeeping information about the data used")
    # timestamp as created if this is the timestamp table is empty
    if (sql.table.status("timestamp")$length == 0) {
        sql.setTimestamp("created")
        println("New database created, timestamping to ", Sys.time())
    } else {
        println("Database already created on ", timestamp.to.date(sql.timestamp("created")))
    }
}

# creates tables for preprocessed data
sql.createTokenizerPreprocessedTables <- function() {
    sql.query("CREATE TABLE IF NOT EXISTS tokenizer_clone_groups (
        groupId INT NOT NULL,
        files INT NOT NULL,
        projects INT NOT NULL,
        PRIMARY KEY(groupId))")
    println("tokenizer_clone_groups - clone groups for files reported by the tokenizer as identical")
    sql.query("CREATE TABLE IF NOT EXISTS tokenizer_clone_info (
        fileId INT NOT NULL,
        groupId INT NOT NULL,
        projectId INT NOT NULL,
        PRIMARY KEY(fileId))")
    println("tokenizer_clone_info - relation between cloned files and their clone groups as reported by the tokenizer (identical files)")
    sql.query("CREATE TABLE IF NOT EXISTS non_empty_files (
        id INT NOT NULL,
        PRIMARY KEY (id))")
    println("non_empty_files - non empty files as reported by the tokenizer")
    sql.query("CREATE TABLE IF NOT EXISTS project_stats (
        projectId INT NOT NULL,
        files INT NOT NULL,
        bytes BIGINT NOT NULL,
        commentBytes BIGINT NOT NULL,
        whitespaceBytes BIGINT NOT NULL,
        tokenBytes BIGINT NOT NULL,
        separatorBytes INT NOT NULL,
        loc INT NOT NULL,
        commentLoc INT NOT NULL,
        emptyLoc INT NOT NULL,
        errors INT NOT NULL,
        PRIMARY KEY(projectId))")
    println("project_stats - cummulative information about projects")
    sql.query("CREATE TABLE IF NOT EXISTS tokenizer_project_stats_original (
        projectId INT NOT NULL,
        files INT NOT NULL,
        bytes BIGINT NOT NULL,
        commentBytes BIGINT NOT NULL,
        whitespaceBytes BIGINT NOT NULL,
        tokenBytes BIGINT NOT NULL,
        separatorBytes BIGINT NOT NULL,
        loc INT NOT NULL,
        commentLoc INT NOT NULL,
        emptyLoc INT NOT NULL,
        errors INT NOT NULL,
        PRIMARY KEY(projectId))")
    println("tokenizer_project_stats_original - cummulative information about projects taken only from original files (as reported by the tokenizer)")
    sql.query("CREATE TABLE IF NOT EXISTS tokens_size (
              id INT NOT NULL,
              textSize INT NOT NULL,
              PRIMARY KEY(id))")
    println("tokens_size - sizes of tokens")
}

# data converting

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


# visualization helpers 

# a simple function to draw logarithmic histograms
logHist <- function (what, main, xlab, ylab, breaks = 50, base = 10) {
    x <- log(what, base = base)
    par(xaxt='n', mar = c(5,4,4,4) + 0.3)
    #plot.new()
    hist(x, breaks = breaks, main = main, xlab = xlab, ylab = ylab, freq = T)
    par(xaxt='s')
    xax <- axis(1, fg = "#ffffff", col.axis = "#ffffff")
    axis(1, xax, 10 ^ xax)
    #d  <- density(x, adjust = 2)
    #par(new = T)
    #plot(d$x, d$y, col = "#ff0000", axes = F, xlab = "", ylab = "", type = "l")
    #axis(side = 4, at = pretty(range(d$y)), col = "#ff0000")
    #mtext("Density", side = 4, line = 3)
}

# a simple function to draw histograms
normalHist <- function (what, main, xlab, ylab, breaks = 50, base = 10) {
    x <- what
    par(mar = c(5,4,4,4) + 0.3)
    #plot.new()
    hist(x, breaks = breaks, main = main, xlab = xlab, ylab = ylab, freq = T)
    #d  <- density(x, adjust = 2)
    #par(new = T)
    #plot(d$x, d$y, col = "#ff0000", axes = F, xlab = "", ylab = "", type = "l")
    #axis(side = 4, at = pretty(range(d$y)), col = "#ff0000")
    #mtext("Density", side = 4, line = 3)
}

