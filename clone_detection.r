library(RMySQL)

# default value for overwrites (see the notebook for details)
OVERWRITE = F

# database connection - we keep it alive 
DB_CONNECTION_ = NULL

# time of the last command, used for reporting
LAST_SQL_TIME_ = NULL

# println + concatenation wrapper
println <- function(...) {
    cat(paste(..., "\n", sep = ""))
}


# SQL functions -------------------------------------------------------------------------------------------------------

# connects to the given db server and opens the database name, if the database name does not exist, creates it. Keeps the connection alive 
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

# disconnects from the database
sql.disconnect <- function() {
    if (! is.null(DB_CONNECTION_)) {
        dbDisconnect(DB_CONNECTION_)
        DB_CONNECTION_ <<- NULL
    }
}

# concatenates the arguments into one string and executes it as query, if updateTime is T, stores the time the query took on server
sql.query <- function(..., updateTime = T) {
    result <- 0
    f <- function() {
        res <- dbSendQuery(DB_CONNECTION_, paste(..., sep = ""))         
        result <<- dbFetch(res, n = -1)
        dbClearResult(res)
    }
    if (updateTime)
        LAST_SQL_TIME_ <<- system.time({
            f()
        })
    else
        f()
    result
}

# returns the time in seconds it took the last query to execute on the server
sql.lastTime <- function() {
    if (is.null(LAST_SQL_TIME_))
        0
    else
        LAST_SQL_TIME_[["elapsed"]]
}

# returns the number of rows affected by the last query
sql.affectedRows <- function() {
    sql.query("SELECT ROW_COUNT()", updateTime = F)
}

# returns the table status (the table name, length in rows and number of bytes it occupies). When precise is T, it uses the COUNT(*) which takes a lot of time, when F, uses the SHOW TABLE STATUS which is fast, but only approximate
sql.tableStatus <- function(table, precise = T) {
    x <- sql.query("SHOW TABLE STATUS WHERE NAME='",table,"'")
    if (precise)
        cnt <- sql.query("SELECT COUNT(*) FROM ", table)
    else
        cnt <- x$Rows
    list(name = x$Name, length = cnt, bytes = x$Data_length)
}

# creates (recreates) index on given table and column
sql.createIndex <- function(table, column, unique = T) {
    index = gsub(",", "", column)
    index = gsub(" ", "", index)
    index = paste("index_", index, sep="")
    x <- sql.query("SHOW INDEX FROM ", table, " WHERE KEY_NAME=\"", index, "\"")$Key_name
    if (length(x) > 0)
        sql.query("DROP INDEX ", index, " ON ", table)
    if (unique)
        sql.query("CREATE UNIQUE INDEX ", index, " ON ", table, "(", column, ")")
    else
        sql.query("CREATE INDEX ", index, " ON ", table, "(", column, ")")
    paste("created index ", column, " on table ", table, " in ", sql.lastTime(), "[s]", sep = "")
}

# sets given timestamp in the database
sql.setTimestamp <- function(what, timestamp = as.integer(Sys.time())) {
    sql.query("DELETE FROM timestamp WHERE what='",what,"'")
    sql.query("INSERT INTO timestamp VALUES ('",what,"', ", timestamp, ")")
}

# returns given timestamp from the database, or 0 if not found
sql.timestamp <- function(what) {
    x <- sql.query("SELECT timestamp FROM timestamp WHERE what='", what, "'")$timestamp
    # if not found, assume UTC 0, otherwise return the integer
    if (length(x) == 0)
        0 
    else 
        x[[1]]
}

# depending on the expected timestamp and overwrite flag either does nothing if the timestamp exists in the database, or raises an error if overwrite is F and different timestamp exists, or always overwrites when overwrite is "force". Returns T if the operation should be overwriten, F if not. 
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

# fills the given table from specified file. Returns TRUE if the table has been created, FALSE if not. The table to which the data are to be loaded must be already created. 
sql.load.table <- function(tableName, fromFile, overwrite = OVERWRITE) {
    if (! file.exists(fromFile))
        stop(paste("Unable to load table", tableName,"from file", fromFile,"- file not found"))
    modified = as.integer(file.info(fromFile)$mtime)
    if (sql.timestamp.shouldOverwrite(modified, tableName, overwrite)) {
        sql.query("DELETE FROM ", tableName) # delete existing content
        println("Loading table ", tableName," from file ", fromFile)
        sql.query("LOAD DATA LOCAL INFILE '", fromFile, "' INTO TABLE ", tableName, " FIELDS TERMINATED BY ',' ENCLOSED BY '\"'")
        println("  done in ", sql.lastTime(), " [s]")
        sql.setTimestamp(tableName, modified)
        result = T
    } else {
        println("Skipping loading table ", tableName," - already loaded, timestamp: ", timestamp.to.date(modified))
        result = F
    }
    x <- sql.tableStatus(tableName, F)
    println("  ~", x$length, " entries [", x$bytes, " bytes]")
    result
}

# data processing functions -------------------------------------------------------------------------------------------

# Creates the tables for the tokenizer and timestamps the `created` entry if the tables are created as new. Also creates the timestamp table
createTokenizerTables <- function() {
    sql.query("CREATE TABLE IF NOT EXISTS bookkeeping_proj(
              id INT NOT NULL,
              path VARCHAR(1000) NOT NULL,
              url VARCHAR(1000) NOT NULL,
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
              url VARCHAR(1000) NOT NULL,
              created INT NOT NULL,
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
              size INT NOT NULL,
              text LONGTEXT NOT NULL,
              PRIMARY KEY(id));")
    println("tokens - tokens and their frequencies as reported by the tokenizer for all files")
    sql.query("CREATE TABLE IF NOT EXISTS timestamp (
              what VARCHAR(100) NOT NULL,
              timestamp INT NOT NULL,
              PRIMARY KEY (what))")
    println("timestamp - bookkeeping information about the data used")
    # timestamp as created if this is the timestamp table is empty
    if (sql.tableStatus("timestamp")$length == 0) {
        sql.setTimestamp("created")
        println("New database created, timestamping to ", Sys.time())
    } else {
        println("Database already created on ", timestamp.to.date(sql.timestamp("created")))
    }
}

# loads the raw data produced by the tokenizer (bookkeeping_proj, files_stats, files_full_stats, files_tokens, clones and tokens).
loadTokenizerData <- function(outputRoot, overwrite = OVERWRITE) {
    # now create the tables, if they do not exist
    createTokenizerTables()
    # now load the data, check the timestamps where necessary, create indexes on the primary keys
    if (sql.load.table("bookkeeping_proj", paste(outputRoot, "bookkeeping_projs/bookkeeping-proj-0.txt", sep = "/"), overwrite))
        println("  ", sql.createIndex("bookkeeping_proj", "id"))
    if (sql.load.table("tokenizer_clones", paste(outputRoot, "clones/clones-0.txt", sep = "/"), overwrite))
        println("  NO INDEX CREATED")
    if (sql.load.table("files_stats", paste(outputRoot, "files_stats/files-stats-0.txt", sep = "/"), overwrite))
        println("  ", sql.createIndex("files_stats", "id"))
    if (sql.load.table("files_full_stats", paste(outputRoot, "files_full_stats/stats-full-0.txt", sep = "/"), overwrite))
        println("  ", sql.createIndex("files_full_stats", "id"))
    if (sql.load.table("tokens", paste(outputRoot, "tokens.txt", sep = "/"), overwrite))
        println("  NO INDEX CREATED")
    invisible(NULL)
}

# Loads sourcererCC's clone pairs. All files in the folder are treated as clonepair definitions so that data from multiple nodes can be ignested by a single command. 
loadSourcererData <- function(outputRoot, overwrite = OVERWRITE) {
    println("sourcerer_clones - clone pairs as reported by the sourcerer")
    x <- list.files(outputRoot, full.names = T)
    println("  ", length(x), " files found")
    # find the newest file
    modified = max(as.integer(file.info(x)$mtime))
    if (sql.timestamp.shouldOverwrite(modified, "sourcerer_clones", overwrite)) {
        t = system.time({
            sql.query("DROP TABLE IF EXISTS sourcerer_clones")
            sql.query("CREATE TABLE sourcerer_clones (
                projectId1 INT NOT NULL,
                fileId1 INT NOT NULL,
                projectId2 INT NOT NULL,
                fileId2 INT NOT NULL,
                PRIMARY KEY(fileId1, fileId2))")
            for (f in x) {
                println("    loading file ", f)
                sql.query("LOAD DATA LOCAL INFILE '", f, "' INTO TABLE sourcerer_clones FIELDS TERMINATED BY ',' ENCLOSED BY '\"'")
                println("      ", sql.affectedRows(), " entries in ", sql.lastTime(), " [s]")
            }
        })
        println("  done in ", t[[3]], " [s]")
        sql.setTimestamp("sourcerer_clones", modified)
    } else {
        println("  skipping loading table sourcerer_clones - already loaded, timestamp ", timestamp.to.date(modified))
    }
    x <- sql.tableStatus("sourcerer_clones", F)
    println("  ~", x$length, " entries [", x$bytes, " bytes]")
    invisible(NULL)
}

# creates specified clone group and clone info tables (helper for clone info and group calculations)
createCloneGroupAndInfoTables <- function(table.clone_groups, table.clone_info) {
    sql.query("DROP TABLE IF EXISTS ", table.clone_info)
    sql.query("DROP TABLE IF EXISTS ", table.clone_groups)
    sql.query("CREATE TABLE ", table.clone_info, "(
            fileId INT NOT NULL,
            groupId INT NOT NULL,
            projectId INT NOT NULL,
            PRIMARY KEY(fileId))")
    sql.query("CREATE TABLE ", table.clone_groups, "(
            groupId INT NOT NULL,
            files INT NOT NULL,
            projects INT NOT NULL,
            oldestId INT NOT NULL,
            PRIMARY KEY(groupId))")
}

# calculates clone group information, including group summaries and oldest file ids for each group (helper for clone info and group calculations).
calculateGroupSummaries <- function(table.clone_groups, table.clone_info) {
    # now find oldest file for each clone group
    sql.query("DROP TABLE IF EXISTS tmp_files")
    sql.query("DROP TABLE IF EXISTS tmp_oldest")
    sql.query("DROP TABLE IF EXISTS tmp_groups")
    println("    finding oldest file in each clone group")
    sql.query("CREATE TABLE tmp_files (SELECT fileId, groupId, created FROM files_full_stats INNER JOIN ", table.clone_info," ON fileId=id)")
    println("      ", sql.affectedRows(), " cloned files accounted ", sql.lastTime(), " [s]")
    sql.query("CREATE TABLE tmp_oldest (
                SELECT tmp_files.groupId AS groupId, MIN(fileId) as oldestId FROM 
                (SELECT groupId, MIN(created) as oldest FROM tmp_files GROUP BY groupId) AS x
                INNER JOIN tmp_files 
                ON x.groupId=tmp_files.groupId AND x.oldest=tmp_files.created GROUP BY tmp_files.groupId)")
    println("      ", sql.affectedRows(), " oldest files for clone groups found ", sql.lastTime(), " [s]")
    println("      ", sql.createIndex("tmp_oldest", "groupId"))
    # now create clone group information 
    println("    creating clone groups")
    sql.query("CREATE TABLE tmp_groups (SELECT groupId, COUNT(fileId) AS files, COUNT(DISTINCT projectId) AS projects FROM ", table.clone_info, " GROUP BY groupId)");
    println("      ", sql.affectedRows(), " groups aggregated ", sql.lastTime(), "[s]")
    println("      ", sql.createIndex("tmp_groups", "groupId"))
    sql.query("INSERT INTO ", table.clone_groups, " SELECT tmp_groups.groupId, files, projects, oldestId FROM tmp_groups INNER JOIN tmp_oldest ON tmp_groups.groupId=tmp_oldest.groupId")
    println("      ", sql.affectedRows(), " groups created ", sql.lastTime(), "[s]")
    println("      ", sql.createIndex(table.clone_groups, "groupId"))
    sql.query("DROP TABLE tmp_files")
    sql.query("DROP TABLE tmp_oldest")
    sql.query("DROP TABLE tmp_groups")
}

# Calculates clone info and clone groups from the tokenizer. Utilizes the fact that tokenizer already outputs proper clone info with the exception of the mappings for the first file from the clone group found, which has to be added separately.  
calculateTokenizerCloneGroups <- function(table.from, prefix, overwrite = OVERWRITE) {
    table.clone_info = paste(prefix, "_clone_info", sep="")
    table.clone_groups = paste(prefix, "_clone_groups", sep="")
    println(table.clone_groups, " - clone groups for files reported by the tokenizer as identical")
    println(table.clone_info, " - relation between cloned files and their clone groups as reported by the tokenizer (identical files)")
    ts = sql.timestamp(table.from)
    if (sql.timestamp.shouldOverwrite(ts, table.clone_groups, overwrite) || 
        sql.timestamp.shouldOverwrite(ts, table.clone_info, overwrite)) {
        println("  calculating tokenizer clone groups from clone pairs in ", table.from)
        createCloneGroupAndInfoTables(table.clone_groups, table.clone_info)
        sql.query("DROP TABLE IF EXISTS tmp_files")
        sql.query("DROP TABLE IF EXISTS tmp_oldest")
        sql.query("DROP TABLE IF EXISTS tmp_groups")
        t = system.time({
            # add clone pair entries
            println("    building clone information")
            sql.query("INSERT INTO ", table.clone_info, " SELECT fileId2, fileId1, projectId2 FROM ", table.from)
            println("      initialized ", sql.affectedRows(), " clonepair entries ", sql.lastTime(), "[s]")
            sql.query("INSERT INTO ", table.clone_info, " SELECT fileId1, fileId1, MIN(projectId1) FROM ", table.from, " GROUP BY fileId1")
            println("      added ", sql.affectedRows(), " groupd id mappings ", sql.lastTime(), "[s]")
            println("      ", sql.createIndex(table.clone_info, "fileId"))
            # create group summaries and find oldest files
            calculateGroupSummaries(table.clone_groups, table.clone_info)
        })
        println("  done in ", t[[3]], " [s]")
        sql.setTimestamp(table.clone_groups, ts)
        sql.setTimestamp(table.clone_info, ts)
    } else {
        println("  skipping tokenizer clone groups preprocessing into ", table.from," - already created, timestamp: ", timestamp.to.date(ts))
    }
    x <- sql.tableStatus(table.clone_info, F)
    println("  ~", x$length, " entries [", x$bytes, " bytes]")
    x <- sql.tableStatus(table.clone_groups)
    println("  ~", x$length, " clone groups [", x$bytes, " bytes]")

}

# Calculates clone groups using iterative process where first all id's from clone pairs are extracted together with *any* of their other sides, so that we have some seed mapping with unique lhs. We then assume rhs to be the clone group id and use the clonepairs to unify the clone groups, i.e. if we see clonepair between X and Y we know that group Y can be unified with group X. 
calculateCloneGroups <- function(table.from, prefix, overwrite = OVERWRITE) {
    table.clone_info = paste(prefix, "_clone_info", sep="")
    table.clone_groups = paste(prefix, "_clone_groups", sep="")
    println(table.clone_groups, " - clone groups for files reported by sourcererCC as similar")
    println(table.clone_info, " - relation between cloned files and their clone groups as reported by the sourcererCC (similar files)")
    ts = sql.timestamp(table.from)
    if (sql.timestamp.shouldOverwrite(ts, table.clone_groups, overwrite) || 
        sql.timestamp.shouldOverwrite(ts, table.clone_info, overwrite)) {
        println("  calculating sourcerer clone groups from clone pairs in ", table.from)
        createCloneGroupAndInfoTables(table.clone_groups, table.clone_info)
        sql.query("DROP TABLE IF EXISTS tmp_clones")
        sql.query("DROP TABLE IF EXISTS tmp_map")
        sql.query("CREATE TABLE tmp_map (
            oldId INT NOT NULL,
            newId INT NOT NULL,
            PRIMARY KEY(oldId))")
        # create index on the map table
        sql.createIndex("tmp_map", "oldId")
        # copy the clone info and index it for faster operations in the future
        sql.query("CREATE TABLE tmp_clones SELECT fileId1, fileId2 FROM ", table.from)
        println("    copied ", sql.affectedRows(), " clone pairs in ", sql.lastTime(), " [s]")
        println("    ", sql.createIndex("tmp_clones", "fileId1, fileId2"))
        # now insert the default mapping for each file
        println("    inserting seed group ids")
        sql.query("INSERT INTO ", table.clone_info, " SELECT fileId1, MIN(fileId2), MIN(projectId1) FROM ", table.from, " GROUP BY fileId1")
        println("      ", sql.affectedRows(), " entries inserted in ", sql.lastTime(), " [s]")
        println("      ", sql.createIndex(table.clone_info, "fileid"));
        println("      ", sql.createIndex(table.clone_info, "groupId", F))
        sql.query("INSERT INTO ", table.clone_info, " SELECT fileId2, MIN(fileId1), MIN(projectId2) FROM ", table.from, " WHERE fileId2 NOT IN (SELECT fileId FROM ", table.clone_info, ") GROUP BY fileId2")
        println("    unifying clone groups")
        repeat {
            sql.query("INSERT INTO tmp_map SELECT fileId1, MIN(fileId2) FROM tmp_clones WHERE fileId1 IN (SELECT groupId FROM ", table.clone_info, ") GROUP BY fileId1");
            x <- sql.affectedRows();
            if (x == 0)
                break;
            println("      ", x, " group mappings to be updated in ", sql.lastTime(), " [s]")
            sql.query("DELETE FROM tmp_clones WHERE EXISTS (SELECT 1 FROM tmp_map WHERE tmp_map.oldId=tmp_clones.fileId1 AND tmp_map.newId=tmp_clones.fileId2)");
            println("        removed from unification list in ", sql.lastTime(), "[s]")
            repeat {
                sql.query("REPLACE INTO ", table.clone_info, " SELECT fileId, newId, projectId FROM ", table.clone_info, " INNER JOIN tmp_map ON groupId=oldId");
                x <- sql.affectedRows();
                if (x == 0)
                    break
                println("        ", x, " group mappings unified")
            }
            sql.query("DELETE FROM tmp_map")
        }
        # now we have unified the clone groups, calculate clone group summaries and oldest files
        calculateGroupSummaries(table.clone_groups, table.clone_info)
        
        sql.query("DROP TABLE tmp_map")
        sql.query("DROP TABLE tmp_clones")
        
        sql.setTimestamp(table.clone_groups, ts)
        sql.setTimestamp(table.clone_info, ts)
    } else {
        println("  skipping clone groups preprocessing into ", table.from," - already created, timestamp: ", timestamp.to.date(ts))
    }
    x <- sql.tableStatus(table.clone_info, F)
    println("  ~", x$length, " entries [", x$bytes, " bytes]")
    x <- sql.tableStatus(table.clone_groups)
    println("  ~", x$length, " clone groups [", x$bytes, " bytes]")
}

# Calculates non-empty files, since tokenizer outputs all, but the zeros in their sizes cause div by zero errors from time to time
calculateNonEmptyFiles <- function(overwrite = OVERWRITE) {
    println("non_empty_files - non empty files as reported by the tokenizer")
    ts = sql.timestamp("files_full_stats")
    if (sql.timestamp.shouldOverwrite(ts, "non_empty_files", overwrite)) {
        println("  calculating non-empty files")
        sql.query("DROP TABLE IF EXISTS non_empty_files")
        sql.query("CREATE TABLE IF NOT EXISTS non_empty_files (
        id INT NOT NULL,
        PRIMARY KEY (id))")
        sql.query("INSERT INTO non_empty_files SELECT id FROM files_full_stats WHERE bytes > 0")
        println("    ", sql.affectedRows(), " non-empty files found in ", sql.lastTime(), " [s]")
        println("    ", sql.createIndex("non_empty_files", "id"))
        println("    done in ", sql.lastTime(), "[s]")
        sql.setTimestamp("non_empty_files", ts)
    } else {
        println("  skipping preprocessing non-empty files - already done, timestamp: ", timestamp.to.date(ts))
    }
    x <- sql.tableStatus("non_empty_files")
    println("  ", x$length, " entries [", x$bytes, " bytes]")
}

# Calculates id of files that are original based on given clone information (identified by the prefix). A file is original if its id is nowhere in the particular clone info table. If include.oldest is T, then the oldest file from each clone group (assumed to be the original file) is also added to the table. 
calculateOriginalFiles <- function(table.into, table.from, prefix, include.oldest = T, overwrite = OVERWRITE) {
    table.clone_info = paste(prefix, "_clone_info", sep="")
    table.clone_groups = paste(prefix, "_clone_groups", sep="")
     if (include.oldest) 
        println(table.into, " - file ids that are not reported as identical by the tokenizer, including the oldest file from each group")
    else
        println(table.into, " - file ids that are not reported as identical by the tokenizer")
    ts = sql.timestamp(table.from)
    if (sql.timestamp.shouldOverwrite(ts, table.into, overwrite)) {
        sql.query("DROP TABLE IF EXISTS ", table.into)
        sql.query("CREATE TABLE ", table.into, " (id INT NOT NULL, PRIMARY KEY(id))")
        sql.query("INSERT INTO ", table.into, " SELECT id FROM ", table.from, " WHERE id NOT IN (SELECT fileId FROM ", table.clone_info, ")")
        println("    ", sql.affectedRows(), " entries in ", sql.lastTime(), " [s]")
        if (include.oldest) {
            sql.query("INSERT INTO ", table.into, " SELECT DISTINCT oldestId FROM ", table.clone_groups)
            println("    ", sql.affectedRows(), " oldest files treated as originals in ", sql.lastTime(), "[s]")
        }
        println("    ", sql.createIndex(table.into, "id"))
        sql.setTimestamp(table.into, ts)
    } else {
        println("  skipping preprocessing non identical ", prefix, " files - already done, timestamp: ", timestamp.to.date(ts))
    }
    x <- sql.tableStatus(table.into)
    println("  ", x$length, " entries [", x$bytes, " bytes]")
}

# given a set of file ids that are to be assumed original, calculates project cummulative statistics only using those files. Useful for knowing how much from a project is original. 
calculateProjectOriginals <- function(table.into, table.originals, overwrite = OVERWRITE) {
    println(table.into, " - statistics for projects using only files from ", table.originals)
    ts = sql.timestamp(table.originals)
    if (sql.timestamp.shouldOverwrite(ts, table.into, overwrite)) {
        sql.query("DROP TABLE IF EXISTS ", table.into)
        sql.query("CREATE TABLE IF NOT EXISTS ",table.into," (
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
        sql.query("INSERT INTO ",table.into," SELECT projectId, COUNT(*), SUM(bytes), SUM(commentBytes), SUM(whitespaceBytes), SUM(tokenBytes), SUM(separatorBytes), SUM(loc), SUM(commentLoc), SUM(emptyLoc), SUM(errors) FROM files_full_stats JOIN ", table.originals, " ON files_full_stats.id=", table.originals,".id GROUP BY projectId")
        println("  inserted ", sql.affectedRows(), " entries in ", sql.lastTime(), "[s]")
        println("  ", sql.createIndex(table.into, "projectId"))
        sql.query("INSERT INTO ",table.into," SELECT id, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 FROM bookkeeping_proj WHERE id NOT IN (SELECT projectId FROM ", table.into, ")")
        println("  added ", sql.affectedRows(), " empty projects in ", sql.lastTime(), "[s]")
        println("  ", sql.createIndex(table.into, "projectId"))
        sql.setTimestamp(table.into, ts)        
    } else {
        println("  skipping preprocessing - already done, timestamp: ", timestamp.to.date(ts))
    }
    x <- sql.tableStatus(table.into)
    println("  ", x$length, " entries [", x$bytes, " bytes]")
}


# calculates project summaries using all files. 
calculateProjectStats <- function(overwrite = OVERWRITE) {
    println("project_stats - cummulative information about projects")
    ts = sql.timestamp("files_full_stats")
    if (sql.timestamp.shouldOverwrite(ts, "project_stats", overwrite)) {
        sql.query("DROP TABLE IF EXISTS project_stats")
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
        sql.query("INSERT INTO project_stats SELECT projectId, COUNT(*), SUM(bytes), SUM(commentBytes), SUM(whitespaceBytes), SUM(tokenBytes), SUM(separatorBytes), SUM(loc), SUM(commentLoc), SUM(emptyLoc), SUM(errors) FROM files_full_stats GROUP BY projectId")
        println("  done in ", sql.lastTime(), "[s]")
        println("  ", sql.createIndex("project_stats", "projectId"))
        sql.setTimestamp("project_stats", ts)
    } else {
        println("  skipping calculating project statistics - already done, timestamp: ", timestamp.to.date(ts))
    }
    x <- sql.tableStatus("project_stats")
    println("  ", x$length, " entries [", x$bytes, " bytes]")
}

calculateProjectOriginalityStatsXY <- function(overwrite = OVERWRITE) {
    ts = sql.timestamp("files_full_stats")
    if (sql.timestamp.shouldOverwrite(ts, "tokenizer_project_stats_original", overwrite)) {
        println("Calculating project statistics from only original files")
        sql.query("DELETE FROM tokenizer_project_stats_original")
        sql.query("INSERT INTO tokenizer_project_stats_original SELECT projectId, COUNT(*), SUM(bytes), SUM(commentBytes), SUM(whitespaceBytes), SUM(tokenBytes), SUM(separatorBytes), SUM(loc), SUM(commentLoc), SUM(emptyLoc), SUM(errors) FROM files_full_stats WHERE id NOT IN (SELECT fileId FROM tokenizer_clone_info) GROUP BY projectId")
        # if there are any projects that have *no* original files, we will insert them as well
        sql.query("INSERT INTO tokenizer_project_stats_original SELECT id, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 FROM bookkeeping_proj WHERE id NOT IN (SELECT projectId FROM tokenizer_project_stats_original)")
        
        
        println("  done in ", sql.lastTime(), "[s]")
        sql.setTimestamp("tokenizer_project_stats_original", ts)
    } else {
        println("Skipping calculating project statistics from original files - already done, timestamp: ", timestamp.to.date(ts))
    }
    x <- sql.tableStatus("tokenizer_project_stats_original")
    println("  ", x$length, " entries [", x$bytes, " bytes]")
}

# creates tables for preprocessed data
sql.createPreprocessedTablesXY <- function() {
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

    # preprocessed tables for the sourcerer output

    sql.query("CREATE TABLE IF NOT EXISTS sourcerer_clone_groups (
        groupId INT NOT NULL,
        files INT NOT NULL,
        projects INT NOT NULL,
        PRIMARY KEY(groupId))")
    println("sourcerer_clone_groups - clone groups for files reported by the sourcerer as similar")
    sql.query("CREATE TABLE IF NOT EXISTS sourcerer_clone_info (
        fileId INT NOT NULL,
        groupId INT NOT NULL,
        projectId INT NOT NULL,
        PRIMARY KEY(fileId))")
    println("sourcerer_clone_info - relation between cloned files and their clone groups as reported by the sourcerer (similar files)")
}


# data presenting

# displays the clone info as obtained from given clone group tables
displayCloneGroupsInfo <- function(clone.groups, clone.info, input.files) {
    nf <- sql.tableStatus(input.files)$length
    nc <- sql.tableStatus(clone.info)$length
    ncg <- sql.tableStatus(clone.groups)$length
    println("Input files:              ", nf)
    println("Files which have clones:  ", nc)
    println("Clone groups:             ", ncg)
    println("Original files:           ", nf - nc + ncg, "  ", (nf - nc + ncg)/ nf, "[%]")
}

displayFilesPerCloneGroup <- function(clone.groups) {
    # histogram of number of files in a clone group
    x <- sql.query(" SELECT files FROM ", clone.groups)$files
    print(summary(x))
    logHist(x, main = "# of files in a clone group ", xlab = " # of files in group", ylab = "# of clone groups", base = 100)
}

displayProjectsPerCloneGroup <- function(clone.groups) {
    # number of projects per clone group
    x <- sql.query(" SELECT projects FROM ", clone.groups)$projects
    print(summary(x))
    logHist(x, main = "# of projects in a clone group ", xlab = " # of projects in group", ylab = "# of clone groups", base = 100)
}

mostClonedFiles <- function(clone.groups, countBy) {
    # displaying clickable url in the final document
    x <- sql.query("SELECT url, files, projects FROM ",clone.groups, " JOIN files_full_stats ON oldestId=id ORDER BY ",countBy," DESC LIMIT ", DT_LIMIT)
    x$url <- sapply(x$url, unescape)
    x
}

originalContentPerProject <- function(table, content) {
    x <- sql.query("SELECT (",table, ".", content, " / project_stats.", content,") AS result FROM project_stats JOIN ", table, " ON project_stats.projectId=", table, ".projectId")$result
    print(summary(x))
    normalHist(x, main = paste("% of original", content, "in projects"), xlab = paste("% of original", content, " in project"), ylab = "# projects", base = 100)
} 

mostOriginalProjects <- function(table, content, limit) {
    x <- sql.query("SELECT url, (two.", content, " / one.",content,") AS original, one.",content," AS total
        FROM project_stats AS one
        JOIN ", table, " AS two ON one.projectId = two.projectId
        JOIN bookkeeping_proj ON bookkeeping_proj.id = two.projectId
        ORDER BY original DESC, total DESC LIMIT ", limit)
    x
}

leastOriginalProjects <- function(table, content, limit) {
    x <- sql.query("SELECT url, (two.", content, " / one.",content,") AS original, one.files AS numFiles
        FROM project_stats AS one
        JOIN ", table, " AS two ON one.projectId = two.projectId
        JOIN bookkeeping_proj ON bookkeeping_proj.id = two.projectId
        ORDER BY original, numFiles DESC LIMIT ", limit)
    x
}

# data converting -----------------------------------------------------------------------------------------------------

# converts the timestamp used in the script to dates. (the timestamp is UTC timestamp)
timestamp.to.date <- function(timestamp) {
    as.POSIXct(timestamp, origin="1970-01-01")    
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

# visualization helpers -----------------------------------------------------------------------------------------------

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

