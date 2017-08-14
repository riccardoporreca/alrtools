LoadFromDatabase <- function (sourcedata = NA, server = NULL, database = NULL, sqlstring = NULL) 
{
    switch(sourcedata, BI = {
        if (is.null(server)) server <- "DBS-edy-BeazleyIntelligenceDatabase-PRD,1498\n"
        if (is.null(database)) database <- "BeazleyIntelligenceDataSets"
        if (is.null(sqlstring)) sqlstring <- "Select top 10 * from [BeazleyIntelligenceDataSets].[Report].[SectionCombinedView];"
    }, `PublicD&O` = {
        if (is.null(server)) server <- "DBS-6ud-PublicDandO-PRD,12520"
        if (is.null(database)) database <- "PublicDandO"
        if (is.null(sqlstring)) sqlstring <- "Select top 10 * from [PublicDandO].[Quote].[Application]"
    }, Eclipse = {
        if (is.null(server)) server <- "LONLIVDB05\\RATING"
        if (is.null(database)) database <- "Eclipse"
        if (is.null(sqlstring)) sqlstring <- "Select top 10 * from [Eclipse].[Quote].[Application]"
    }, Lawyers = {
        if (is.null(server)) server <- "DBS-dvq-lplrater-PRD,12770"
        if (is.null(database)) database <- "LPLRater"
        if (is.null(sqlstring)) sqlstring <- "Select top 10 * from [LPLRater].[dbo].[Quotes]"
    }, sClaimCat = {
        if (is.null(server)) server <- "ALDLIVDBV03\\BCDS"
        if (is.null(database)) database <- "BeazleyCentralClaimsMI"
        if (is.null(sqlstring)) sqlstring <- "select * from [BeazleyCentralClaimsMI].[dbo].[v_BaseClaims];"
    }, Hospitals = {
        stop("This database is no longer valid, please use HealthCarePROD and adapt SQL queries to new DB structure")
    }, HealthCarePROD = {
        if (is.null(server)) server <- "DBS-fy7-HPL-PRD,13170"
        if (is.null(database)) database <- "HPL"
        if (is.null(sqlstring)) sqlstring <- "select * from [HPL].[dbo].[h_layer];"
    }, HealthCareDEV = {
        if (is.null(server)) server <- "DBS-neh-HPL-DEV, 13171"
        if (is.null(database)) database <- "HPL"
        if (is.null(sqlstring)) sqlstring <- "select * from [HPL].[dbo].[Quote_layer];"
    }, HealthCareSTAGING = {
        if (is.null(server)) server <- "DBS-4uk-HPL-SYS,13170"
        if (is.null(database)) database <- "HPL"
        if (is.null(sqlstring)) sqlstring <- "select * from [HPL].[dbo].[Quote_layer];"
    }, HealthCareUAT = {
        if (is.null(server)) server <- "DBS-mtn-HPL-UAT,13170"
        if (is.null(database)) database <- "HPL"
        if (is.null(sqlstring)) sqlstring <- "select * from [HPL].[dbo].[Quote_layer];"
    }, `A&E` = {
        if (is.null(server)) server <- "LONLIVdb05\\rating"
        if (is.null(database)) database <- "LAE"
        if (is.null(sqlstring)) sqlstring <- "select * from [LAE].[dbo].[Quote];"
    }, PrePeer = {
        if (is.null(server)) server <- "DBS-fg4-SLPrePeer-PRD, 12518"
        if (is.null(database)) database <- "SLPrePeer"
        if (is.null(sqlstring)) sqlstring <- "select top 10 * from Mapping.SpotRate;"
    }, SnapShot = {
        if (is.null(server)) server <- "DBS-xus-SpecialtyLinesSnapshot-PRD,12770"
        if (is.null(database)) database <- "SpecialtyLinesSnapshot"
        dsqlstring <- paste0("SELECT  T.name AS [TABLE NAME] ,\n                       C.name AS [COLUMN NAME] ,\n                       P.name AS [DATA TYPE] ,\n                       P.max_length AS [SIZE] ,\n                       CAST(P.precision AS VARCHAR) + '/' + CAST(P.scale AS VARCHAR) AS [PRECISION/SCALE]\n                       FROM    sys.objects AS T\n                       JOIN sys.columns AS C ON T.object_id = C.object_id\n                       JOIN sys.types AS P ON C.system_type_id = P.system_type_id\n                       WHERE   T.type_desc = 'USER_TABLE';")
        dsqlstring <- gsub("\n", "  ", dsqlstring)
        if (is.null(sqlstring)) sqlstring <- dsqlstring
    }, stop("must specify valid sourcedata name or provide complete server and database"))
    if (is.null(sqlstring)) {
        stop("must specify sqlstring")
    }
    sqlstring <- gsub("\\n", " ", sqlstring)
    conn.string <- SQLConnectionString(server, database)
    channel <- RODBC::odbcDriverConnect(connection = conn.string)
    if (channel < 0) {
        stop(paste("Can't connect to database using", conn.string))
        return(-1)
    }
    ClaimCat <- RODBC::sqlQuery(channel, sqlstring, stringsAsFactors = FALSE)
    close(channel)
    return(ClaimCat)
}
