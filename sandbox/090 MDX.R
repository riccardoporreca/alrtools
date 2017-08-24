# Author: Adam  L. Rich
# Date:   March 8, 2013
# Description:
#
#   Test of MDX query
#

require(RODBC)

channel <- odbcDriverConnect(
  # connection = 'Provider=MSOLAP;Integrated Security=SSPI;Initial Catalog=BeazleyRatingMI;Data Source=lonlivdb05\rating;MDX Compatibility=1;Safety Options=2;MDX Missing Member Mode=Error'
  # connection = 'Driver={SQL Server};Server={LONLIVDB05\\rating};Database={tempdb};TrustedConnection=Yes'
  connection = 'Driver={MSOLAP};Server={LONLIVDB05\\rating};TrustedConnection=Yes'
)


mdx <- "SELECT {
  [Measures].[Total Incurred],
  [Measures].[Count Of Claim],
  [Measures].[Paid Defence],
  [Measures].[Paid Fees],
  [Measures].[Paid Indemnity],
  [Measures].[Total Paid],
  [Measures].[Incurred Defence],
  [Measures].[Incurred Fees],
  [Measures].[Incurred Indemnity]
} ON COLUMNS,

NON EMPTY {
  CrossJoin(
    [Claim].[Claim Reference].[Claim Reference],
    [Claim].[Cause Of Loss].[Cause Of Loss],
    [Claim].[Claim Category].[Claim Category],
    [Claim].[Claim Status].[Claim Status],
    [Claim].[Claimant Name].[Claimant Name],
    [Claim].[Section].[Section],
    [Claim].[Claim Made Date].[Claim Made Date],
    [Claim].[Claim Created Date].[Claim Created Date]
  )
} ON ROWS

FROM [RatingMI] 
WHERE ([Policy].[Product].&[24])
"

mdx <- gsub('\\n|\\t', ' ', mdx)

sqlQuery(channel, mdx, stringsAsFactors = FALSE)