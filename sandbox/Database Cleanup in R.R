# Author: Adam L. Rich
# Date:   January 18, 2012
# Desciption:
#
#   To clean up the BBR database
#

source('w:/sl/actuarial/richad/r/SLPseudoPackage.r')
source('w:/sl/actuarial/richad/r/SQLServer.r')

conn <- GetConnection(db = 'BeazleyBreachResponse_20111122153504')
tables <- RunSQL('select name from sys.tables;', conn)

columns <- RunSQL('
  select t.name as [Table], 
         c.name as [Field]
    from sys.columns c,
                sys.tables t
   where c.object_id = t.object_id;
', conn)

# 

# Compare the two data frames
comparer <- merge(rater_fields, columns, all = TRUE)