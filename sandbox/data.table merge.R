# Author: Adam L. Rich
# Date:   July 9, 2013
# Description:
#
#   Experimenting with data.table
#

require(data.table)



# Use the iris dataset,
# but delete random rows
random.mask <- c(T, F, T, T, T, T, T, T, T, T, 
                 F, F, T, F, T, T, T, T, T, T, 
                 T, T, T, T, F, T, T, T, T, F, 
                 T, T, T, T, F, T, T, F, T, T, 
                 T, T, T, F, T, T, T, T, T, F, 
                 T, T, T, F, T, F, T, T, T, F, 
                 T, T, T, F, F, T, T, F, T, T, 
                 T, F, T, F, F, T, T, T, F, T, 
                 F, T, F, F, T, T, T, T, T, T, 
                 T, T, F, T, T, T, T, F, T, F, 
                 T, F, T, T, F, T, T, F, T, F, 
                 T, F, T, F, T, T, F, T, T, F, 
                 T, T, T, T, F, T, F, F, F, F, 
                 T, F, F, T, T, T, F, T, T, T, 
                 T, T, T, T, F, T, T, F, T, F)
rm(iris); iris <- iris[random.mask, ]
IRIS <- as.data.table(iris)



# Create another data frame for testing
colors <- data.frame(
  Species = c("uniflora", "versicolor", "versicolor", 
              "setosa", "setosa", "setosa"),
  Color = c('Violet', 'Blue', 'Yellow', 
            'Orange', 'Blue', 'Pink')
)
COLORS <- as.data.table(colors)



system.time(
  for (x in 1:1000) {

  join.left <- merge(
    x = iris, 
    y = colors,
      all.x = TRUE
  )
  
  join.right <- merge(
    x = iris, 
    y = colors, 
    all.y = TRUE
  )
  
  join.inner <- merge(
    x = iris, 
    y = colors, 
    all = FALSE
  )
  
  join.outer <- merge(
    x = iris, 
    y = colors, 
    all = TRUE
  )
})



# How many rows for each species?
#   setosa:       40 * 3 = 120
#   versicolor:   34 * 2 =  68
#   virginica:    31 * 0 =   0
#   uniflora:      0 * 1 =   0
#
#   left join:   120 + 68 +  31     = 219
#   right join:    1 + 68 + 120     = 189
#   inner join:  120 + 68           = 188
#   outer join:  120 + 68 +  31 + 1 = 220
#
table(iris$Species)
table(colors$Species)

nrow(join.left)
nrow(join.right)
nrow(join.inner)
nrow(join.outer)



system.time(
  for (x in 1:1000) {

  JOIN.LEFT <- merge(
    x = IRIS, 
    y = COLORS,
    all.x = TRUE,
    by = 'Species',
    allow.cartesian = TRUE
  )
  
  JOIN.RIGHT <- merge(
    x = IRIS, 
    y = COLORS, 
    all.y = TRUE,
    by = 'Species',
    allow.cartesian = TRUE
  )
  
  JOIN.INNER <- merge(
    x = IRIS, 
    y = COLORS, 
    all = FALSE,
    by = 'Species',
    allow.cartesian = TRUE
  )
  
  JOIN.OUTER <- merge(
    x = IRIS, 
    y = COLORS, 
    all = TRUE,
    by = 'Species',
    allow.cartesian = TRUE
  )
})

nrow(JOIN.LEFT)
nrow(JOIN.RIGHT)
nrow(JOIN.INNER)
nrow(JOIN.OUTER)

