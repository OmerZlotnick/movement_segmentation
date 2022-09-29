

ListCompare <- function(Left,Right, PrintOnly=T)
{
 INTERSECT <-  intersect(Left,Right)
 Left <- sort(unique(Left))
 Right <- sort(unique(Right))
 if (PrintOnly){
   print ('In both:')
   print( INTERSECT)
   print ('only in Left:')
   print(Left[!(Left %in% INTERSECT)])
   print ('only in Right:')
   print(Right[!(Right %in% INTERSECT)])
  }
 if(!PrintOnly)
 return(list('both'= INTERSECT, 'OnlyLeft'=Left[!(Left %in% INTERSECT)], 'OnlyRight'= Right[!(Right %in% INTERSECT)]))
}


##           # examples:
# A <- as.character((1:10))
# B <- as.character((15:5))
# ListCompare(A,B)
# ListCompare(A,B,PrintOnly = F)
 