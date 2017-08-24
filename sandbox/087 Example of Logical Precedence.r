t <- c(T && T || T,
       T && T || F,
       T && F || T,
       T && F || F,
       F && T || T,
       F && T || F,
       F && F || T,
       F && F || F)

u <- c((T && T) || T,
       (T && T) || F,
       (T && F) || T,
       (T && F) || F,
       (F && T) || T,
       (F && T) || F,
       (F && F) || T,
       (F && F) || F)

v <- c(T && (T || T),
       T && (T || F),
       T && (F || T),
       T && (F || F),
       F && (T || T),
       F && (T || F),
       F && (F || T),
       F && (F || F))


sum(t == c(T, T, T, F, T, F, T, F)) == 8