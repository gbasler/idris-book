twice: (a -> a) -> (a -> a)
twice f x = f (f x)

double: Num tp => tp -> tp
double x = x + x

quadruple: Num tp => tp -> tp
quadruple = twice double
