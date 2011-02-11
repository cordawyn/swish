> {-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

%-------------------------------=  --------------------------------------------
\chapter{Partioning sort}
%-------------------------------=  --------------------------------------------

%align

> module Swish.HaskellRDF.Sort.QuickSort
> where
> import Data.List
> import Swish.HaskellRDF.Sort.LibBase

%-------------------------------=  --------------------------------------------
\section{Augustsson's stable quicksort}
%-------------------------------=  --------------------------------------------

Neither |qsort3| nor |quickSort| is stable. This is due to the fact
that partition reverses the order of elements (the lists |l| and |r|
are used as a stack). By duplicating the code into a stable and an
anti-stable variant we obtain stability.

> stableQuickSort		:: (Ord a) => [a] -> [a]
> stableQuickSort		=  stableQuickSortBy (<=)
>
> stableQuickSortBy		:: Rel a -> [a] -> [a]
> stableQuickSortBy (<=) as	=  qsortBy (<=) as []
>
> qsortBy			:: Rel a -> [a] -> [a] -> [a]
> qsortBy (<=) []       x 	=  x
> qsortBy (<=) [a]      x	=  a : x
> qsortBy (<=) (p : as) x	=  partition [] [] as
>     where
>     partition l r []		=  rqsortBy (<=) l (p : rqsortBy (<=) r x)
>     partition l r (a : as)
>         | p <= a 		=  partition l (a : r) as
>	  | otherwise		=  partition (a : l) r as

The function |partition| partitions and sorts the sublists. Note that
|l| and |r| are in reverse order and must be sorted with an anti-stable
sorting.


The function |rqsortBy| is as |qsort| but anti-stable, ie it reverses
equal elements (compare the last two equations of |partition|).

> rqsortBy			:: Rel a -> [a] -> [a] -> [a]
> rqsortBy (<=) []       x 	=  x
> rqsortBy (<=) [a]      x	=  a : x
> rqsortBy (<=) (p : as) x	=  partition [] [] as
>     where
>     partition l r []		=  qsortBy (<=) l (p : qsortBy (<=) r x)
>     partition l r (a : as)
>         | a <= p 		=  partition (a : l) r as
>	  | otherwise		=  partition l (a : r) as

