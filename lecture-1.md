
# Lecture 1

## Remarks / Proposals / Improvements
1. a
2. b
3. c

## First Exercise
Write Scala code for RHO-calculi (or in Haskell)

    Gramma:
        P, Q :== 0 | for ( x <- y )P | x!(Q) | P|Q | *x
        x,y :== @P

    Domain equation
        P[x] = 1 + ( X ⨯ X ⨯ P[X] ) + ( X ⨯ P[X]) + ( P[X] ⨯ P[X] ) + X 
        R = P[R]

## Second Exercise
???

## Third Exercise
    Find the grammar for ambient processes or blue calculus or (extra credit) the linear lambda calculus
    Write the domain equation
    Write the Scala code
