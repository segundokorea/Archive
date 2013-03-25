## [Pen and Paper (Ruby Quiz #90)](http://www.rubyquiz.com/quiz90.html)

Implementation of Warnsdorff&#8217;s Rule for finding a Knight&#8217;s Tour through a chessboard of size *N*-by-*N*. Warnsdorff&#8217;s Rule also applies to the **Pen and Paper** variation described in [Ruby Quiz #90](http://www.rubyquiz.com/quiz90.html). Pass the `-p` flag to `tour` to use the **Pen and Paper** method, and supply a size *N* to start generating:

    $ ./tour -p 6
     21 | 4  | 32 | 22 | 5  | 31 
    -----------------------------
     28 | 16 | 7  | 27 | 15 | 8  
    -----------------------------
     11 | 34 | 24 | 12 | 33 | 23 
    -----------------------------
     20 | 3  | 29 | 17 | 6  | 30 
    -----------------------------
     25 | 13 | 10 | 26 | 14 | 9  
    -----------------------------
     1  | 35 | 19 | 2  | 36 | 18 
    $ ./tour 6
     19 | 2  | 11 | 28 | 17 | 4  
    -----------------------------
     10 | 27 | 18 | 3  | 12 | 29 
    -----------------------------
     1  | 20 | 9  | 30 | 5  | 16 
    -----------------------------
     26 | 31 | 24 | 15 | 34 | 13 
    -----------------------------
     21 | 8  | 35 | 32 | 23 | 6  
    -----------------------------
     36 | 25 | 22 | 7  | 14 | 33 
