## Even Tree Challenge

Solution for the Interviewstreet Even Tree Challenge. Essentially you need to calculate the maximum number of edges you can remove from a tree and end up with a forest where each connected component has an even number of vertices. The solution is simple: Try removing each branch one by one and see if the forest invariant holds. If it does, good, that&#8217;s one more edge we can count towards our maximum; otherwise replace the edge and keep trying.

To use this program, pipe the sample input through to `runhaskell`, like so:

    $ cat SampleInput.txt | runhaskell EvenTree.hs
