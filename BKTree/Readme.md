## BKTree

Simple implementation of a BK-Tree based on [Damn Cool Algorithms, Part 1: BK-Trees](http://blog.notdot.net/2007/4/Damn-Cool-Algorithms-Part-1-BK-Trees). Use `search` to find near-matches (pass in the fuzz factor as an argument) to a word definied by a smallish English dictionary (included). Building the search tree takes little time, but executing the search can take on the order of minutes on recent machines. As an example:

    $ time ./search goodbye 2
    goldeye
    goodbys
    goodbye
    goodby
    goodly
    goodie
    goodbyes
    goody
    ./search goodbye  56.44s user 0.72s system 97% cpu 58.883 total
