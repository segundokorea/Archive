## [Huffman Encoder (Ruby Quiz #123)](http://www.rubyquiz.com/quiz123.html)

Huffman encoder (and decoder) with serialization via Ruby&#8217;s Marshal. To use, simply pass a message into `encode` like so:

    $ ./encode Hello, world!
    Encoded:
    11111111 11111111 10001011 11111011
    11110111 11010111 10011101 10000000
    Encoded Bytes: 8
    Encoded Padding: 6

    Original:
    Hello, world!
    Original Bytes: 13

    Compressed: 38.5%
