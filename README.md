# Random things

### huffman.hs - an implementation of Huffman coding in Haskell

To encode things with it use `huffmanEncode`.

`huffmanTable` returns the encryption table and `huffman` returns the binary tree of the table.

example:

```console
$ ghci huffman.hs
> huffmanEncode "hello world!"
"0011100010110111111001010000111011110"

> huffmanTable "aaaaabbbcc"
[('a',"1"),('b',"01"),('c',"00")]
```
