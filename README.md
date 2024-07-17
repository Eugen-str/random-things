# Random things

### huffman.hs - an implementation of Huffman coding in Haskell

To encode things use `huffmanEncode x`, where `x` is the string/list to encode.

To decode things use `huffmanDecode encoded ht`, where `encoded` are the encoded bits in a String and `ht` is the HuffmanTable that was used for encoding.

`huffmanTable` returns the encryption table and `huffman` returns the binary tree of the table.

example, encoding the text "hello world!" and then decoding it with the table and bits:

```haskell
$ ghci huffman.hs

> encoded = huffmanEncode "hello world!"
> encoded
"0011100010110111111001010000111011110"

> ht = huffmanTable "hello world!"
> ht
[('l',"01"),('r',"000"),('h',"001"),('w',"100"),('o',"101"),('e',"1100"),('d',"1101"),('!',"1110"),(' ',"1111")]

> huffmanDecode encoded ht
"hello world!"
```
