
## Bytes et Words 

Un bit correspond à l'alliance d'un constructeur " Bit" et d'une valeur Booléenne.
Un Polynome est un type qui correspond au constructeur "Polynomial" et d'un tableau de type a
Un Byte est un polynome de Bit, on peut donc écrire " Byte $ Polynomial [Bit False, Bit True, Bit False, Bit False]" qui est de type Byte .
Un Word est un Polynome de Byte c'est à dire que "Word (Polynomial[ Byte $ Polynomial [Bit False, Bit True, Bit False, Bit False], Byte $ Polynomial [Bit True, Bit True, Bit True, Bit False]]) est un Word.

## Chiffrement Cipher.hs
Le chiffrement peut se faire de trois manières différentes. 
La fonction ecrypt permettra d'encrypter le message selon la méthode AES ( 10 encryptions du message, à l'aide de 10 clefs).
La fonction ecb_encrypt encrypte selon la technique ecb . Elle prend des blocs plus longs que encrypt et ne les encryptera qu'une seule fois, avec la même clé pour chacune, selon la méthode Electronic Codebook Block.
La fonction cbc_encrypt encrypte selon la méthode Cipher Block Chaining , c'est à dire que comme la méthode ecb elle prend des blocs plus longs que ecnrypt et va à l'aide d'une opération xor entre un vecteur d'initialisation et la clé créer la première clé avec laquelle sera codé le premier bloc. Ensuite chaque bloc codé est "xoré" avec la clé initiale afin de créer une nouv elle clé pour encrypter le bloc suivant.