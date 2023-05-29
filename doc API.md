# Description
---
La librairie lib-chloride permet d'encrypter/ décrypter une liste de Byte en utilisant la méthode AES, ou avec différentes méthodes s'appuyant sur l'AES (mode ECB, CBC, etc...)

## Démarrage rapide

``` haskell
import Byte
import Cipher
import CipherUtils

main = do
    -- On créer un bloc de données (16 bytes)
    let block = intsAsCipherData [0x32, 0x43, 0xf6, 0xa8, 0x88, 0x5a, 0x30, 0x8d, 0x31, 0x31, 0x98, 0xa2, 0xe0, 0x37,0x07, 0x34]
    -- La clé à utiliser (ici elle fait 16 bytes, mais elle pourrait aussi faire 24 ou 32 bytes)
    let key   = intsAsCipherData [0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf,0x4f, 0x3c]
    -- Encryptage :
    let res = encrypt key block
    putStrLn $ showByteBlock res
    -- Décryptage :
    let inv = decrypt key res
    putStrLn $ showByteBlock inv
```

Ici, nos données sont initialement représentées sous forme d'entier (`Int`), qui sont ensuite converti en liste de bytes par `intsAsCipherData`. On encrypte d'abord ce bloc avec la clé donnée, pour ensuite décrypter le résultat de l'opération précédente, ce qui devrait nous donner le bloc initial.

## Fonctionnement
Le Cipher prend en entrée deux tableau de Byte, la clé et le bloc.
Un Byte est un type qui correspondn à un Polynome de Bits, qu'on peut représenter ainsi `Byte $ Polynomial Bit`.
Le Cipher encode à la fois le bloc et la clé.
Dans un premier temps, le Cipher modifie le bloc à l'aide de fonctions comme  SubBytes, ShiftRows, MixColumn et  Round Key . Les trois premières fonctions modifient le bloc sans utiliser la clé, et c'est la quatrième ( AddRoundKey) qui utilise la clé. Ensuite le bloc trouvé devient le nouveau bloc à coder et ainsi de suite, dix fois donc.
De son côté, la clé initiale n'est utilisée qu'une fois puisqu'elle est après elle même encryptée : on utilise la fonction nextKey pour créer la clé suivante. Elle se base 

## Autres types de chiffrement

Il est possible de chiffrer un bloc avec une méthode dite *Electronic Codebook* (ECB), qui permet de chiffrer plus de 16 bytes à la fois. Pour cela il suffit de remplacer l'appel à `encrypt` par `encryptECB`.
Une autre technique de chiffrement est le *Cipher Block Chaining* (CBC), qui, pour chaque bloc de 16 bytes, utilise le résultat du chiffrement précédent et le XOR avec le bloc à encrypter. Cette fonctionnalité est disponible en utilisant `encryptCBC`