# Description
--- 
La librairie lib-chloride permet d'encrypter/ décrypter une liste de Byte en utilisant la méthode AES, ou avec différentes méthodes s'appuyant sur l'AES ( encryption EBC, CBC..)

## Exemple d'utilisation de la lib-chloride
``` haskell
    import Byte
    import Cipher
    do 
        let block = concatMap (byteFromInt) [0x3243f6a8 , 0x885a308d, 0x313198a2, 0xe0370734 ]
        let key   = concatMap (byteFromInt) [0x2b7e1516 , 0x28aed2a6, 0xabf71588, 0x09cf4f3c ]
        let res = encrypt key block
        putStrLn $ showByteBlock res
        -- Pour décrypter : 
        let inv = decrypt key inv
        inv putStrLn $ showByteBlock inv 

```
Dans cet exemple, on commence par créer un bloc qu'on va encrypter et décrypter et une clé. On les passe du format hexadécimal au format byte avec un map de la fonction byteFromInt . 
La variable res est le résultat de l'encryption. Elle se fait à l'aide de la fonction encrypt qui prend deux paramètres, la clé et le bloc; pour décrypter, c'est la même logique puisqu'il ya une fonction decrypt qui prends deux arguments, la clé et le bloc.

## Autres types de chiffrement 

Il est possible de chiffrer un bloc avec une méthode dite "EBC", qui permet de chiffrer un bloc plus long que ce que peut prendre la fonction encrypte.
On appelle la fonction `ecb_encrypt` . Mais cette technique de chiffrement n'est pas très efficace puisque elle code chaque bloc avec la même clé.
Une autre technique de chiffrement est le CBC, qui "xore" chaque bloc obtenu avec la clé initiale pour chiffrer le bloc suivant ( et le premier bloc est obtenu comme une combinaison xor de la clé et d'un vecteur initial ). On l'appelle avec la fonction `cbc_encrypt`.