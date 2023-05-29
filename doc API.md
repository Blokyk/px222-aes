# Description
--- 
La librairie lib-chloride permet d'encrypter/ décrypter une liste de Byte en utilisant la méthode AES, ou avec différentes méthodes s'appuyant sur l'AES ( encryption EBC, CBC..)

## Exemple d'utilisation de la lib-chloride
``` haskell
    import Byte
    import Cipher
    do 
        let block = concatMap (byteFromInt) [0x32, 0x43, 0xf6, 0xa8, 0x88, 0x5a, 0x30, 0x8d, 0x31, 0x31, 0x98, 0xa2, 0xe0, 0x37,0x07, 0x34 ]
        let key   = concatMap (byteFromInt) [0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf,0x4f, 0x3c ]
        let res = encrypt key block
        putStrLn $ showByteBlock res 
    -- Pour décrypter : 
        let inv = decrypt key inv
        putStrLn $ showByteBlock inv 

```
Dans cet exemple, on commence par créer un bloc qu'on va encrypter et décrypter et une clé. On les passe du format hexadécimal au format byte avec un map de la fonction byteFromInt . 
La variable res est le résultat de l'encryption. Elle se fait à l'aide de la fonction encrypt qui prend deux paramètres, la clé et le bloc; pour décrypter, c'est la même logique puisqu'il ya une fonction decrypt qui prends deux arguments, la clé et le bloc.

## Autres types de chiffrement 

Il est possible de chiffrer un bloc avec une méthode dite "EBC", qui permet de chiffrer un bloc plus long que ce que peut prendre la fonction encrypte.
On appelle la fonction `ecb_encrypt` . Mais cette technique de chiffrement n'est pas très efficace puisque elle code chaque bloc avec la même clé.
Une autre technique de chiffrement est le CBC, qui XOR chaque bloc obtenu avec la clé initiale pour chiffrer le bloc suivant ( et le premier bloc est obtenu comme une combinaison xor de la clé et d'un vecteur initial ). On l'appelle avec la fonction `cbc_encrypt`.