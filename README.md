# PX222 IRC -- AES

Ce repo contient les notes et le code produits dans le cadre du PX222
enseigné à l'ESISAR pendant l'année 2022-2023, visant à la découverte
et implémentation de [l'algorithme AES](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) défini dans le standard [FIPS 197](https://nvlpubs.nist.gov/nistpubs/fips/nist.fips.197.pdf).

L'objectif est de prototyper une implémentation "mathématique" d'AES
en haskell, en faisant usage de la fléxibilité du système de type du
language. Dans un second temps, une implémentation en C a visée plus
"technique," sera créée.

Dans les deux cas, le produit final sera une librairie exposant une
poignée de fonctions permettant d'encrypter ou décrypter une séquence
de blocs de 128 bits à l'aide d'une clé de 128, 192 ou 256 bits.
Celle-ci sera également accompagnée d'une documentation telle qu'on
pourrait en trouver pour n'importe quelle autre libraire.

En plus du code, le projet de PX222 requiert la production d'un
"carnet de bord," faisant état des objectifs et produits de chaque
séances. Celui-ci peut être retrouvé dans [carnet-de-bord.md]

Projet réalisé par [Zoë Courvoisier (blokyk)](github.com/blokyk) et
[Léa Tavier (lelemeline)](github.com/lelemeline), sous la license
[GNU LGPL v3.0](LICENSE.md)
