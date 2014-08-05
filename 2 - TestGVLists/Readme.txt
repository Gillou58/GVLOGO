|========================================================|
|
|                  G V S O F T                                           |
|                  Projet : GVLogo                                       |
|                  Description : Traitement des listes                     |
|                  Unit� : GVLists.pas                                   |
|                  Ecrit par  : VASSEUR Gilles                           |
|                  e-mail : g.vasseur58@laposte.net                      |
|                  Copyright : � G. VASSEUR                              |
|                  Date:    19-06-2014 16:08:15                          |
|                  Version : 2.0.0                                       |
|                                                                        |
|========================================================|

//
// Unit� pour le traitement des listes
//
// ##############################################################
//
// LISTES SIMPLES
//
// 1. Un mot est un ensemble de caract�res autres que le caract�re blanc.
// 2. Une liste est un ensemble de mots plac�s entre crochets : [ et ].
// 3. Une liste peut comprendre d'autres listes.
// 4. Un �l�ment d'une liste est soit un mot soit une liste.
// 5. Il existe une liste qui ne comprend aucun �l�ment et qu'on appelle
// la liste vide.
//
// Exemples :
//
// [coucou] est une liste valide.
// [coucou [encore coucou]] est une liste valide.
// [coucou coucou2] est une liste valide.
// [[coucou]] est une liste valide.
// [[coucou] n'est pas une liste valide (il manque un crochet fermant).
//
// Les listes sont � la base du langage GVLogo.
// Elles permettent un traitement des cha�nes de caract�res � partir de
// primitives simples.
// Les lignes d'un programme en GVLogo sont consid�r�es comme des listes.
//
// On notera que les �l�ments sont num�rot�s de 0 � Count-1.
// Cette fa�on de compter est conforme aux listes de Pascal.
//

Le logiciel de test fonctionne avec des versions r�centes de :
* Delphi (Win32 et Win64)
* Lazarus (Win32 et Linux)

Le dossier Length contient un programme � tester avec Lazarus. Il montre les probl�mes li�s � la reconnaissance des caract�res accentu�s.