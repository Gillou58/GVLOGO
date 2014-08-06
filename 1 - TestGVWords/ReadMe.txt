|========================================================|
|
|                  G V S O F T                                           |
|                  Projet : GVLogo                                       |
|                  Description : Traitement des mots                     |
|                  Unité : GVWords.pas                                   |
|                  Ecrit par  : VASSEUR Gilles                           |
|                  e-mail : g.vasseur58@laposte.net                      |
|                  Copyright : © G. VASSEUR                              |
|                  Date:    06-08-2014                         |
|                  Version : 1.0.0                                       |
|                                                                        |
|========================================================|

//
// classe TGVWord pour GVLOGO
//
// ##############################################################
//
// L'unité regroupe la classe chargée de traiter les mots du
// projet GVLOGO.
//
// MOTS
//
// Les mots en GVLOGO sont définis comme une suite de
// caractères ne comprenant pas d'espace.
//
// CARACTERE D'ECHAPPEMENT
//
// Le caractère d'échappement ("$" par défaut) permet
// d'inclure un blanc
// dans un mot, le caractère d'échappement lui-même ou tout
// caractère
// interdit car utilisé par l'interpréteur : "[", "]", "(",
// ")" et " ".
// Ce caractère permet donc de ne plus considérer le caractère
// suivant comme un séparateur.
//
//

Le logiciel de test fonctionne avec des versions récentes de :
* Delphi (Win32 et Win64)
* Lazarus (Win32 et Linux)

________________________________________________________

L'ensemble est protégé par la licence GNU GPL :

    TestGVWords - partie de GVLOGO
    Copyright (C) 2014 Gilles VASSEUR

    This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
