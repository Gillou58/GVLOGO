|========================================================|
|
|                  G V S O F T                                           |
|                  Projet : GVLogo                                       |
|                  Description : Traitement des mots                     |
|                  Unit� : GVWords.pas                                   |
|                  Ecrit par  : VASSEUR Gilles                           |
|                  e-mail : g.vasseur58@laposte.net                      |
|                  Copyright : � G. VASSEUR                              |
|                  Date:    06-08-2014                         |
|                  Version : 1.0.0                                       |
|                                                                        |
|========================================================|

//
// classe TGVWord pour GVLOGO
//
// ##############################################################
//
// L'unit� regroupe la classe charg�e de traiter les mots du
// projet GVLOGO.
//
// MOTS
//
// Les mots en GVLOGO sont d�finis comme une suite de
// caract�res ne comprenant pas d'espace.
//
// CARACTERE D'ECHAPPEMENT
//
// Le caract�re d'�chappement ("$" par d�faut) permet
// d'inclure un blanc
// dans un mot, le caract�re d'�chappement lui-m�me ou tout
// caract�re
// interdit car utilis� par l'interpr�teur : "[", "]", "(",
// ")" et " ".
// Ce caract�re permet donc de ne plus consid�rer le caract�re
// suivant comme un s�parateur.
//
//

Le logiciel de test fonctionne avec des versions r�centes de :
* Delphi (Win32 et Win64)
* Lazarus (Win32 et Linux)

________________________________________________________

L'ensemble est prot�g� par la licence GNU GPL :

    TestGVWords - partie de GVLOGO
    Copyright (C) 2014 Gilles VASSEUR

    This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
