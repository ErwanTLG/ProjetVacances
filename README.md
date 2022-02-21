# Projet vacances hiver

- Compiler le projet: `dune build`
- Exécuter le jeu: `dune exec chess`
> Arguments CLI (facultatifs) : 
> - `-ia <difficulté : int>` active l'ia avec la difficulté sélectionnée (fonctionnalité en cours de development)
> - `-ia_def <bool>` spécifie si l'ia joue en tant que défenseur (`true`) ou attaquant (`false`). Défaut : `true` 

Erreurs connues :
- IA non testée, sans doute complètement buggée
- Déplacements pas tous légaux...

## Règles du jeu :

- Le jeu se joue à deux joueurs, un attaquant et un défenseur.
- Chaque joueur joue l'un après l'autre. Ils disposent de différentes pièces, parfois en commun,
  parfois propre à leur camp. Les pièces ont toutes des caractéristiques distinctes, que ce soit
  leur façon de se déplacer, comme leur manière de réduire à néant l'ennemi.

### Conditions de victoire :

- Si l'attaquant veut gagner il peut :
  - [x] Prendre la vie du Souverain adverse 
  - [ ] Prendre possession des 3 points cruciaux 
  - [ ] Ramener le butin de l'adversaire dans sa base 

- Si le défenseur veut gagner, il peut :
  - [x] Prendre la vie du général adverse 
  - [x] Réduire les troupes des agresseurs sous un seuil de 5 pièces (inclus)
  
### Le Terrain :

- [x] Cette carte est un octogone comprit dans un carré de 32 cases de côtés 
- [x] La bataille décrit l'attaque d'une forteresse, celle-ci est donc placée au centre de la carte. 
- [x] La particularité de cette carte est qu'elle est pourvue d'une flore jouant un rôle central. 
- [ ] Rochers, arbres et même fleuves sont disposés aléatoirement créant un décor de jeu changeant à chaque partie.

### Les Pièces de jeu :

- Pièces faibles
  - [x] Unité → Des déplacements classiques pour une pièce bien trop souvent sous-estimée.
  - [ ] Bouclier → Doué simplement de mouvements, il est un obstacle mobile protégeant la pièce qui le suit.
  - [ ] Chambellan → Piece unique du défenseur, porteuse du butin.
  - [ ] Bombardier → Capable de lancer une bombe qui cause sa mort, celle-ci explose sur un large rayon, prenant avec
                      lui ennemis, alliés et surtout les murs du chateau. Les plus chanceux ne seront que repoussés.

- Pièces intermédiaires
  - [ ] Lancier → Une grande allonge et une possibilité de tirer sa lance pour finalement redevenir simple unité.
  - [x] Cavalier → Une très grande mobilité et un large choix de positionnement.
  - [ ] Archer → Postés sur le chateau, ils tirent a longue distance, les décors les encombrent.
  - [ ] Ouvrier → Capables seulement de créer des unités pour la défense, et de reboucher les brèches des murs.

- Pièces fortes
  - [x] Officier → Des déplacements plus larges pour une force de frappe assurée, permettent le contrôle des points cruciaux.
  - [x] General → Doit rester sur sa position de base ou y retourné, s'il veut jouer le contrôle des camps cruciaux.
  - [ ] Souverain → Très peu de mobilité, il ne peut être tué que s'il est mis en danger par 4 pièces faible,
                     deux pièces intermédiaires ou 1 pièce forte.
  - [x] Garde → Forces de défenses direct du Souverain, un mur très solide à passer si l'on veut jouer la mort du Souverain.

# Représentation du plateau

## Terrain
Les cases du terrain sont représentées à l'aide de simples caractères, chaque case terrain possède un effet différent.\

|  Type  | Dessin | Déplacement | Bloque ligne de vue |
|:------:|:------:|:-----------:|:-------------------:|
|  Eau   |  ` `   |     [ ]     |         [ ]         |
|  Sol   |  `.`   |     [x]     |         [ ]         |
|  Mur   |  `#`   |     [ ]     |         [x]         |
| Arbre  |  `*`   |     [ ]     |         [x]         |
| Rocher |  `@`   |     [ ]     |         [ ]         |
|  Camp  |  `^`   |     [x]     |         [ ]         |

# Partie programmation

Tous les tableaux sont représentés sous forme de tableaux en lignes.
Il y a actuellement de gros problèmes avec l'affichage des unités.
