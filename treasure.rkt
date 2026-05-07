#lang COMP360_FinalProject

(health 100) ; player starts with 100 health
(room cave
  (desc "A dark cave dripping with water")
  (monster bat 15)
  (exit north forest))

(room forest
  (desc "Dense trees surround you")
  (item sword
    (desc "A sharp sword")
    (type weapon))
  (monster wolf 30)
  (exit south cave)
  (exit west treasure))

(room treasure
  (desc "You found the treasure room. Gold glitters everywhere.")
  (exit east vault))

(room vault
  (desc "A sealed vault. There is no way out. You have won."))