#lang COMP360_FinalProject
(room wake
  (desc "You awake underwater, lungs full of water and yet somehow not drowning. The water is cold and silent and you can see the pale light filtering through cracks above. Something is wrong, and you need to find your way back to the surface.")
  (item rusted-dagger
    (desc "A rusted dagger, worn but still sharp enough to harm anything that approaches.")
    (type weapon))
  (power 10)
  (exit north maze1)
  (exit east kelp))

(room kelp
  (desc "Tall forests of kelp block your vision, swaying and tickling your skin. A shudder runs down you as you turn your head and see eyes peering through the green. Immediately you look away praying it was a hallucination and spot kelp-wrap drifting in the current.")
  (item kelp-wrap
    (desc "A long strip of kelp, surprisingly strong. Could be used for armor or binding.")
    (type armor))
  (power 5)
  (exit west wake)
  (exit north maze2))

(room maze1
  (desc "Flooded stone corridors greet you, stretching in every direction. As you peer closer you notice that every inch of the walls are covered in strange markings and drawings. Something brushes your skin before you can look closer and you jump.")
  (monster eel 25)
  (exit south wake)
  (exit north maze2)
  (exit east maze3))

(room maze2
  (desc "You drift through a chamber that looks vaguely familiar, confused you look around trying to remember when you could have been here. Distracted you bump into something that falls and makes a loud metal clang.")
  (item broken-helmet
    (desc "An old iron helmet, cracked in several places. Provides minimal protection, but better than nothing.")
    (type armor))
  (power 3)
  (exit south maze1)
  (exit west kelp)
  (exit east maze4))

(room maze3
  (desc "A narrow passage forces you sideways, fighting for your breath between jagged stones. Bubbles rise from the cracks in the wall but disappear before they reach the surface. In the rubble you spot another blade.")
  (monster crab 15)
  (item coral-blade
    (desc "A blade formed from coral, sturdy and sharp. Slightly magical in the way it glows.")
    (type weapon))
  (power 20)
  (exit west maze1)
  (exit north maze4))

(room maze4
  (desc "The pressure increases here. You should be dead by now, but somehow you are not. You see movement out of the corner of your eye, something large watching you.")
  (exit west maze2)
  (exit south maze3)
  (exit north selkie-lair (key coral-blade)))

(room selkie-lair
  (desc "A hidden chamber opens suddenly. There in front of you is a creature that would seem almost human if it were not for the mass of seal skin laying next to her.")
  (item seal-skin
    (desc "The selkie's own seal-skin. A piece vital if you want a chance of defeating her.")
    (type misc))
  (power 0)
  (exit south maze4)
  (exit north abyss (key seal-skin)))

(room abyss
  (desc "The dungeon falls away into an endless underwater abyss. There is no floor, and your legs are hovering treading water. The selkie approaches slowly, eyes trained on you. Whatever happens next determines if you make it to the surface.")
  (monster deep-thing 50))
