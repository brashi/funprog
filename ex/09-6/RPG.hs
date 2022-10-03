module RPG where

-- Improvise!  A Character could have, for example:
-- a Name, a Race, a Class, a Level, XP (experience points),
-- 6 attributes:
--   Strength Intelligence Wisdom Dexterity Constitution Charisma
-- current and total HP (hit points)
-- current and total MP (mana points)
-- current and total GP (gold pieces)
data Character =
   Character {name::String, race::String, classRPG::String,
              level::Int, xp::Int, hp::Int, mp::Int, gp::Int,
              attrs::Attributes} deriving (Show)

data Attributes =
    Attributes {strength::Int, intelligence::Int, wisdom::Int, dexterity::Int, constitution::Int, charisma::Int} deriving(Show)

-- does that make sense?
type Party = [Character]
-- A new character and party
party = [char1]
char1Attrs = Attributes 5 9 9 6 3 6
char2Attrs = Attributes 10 8 6 7 12 5
char1 = Character "Edward Mountgrande" "Human" "Mage" 4 0 120 340 85 char1Attrs
char2 = Character "Saint Alexander Austrius" "Human" "Paladin" 8 21 430 200 203 char2Attrs

-- gets a character and returns one that is the same but +1 level
gainLevel :: Character -> Character
gainLevel cchar = cchar  { level = level cchar + 1 }

-- to be used when a character is hit
hitCharacter :: Character -> Int -> Character
hitCharacter cchar hit = cchar { hp = hp cchar - hit}

alive :: Character -> Bool
alive cchar = hp cchar > 0


-- How would you implement skills and spells?

data Skill =
    Skill {skillClass::String, skillName::String, skillEffect::String, skillValue::Int} deriving(Show)

skill1 = Skill "Mage" "Alchemy" "Potion crafting skill" 1
skill2 = Skill "Paladin" "Swordsman" "Sword fighting skill" 1
skill3 = Skill "Paladin" "Shield" "Shield wield skill" 1

data Spell
    = Spell {spellClass::String, spellname::String, spellEffect::String, spellValue::Int} deriving(Show)

spell1 = Spell "Mage" "Fireball" "Damages target with Fire DMG" 15
spell2 = Spell "Mage" "Heal" "Heals Target HP" 10
spell3 = Spell "Paladin" "Holy Light" "Damages target with Holy DMG" 12
spell4 = Spell "Paladin" "Shield Bash" "Damages target with a powerfull bash" 10

availableSkills = [skill1, skill2, skill3]
availableSpells = [spell1, spell2, spell3, spell4]

skills :: Character -> [Skill]
skills cchar = [x | x <- availableSkills, skillClass x == classRPG cchar]
spells :: Character -> [Spell]
spells cchar = [x | x <- availableSpells, spellClass x == classRPG cchar]
