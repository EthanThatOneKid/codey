
DIM Username AS STRING
DIM Imput AS STRING

INPUT "Hello, my name is Codey. What is yours?   ", Username
CLS
PRINT "Well, hello then, "; Username

ORIGINAL:
CLS
COLOR 3:
PRINT "   _________                "
PRINT "  /         \      ______   "
PRINT " /   Press   \    /      \  "
PRINT "|   Spacebar  |  |        | "
PRINT "|   to see    |  |  0  0  | "
PRINT " \ my options/   |   __   | "
PRINT "  \_________/     \______/  "
COLOR 31: PRINT "Press the Spacebar to see my options..."
COLOR 18: PRINT "Press Esc to end the program..."
DO
    DO: K$ = UCASE$(INKEY$)
    LOOP UNTIL K$ = CHR$(63) OR K$ = CHR$(32)
    IF K$ = CHR$(27) THEN END
    IF K$ = CHR$(32) THEN GOTO OPTIONS:
LOOP

OPTIONS:
CLS
COLOR 15:
PRINT "   _________                "
PRINT "  /         \      ______   "
PRINT " /   These   \    /      \  "
PRINT "|     are     |  |        | "
PRINT "|  my options |  |  0  0  | "
PRINT " \           /   |   __   | "
PRINT "  \_________/     \______/  "
PRINT "Pick an option, "; Username
PRINT "It is currenty"
PRINT Clock$
PRINT DATE$
PRINT
PRINT
COLOR 9: PRINT "Key 'P' to play on my digital arcade..."
PRINT
COLOR 10: PRINT "Key 'C' to use the calculator..."
PRINT
COLOR 14: PRINT "Key 'S' to use soundboard..."
PRINT
COLOR 13: PRINT "Key 'G' to use graph..."
PRINT
COLOR 11: PRINT "Press the spacebar to talk with me..."
PRINT
COLOR 4: PRINT "Key Esc to end program..."
DO
    DO: K$ = UCASE$(INKEY$)
    LOOP UNTIL K$ = "O" OR K$ = "E" OR K$ = "P" OR K$ = "C" OR K$ = "S" OR K$ = "G" OR K$ = CHR$(27) OR K$ = CHR$(32)

    IF K$ = CHR$(27) THEN END
    IF K$ = "O" THEN GOTO OPTIONS:
    IF K$ = "P" THEN GOTO Gamesmenu:
    IF K$ = "C" THEN GOTO Calculator:
    IF K$ = "S" THEN GOTO Soundboard:
    IF K$ = CHR$(27) THEN END
    IF K$ = "G" THEN GOTO Graph:
    IF K$ = CHR$(32) THEN GOTO Conversation:
LOOP

Calculator:
5 CLS
PRINT "1) ADD"
PRINT "2) MULTIPLY"
PRINT "3) DIVIDE"
PRINT "4) SQUARE ROOT"
PRINT "5) EXIT"
INPUT I
IF I = 1 GOTO 10
IF I = 2 GOTO 20
IF I = 3 GOTO 30
IF I = 4 GOTO 40
IF I = 5 GOTO 50
END
10 CLS: PRINT "INPUT NUMBER"
INPUT A
CLS
PRINT "INPUT NEXT NUMBER"
INPUT B
CLS
PRINT A + B
PRINT "PRESS SPACE TO RUN"
SLEEP 0
PRINT "Continue calculating, "; Username
PRINT "Y/N"
IF K$ = "Y" THEN GOTO 5
IF K$ = "N" THEN GOTO OPTIONS:

20 CLS: PRINT "INPUT NUMBER"
INPUT C
CLS
PRINT "INPUT NEXT NUMBER"
INPUT D
CLS
PRINT C * D
PRINT "PRESS SPACE TO RUN"
SLEEP 0
PRINT "Continue calculating, "; Username
PRINT "Y/N"
IF K$ = "Y" THEN GOTO 5
IF K$ = "N" THEN GOTO OPTIONS:

30 CLS: PRINT "INPUT NUMBER"
INPUT E
CLS
PRINT "INPUT NEXT NUMBER"
INPUT F
CLS
PRINT E / F
PRINT "PRESS SPACE TO RUN"
SLEEP 0
PRINT "Continue calculating, "; Username
PRINT "Y/N"
IF K$ = "Y" THEN GOTO 5
IF K$ = "N" THEN GOTO OPTIONS:

40 CLS: PRINT "INPUT NUMBER"
INPUT G
CLS
PRINT SQR(G)
PRINT "PRESS SPACE TO RUN"
SLEEP 0
PRINT "Continue calculating, "; Username
PRINT "Y/N"
IF K$ = "Y" THEN GOTO 5
IF K$ = "N" THEN GOTO OPTIONS:

50 END

Soundboard:
CLS
COLOR 15:
PRINT "Press A to play A note..."
PRINT
PRINT "Press B to play B note..."
PRINT
PRINT "Press C to play C note..."
PRINT
PRINT "Press D to play D note..."
PRINT
PRINT "Press E to play E note..."
PRINT
PRINT "Press F to play F note..."
PRINT
PRINT "Press G to play G note..."
PRINT
PRINT "Press Esc to exit Soundboard..."
DO
    DO: K$ = UCASE$(INKEY$)
    LOOP UNTIL K$ = "A" OR K$ = "B" OR K$ = "C" OR K$ = "D" OR K$ = "E" OR K$ = "F" OR K$ = "G" OR K$ = CHR$(27)
    PRINT K$
    IF K$ = "E" THEN GOTO E:
    IF K$ = "A" THEN GOTO A:
    IF K$ = "B" THEN GOTO B:
    IF K$ = "C" THEN GOTO C:
    IF K$ = "D" THEN GOTO D:
    IF K$ = "G" THEN GOTO G:
    IF K$ = "F" THEN GOTO F:
    IF K$ = CHR$(27) THEN GOTO OPTIONS:
LOOP
E:
CLS
PLAY "E"
GOTO Soundboard:
A:
CLS
PLAY "A"
GOTO Soundboard:
B:
CLS
PLAY "B"
GOTO Soundboard:
C:
CLS
PLAY "C"
GOTO Soundboard:
D:
CLS
PLAY "D"
GOTO Soundboard:
G:
CLS
PLAY "G"
GOTO Soundboard:
F:
CLS
PLAY "F"
GOTO Soundboard:

Graph:

SCREEN 0, 0, 0
CLS
WIDTH 40, 25
COLOR 10: PRINT "The slope formula is y = m(x) +/- b."
PRINT "Add(Positive) or Subtract(negative) m of first slope? A or S?"
DO: K$ = UCASE$(INKEY$)
LOOP UNTIL K$ = "A" OR K$ = "S"
IF K$ = "A" THEN GOTO Add1:
IF K$ = "S" THEN GOTO Subtract1:

Add1:
CLS
COLOR 10: PRINT "The slope formula is y = m(x) +/- b."
INPUT "Enter first term for m. ", m
INPUT "Over? ", C
m2 = m / C
GOTO start2:

Subtract1:
CLS
COLOR 10: PRINT "The slope formula is y = m(x) +/- b."
INPUT "Enter first term for m. ", m
INPUT "Over? ", D
rand3 = m / D
m3 = rand3 * -1
GOTO start3:

start2:
CLS
COLOR 10: PRINT "The slope formula is y = m(x) +/- b."
PRINT "Add(Positive) or Subtract(negative) b of first slope? A or S?"
DO: K$ = UCASE$(INKEY$)
LOOP UNTIL K$ = "A" OR K$ = "S"
IF K$ = "A" THEN GOTO Add2:
IF K$ = "S" THEN GOTO Subtract2:

start3:
CLS
COLOR 10: PRINT "The slope formula is y = m(x) +/- b."
PRINT "Add(Positive) or Subtract(negative) b of first slope? A or S?"
DO: K$ = UCASE$(INKEY$)
LOOP UNTIL K$ = "A" OR K$ = "S"
IF K$ = "A" THEN GOTO Add3:
IF K$ = "S" THEN GOTO Subtract3:

Add2:
CLS
COLOR 10: PRINT "The slope formula is y = m(x) +/- b."
INPUT "Enter first term for b. ", B
b2 = B
GOTO Stats1:

Subtract2:
CLS
COLOR 10: PRINT "The slope formula is y = m(x) +/- b."
INPUT "Enter first term for b. ", B
b3 = B * -1
GOTO Stats2:

Add3:
CLS
COLOR 10: PRINT "The slope formula is y = m(x) +/- b."
INPUT "Enter first term for b. ", B
b4 = B
GOTO Stats3:

Subtract3:
CLS
COLOR 10: PRINT "The slope formula is y = m(x) +/- b."
INPUT "Enter first term for b. ", B
b5 = B * -1
GOTO Stats4:



Stats1:
CLS
SCREEN 13
size = 5
WINDOW (-size, -size)-(size, size)
LINE (-size, 0)-(size, 0), 15
LINE (0, -size)-(0, size), 15
FOR x = -size TO size
    LINE (-size, x)-(size, x), 8
    LINE (x, -size)-(x, size), 8
NEXT x
LINE (-size, 0)-(size, 0), 15
LINE (0, -size)-(0, size), 15
FOR x = -size TO size STEP .01
    y = x * m2 + b2

    PSET (x, y), 13

    LOCATE 20, 1: PRINT "Y Intercept is "; B
NEXT x
GOTO again
Stats2:
CLS
SCREEN 13
size = 5
WINDOW (-size, -size)-(size, size)
LINE (-size, 0)-(size, 0), 15
LINE (0, -size)-(0, size), 15
FOR x = -size TO size
    LINE (-size, x)-(size, x), 8
    LINE (x, -size)-(x, size), 8
NEXT x
LINE (-size, 0)-(size, 0), 15
LINE (0, -size)-(0, size), 15
FOR x = -size TO size STEP .01
    y = x * m2 + b3

    PSET (x, y), 13

    LOCATE 20, 1: PRINT "Y Intercept is "; B
NEXT x
GOTO again
Stats3:
CLS
SCREEN 13
size = 5
WINDOW (-size, -size)-(size, size)
LINE (-size, 0)-(size, 0), 15
LINE (0, -size)-(0, size), 15
FOR x = -size TO size
    LINE (-size, x)-(size, x), 8
    LINE (x, -size)-(x, size), 8
NEXT x
LINE (-size, 0)-(size, 0), 15
LINE (0, -size)-(0, size), 15
FOR x = -size TO size STEP .01
    y = x * m3 + b4

    PSET (x, y), 13

    LOCATE 20, 1: PRINT "Y Intercept is "; B
NEXT x
GOTO again
Stats4:
CLS
SCREEN 13
size = 5
WINDOW (-size, -size)-(size, size)
LINE (-size, 0)-(size, 0), 15
LINE (0, -size)-(0, size), 15
FOR x = -size TO size
    LINE (-size, x)-(size, x), 8
    LINE (x, -size)-(x, size), 8
NEXT x
LINE (-size, 0)-(size, 0), 15
LINE (0, -size)-(0, size), 15
FOR x = -size TO size STEP .01
    y = x * m3 + b5

    PSET (x, y), 13

    LOCATE 20, 1: PRINT "Y Intercept is "; B
NEXT x
GOTO again:
again:
LOCATE 23, 1: PRINT "New graph? (Y/N)"
DO
    a$ = UCASE$(INKEY$)
LOOP UNTIL a$ = "Y" OR a$ = "N"
IF a$ = "Y" THEN GOTO Graph:
IF a$ = "N" THEN
    SCREEN 0, 0, 0:
    WIDTH 80, 25
    CLS
END IF
IF a$ = "N" GOTO OPTIONS:
END

Conversation:
DIM UserInput AS STRING
DO
    COLOR 7:
    PRINT "Press Esc to go to Codey's main menu at any time"
    a$ = UCASE$(INKEY$)
    INPUT "Type something for me to respond to.   ", UserInput
    IF K$ = CHR$(27) THEN GOTO OPTIONS:
    IF UserInput = "I WANNA BE THE VERY BEST" THEN
        PRINT
        PRINT "   _________                "
        PRINT "  /         \      ______   "
        PRINT " /   Like    \    /      \  "
        PRINT "|   no one    |  |        | "
        PRINT "|  ever was   |  |  0  0  | "
        PRINT " \           /   |   __   | "
        PRINT "  \_________/     \______/  "
    END IF
    IF UserInput = "TO CATCH THEM IS MY TEST" THEN
        PRINT
        PRINT "   _________                "
        PRINT "  /         \      ______   "
        PRINT " / To train  \    /      \  "
        PRINT "|   them is   |  |        | "
        PRINT "|  my cause   |  |  0  0  | "
        PRINT " \           /   |   __   | "
        PRINT "  \_________/     \______/  "
    END IF
    IF UserInput = "POKEMON" THEN
        PRINT
        PRINT "   _________                "
        PRINT "  /         \      ______   "
        PRINT " /   Gotta   \    /      \  "
        PRINT "|   catchem   |  |        | "
        PRINT "|     all     |  |  0  0  | "
        PRINT " \           /   |   __   | "
        PRINT "  \_________/     \______/  "
    END IF
    IF UserInput = "SLEEP" THEN
        PRINT "It is already "
        PRINT Clock$
        PRINT "   _________                "
        PRINT "  /         \      ______   "
        PRINT " /   ZZZZZ   \    /      \  "
        PRINT "|   ZZZZZZZ   |  |        | "
        PRINT "|     ZZZ     |  |  -  -  | "
        PRINT " \(goodnight)/   |   __   | "
        PRINT "  \_________/     \______/  "
        SOUND 200, 20
        END
    END IF
    IF UserInput = "I WANT TO GO HOME" THEN GOTO OPTIONS:
    IF UserInput = "HOME" THEN GOTO OPTIONS:
    IF UserInput = "OPTIONS" THEN GOTO OPTIONS:
    IF UserInput = "MENU" THEN GOTO OPTIONS:
    IF UserInput = "GAMES" THEN GOTO Gamesmenu:
    IF UserInput = "PLAY" THEN GOTO Gamesmenu:
    IF UserInput = "CALCULATOR" THEN GOTO Calculator:
    IF UserInput = "CALCULATE" THEN GOTO Calculator:
    IF UserInput = "FART" THEN SOUND 100, 20
    IF UserInput = "END" THEN END
    IF UserInput = "END PROGRAM" THEN END
    IF UserInput = "FAVORITE MOVIE" THEN
        PRINT
        PRINT "   _________                "
        PRINT "  /         \      ______   "
        PRINT " /           \    /      \  "
        PRINT "|    Tron     |  |        | "
        PRINT "|             |  |  0  0  | "
        PRINT " \           /   |   __   | "
        PRINT "  \_________/     \______/  "
    END IF
    IF UserInput = "FAVORITE FOOD" THEN
        PRINT
        PRINT "   _________                "
        PRINT "  /         \      ______   "
        PRINT " /           \    /      \  "
        PRINT "|    Block    |  |        | "
        PRINT "|    Bytes    |  |  0  0  | "
        PRINT " \           /   |   __   | "
        PRINT "  \_________/     \______/  "
    END IF
    IF UserInput = "NAME" THEN
        PRINT "Your name is "; Username
        PRINT "   _________                "
        PRINT "  /         \      ______   "
        PRINT " /     My    \    /      \  "
        PRINT "|   name is   |  |        | "
        PRINT "|    Codey    |  |  0  0  | "
        PRINT " \           /   |   __   | "
        PRINT "  \_________/     \______/  "
    END IF
    IF UserInput = "FAVORITE GAME" THEN
        PRINT
        PRINT "   _________                "
        PRINT "  /         \      ______   "
        PRINT " /     I     \    /      \  "
        PRINT "|    like     |  |        | "
        PRINT "|    Atari    |  |  0  0  | "
        PRINT " \           /   |   __   | "
        PRINT "  \_________/     \______/  "
    END IF
    IF UserInput = "GRAPH" THEN GOTO Graph:
    IF UserInput = "TOWER TRIALS" THEN GOTO TowerTrials:
    IF UserInput = "NIBBLES" THEN GOTO Nibbles:
    IF UserInput = "HOW ARE YOU" THEN PRINT "I am good, how are you, "; Username
    IF UserInput = "DATE" THEN PRINT Clock$
    IF UserInput = "TIME" THEN PRINT Clock$
    IF UserInput = "CLOCK" THEN PRINT Clock$
LOOP

Gamesmenu:
CLS

COLOR 31: LOCATE 2, 2: PRINT "This is my Digital Arcade..."
COLOR 15: LOCATE 3, 2: PRINT STRING$(78, 196)
COLOR 25: LOCATE 4, 2: PRINT "Commands"
COLOR 23: LOCATE 9, 2: PRINT "N";: COLOR 7: PRINT "ibbles";: COLOR 15: PRINT "............ Navigate your snakes around, trying to eat up numbers"
COLOR 18: LOCATE 13, 2: PRINT "T";: COLOR 2: PRINT "ower Trials";: COLOR 15: PRINT ".................. Smite all the creatures in your trials"
COLOR 20: LOCATE 21, 2: PRINT "Esc";: COLOR 12: PRINT "................... press Esc to end the program"

DO
    K$ = INKEY$
    K$ = UCASE$(K$)
LOOP UNTIL K$ = "N" OR K$ = "T" OR K$ = "B" OR K$ = CHR$(27)
_SNDPLAY h&
IF K$ = "N" THEN GOTO Nibbles:
IF K$ = "T" THEN GOTO TowerTrials:
IF K$ = CHR$(27) THEN GOTO OPTIONS:

TowerTrials:
CONST TRUE2 = 1
CONST FALSE2 = 0

CLS
ask = question

IF ask = 1 THEN
    SOUND 100, 18.2
    SOUND 300, 18.2
    SOUND 200, 18.2
    SOUND 500, 18.5
    SOUND 200, 20
    SOUND 0, 18.2
    SOUND 300, 6
    SOUND 800, 18.5
END IF

STARTALLOVER:
COLOR 12, 0: PRINT TAB(23); "Welcome to  T O W E R  T R I A L S"

COLOR 5: PRINT "Welcome to this game. From here you will solve your way through the game until  completion is reached... (Make sure that everything you type should be in CAPS)"
COLOR 31, 1: PRINT "(WARNING: this game is very confusing and requires thought and patience. PLAYER DISGRESSION IS ADVIZED)"
COLOR 10, 0: PRINT "Darn! I forgot my glasses in my other cloak. Can you tell me if you are a boy or a girl"

DO
    INPUT pASSWORD$
    IF pASSWORD$ = "GIRL" THEN GOTO GIRL:
    IF pASSWORD$ = "BOY" THEN GOTO BOY: ELSE PRINT "I don't understand..."

LOOP UNTIL GETOUT = 1

BOY:
PRINT "Alright! Let's venture yonder, champ!"
GOTO FIRSTRIDDLE:

GIRL:
PRINT "Oh. Now I recognize you! Alright! Follow me, milady..."

FIRSTRIDDLE:
COLOR 4: PRINT "Okay, let's start with a mathematical riddle."
COLOR 12: PRINT "If 2=6, 3=12, 4=20, what does 5 equal?"
LET GETOUT = 0
DO
    INPUT pASSWORD$
    IF pASSWORD$ = "30" THEN GETOUT = 1: PRINT "Good thinking! You're just the kind of person I'm looking for!" ELSE PRINT "Try again please..."
LOOP UNTIL GETOUT = 1
COLOR 2: PRINT "Let's move on..."

COLOR 6: PRINT "Let's go on our way through town to the tower."
PRINT "There is a gate to the place we are visiting and the guardsman asks for a passcode to get in."

COLOR 3: PRINT "The guardsman says... 'What is the answer to 2(5+7) 1 + 2 x 4'"

LET GETOUT = 0
DO
    INPUT pASSWORD$
    IF pASSWORD$ = "30" THEN GETOUT = 1: PRINT "You shall pass, brilliant one." ELSE PRINT "Please try again, your answer is incorrect."
LOOP UNTIL GETOUT = 1

COLOR 8: PRINT "This is a palace in which there are 3 floors. Each floor contains a trial. But, only on the roof, will be your final trial and crowning moment."

FIRSTTRIAL:
COLOR 4: PRINT "HERE IS YOUR FIRST TRIAL!!! (An experienced witch flies down with 500 health points.Your magic deals 25% damage of what the witch already has and your punch deals 100 health points. Your objctive is to slay the witch by choosing either MAGIC or PUNCH to attack)"

LET GETOUT = 0
LET Life = 500
LET Witch = 500

COLOR 6:
DO
    INPUT pASSWORD$
    IF pASSWORD$ = "MAGIC" THEN
        Witch = Witch * .25
        Life = Life - 110

        COLOR 2: PRINT "You did 25% damage of what the witch already has! You got hit by the witch's magical staff's WACK!"
        PRINT "You currntly have"; Life
        PRINT "life points."
        PRINT "The witch still has"; Witch
        PRINT "life points. Make another move. Your options are MAGIC, or PUNCH."

    ELSE PRINT "You punched 100 damage! You got hit by the witch's magical staff's flame bolt."
        Life = Life - 110
        Witch = Witch - 100

        PRINT "You currently have"; Life
        PRINT "life points."
        PRINT "The witch still has"; Witch
        PRINT "life points. Make another move. Your options are MAGIC, or PUNCH."

    END IF
    IF Witch <= 0 OR Life <= 0 THEN GETOUT = 1
LOOP UNTIL GETOUT = 1
IF Witch <= 0 THEN PRINT "You defeated the witch!"
IF Life <= 0 THEN PRINT "You got annihilated by the witch and have been slain."

Decision = StillWantsToPlay2
IF Life <= 0 AND Decision = TRUE2 THEN GOTO STARTALLOVER:
IF Decision = FALSE2 THEN END

COLOR 0, 0
COLOR 2: PRINT "Hoo-ray!!! You have defeated the witch. It's time to go to to the next floor for trial 2."
COLOR 6: PRINT "(As you step up to the second trial, you see a minotaur inside a cage) Here is your second trial... Defeat the MINOTAUR!!!"
COLOR 14: PRINT "(You have earned a new power and your magic has improved! Your options are MAGIC: does 50% damage of what the minotaur currently has; and SWORD that does 300 damage to the minotaur health points. YOU BOTH HAVE 1,000 HEALTH POINTS"

LET GETOUT = 0
LET Life = 1000
LET Minotaur = 1000

COLOR 10:
DO
    INPUT pASSWORD$
    IF pASSWORD$ = "MAGIC" THEN
        Minotaur = Minotaur * .50
        Life = Life - 230

        PRINT "You did 50% damage! You got hit by the minotaur's RAM"
        PRINT "You currently have"; Life
        PRINT "life points."
        PRINT "The minotaur still has"; Minotaur
        PRINT "life points. Make another move. Your options are MAGIC, or SWORD."

    ELSE
        PRINT "You hacked 300 damage! You got hit by the minotaur's GIGA IMPACT!"
        Life = Life - 230
        Minotaur = Minotaur - 300

        PRINT "You currently have"; Life
        PRINT "life points."
        PRINT "The minotaur still has"; Minotaur
        PRINT "life points. Make another move. Your options are MAGIC, or SWORD."

    END IF
    IF Minotaur <= 0 OR Life <= 0 THEN GETOUT = 1
LOOP UNTIL GETOUT = 1

COLOR 20, 7:
IF Minotaur <= 0 THEN PRINT "You defeated the minotaur!"
IF Life <= 0 THEN PRINT "You got annihilated by the minotaur and have been slain."

Decision = StillWantsToPlay2
IF Life <= 0 AND Decision = TRUE2 THEN GOTO STARTALLOVER:
IF Decision = FALSE2 THEN GOTO OPTIONS:

COLOR 3, 0: PRINT "We are heading to the third floor for your final trial. How do you feel?"
INPUT pASSWORD$
IF pASSWORD$ = "GOOD" THEN PRINT "It's all good, just remember what you have learned." ELSE PRINT "It's all good, just remember what you have learned."

PRINT "We are heading towards the third floor now."

COLOR 15: PRINT "(The dragon awits your arrival) It says: I SHALL DISINTIGRATE YOU!!!"

LET GETOUT = 0
LET Life = 10000
LET Dragon = 10000

PRINT "Make your first move. Your options are MAGIC, or SWORD."
COLOR 10:
DO
    INPUT pASSWORD$
    IF pASSWORD$ = "MAGIC" THEN
        Dragon = Minotaur * .50
        Life = Life - 1000

        PRINT "You did 50% damage! You got hit by the dragon's DRILL RUN!!!"
        PRINT "You currently have"; Life
        PRINT "life points."
        PRINT "The dragon still has"; Dragon
        PRINT "life points. Make another move. Your options are MAGIC, or SWORD."

    ELSE
        PRINT "You hacked 300 damage! You got hit by the dragon's FLARE CRASH!!!"
        Life = Life - 1000
        Dragon = Dragon - 300
        PRINT "You currently have"; Life
        PRINT "life points."
        PRINT "The dragon still has"; Dragon
        PRINT "life points. Make another move. Your options are MAGIC, or SWORD."

    END IF
    IF Dragon <= 0 OR Life <= 0 THEN GETOUT = 1
LOOP UNTIL GETOUT = 1
COLOR 20, 7:
IF Dragon <= 0 THEN PRINT "You defeated the dragon!"
COLOR 12, 0: PRINT TAB(23); "C O N G R A D U L A T I O N S ! ! ! You have won T O W E R  T R I A L S"
IF Life <= 0 THEN PRINT "You got annihilated by the dragon and have been slain. You shall start over..."

Decision = StillWantsToPlay2
IF Life <= 0 AND Decision = TRUE2 THEN GOTO STARTALLOVER:
IF Decision = FALSE2 THEN GOTO OPTIONS:


COLOR 5: PRINT "(Word qickly spread of the victor of all three trials at the tower. It has never been done by any other challenger, wizard, nor even royal blood had completed the challenge.) This is your ceremony, on the roof of the tower. As promised"
COLOR 8: PRINT "You hear the king say: ... and here you are, your medal to show that you are the first in all the kingdom to complete the legendary challenge and I hereby claim you as CHAMPION of our kingd---"
COLOR 4: PRINT "(A rumble is felt on the ground and the distance you see a huge giant, as tall as the tower, coming to crash your party)"
COLOR 5: PRINT "It is only you who can save us all from the marauding giant. Your SWORD has been upgraded to FLAMING SWORD: does 1,000 damage; MAGIC: does 50% damage of what the marauding giant currently has. You both have 10,000 life points."
COLOR 14: PRINT "Your options are MAGIC, or FLAMING SWORD"

LET GETOUT = 0
LET Life = 10000
LET giant = 10000

COLOR 1:
DO
    INPUT pASSWORD$
    IF pASSWORD$ = "MAGIC" THEN
        giant = giant * .50
        Life = Life - 1500

        PRINT "You did 50% damage! You got hit by the marauding giant's CRASH!"
        PRINT "You currently have"; Life
        PRINT "life points."
        PRINT "The marauding giant still has"; giant
        PRINT "life points. Make another move. Your options are MAGIC, or SWORD."

        COLOR 20, 7: ELSE PRINT "You hacked 1000 damage! You got hit by the marauding giant's HEAD SMASH!"
        Life = Life - 1500
        giant = giant - 1000

        PRINT "You currently have"; Life
        PRINT "life points."
        PRINT "The marauding giant still has"; giant
        PRINT "life points remaining. Make another move. Your options are MAGIC, or SWORD."

    END IF
    IF giant <= 0 OR Life <= 0 THEN GETOUT = 1
LOOP UNTIL GETOUT = 1
COLOR 4:
IF giant <= 0 THEN PRINT "You defeated the marauding giant in front of everybody! Good work, champion!"
IF Life <= 0 THEN PRINT "You got annihilated by the marauding giant and have been slain. You shall restart at the first trial..."
IF Life <= 0 THEN GOTO FIRSTTRIAL:

COLOR 17, 15: PRINT "You have won Tower Trials! Please play again soon."

COLOR 15, 0: PRINT "CREDITS"
COLOR 7: PRINT "Helped by Gregory Davidson and Albert Davidson. Everything else: Ethan Davidson."


COLOR 10, 0: PRINT "THE END... or to be continued"

END

Nibbles:
CLS
'[RDHY
'[E '
'                         Q B a s i c   N i b b l e s
'
'                   Copyright (C) Microsoft Corporation 1990
'
' Nibbles is a game for one or two players.  Navigate your snakes
' around the game board trying to eat up numbers while avoiding
' running into walls or other snakes.  The more numbers you eat up,
' the more points you gain and the longer your snake becomes.
'
' To run this game, press Shift+F5.
'
' To exit QBasic, press Alt, F, X.
'
' To get help on a BASIC keyword, move the cursor to the keyword and press
' F1 or click the right mouse button.
'


'Set default data type to integer for faster game play
DEFINT A-Z

'User-defined TYPEs
TYPE snakeBody
    row AS INTEGER
    col AS INTEGER
END TYPE

'This type defines the player's snake
TYPE snaketype
    head AS INTEGER
    length AS INTEGER
    row AS INTEGER
    col AS INTEGER
    direction AS INTEGER
    lives AS INTEGER
    score AS INTEGER
    scolor AS INTEGER
    alive AS INTEGER
END TYPE

'This type is used to represent the playing screen in memory
'It is used to simulate graphics in text mode, and has some interesting,
'and slightly advanced methods to increasing the speed of operation.
'Instead of the normal 80x25 text graphics using chr$(219) " ", we will be
'using chr$(220)"_" and chr$(223) " " and chr$(219) " " to mimic an 80x50
'pixel screen.
'Check out sub-programs SET and POINTISTHERE to see how this is implemented
'feel free to copy these (as well as arenaType and the DIM ARENA stmt and the
'initialization code in the DrawScreen subprogram) and use them in your own
'programs
TYPE arenaType
    realRow AS INTEGER 'Maps the 80x50 point into the real 80x25
    acolor AS INTEGER 'Stores the current color of the point
    sister AS INTEGER 'Each char has 2 points in it.  .SISTER is
END TYPE '-1 if sister point is above, +1 if below

'Sub Declarations
DECLARE SUB Intro ()
DECLARE SUB SpacePause (text$)
DECLARE SUB PrintScore (numplayers%, score1%, score2%, lives1%, lives2%)

DECLARE SUB GetInputs (numplayers, speed, diff$, monitor$)
DECLARE SUB DrawScreen ()
DECLARE SUB PlayNibbles (numplayers, speed, diff$)
DECLARE SUB Set (row, col, acolor)
DECLARE SUB Center (row, text$)
DECLARE SUB DoIntro ()
DECLARE SUB Initialize ()
DECLARE SUB SparklePause ()
DECLARE SUB Level (WhatToDO, sammy() AS snaketype)
DECLARE SUB InitColors ()
DECLARE SUB EraseSnake (snake() AS ANY, snakeBod() AS ANY, snakeNum%)
DECLARE FUNCTION StillWantsToPlay ()
DECLARE FUNCTION PointIsThere (row, col, backColor)

'Constants
'CONST TRUE = -1
'CONST FALSE = NOT TRUE
'CONST MAXSNAKELENGTH = 1000
'CONST STARTOVER = 1             ' Parameters to 'Level' SUB
'CONST SAMELEVEL = 2
'CONST NEXTLEVEL = 3
DIM SHARED TRUE AS INTEGER
DIM SHARED FALSE AS INTEGER
DIM SHARED MAXSNAKELENGTH AS INTEGER
DIM SHARED STARTOVER AS INTEGER
DIM SHARED SAMELEVEL AS INTEGER
DIM SHARED NEXTLEVEL AS INTEGER

TRUE = -1
FALSE = NOT TRUE
MAXSNAKELENGTH = 1000
STARTOVER = 1
SAMELEVEL = 2
NEXTLEVEL = 3


'Global Variables
DIM SHARED arena(1 TO 50, 1 TO 80) AS arenaType
DIM SHARED curLevel, colorTable(10)



RANDOMIZE TIMER
GOSUB ClearKeyLocks

Intro
GetInputs numplayers, speed, diff$, monitor$


GOSUB SetColors

DrawScreen



DO
    PlayNibbles numplayers, speed, diff$
LOOP WHILE StillWantsToPlay

GOSUB RestoreKeyLocks
COLOR 15, 0
CLS
END

ClearKeyLocks:
DEF SEG = 0 ' Turn off CapLock, NumLock and ScrollLock
KeyFlags = PEEK(1047)
POKE 1047, &H0
DEF SEG
RETURN

RestoreKeyLocks:
DEF SEG = 0 ' Restore CapLock, NumLock and ScrollLock states
POKE 1047, KeyFlags
DEF SEG
RETURN

SetColors:
IF monitor$ = "M" THEN
    RESTORE mono
ELSE
    RESTORE normal
END IF

FOR a = 1 TO 6
    READ colorTable(a)
NEXT a
RETURN

'snake1     snake2   Walls  Background  Dialogs-Fore  Back
mono: DATA 15,7,7,0,15,0
normal: DATA 14,13,12,1,15,4
END

'Center:
'  Centers text on given row
SUB Center (row, text$)
LOCATE row, 41 - LEN(text$) / 2
PRINT text$;
END SUB

'DrawScreen:
'  Draws playing field
SUB DrawScreen

'initialize screen
VIEW PRINT
COLOR colorTable(1), colorTable(4)
CLS

'Print title & message
Center 1, "Nibbles!"
Center 11, "Initializing Playing Field..."

'Initialize arena array
FOR row = 1 TO 50
    FOR col = 1 TO 80
        arena(row, col).realRow = INT((row + 1) / 2)
        arena(row, col).sister = (row MOD 2) * 2 - 1
    NEXT col
NEXT row

END SUB

'EraseSnake:
'  Erases snake to facilitate moving through playing field
SUB EraseSnake (snake() AS snaketype, snakeBod() AS snakeBody, snakeNum)

FOR c = 0 TO 9
    FOR b = snake(snakeNum).length - c TO 0 STEP -10
        tail = (snake(snakeNum).head + MAXSNAKELENGTH - b) MOD MAXSNAKELENGTH
        Set snakeBod(tail, snakeNum).row, snakeBod(tail, snakeNum).col, colorTable(4)
    NEXT b
NEXT c

END SUB

'GetInputs:
'  Gets player inputs
SUB GetInputs (numplayers, speed, diff$, monitor$)

COLOR 7, 0
CLS

DO
    LOCATE 5, 47: PRINT SPACE$(34);
    LOCATE 5, 20
    INPUT "How many players (1 or 2)"; num$
LOOP UNTIL VAL(num$) = 1 OR VAL(num$) = 2
numplayers = VAL(num$)

LOCATE 8, 21: PRINT "Skill level (1 to 100)"
LOCATE 9, 22: PRINT "1   = Novice"
LOCATE 10, 22: PRINT "90  = Expert"
LOCATE 11, 22: PRINT "100 = Twiddle Fingers"
LOCATE 12, 15: PRINT "(Computer speed may affect your skill level)"
DO
    LOCATE 8, 44: PRINT SPACE$(35);
    LOCATE 8, 43
    INPUT gamespeed$
LOOP UNTIL VAL(gamespeed$) >= 1 AND VAL(gamespeed$) <= 100
speed = VAL(gamespeed$)

speed = (100 - speed) * 5 + 1

DO
    LOCATE 15, 56: PRINT SPACE$(25);
    LOCATE 15, 15
    INPUT "Increase game speed during play (Y or N)"; diff$
    diff$ = UCASE$(diff$)
LOOP UNTIL diff$ = "Y" OR diff$ = "N"

DO
    LOCATE 17, 46: PRINT SPACE$(34);
    LOCATE 17, 17
    INPUT "Monochrome or color monitor (M or C)"; monitor$
    monitor$ = UCASE$(monitor$)
LOOP UNTIL monitor$ = "M" OR monitor$ = "C"

'    startTime# = TIMER                          ' Calculate speed of system
'    FOR i# = 1 TO 1000: NEXT i#                 ' and do some compensation
'    stopTime# = TIMER
'
'    speed = speed * .5 / (stopTime# - startTime# + .01)


END SUB

'InitColors:
'Initializes playing field colors
SUB InitColors


FOR row = 1 TO 50
    FOR col = 1 TO 80
        arena(row, col).acolor = colorTable(4)
    NEXT col
NEXT row

CLS

'Set (turn on) pixels for screen border
FOR col = 1 TO 80
    Set 3, col, colorTable(3)
    Set 50, col, colorTable(3)
NEXT col



FOR row = 4 TO 49
    Set row, 1, colorTable(3)
    Set row, 80, colorTable(3)
NEXT row


END SUB

'Intro:
'  Displays game introduction
SUB Intro
SCREEN 0
WIDTH 80, 25
COLOR 15, 0
CLS

Center 4, "Q B a s i c   N i b b l e s"
COLOR 7
Center 6, "Copyright (C) Microsoft Corporation 1990"
Center 8, "Nibbles is a game for one or two players.  Navigate your snakes"
Center 9, "around the game board trying to eat up numbers while avoiding"
Center 10, "running into walls or other snakes.  The more numbers you eat up,"
Center 11, "the more points you gain and the longer your snake becomes."
Center 13, " Game Controls "
Center 15, "  General             Player 1               Player 2    "
Center 16, "                        (Up)                   (Up)      "
Center 17, "P - Pause                " + CHR$(24) + "                      W       "
Center 18, "                     (Left) " + CHR$(27) + "   " + CHR$(26) + " (Right)   (Left) A   D (Right)  "
Center 19, "                         " + CHR$(25) + "                      S       "
Center 20, "                       (Down)                 (Down)     "
Center 24, "Press any key to continue"

PLAY "MBT160O1L8CDEDCDL4ECC"

SparklePause

END SUB

'Level:
'Sets game level
SUB Level (WhatToDO, sammy() AS snaketype) 'STATIC

SELECT CASE (WhatToDO)

    CASE STARTOVER
        curLevel = 1
    CASE NEXTLEVEL
        curLevel = curLevel + 1
END SELECT

sammy(1).head = 1 'Initialize Snakes
sammy(1).length = 2
sammy(1).alive = TRUE
sammy(2).head = 1
sammy(2).length = 2
sammy(2).alive = TRUE

InitColors



SELECT CASE curLevel
    CASE 1
        sammy(1).row = 25: sammy(2).row = 25
        sammy(1).col = 50: sammy(2).col = 30
        sammy(1).direction = 4: sammy(2).direction = 3


    CASE 2
        FOR i = 20 TO 60
            Set 25, i, colorTable(3)
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 60: sammy(2).col = 20
        sammy(1).direction = 3: sammy(2).direction = 4

    CASE 3
        FOR i = 10 TO 40
            Set i, 20, colorTable(3)
            Set i, 60, colorTable(3)
        NEXT i
        sammy(1).row = 25: sammy(2).row = 25
        sammy(1).col = 50: sammy(2).col = 30
        sammy(1).direction = 1: sammy(2).direction = 2

    CASE 4
        FOR i = 4 TO 30
            Set i, 20, colorTable(3)
            Set 53 - i, 60, colorTable(3)
        NEXT i
        FOR i = 2 TO 40
            Set 38, i, colorTable(3)
            Set 15, 81 - i, colorTable(3)
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 60: sammy(2).col = 20
        sammy(1).direction = 3: sammy(2).direction = 4

    CASE 5
        FOR i = 13 TO 39
            Set i, 21, colorTable(3)
            Set i, 59, colorTable(3)
        NEXT i
        FOR i = 23 TO 57
            Set 11, i, colorTable(3)
            Set 41, i, colorTable(3)
        NEXT i
        sammy(1).row = 25: sammy(2).row = 25
        sammy(1).col = 50: sammy(2).col = 30
        sammy(1).direction = 1: sammy(2).direction = 2

    CASE 6
        FOR i = 4 TO 49
            IF i > 30 OR i < 23 THEN
                Set i, 10, colorTable(3)
                Set i, 20, colorTable(3)
                Set i, 30, colorTable(3)
                Set i, 40, colorTable(3)
                Set i, 50, colorTable(3)
                Set i, 60, colorTable(3)
                Set i, 70, colorTable(3)
            END IF
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    CASE 7
        FOR i = 4 TO 49 STEP 2
            Set i, 40, colorTable(3)
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    CASE 8
        FOR i = 4 TO 40
            Set i, 10, colorTable(3)
            Set 53 - i, 20, colorTable(3)
            Set i, 30, colorTable(3)
            Set 53 - i, 40, colorTable(3)
            Set i, 50, colorTable(3)
            Set 53 - i, 60, colorTable(3)
            Set i, 70, colorTable(3)
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    CASE 9
        FOR i = 6 TO 47
            Set i, i, colorTable(3)
            Set i, i + 28, colorTable(3)
        NEXT i
        sammy(1).row = 40: sammy(2).row = 15
        sammy(1).col = 75: sammy(2).col = 5
        sammy(1).direction = 1: sammy(2).direction = 2

    CASE ELSE
        FOR i = 4 TO 49 STEP 2
            Set i, 10, colorTable(3)
            Set i + 1, 20, colorTable(3)
            Set i, 30, colorTable(3)
            Set i + 1, 40, colorTable(3)
            Set i, 50, colorTable(3)
            Set i + 1, 60, colorTable(3)
            Set i, 70, colorTable(3)
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

END SELECT
END SUB

'PlayNibbles:
'  Main routine that controls game play
SUB PlayNibbles (numplayers, speed, diff$)

'Initialize Snakes
DIM sammyBody(MAXSNAKELENGTH - 1, 1 TO 2) AS snakeBody
DIM sammy(1 TO 2) AS snaketype
sammy(1).lives = 5
sammy(1).score = 0
sammy(1).scolor = colorTable(1)
sammy(2).lives = 5
sammy(2).score = 0
sammy(2).scolor = colorTable(2)

Level STARTOVER, sammy()





startRow1 = sammy(1).row: startCol1 = sammy(1).col
startRow2 = sammy(2).row: startCol2 = sammy(2).col


curSpeed = speed

'play Nibbles until finished


SpacePause "     Level" + STR$(curLevel) + ",  Push Space"
gameOver = FALSE
DO
    IF numplayers = 1 THEN
        sammy(2).row = 0
    END IF

    number = 1 'Current number that snakes are trying to run into
    nonum = TRUE 'nonum = TRUE if a number is not on the screen

    playerDied = FALSE
    PrintScore numplayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives
    PLAY "T160O1>L20CDEDCDL10ECC"

    DO
        'Print number if no number exists
        IF nonum = TRUE THEN
            DO
                numberRow = INT(RND(1) * 47 + 3)
                NumberCol = INT(RND(1) * 78 + 2)
                sisterRow = numberRow + arena(numberRow, NumberCol).sister
            LOOP UNTIL NOT PointIsThere(numberRow, NumberCol, colorTable(4)) AND NOT PointIsThere(sisterRow, NumberCol, colorTable(4))
            numberRow = arena(numberRow, NumberCol).realRow
            nonum = FALSE
            COLOR colorTable(1), colorTable(4)
            LOCATE numberRow, NumberCol
            PRINT RIGHT$(STR$(number), 1);
            count = 0
        END IF

        'Delay game
        'FOR a# = 1 TO curSpeed:  NEXT a#
        DO: LOOP WHILE TIMER = oldtimer!
        oldtimer! = TIMER
        quit = 0
        'Get keyboard input & Change direction accordingly
        kbd$ = INKEY$
        SELECT CASE kbd$
            CASE "w", "W": IF sammy(2).direction <> 2 THEN sammy(2).direction = 1
            CASE "s", "S": IF sammy(2).direction <> 1 THEN sammy(2).direction = 2
            CASE "a", "A": IF sammy(2).direction <> 4 THEN sammy(2).direction = 3
            CASE "d", "D": IF sammy(2).direction <> 3 THEN sammy(2).direction = 4
            CASE CHR$(0) + "H": IF sammy(1).direction <> 2 THEN sammy(1).direction = 1
            CASE CHR$(0) + "P": IF sammy(1).direction <> 1 THEN sammy(1).direction = 2
            CASE CHR$(0) + "K": IF sammy(1).direction <> 4 THEN sammy(1).direction = 3
            CASE CHR$(0) + "M": IF sammy(1).direction <> 3 THEN sammy(1).direction = 4
            CASE "p", "P": SpacePause " Game Paused ... Push Space  "
            CASE CHR$(27): quit = 1
            CASE ELSE
        END SELECT

        FOR a = 1 TO numplayers
            'Move Snake
            SELECT CASE sammy(a).direction
                CASE 1: sammy(a).row = sammy(a).row - 1
                CASE 2: sammy(a).row = sammy(a).row + 1
                CASE 3: sammy(a).col = sammy(a).col - 1
                CASE 4: sammy(a).col = sammy(a).col + 1
            END SELECT

            'If snake hits number, respond accordingly
            IF numberRow = INT((sammy(a).row + 1) / 2) AND NumberCol = sammy(a).col THEN
                PLAY "MBO0L16>CCCE"

                IF sammy(a).length < (MAXSNAKELENGTH - 30) THEN
                    sammy(a).length = sammy(a).length + number * 4
                END IF
                sammy(a).score = sammy(a).score + number
                PrintScore numplayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives
                number = number + 1
                IF number = 10 THEN
                    EraseSnake sammy(), sammyBody(), 1
                    EraseSnake sammy(), sammyBody(), 2
                    LOCATE numberRow, NumberCol: PRINT " "
                    Level NEXTLEVEL, sammy()
                    PrintScore numplayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives
                    SpacePause "     Level" + STR$(curLevel) + ",  Push Space"
                    IF numplayers = 1 THEN sammy(2).row = 0
                    number = 1
                    IF diff$ = "P" THEN speed = speed - 10: curSpeed = speed
                END IF
                nonum = TRUE
                IF curSpeed < 1 THEN curSpeed = 1
            END IF
        NEXT a

        FOR a = 1 TO numplayers
            'If player runs into any point, or the head of the other snake, it dies.
            IF PointIsThere(sammy(a).row, sammy(a).col, colorTable(4)) OR (sammy(1).row = sammy(2).row AND sammy(1).col = sammy(2).col) THEN
                PLAY "MBO0L32EFGEFDC"

                COLOR , colorTable(4)
                LOCATE numberRow, NumberCol
                PRINT " "

                playerDied = TRUE
                sammy(a).alive = FALSE
                sammy(a).lives = sammy(a).lives - 1

                'Otherwise, move the snake, and erase the tail
            ELSE
                sammy(a).head = (sammy(a).head + 1) MOD MAXSNAKELENGTH
                sammyBody(sammy(a).head, a).row = sammy(a).row
                sammyBody(sammy(a).head, a).col = sammy(a).col
                tail = (sammy(a).head + MAXSNAKELENGTH - sammy(a).length) MOD MAXSNAKELENGTH
                Set sammyBody(tail, a).row, sammyBody(tail, a).col, colorTable(4)
                sammyBody(tail, a).row = 0
                Set sammy(a).row, sammy(a).col, sammy(a).scolor
            END IF
        NEXT a

    LOOP UNTIL playerDied

    curSpeed = speed ' reset speed to initial value

    FOR a = 1 TO numplayers
        EraseSnake sammy(), sammyBody(), a

        'If dead, then erase snake in really cool way
        IF sammy(a).alive = FALSE THEN
            'Update score
            sammy(a).score = sammy(a).score - 10
            PrintScore numplayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives

            IF a = 1 THEN
                SpacePause " Sammy Dies! Push Space! --->"
            ELSE
                SpacePause " <---- Jake Dies! Push Space "
            END IF
        END IF
    NEXT a

    Level SAMELEVEL, sammy()
    PrintScore numplayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives

    'Play next round, until either of snake's lives have run out.
LOOP UNTIL sammy(1).lives = 0 OR sammy(2).lives = 0 OR quit = 1

END SUB

'PointIsThere:
'  Checks the global  arena array to see if the boolean flag is set
FUNCTION PointIsThere (row, col, acolor)
IF row <> 0 THEN
    IF arena(row, col).acolor <> acolor THEN
        PointIsThere = TRUE
    ELSE
        PointIsThere = FALSE
    END IF
END IF
END FUNCTION

'PrintScore:
'  Prints players scores and number of lives remaining
SUB PrintScore (numplayers, score1, score2, lives1, lives2)
COLOR 15, colorTable(4)

IF numplayers = 2 THEN
    LOCATE 1, 1
    'PRINT USING "#,###,#00  Lives: #  <--JAKE"; score2; lives2
    PRINT STR$(score2 * 100) + "  Lives:"; lives2; " <--JAKE"
END IF

LOCATE 1, 49
'PRINT USING "SAMMY-->  Lives: #     #,###,#00"; lives1; score1
PRINT "SAMMY-->  Lives:"; lives1; "   "; STR$(score1 * 100)

END SUB

'Set:
'  Sets row and column on playing field to given color to facilitate moving
'  of snakes around the field.
SUB Set (row, col, acolor)
IF row <> 0 THEN
    arena(row, col).acolor = acolor 'assign color to arena
    realRow = arena(row, col).realRow 'Get real row of pixel

    topFlag = arena(row, col).sister + 1 / 2 'Deduce whether pixel
    'is on top , or bottom_

    '        IF arena(row, col).sister = 1 THEN topFlag = 2 ELSE topFlag = 0


    sisterRow = row + arena(row, col).sister 'Get arena row of sister
    sisterColor = arena(sisterRow, col).acolor 'Determine sister's color

    'LOCATE 1, 1: PRINT topFlag, arena(row, col).sister

    LOCATE realRow, col

    IF acolor = sisterColor THEN 'If both points are same
        COLOR acolor, acolor 'Print chr$(219) " "
        PRINT CHR$(219);
    ELSE
        IF topFlag THEN 'Since you cannot have
            IF acolor > 7 THEN 'bright backgrounds
                COLOR acolor, sisterColor 'determine best combo
                PRINT CHR$(223); 'to use.
            ELSE
                COLOR sisterColor, acolor
                PRINT CHR$(220);
            END IF
        ELSE
            IF acolor > 7 THEN
                COLOR acolor, sisterColor
                PRINT CHR$(220);
            ELSE
                COLOR sisterColor, acolor
                PRINT CHR$(223);
            END IF
        END IF
    END IF
END IF
'SLEEP

END SUB

'SpacePause:
'  Pauses game play and waits for space bar to be pressed before continuing
SUB SpacePause (text$)

COLOR colorTable(5), colorTable(6)
Center 11, "                                 "
Center 12, "  " + LEFT$(text$ + SPACE$(29), 29) + "  "
Center 13, " _______________________________ "
WHILE INKEY$ <> "": WEND
WHILE INKEY$ <> " ": WEND
COLOR 15, colorTable(4)

FOR i = 21 TO 26 ' Restore the screen background
    FOR j = 24 TO 56
        Set i, j, arena(i, j).acolor
    NEXT j
NEXT i

END SUB

'SparklePause:
'  Creates flashing border for intro screen
SUB SparklePause

COLOR 4, 0
a$ = "*    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    "
WHILE INKEY$ <> "": WEND 'Clear keyboard buffer

WHILE INKEY$ = ""
    FOR a = 1 TO 5
        LOCATE 1, 1 'print horizontal sparkles
        PRINT MID$(a$, a, 80);
        LOCATE 22, 1
        PRINT MID$(a$, 6 - a, 80);

        FOR b = 2 TO 21 'Print Vertical sparkles
            c = (a + b) MOD 5
            IF c = 1 THEN
                LOCATE b, 80
                PRINT "*";
                LOCATE 23 - b, 1
                PRINT "*";
            ELSE
                LOCATE b, 80
                PRINT " ";
                LOCATE 23 - b, 1
                PRINT " ";
            END IF
        NEXT b
    NEXT a
WEND

END SUB

'StillWantsToPlay:
'  Determines if users want to play game again.
FUNCTION StillWantsToPlay

COLOR colorTable(5), colorTable(6)
Center 10, "                                 "
Center 11, "        G A M E   O V E R        "
Center 12, "                                 "
Center 13, "       Play Again?   (Y/N)       "
Center 14, " _______________________________ "

WHILE INKEY$ <> "": WEND
DO
    kbd$ = UCASE$(INKEY$)
LOOP UNTIL kbd$ = "Y" OR kbd$ = "N"

COLOR 15, colorTable(4)
Center 10, "                                 "
Center 11, "                                 "
Center 12, "                                 "
Center 13, "                                 "
Center 14, "                                 "

IF kbd$ = "Y" THEN
    StillWantsToPlay = TRUE
ELSE
    StillWantsToPlay = FALSE
    COLOR 7, 0
    CLS
END IF

END FUNCTION



'StillWantsToPlay:
'  Determines if users want to play game again.
FUNCTION StillWantsToPlay2 ()

COLOR colortable2(5), colortable2(0)
PRINT TAB(10), "?????????????????????????????????"
PRINT TAB(11), "?       C O N T I N U E ?       ?"
PRINT TAB(12), "?                               ?"
PRINT TAB(13), "?            (Y/N)              ?"
PRINT TAB(14), "?????????????????????????????????"

WHILE INKEY$ <> "": WEND
DO
    kbd$ = UCASE$(INKEY$)
LOOP UNTIL kbd$ = "Y" OR kbd$ = "N"

COLOR 15, colortable2(4)
PRINT TAB(10), "                                 "
PRINT TAB(11), "                                 "
PRINT TAB(12), "                                 "
PRINT TAB(13), "                                 "
PRINT TAB(14), "                                 "

IF kbd$ = "Y" THEN
    StillWantsToPlay2 = TRUE2
ELSE
    StillWantsToPlay2 = FALSE2
    COLOR 7, 0
    CLS
END IF

END SUB

FUNCTION colortable2 (which)

colortable2 = which

END FUNCTION

FUNCTION question ()
COLOR 9, 0: PRINT "Do you want Tower Trials Song to begin the game? (Y/N)"
DO
    a$ = INKEY$
LOOP UNTIL a$ <> ""
IF UCASE$(a$) = "Y" THEN question = TRUE2 ELSE question = FALSE2
END FUNCTION


FUNCTION Clock$
hour$ = LEFT$(TIME$, 2): H% = VAL(hour$)
min$ = MID$(TIME$, 3, 3)
IF H% >= 12 THEN ampm$ = " PM" ELSE ampm$ = " AM"
IF H% > 12 THEN
    IF H% - 12 < 10 THEN hour$ = STR$(H% - 12) ELSE hour$ = LTRIM$(STR$(H% - 12))
ELSEIF H% = 0 THEN hour$ = "12" ' midnight hour
ELSE: IF H% < 10 THEN hour$ = STR$(H%) ' eliminate leading zeros
END IF
Clock$ = hour$ + min$ + ampm$
END FUNCTION


