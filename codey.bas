_FULLSCREEN
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
COLOR 18: LOCATE 13, 2: PRINT "T";: COLOR 2: PRINT "ower Trials";: COLOR 15: PRINT ".................. Smite all the creatures in your trials"
COLOR 20: LOCATE 21, 2: PRINT "Esc";: COLOR 12: PRINT "................... press Esc to end the program"

DO
    K$ = INKEY$
    K$ = UCASE$(K$)
LOOP UNTIL K$ = "N" OR K$ = "T" OR K$ = "B" OR K$ = CHR$(27)
_SNDPLAY h&
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


