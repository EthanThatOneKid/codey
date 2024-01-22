_FullScreen
Dim Username As String

Input "Hello, my name is Codey. What is yours?   ", Username
Cls
Print "Well, hello then, "; Username

ORIGINAL:
Cls
Color 3:
Print "   _________                "
Print "  /         \      ______   "
Print " /   Press   \    /      \  "
Print "|   Spacebar  |  |        | "
Print "|   to see    |  |  0  0  | "
Print " \ my options/   |   __   | "
Print "  \_________/     \______/  "
Color 31: Print "Press the Spacebar to see my options..."
Color 18: Print "Press Esc to end the program..."
Do
    Do: K$ = UCase$(InKey$)
    Loop Until K$ = Chr$(63) Or K$ = Chr$(32)
    If K$ = Chr$(27) Then End
    If K$ = Chr$(32) Then GoTo OPTIONS:
Loop

OPTIONS:
Cls
Color 15:
Print "   _________                "
Print "  /         \      ______   "
Print " /   These   \    /      \  "
Print "|     are     |  |        | "
Print "|  my options |  |  0  0  | "
Print " \           /   |   __   | "
Print "  \_________/     \______/  "
Print "Pick an option, "; Username
Print "It is currenty"
Print Clock$
Print Date$
Print
Print
Color 9: Print "Key 'P' to play on my digital arcade..."
Print
Color 10: Print "Key 'C' to use the calculator..."
Print
Color 14: Print "Key 'S' to use soundboard..."
Print
Color 13: Print "Key 'G' to use graph..."
Print
Color 11: Print "Press the spacebar to talk with me..."
Print
Color 4: Print "Key Esc to end program..."
Do
    Do: K$ = UCase$(InKey$)
    Loop Until K$ = "O" Or K$ = "E" Or K$ = "P" Or K$ = "C" Or K$ = "S" Or K$ = "G" Or K$ = Chr$(27) Or K$ = Chr$(32)

    If K$ = Chr$(27) Then End
    If K$ = "O" Then GoTo OPTIONS:
    If K$ = "P" Then GoTo Gamesmenu:
    If K$ = "C" Then GoTo Calculator:
    If K$ = "S" Then GoTo Soundboard:
    If K$ = Chr$(27) Then End
    If K$ = "G" Then GoTo Graph:
    If K$ = Chr$(32) Then GoTo Conversation:
Loop

Calculator:
5 Cls
Print "1) ADD"
Print "2) MULTIPLY"
Print "3) DIVIDE"
Print "4) SQUARE ROOT"
Print "5) EXIT"
Input I
If I = 1 GoTo 10
If I = 2 GoTo 20
If I = 3 GoTo 30
If I = 4 GoTo 40
If I = 5 GoTo 50
End
10 Cls: Print "INPUT NUMBER"
Input A
Cls
Print "INPUT NEXT NUMBER"
Input B
Cls
Print A + B
Print "PRESS SPACE TO RUN"
Sleep 0
Print "Continue calculating, "; Username
Print "Y/N"
If K$ = "Y" Then GoTo 5
If K$ = "N" Then GoTo OPTIONS:

20 Cls: Print "INPUT NUMBER"
Input C
Cls
Print "INPUT NEXT NUMBER"
Input D
Cls
Print C * D
Print "PRESS SPACE TO RUN"
Sleep 0
Print "Continue calculating, "; Username
Print "Y/N"
If K$ = "Y" Then GoTo 5
If K$ = "N" Then GoTo OPTIONS:

30 Cls: Print "INPUT NUMBER"
Input E
Cls
Print "INPUT NEXT NUMBER"
Input F
Cls
Print E / F
Print "PRESS SPACE TO RUN"
Sleep 0
Print "Continue calculating, "; Username
Print "Y/N"
If K$ = "Y" Then GoTo 5
If K$ = "N" Then GoTo OPTIONS:

40 Cls: Print "INPUT NUMBER"
Input G
Cls
Print Sqr(G)
Print "PRESS SPACE TO RUN"
Sleep 0
Print "Continue calculating, "; Username
Print "Y/N"
If K$ = "Y" Then GoTo 5
If K$ = "N" Then GoTo OPTIONS:

50 End

Soundboard:
Cls
Color 15:
Print "Press A to play A note..."
Print
Print "Press B to play B note..."
Print
Print "Press C to play C note..."
Print
Print "Press D to play D note..."
Print
Print "Press E to play E note..."
Print
Print "Press F to play F note..."
Print
Print "Press G to play G note..."
Print
Print "Press Esc to exit Soundboard..."
Do
    Do: K$ = UCase$(InKey$)
    Loop Until K$ = "A" Or K$ = "B" Or K$ = "C" Or K$ = "D" Or K$ = "E" Or K$ = "F" Or K$ = "G" Or K$ = Chr$(27)
    Print K$
    If K$ = "E" Then GoTo E:
    If K$ = "A" Then GoTo A:
    If K$ = "B" Then GoTo B:
    If K$ = "C" Then GoTo C:
    If K$ = "D" Then GoTo D:
    If K$ = "G" Then GoTo G:
    If K$ = "F" Then GoTo F:
    If K$ = Chr$(27) Then GoTo OPTIONS:
Loop
E:
Cls
Play "E"
GoTo Soundboard:
A:
Cls
Play "A"
GoTo Soundboard:
B:
Cls
Play "B"
GoTo Soundboard:
C:
Cls
Play "C"
GoTo Soundboard:
D:
Cls
Play "D"
GoTo Soundboard:
G:
Cls
Play "G"
GoTo Soundboard:
F:
Cls
Play "F"
GoTo Soundboard:

Graph:

Screen 0, 0, 0
Cls
Width 40, 25
Color 10: Print "The slope formula is y = m(x) +/- b."
Print "Add(Positive) or Subtract(negative) m of first slope? A or S?"
Do: K$ = UCase$(InKey$)
Loop Until K$ = "A" Or K$ = "S"
If K$ = "A" Then GoTo Add1:
If K$ = "S" Then GoTo Subtract1:

Add1:
Cls
Color 10: Print "The slope formula is y = m(x) +/- b."
Input "Enter first term for m. ", m
Input "Over? ", C
m2 = m / C
GoTo start2:

Subtract1:
Cls
Color 10: Print "The slope formula is y = m(x) +/- b."
Input "Enter first term for m. ", m
Input "Over? ", D
rand3 = m / D
m3 = rand3 * -1
GoTo start3:

start2:
Cls
Color 10: Print "The slope formula is y = m(x) +/- b."
Print "Add(Positive) or Subtract(negative) b of first slope? A or S?"
Do: K$ = UCase$(InKey$)
Loop Until K$ = "A" Or K$ = "S"
If K$ = "A" Then GoTo Add2:
If K$ = "S" Then GoTo Subtract2:

start3:
Cls
Color 10: Print "The slope formula is y = m(x) +/- b."
Print "Add(Positive) or Subtract(negative) b of first slope? A or S?"
Do: K$ = UCase$(InKey$)
Loop Until K$ = "A" Or K$ = "S"
If K$ = "A" Then GoTo Add3:
If K$ = "S" Then GoTo Subtract3:

Add2:
Cls
Color 10: Print "The slope formula is y = m(x) +/- b."
Input "Enter first term for b. ", B
b2 = B
GoTo Stats1:

Subtract2:
Cls
Color 10: Print "The slope formula is y = m(x) +/- b."
Input "Enter first term for b. ", B
b3 = B * -1
GoTo Stats2:

Add3:
Cls
Color 10: Print "The slope formula is y = m(x) +/- b."
Input "Enter first term for b. ", B
b4 = B
GoTo Stats3:

Subtract3:
Cls
Color 10: Print "The slope formula is y = m(x) +/- b."
Input "Enter first term for b. ", B
b5 = B * -1
GoTo Stats4:



Stats1:
Cls
Screen 13
size = 5
Window (-size, -size)-(size, size)
Line (-size, 0)-(size, 0), 15
Line (0, -size)-(0, size), 15
For x = -size To size
    Line (-size, x)-(size, x), 8
    Line (x, -size)-(x, size), 8
Next x
Line (-size, 0)-(size, 0), 15
Line (0, -size)-(0, size), 15
For x = -size To size Step .01
    y = x * m2 + b2

    PSet (x, y), 13

    Locate 20, 1: Print "Y Intercept is "; B
Next x
GoTo again
Stats2:
Cls
Screen 13
size = 5
Window (-size, -size)-(size, size)
Line (-size, 0)-(size, 0), 15
Line (0, -size)-(0, size), 15
For x = -size To size
    Line (-size, x)-(size, x), 8
    Line (x, -size)-(x, size), 8
Next x
Line (-size, 0)-(size, 0), 15
Line (0, -size)-(0, size), 15
For x = -size To size Step .01
    y = x * m2 + b3

    PSet (x, y), 13

    Locate 20, 1: Print "Y Intercept is "; B
Next x
GoTo again
Stats3:
Cls
Screen 13
size = 5
Window (-size, -size)-(size, size)
Line (-size, 0)-(size, 0), 15
Line (0, -size)-(0, size), 15
For x = -size To size
    Line (-size, x)-(size, x), 8
    Line (x, -size)-(x, size), 8
Next x
Line (-size, 0)-(size, 0), 15
Line (0, -size)-(0, size), 15
For x = -size To size Step .01
    y = x * m3 + b4

    PSet (x, y), 13

    Locate 20, 1: Print "Y Intercept is "; B
Next x
GoTo again
Stats4:
Cls
Screen 13
size = 5
Window (-size, -size)-(size, size)
Line (-size, 0)-(size, 0), 15
Line (0, -size)-(0, size), 15
For x = -size To size
    Line (-size, x)-(size, x), 8
    Line (x, -size)-(x, size), 8
Next x
Line (-size, 0)-(size, 0), 15
Line (0, -size)-(0, size), 15
For x = -size To size Step .01
    y = x * m3 + b5

    PSet (x, y), 13

    Locate 20, 1: Print "Y Intercept is "; B
Next x
GoTo again:
again:
Locate 23, 1: Print "New graph? (Y/N)"
Do
    a$ = UCase$(InKey$)
Loop Until a$ = "Y" Or a$ = "N"
If a$ = "Y" Then GoTo Graph:
If a$ = "N" Then
    Screen 0, 0, 0:
    Width 80, 25
    Cls
End If
If a$ = "N" GoTo OPTIONS:
End

Conversation:
Dim UserInput As String
Do
    Color 7:
    Print "Press Esc to go to Codey's main menu at any time"
    a$ = UCase$(InKey$)
    Input "Type something for me to respond to.   ", UserInput
    If K$ = Chr$(27) Then GoTo OPTIONS:
    If UserInput = "I WANNA BE THE VERY BEST" Then
        Print
        Print "   _________                "
        Print "  /         \      ______   "
        Print " /   Like    \    /      \  "
        Print "|   no one    |  |        | "
        Print "|  ever was   |  |  0  0  | "
        Print " \           /   |   __   | "
        Print "  \_________/     \______/  "
    End If
    If UserInput = "TO CATCH THEM IS MY TEST" Then
        Print
        Print "   _________                "
        Print "  /         \      ______   "
        Print " / To train  \    /      \  "
        Print "|   them is   |  |        | "
        Print "|  my cause   |  |  0  0  | "
        Print " \           /   |   __   | "
        Print "  \_________/     \______/  "
    End If
    If UserInput = "POKEMON" Then
        Print
        Print "   _________                "
        Print "  /         \      ______   "
        Print " /   Gotta   \    /      \  "
        Print "|   catchem   |  |        | "
        Print "|     all     |  |  0  0  | "
        Print " \           /   |   __   | "
        Print "  \_________/     \______/  "
    End If
    If UserInput = "SLEEP" Then
        Print "It is already "
        Print Clock$
        Print "   _________                "
        Print "  /         \      ______   "
        Print " /   ZZZZZ   \    /      \  "
        Print "|   ZZZZZZZ   |  |        | "
        Print "|     ZZZ     |  |  -  -  | "
        Print " \(goodnight)/   |   __   | "
        Print "  \_________/     \______/  "
        Sound 200, 20
        End
    End If
    If UserInput = "I WANT TO GO HOME" Then GoTo OPTIONS:
    If UserInput = "HOME" Then GoTo OPTIONS:
    If UserInput = "OPTIONS" Then GoTo OPTIONS:
    If UserInput = "MENU" Then GoTo OPTIONS:
    If UserInput = "GAMES" Then GoTo Gamesmenu:
    If UserInput = "PLAY" Then GoTo Gamesmenu:
    If UserInput = "CALCULATOR" Then GoTo Calculator:
    If UserInput = "CALCULATE" Then GoTo Calculator:
    If UserInput = "FART" Then Sound 100, 20
    If UserInput = "END" Then End
    If UserInput = "END PROGRAM" Then End
    If UserInput = "FAVORITE MOVIE" Then
        Print
        Print "   _________                "
        Print "  /         \      ______   "
        Print " /           \    /      \  "
        Print "|    Tron     |  |        | "
        Print "|             |  |  0  0  | "
        Print " \           /   |   __   | "
        Print "  \_________/     \______/  "
    End If
    If UserInput = "FAVORITE FOOD" Then
        Print
        Print "   _________                "
        Print "  /         \      ______   "
        Print " /           \    /      \  "
        Print "|    Block    |  |        | "
        Print "|    Bytes    |  |  0  0  | "
        Print " \           /   |   __   | "
        Print "  \_________/     \______/  "
    End If
    If UserInput = "NAME" Then
        Print "Your name is "; Username
        Print "   _________                "
        Print "  /         \      ______   "
        Print " /     My    \    /      \  "
        Print "|   name is   |  |        | "
        Print "|    Codey    |  |  0  0  | "
        Print " \           /   |   __   | "
        Print "  \_________/     \______/  "
    End If
    If UserInput = "FAVORITE GAME" Then
        Print
        Print "   _________                "
        Print "  /         \      ______   "
        Print " /     I     \    /      \  "
        Print "|    like     |  |        | "
        Print "|    Atari    |  |  0  0  | "
        Print " \           /   |   __   | "
        Print "  \_________/     \______/  "
    End If
    If UserInput = "GRAPH" Then GoTo Graph:
    If UserInput = "TOWER TRIALS" Then GoTo TowerTrials:
    If UserInput = "HOW ARE YOU" Then Print "I am good, how are you, "; Username
    If UserInput = "DATE" Then Print Clock$
    If UserInput = "TIME" Then Print Clock$
    If UserInput = "CLOCK" Then Print Clock$
Loop

Gamesmenu:
Cls

Color 31: Locate 2, 2: Print "This is my Digital Arcade..."
Color 15: Locate 3, 2: Print String$(78, 196)
Color 25: Locate 4, 2: Print "Commands"
Color 18: Locate 13, 2: Print "T";: Color 2: Print "ower Trials";: Color 15: Print ".................. Smite all the creatures in your trials"
Color 20: Locate 21, 2: Print "Esc";: Color 12: Print "................... press Esc to end the program"

Do
    K$ = InKey$
    K$ = UCase$(K$)
Loop Until K$ = "N" Or K$ = "T" Or K$ = "B" Or K$ = Chr$(27)
_SndPlay h&
If K$ = "T" Then GoTo TowerTrials:
If K$ = Chr$(27) Then GoTo OPTIONS:

TowerTrials:
Const TRUE2 = 1
Const FALSE2 = 0

Cls
ask = question

If ask = 1 Then
    Sound 100, 18.2
    Sound 300, 18.2
    Sound 200, 18.2
    Sound 500, 18.5
    Sound 200, 20
    Sound 0, 18.2
    Sound 300, 6
    Sound 800, 18.5
End If

STARTALLOVER:
Color 12, 0: Print Tab(23); "Welcome to  T O W E R  T R I A L S"

Color 5: Print "Welcome to this game. From here you will solve your way through the game until  completion is reached... (Make sure that everything you type should be in CAPS)"
Color 31, 1: Print "(WARNING: this game is very confusing and requires thought and patience. PLAYER DISGRESSION IS ADVIZED)"
Color 10, 0: Print "Darn! I forgot my glasses in my other cloak. Can you tell me if you are a boy or a girl"

Do
    Input pASSWORD$
    If pASSWORD$ = "GIRL" Then GoTo GIRL:
    If pASSWORD$ = "BOY" Then GoTo BOY: Else Print "I don't understand..."

Loop Until GETOUT = 1

BOY:
Print "Alright! Let's venture yonder, champ!"
GoTo FIRSTRIDDLE:

GIRL:
Print "Oh. Now I recognize you! Alright! Follow me, milady..."

FIRSTRIDDLE:
Color 4: Print "Okay, let's start with a mathematical riddle."
Color 12: Print "If 2=6, 3=12, 4=20, what does 5 equal?"
Let GETOUT = 0
Do
    Input pASSWORD$
    If pASSWORD$ = "30" Then GETOUT = 1: Print "Good thinking! You're just the kind of person I'm looking for!" Else Print "Try again please..."
Loop Until GETOUT = 1
Color 2: Print "Let's move on..."

Color 6: Print "Let's go on our way through town to the tower."
Print "There is a gate to the place we are visiting and the guardsman asks for a passcode to get in."

Color 3: Print "The guardsman says... 'What is the answer to 2(5+7) 1 + 2 x 4'"

Let GETOUT = 0
Do
    Input pASSWORD$
    If pASSWORD$ = "30" Then GETOUT = 1: Print "You shall pass, brilliant one." Else Print "Please try again, your answer is incorrect."
Loop Until GETOUT = 1

Color 8: Print "This is a palace in which there are 3 floors. Each floor contains a trial. But, only on the roof, will be your final trial and crowning moment."

FIRSTTRIAL:
Color 4: Print "HERE IS YOUR FIRST TRIAL!!! (An experienced witch flies down with 500 health points.Your magic deals 25% damage of what the witch already has and your punch deals 100 health points. Your objctive is to slay the witch by choosing either MAGIC or PUNCH to attack)"

Let GETOUT = 0
Let Life = 500
Let Witch = 500

Color 6:
Do
    Input pASSWORD$
    If pASSWORD$ = "MAGIC" Then
        Witch = Witch * .25
        Life = Life - 110

        Color 2: Print "You did 25% damage of what the witch already has! You got hit by the witch's magical staff's WACK!"
        Print "You currntly have"; Life
        Print "life points."
        Print "The witch still has"; Witch
        Print "life points. Make another move. Your options are MAGIC, or PUNCH."

    Else Print "You punched 100 damage! You got hit by the witch's magical staff's flame bolt."
        Life = Life - 110
        Witch = Witch - 100

        Print "You currently have"; Life
        Print "life points."
        Print "The witch still has"; Witch
        Print "life points. Make another move. Your options are MAGIC, or PUNCH."

    End If
    If Witch <= 0 Or Life <= 0 Then GETOUT = 1
Loop Until GETOUT = 1
If Witch <= 0 Then Print "You defeated the witch!"
If Life <= 0 Then Print "You got annihilated by the witch and have been slain."

Decision = StillWantsToPlay2
If Life <= 0 And Decision = TRUE2 Then GoTo STARTALLOVER:
If Decision = FALSE2 Then End

Color 0, 0
Color 2: Print "Hoo-ray!!! You have defeated the witch. It's time to go to to the next floor for trial 2."
Color 6: Print "(As you step up to the second trial, you see a minotaur inside a cage) Here is your second trial... Defeat the MINOTAUR!!!"
Color 14: Print "(You have earned a new power and your magic has improved! Your options are MAGIC: does 50% damage of what the minotaur currently has; and SWORD that does 300 damage to the minotaur health points. YOU BOTH HAVE 1,000 HEALTH POINTS"

Let GETOUT = 0
Let Life = 1000
Let Minotaur = 1000

Color 10:
Do
    Input pASSWORD$
    If pASSWORD$ = "MAGIC" Then
        Minotaur = Minotaur * .50
        Life = Life - 230

        Print "You did 50% damage! You got hit by the minotaur's RAM"
        Print "You currently have"; Life
        Print "life points."
        Print "The minotaur still has"; Minotaur
        Print "life points. Make another move. Your options are MAGIC, or SWORD."

    Else
        Print "You hacked 300 damage! You got hit by the minotaur's GIGA IMPACT!"
        Life = Life - 230
        Minotaur = Minotaur - 300

        Print "You currently have"; Life
        Print "life points."
        Print "The minotaur still has"; Minotaur
        Print "life points. Make another move. Your options are MAGIC, or SWORD."

    End If
    If Minotaur <= 0 Or Life <= 0 Then GETOUT = 1
Loop Until GETOUT = 1

Color 20, 7:
If Minotaur <= 0 Then Print "You defeated the minotaur!"
If Life <= 0 Then Print "You got annihilated by the minotaur and have been slain."

Decision = StillWantsToPlay2
If Life <= 0 And Decision = TRUE2 Then GoTo STARTALLOVER:
If Decision = FALSE2 Then GoTo OPTIONS:

Color 3, 0: Print "We are heading to the third floor for your final trial. How do you feel?"
Input pASSWORD$
If pASSWORD$ = "GOOD" Then Print "It's all good, just remember what you have learned." Else Print "It's all good, just remember what you have learned."

Print "We are heading towards the third floor now."

Color 15: Print "(The dragon awits your arrival) It says: I SHALL DISINTIGRATE YOU!!!"

Let GETOUT = 0
Let Life = 10000
Let Dragon = 10000

Print "Make your first move. Your options are MAGIC, or SWORD."
Color 10:
Do
    Input pASSWORD$
    If pASSWORD$ = "MAGIC" Then
        Dragon = Minotaur * .50
        Life = Life - 1000

        Print "You did 50% damage! You got hit by the dragon's DRILL RUN!!!"
        Print "You currently have"; Life
        Print "life points."
        Print "The dragon still has"; Dragon
        Print "life points. Make another move. Your options are MAGIC, or SWORD."

    Else
        Print "You hacked 300 damage! You got hit by the dragon's FLARE CRASH!!!"
        Life = Life - 1000
        Dragon = Dragon - 300
        Print "You currently have"; Life
        Print "life points."
        Print "The dragon still has"; Dragon
        Print "life points. Make another move. Your options are MAGIC, or SWORD."

    End If
    If Dragon <= 0 Or Life <= 0 Then GETOUT = 1
Loop Until GETOUT = 1
Color 20, 7:
If Dragon <= 0 Then Print "You defeated the dragon!"
Color 12, 0: Print Tab(23); "C O N G R A D U L A T I O N S ! ! ! You have won T O W E R  T R I A L S"
If Life <= 0 Then Print "You got annihilated by the dragon and have been slain. You shall start over..."

Decision = StillWantsToPlay2
If Life <= 0 And Decision = TRUE2 Then GoTo STARTALLOVER:
If Decision = FALSE2 Then GoTo OPTIONS:


Color 5: Print "(Word qickly spread of the victor of all three trials at the tower. It has never been done by any other challenger, wizard, nor even royal blood had completed the challenge.) This is your ceremony, on the roof of the tower. As promised"
Color 8: Print "You hear the king say: ... and here you are, your medal to show that you are the first in all the kingdom to complete the legendary challenge and I hereby claim you as CHAMPION of our kingd---"
Color 4: Print "(A rumble is felt on the ground and the distance you see a huge giant, as tall as the tower, coming to crash your party)"
Color 5: Print "It is only you who can save us all from the marauding giant. Your SWORD has been upgraded to FLAMING SWORD: does 1,000 damage; MAGIC: does 50% damage of what the marauding giant currently has. You both have 10,000 life points."
Color 14: Print "Your options are MAGIC, or FLAMING SWORD"

Let GETOUT = 0
Let Life = 10000
Let giant = 10000

Color 1:
Do
    Input pASSWORD$
    If pASSWORD$ = "MAGIC" Then
        giant = giant * .50
        Life = Life - 1500

        Print "You did 50% damage! You got hit by the marauding giant's CRASH!"
        Print "You currently have"; Life
        Print "life points."
        Print "The marauding giant still has"; giant
        Print "life points. Make another move. Your options are MAGIC, or SWORD."

        Color 20, 7: Else Print "You hacked 1000 damage! You got hit by the marauding giant's HEAD SMASH!"
        Life = Life - 1500
        giant = giant - 1000

        Print "You currently have"; Life
        Print "life points."
        Print "The marauding giant still has"; giant
        Print "life points remaining. Make another move. Your options are MAGIC, or SWORD."

    End If
    If giant <= 0 Or Life <= 0 Then GETOUT = 1
Loop Until GETOUT = 1
Color 4:
If giant <= 0 Then Print "You defeated the marauding giant in front of everybody! Good work, champion!"
If Life <= 0 Then Print "You got annihilated by the marauding giant and have been slain. You shall restart at the first trial..."
If Life <= 0 Then GoTo FIRSTTRIAL:

Color 17, 15: Print "You have won Tower Trials! Please play again soon."

Color 15, 0: Print "CREDITS"
Color 7: Print "Helped by Gregory Davidson and Albert Davidson. Everything else: Ethan Davidson."


Color 10, 0: Print "THE END... or to be continued"

End

Function question ()
    Color 9, 0: Print "Do you want Tower Trials Song to begin the game? (Y/N)"
    Do
        a$ = InKey$
    Loop Until a$ <> ""
    If UCase$(a$) = "Y" Then question = TRUE2 Else question = FALSE2
End Function


Function Clock$
    hour$ = Left$(Time$, 2): H% = Val(hour$)
    min$ = Mid$(Time$, 3, 3)
    If H% >= 12 Then ampm$ = " PM" Else ampm$ = " AM"
    If H% > 12 Then
        If H% - 12 < 10 Then hour$ = Str$(H% - 12) Else hour$ = LTrim$(Str$(H% - 12))
    ElseIf H% = 0 Then hour$ = "12" ' midnight hour
    Else: If H% < 10 Then hour$ = Str$(H%) ' eliminate leading zeros
    End If
    Clock$ = hour$ + min$ + ampm$
End Function


