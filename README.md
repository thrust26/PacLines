# Pac-Line x 8
Copyright 2024 - Thomas Jentzsch

## The Game

**Pac-Line x 8** is a 4K, one button, up to eight simultaneous players, Pac-Line game for the Atari 2600.

## Controls
### Paddles
If you have a QuadTari, plug it into the two ports of the console and plug the paddles into the four ports of the QuadTari **before** you switch on the console.

Without a QuadTari, plug the paddles into left and right ports of the console.

### Switches
On the title screen or any time during the game, press **Select** to select a game variation. There are 8 game variations, 4 different starting lines each, with or without bonus fruits. The currently selected variation's starting line number is displayed at the top of the screen. If a local high score exists for this variation, it is displayed alternating with the game variation.

To start a new game press **Reset**. Or **Fire** while not in a running game.

## Playing the Game
### Start Countdown
After pressing **Reset** or **Fire**, a short countdown starts. During this countdown, each player must press the paddle fire button to activate her/his Pac-Man for the game. Activated lines are displayed colored and with their pellets enabled. The more Pac-Men are getting activated, the longer the countdown lasts. 
 
The game starts immediately after the countdown. Inactive Pac-Men will be controlled by the game's AI. If no Pac-Man got actived, the AI will start playing with itself (demo mode).

### The Game
There are two options for determining the direction in which your Pac-Man automatically moves:
- **Left Diffculty = B**: To move left, hold the fire button. Else your Pac-Man moves right.
- **Left Diffculty = A**: Pressing the fire button reverses the current direction.

The main goal is to clear all pellets of your line without getting caught by a ghost. Unlike your Pac-Man, the ghosts cannot wrap around. If you eat a bigger power pellet, you can start chasing the ghost for a limited time. The ghost will start to flash when the time expires. Also you can earn extra points by eating a fruit which appears every 4th line.

After clearing a line, a new line with all pellets reenabled appears. Each new line is a little faster and more difficult. The game ends when all human controlled Pac-Men have been caught by a ghost or reached 9,999 points.

### Scoring
The row number and score of the current leading player is displayed at the top of the screen, alternating with the current line number and using the player's color.

Points are awarded for each eaten or caught object as follows:
- pellet: 1.. points 
- power pellet: 5.. points
- ghost: 10.. points
- fruit: 10..500 points

The points for pellets and ghosts are multipied by a factor. It starts at 1 and is increased by 1 every 4th line. 

### High Scores
When the game is over, the high score of the current game variation will be displayed first (white, e.g. "HI 1234"). Then all active player's results will be displayed one by one, ordered by their rank and using the player's color. For each player, there will be two displays:

1. rank & score (e.g. "1. 1234")
2. player number & line (e.g. "4 LN.17")

The local high score will be reset when a game with a different variation is started. Every new high score will be send to the PlusCart High Score Club. 

## Acknowledgements
**Pac-Line x 8** has been developed and tested with **Stella**, the best emulator for the Atari 2600. Pac-Line x 8 is based on _Paku Paku_ (https://abagames.github.io/crisp-game-lib-11-games/?pakupaku).

Thanks to the AtariAge community for testing the game and providing valuable input.

## Legal
This game is freeware, but copyrighted; it may be freely distributed, but it **must** be always accompanied by this documentation file. This program may be included on websites and shareware or freeware compilations, but please contact me first. You **cannot** distribute Pac-Line x 8 on Atari 2600 cartridges without my **explicit** permission.
