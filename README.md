# HexaGameOfLife
BME-VIK Basics of programming 3 homework

*This is my game of life simulator, with hexagonal shaped tiles for my programming 3 course of university.*

Upon running the program a window shows up with a menu bar and the new game part of the program is active.
The menu bar has three options, New game and Load game or exit.
### New game:
When starting a new game, you should consider the parameters to give,
for example the width and height of the matrix is better when bigger,
but the size of the hexes should be relatively choosen (it also depends on the screens resolution and pixeldensity).
A nice middleground is around 10-20 pixels of hexagon size, and a 40 by 40 matrix looks good.
### Load game:
Select a previous saved state from the list, the name of the saves tell the date and time of the saving,
and the born/survive rate of the game.
Pressing the Load button loads the game and the same sationary window can be seen, with the hexagons in
the same state as they were saved, and the parameters of the simulation are also the same.

### Simulation:

When you start the game, it will be stationary, so that you can click on hexagons to make them alive before the simulation starts.

Stationary features:
* click on hexagons to toggle their state
* press Space to start/continue simlation
* press Enter to save game state to a file

Dinamic features:
* press space to stop simulation

Usable in both modes:
* right arrow to speed up the animation
* left arrow to slow down the animation
* esc to exit to menu

#### Speed:
There are five 6 speeds that can be set:

* Slowest   (1000 ms)
* Slower    (750 ms)
* Normal    (500 ms)
* Fast      (350 ms)
* Faster    (200 ms)
* Fastest   (100 ms)
#### Exiting the simulation/application
When in main window, closing the windows and the exit button in the menu exits the application but when the simulation window is active, closing that or pressing escape returns you to the main window.

