# Weaver 2
Remake of Weaver 2 a 1990 Commodore 64 game by '64er

Original version Copyright 1990 '64er

This version Copyright 2025 MKSZTSZ

## Programming language
FreePascal (Lazarus 4.0.0rc2 with FPC 3.2.2)
https://www.lazarus-ide.org/

## Source codes
SDL2 pascal headers (source\units\sdl2) is licensed under MPL or zlib license.
GitHub for SDL2 pascal headers: https://github.com/PascalGameDevelopment/SDL2-for-Pascal

The rest of source code is licensed under GNU GPL v3 (or later).

## Tools
PNGOut tool is by Ken Silverman (http://advsys.net/ken)

MKConv2, FontBuild2 and MAD4 tools are made by me.

## Compiled binaries from current build with datafiles and DLLs
[x64](https://mksztsz.hu/tmpfiles/Weaver2_0.0.0.11.zip "Download x64 version") or
[x86](https://mksztsz.hu/tmpfiles/Weaver2_x86_0.0.0.11.zip "Download x86 version").

## 2025.01.19 - Build 11
- Added horizontal enemy gfx.

## 2025.01.17 - Build 10
- Collision detection works for moving enemies.
- Added fourth map.
- Added doors and buttons. Move over buttons to open doors.

## 2025.01.16 - Build 9
- Sprites are separated into two files, masked and non-masked.
- Masks are created for masked sprites.
- Collision detection works for zappers. 
  (A "COLLIDE!" text appears when collision occurs.)

## 2025.01.12 - Build 8
- Added roaming enemy gfx.

## 2025.01.10 - Build 7
- Added third map with moving enemies.
- The roaming enemy uses the vertical enemy gfx for now.
- Fixed first map color circles. 

## 2025.01.01 - Build 6
- Moving reworked, it now remembers last direction when stuck. (Used in second map.)

## 2024.12.20 - Build 5
- Added teleport graphics.
- Added second map.
- Game proceeds to next map when moving onto exit tile. (No transition though.)
- Teleport works (with 2 sec cooldown).
- Color circle tile border simplified.

## 2024.12.19 - Build 4
- Exit appears when all blocks are destroyed.

## 2024.12.19 - Build 3
- Player can pick up color by going over "colored" circles.
- Blocks are destroyed if touched by appropriate color.
 
## 2024.12.19 - Build 2
- Player appears.
- Player can move around and bounces back from objects.
- Added font, but not yet used.

## 2024.12.09 - Build 1
- First map appears (in glorious amber color).
- Zappers zaps (graphically).
- Blocks shows.

## 2024.11.27 - Initial commit
- Graphics for the first map and the player are ready.


