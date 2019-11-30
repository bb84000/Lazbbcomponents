# Lazbbcomponents packet for Lazarus

## Extra components with properties unavailable in Lazarus/FPC native components

__TSCrollButton__ : Text scrolling button (TSpeedbutton descendant)<br>
__TSCrollLabel__ : Text scrolling label (TLabel descendant)

## Common properties

__Scrolling__ (True or false): Enable or disable caption scrolling. When caption is shorter than button width, scrolling is always disabled. Default: scrolling  is not enabled.<br>			
__ScrollInterval__ (ms): Set the scroolling speed. A low interval means a high scrolling speed. A too low value doesn(t allow a smooth scrolling as the processor cannot do its job in the interval. Default : 50 ms.<br>
__ScrollAutoString__ (string): String added at teh end of the text during scrolling. Default is '...' .<br>
__ScrollGraph__ (True or false): Enable graphic (smooth) scrolling (pixel by pixel) instead text scrolling (character by character). Default: graphic.<br>
__ScrollStep__ (number): Scrolling step (character or pixel according scrolling mode). Default 1.<br>
__ScrollDirection__ : Scrolling direction, left to right or right to left. Default : left to right.  

Free use.

bb - sdtp - november 2019
