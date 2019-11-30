# Lazbbcomponents packet for Lazarus

Extra components with properties unavailable in Lazarus/FPC native components

TSCrollButton : Text scrolling button (TSpeedbutton descendant)
TSCrollLabel : Text scrolling label (TLabel descendant)

Common properties

  Scrolling (True or false): Enable or disable caption scrolling. When caption is
            shorter than button width, scrolling is always disabled. Default: 
			scrolling  is not enabled.
  ScrollInterval (ms): Set the scroolling speed. A low interval means a high
            scrolling speed. A too low value doesn(t allow a smooth scrolling as 
			the processor cannot do its job in the interval. Default : 50 ms.
  ScrollAutoString (string): String added at teh end of the text during scrolling.
            Default is '...' 
  ScrollGraph (True or false): Enable graphic (smooth) scrolling (pixel by pixel 
            instead text scrolling (character by character). Default: graphic.
  ScrollStep (number): Scrolling step (character or pixel according scrolling 
            mode). Default 1
  ScrollDirection : Scrolling direction, left to right or right to left.
            Default : left to right.  

Free use.

bb - sdtp - november 2019
