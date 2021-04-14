# Lazbbcomponents packet for Lazarus

## Extra components with properties unavailable in Lazarus/FPC native components

__TSCrollButton__ : Text scrolling button (TSpeedbutton descendant)<br>
__TSCrollLabel__ : Text scrolling label (TLabel descendant)<br>
__TColorPicker__ : Similar to the object inspector color selector, combine a system colors combo box and a color dialog.<br>
__TCheckBoxX__ : A checkbox with extra features, cross mark,, bullet, diamond, color fill and picture in addition to usual check mark.<br>
__TitlePanel__ : A panel with groupbos like title. Title can be centered or left/right justified.

## TSCrollButton and TScrollLabel properties

__Scrolling__ (True or false): Enable or disable caption scrolling. When caption is shorter than button width, scrolling is always disabled. Default: scrolling  is not enabled.<br>			
__ScrollInterval__ (ms): Set the scroolling speed. A low interval means a high scrolling speed. A too low value doesn(t allow a smooth scrolling as the processor cannot do its job in the interval. Default : 50 ms.<br>
__ScrollAutoString__ (string): String added at teh end of the text during scrolling. Default is '...' .<br>
__ScrollGraph__ (True or false): Enable graphic (smooth) scrolling (pixel by pixel) instead text scrolling (character by character). Default: graphic.<br>
__ScrollStep__ (number): Scrolling step (character or pixel according scrolling mode). Default 1.<br>
__ScrollDirection__ : Scrolling direction, left to right or right to left. Default : left to right.  

## TcolorPicker properties

  Base colors, and their identifiers cannot be changed.<br>
  Selected text cannot be edited. To change the selected color, use the colors dialog or paste a color with the popup menu. An invalid color is ignored.
  Selected colours not in base colours are added at the end of the list.<br>
  Like base combo box, component height is related to the items height.<br>
  The colors dialog is localized by the operating system.

__Color__ (TColor): Initial color<br>
__MnuCopyCaption, MnuPasteCaption__ : allow popup localization<br>

## TCheckBoxX properties

__CheckBoxType__ Cross, Mark, Bullet, Diamond, Color, picture<br>
__CheckColor__ Color of the above item (excepted picture)

## TitlePanel properties

  Bevel and border style are disabled. Border color can be customized. Title can be centered, left justified or right justified.

Free use.<br>
bb - sdtp - april 2021
