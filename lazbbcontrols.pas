{******************************************************************************}
{ lazbbcontrols : Controls with properties unavailable in lazarus controls     }
{ Added to lazbbComponents palette                                             }
{ bb - sdtp - march 2022                                                    }

{ TColorPicker : Combine color combobox with color dialog                      }
{                Popup menu to copy/paste colour name can be localized         }
{ TSignalMeter : Like progress bar but more responsive                                                              }
{ TLFPTimer : TFPtimer addition to the palette                                                                 }
{******************************************************************************}

unit lazbbcontrols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, LResources, Forms, Controls, Graphics,
  Dialogs, Buttons, Menus, Clipbrd, PropEdits, Messages, LCLIntf, lclproc, fptimer;

Const
  ColorArr: array of string = (
               'clBlack',
               'clMaroon',
               'clGreen',
               'clOlive',
               'clNavy',
               'clPurple',
               'clTeal',
               'clGray',
               'clSilver',
               'clRed',
               'clLime',
               'clYellow',
               'clBlue',
               'clFuchsia',
               'clAqua',
               'clMedGray',
               'clWhite',
               'clMoneyGreen',
               'clSkyBlue',
               'clCream',
               'clNone',
               'clDefault');

   { Other constants }
   fRBoxWidth  : Integer = 13; // Width of rectangular checkbox
   fRBoxHeight : Integer = 13; // Height of rectangular checkbox
   DT_SINGLELINE = $20;
   DT_NOPREFIX = $800;
   DT_CENTER = 1;
   DT_TOP = 0;
   DT_LEFT = 0;
   DT_BOTTOM = 8;

type
  TBidiMod = (Disabled);
  TSCrollDirection= (sdLeftToRight, sdRightToLeft);

  // TChceckboxX
  TState = (cbUnchecked,cbChecked,cbGrayed);
  TType = (cbCross,cbMark,cbBullet,cbDiamond,cbRect, cbBMP); // Added
  TMouseState = (msMouseUp,msMouseDown);

  // TColorPicker
  // System color combo plus color dialog
  // ColorDialog title cannot change, so no title property

 TColorPicker = Class(TWinControl)
   private
     FColor: Tcolor;
     FItemHeight: Integer;
     FItemWidth: Integer;
     FItems: TStrings;
     FOnchange: TNotifyEvent;
     FMnuCopyCaption: String;
     FMnuPasteCaption: String;
     ColorCombo: TComboBox;
     ColorBtn: TSpeedButton;
     ColorDlg: TColorDialog;
     PopupMnu: TPopupMenu;
     MnuCopy, MnuPaste: TMenuItem;
     procedure DoResize(Sender: TObject);
     procedure DoDrawItem (Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
     procedure DoSelect(Sender: TObject);
     procedure DoBtnClick(Sender: TObject);
     procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
     procedure SetItemHeight(ih: integer);
     procedure SetItemWidth(iw: integer);
     procedure SetColor(cl: TColor);
     procedure MnuCopyClick(Sender: TObject);
     procedure MnuPasteClick(Sender: TObject);
     procedure SetMnuCopyCaption (mcpy: string);
     procedure SetMnuPasteCaption (mpast: string);
     procedure MnuPopup(Sender: TObject);

   protected

   public
     constructor Create(AOwner: TComponent); override;

   published
     property ItemHeight : integer  read FItemHeight write SetItemHeight;
     property ItemWidth : integer  read FItemWidth write SetItemWidth;
     property Color: TColor read FColor write SetColor;
     property MnuCopyCaption: String read FMnuCopyCaption write SetMnuCopyCaption;
     property MnuPasteCaption: string read FMnuPasteCaption write SetMnuPasteCaption;
     property Enabled;
     property TabOrder;
     Property Tabstop;
     property Visible;
     property Align;
     property Font;
     property Onchange: TNotifyEvent read fOnchange write FOnchange;
  end;


 TCheckBoxX = class(TCustomControl)
 private
   { Private declarations }
   fChecked        : Boolean;
   fCaption       : String;
   fColor          : TColor;
   fState          : TState;
   fFont           : TFont;
   fAllowGrayed    : Boolean;
   fFocus          : Boolean;
   fType           : TType;
   fCheckColor     : TColor;
   fMouseState     : TMouseState;
   fAlignment      : TAlignment;
   fTextTop        : Integer;  // top of text
   fTextLeft       : Integer;  // left of text
   fBoxTop         : Integer;  // top of box
   fBoxLeft        : Integer;  // left of box
   fOnStateChange  : TNotifyEvent;
   fBitMap         : TbitMap;
   Procedure fSetAlignment(A : TAlignment);
   Procedure fSetAllowGrayed(Bo : Boolean);
   Procedure fSetCaption(S : String);
   Procedure fSetType(T : TType);
   Procedure fSetCheckColor(C : TColor);
   Procedure fSetChecked(Bo : Boolean);
   Procedure fSetColor(C : TColor);
   Procedure fSetFont(cbFont : TFont);
   Procedure fSetState(cbState : TState);
 protected
   Procedure Paint; override;
   Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   Procedure WMKillFocus(var Message : TWMKillFocus); Message WM_KILLFOCUS; // Yes, this removes the focus rect!
   Procedure WMSetFocus(var Message : TWMSetFocus); Message WM_SETFOCUS; // If you are using the TAB or  Shift-Tab key
   Procedure KeyDown(var Key : Word; Shift : TShiftState); override;
      // Interception of KeyDown
    Procedure KeyUp(var Key : Word; Shift : TShiftState); override;
      // Interception of KeyUp
 public
     { Public declarations }
    // If you put Create and Destroy under protected,
    // Delphi complains about that.
    //Bitmap: TBitmap;
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
 published
    Property Action;
    Property Alignment : TAlignment read fAlignment write fSetAlignment;
    Property AllowGrayed : Boolean read fAllowGrayed write fSetAllowGrayed;
    Property Anchors;
    Property BiDiMode;
    Property Caption: String read fCaption write fSetCaption;
    Property CheckBoxType : TType read fType write fSetType;
    Property CheckColor : TColor read fCheckColor write fsetCheckColor;
    Property Checked : Boolean read fChecked write fSetChecked;
    Property Color : TColor read fColor write fSetColor;
    Property Constraints;
    //Property Ctrl3D;
    Property Cursor;
    Property DragCursor;
    Property DragKind;
    Property DragMode;
    Property Enabled;
    Property Font : TFont read fFont write fSetFont;
    //Property Height;
    Property HelpContext;
    Property Hint;
    Property Left;
    Property Name;
    //Property PartenBiDiMode;
    Property ParentColor;
    //Property ParentCtrl3D;
    Property ParentFont;
    Property ParentShowHint;
    //Property PopMenu;
    Property ShowHint;
    Property State : TState read fState write fSetState;
    Property TabOrder;
    Property TabStop;
    Property Tag;
    Property Top;
    Property Visible;
    //Property Width;
    { --- Events --- }
    Property OnClick;
    Property OnContextPopup;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDock;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnStartDock;
    Property OnStartDrag;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property Bitmap: TBitMap read fBitMap write fBitmap;
 end;

type
  TTitlePanel = class(TCustomPanel)
  private
    fBorderLine: TBorderStyle;
    fBorderColor: TColor;
    procedure setBorderLine(bl: TBorderStyle);
    procedure SetBorderColor(bc: TColor);
  protected
    Procedure Paint; override;

  public
    Constructor Create(AOwner: TComponent); override;

  published
    property Align;
    property Alignment;
    property Anchors;
    property BorderLine: TBorderStyle read fBorderLine write setBorderLine;
    property BorderColor: TColor read fBorderColor write setBorderColor;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentBackground;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property Wordwrap;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

  end;

  type
  TSignalMeterOrientation = (gmHorizontal, gmVertical);
  TSignalMeter = class(TGraphicControl)
  private
    { Private declarations }
    fValue          : Double;
    fColorFore      : TColor;
    fColorBack      : TColor;
    fSignalUnit     : ShortString;
    fValueMax       : Double;
    fValueMin       : Double;
    fDigits         : Byte;
    fIncrement      : Double;
    fShowIncrements : Boolean;
    fGapTop         : Word;
    fGapBottom      : Word;
    fBarThickness   : Word;
    fMarkerColor    : TColor;
    fShowMarker     : Boolean;
    fShowTopText    : Boolean;
    fShowValueMin   : Boolean;
    fShowValueMax   : Boolean;
    fOrientation    : TSignalMeterOrientation;
    //Variables used internallly
    TopTextHeight: Word;
    fLeftMeter    : Word;
    DisplayValue : String;
    DrawStyle : Integer;
    TheRect : TRect;
    //End of variables used internallly
    procedure SetValue(val : Double);
    procedure SetColorBack(val : TColor);
    procedure SetColorFore(val : TColor);
    procedure SetSignalUnit(val : ShortString);
    procedure SetValueMin(val : Double);
    procedure SetValueMax(val : Double);
    procedure SetDigits(val : Byte);
    procedure SetTransparent(val : Boolean);
    Function GetTransparent : Boolean;
    procedure SetIncrement(val : Double);
    procedure SetShowIncrements(val : Boolean);
    procedure SetGapTop(val : Word);
    procedure SetGapBottom(val : Word);
    procedure SetLeftMeter (val : Word);
    procedure SetBarThickness(val : Word);
    procedure SetMarkerColor(val : TColor);
    procedure SetShowMarker(val : Boolean);
    procedure SetShowTopText(val : Boolean);
    procedure SetShowValueMin(val : Boolean);
    procedure SetShowValueMax(val : Boolean);
    procedure DrawTopText;
    procedure DrawMeterBar;
    procedure DrawIncrements;
    Function ValueToPixels(val : Double) : Integer;
    procedure DrawValueMax;
    procedure DrawValueMin;
    procedure DrawMarker;
    procedure SetOrientation(Value: TSignalMeterOrientation);
  protected
    { Protected declarations }
    procedure Paint;override;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  public
    { Public declarations }
    constructor Create(AOwner : Tcomponent);override;
    destructor Destroy ; override;
  published
    { Published declarations }
    property Align;
    property Caption;
    property Visible;
    property Value          : Double read fValue write SetValue;
    property Color;
    property ColorFore      : Tcolor read fColorFore write SetColorFore;
    property ColorBack      : Tcolor read fColorBack write SetColorBack;
    property SignalUnit     : ShortString read fSignalUnit write SetSignalUnit;
    property ValueMin       : Double read fValueMin write SetValueMin;
    property ValueMax       : Double read fValueMax write SetValueMax;
    property Digits         : Byte read fDigits write SetDigits;
    property Increment      : Double read fIncrement write SetIncrement;
    property ShowIncrements : Boolean read fShowIncrements write SetShowIncrements;
    property Transparent    : Boolean read GetTransparent write SetTransparent;
    property GapTop         : Word read fGapTop write SetGapTop;
    property GapBottom      : Word read fGapBottom write SetGapBottom;
    property LeftMeter      : Word read fLeftMeter write SetLeftMeter;
    property BarThickness   : Word read fBarThickness write SetBarThickness;
    property MarkerColor    : TColor read fMarkerColor write SetMarkerColor;
    property ShowMarker     : Boolean read fShowMarker write SetShowMarker;
    property ShowTopText    : Boolean read fShowTopText write SetShowTopText;
    property ShowValueMin    : Boolean read fShowValueMin write SetShowValueMin;
    property ShowValueMax    : Boolean read fShowValueMax write SetShowValueMax;
    property Orientation: TSignalMeterOrientation read FOrientation
      write SetOrientation default gmVertical;
  end;

  type
  TLFPTimer = Class(TFPCustomTimer)
  private

  protected

  public

  published
    Property Enabled;
    Property Interval;
    Property UseTimerThread;
    Property OnTimer;
    Property OnStartTimer;
    Property OnStopTimer;
  end;


procedure Register;

implementation

procedure Register;
begin
   {$I lazbbcontrols_icon.lrs}
   RegisterComponents('lazbbComponents',[TColorPicker]);
   RegisterComponents('lazbbComponents',[TCheckBoxX]);
   RegisterComponents('lazbbComponents',[TTitlePanel]);
   RegisterComponents('lazbbComponents',[TSignalMeter]);
   RegisterComponents('lazbbComponents',[TLFPTimer]);


   // Hide some properties from
   {RegisterPropertyEditor(TypeInfo(Boolean), TColorPicker, 'Autosize', THiddenPropertyEditor); // Need IDEIntf packet }
end;


// TColorPicker

constructor TColorPicker.Create(AOwner: TComponent);
var
  AStr:String;
begin
  {$I lazbbcontrols_icon.lrs}
  inherited;

  Caption:= '';
  Width:= 128;

  OnResize:= @DoResize;
  ColorCombo:= TcomboBox.Create(self);
  ColorBtn:= TSpeedButton.Create(self);
  ColorCombo.Parent:= self;
  ColorCombo.Style:= csOwnerDrawfixed;
  ColorCombo.Left:= 0;
  ColorCombo.Top:= 0;
  Height:= 23;
  ItemHeight:= 15;
  ItemWidth:= 0;
  ColorCombo.height:= height;
  ColorCombo.ItemHeight:= ItemHeight;
  ColorCombo.ItemWidth:= ItemWidth;
  ColorCombo.width:= 100;
  ColorCombo.BorderStyle:= bsNone;
  ColorCombo.visible:= true;
  ColorCombo.Items:= TstringList.Create;
  ParentFont:= false;
  ColorCombo.Font:= Font;
  for AStr in ColorArr do ColorCombo.Items.Add (AStr);
  ColorCombo.ItemIndex:= ColorCombo.Items.Count-1;
  FColor:= clDefault;
  ColorCombo.OnDrawItem:= @DoDrawItem;
  ColorCombo.OnSelect:=  @DoSelect;
  ColorCombo.OnKeyDown:= @DoKeyDown;

  ColorBtn.Parent:= self;
  ColorBtn.left:= 105;
  ColorBtn.Top:= 0;
  ColorBtn.Height:= Height;
  ColorBtn.Width:= Height;
  ColorBtn.Margin:= -1;
  ColorBtn.Visible:= true;
  ColorBtn.LoadGlyphFromLazarusResource('tcolorbtn');
  ColorBtn.OnClick:= @DoBtnClick;
  ColorDlg:= TColorDialog.Create(self);
  PopupMnu:= TPopupMenu.Create(ColorCombo);
  PopupMnu.OnPopup:= @MnuPopup;
  MnuCopy := TMenuItem.Create(PopupMnu);
  MnuCopy.Caption := MnuCopyCaption;
  MnuCopy.ShortCut:= TextToShortCut('CTRL+C');
  MnuCopy.OnClick := @MnuCopyClick;
  PopupMnu.Items.Add(MnuCopy);
  MnuPaste := TMenuItem.Create(PopupMnu);
  Mnupaste.Caption := MnuPasteCaption;
  MnuPaste.ShortCut:= TextToShortCut('CTRL+V');  ;
  MnuPaste.OnClick := @MnuPasteClick;
  PopupMnu.Items.Add(MnuPaste);
  PopupMenu:= PopupMnu;
  MnuCopyCaption:= 'Copy';
  MnuPasteCaption:= 'Paste';
  FItems:= TStringList.create;
  FItems.Assign(ColorCombo.Items);
end;

procedure TColorPicker.MnuCopyClick(Sender: TObject);
begin
  Clipboard.AsText:= ColorCombo.Items [ColorCombo.ItemIndex];
end;

procedure TColorPicker.MnuPasteClick(Sender: TObject);
var
  col: TColor;
begin
  try
    col:= StringToColor(Clipboard.AsText);
    SetColor(col);
  except
    ShowMessage('Wrong color value');
  end;
end;

procedure TColorPicker.MnuPopup(Sender: TObject);
var
  col: TColor;
begin
  try
    // avoid to paste a wrong color name
    col:= StringToColor(Clipboard.AsText);
    MnuPaste.Enabled:= True;
  except
    MnuPaste.Enabled:= False;
  end;
end;

procedure TColorPicker.SetMnuCopyCaption(mcpy: string);
begin
  if FMnuCopyCaption <> mcpy then
  begin
    FMnuCopyCaption:= mcpy;
    MnuCopy.Caption := mcpy;
  end;
end;

procedure TColorPicker.SetMnuPasteCaption(mpast: string);
begin
  if FMnupasteCaption <> mpast then
  begin
    FMnuPasteCaption:= mpast;
    MnuPaste.Caption := mpast;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;


procedure TColorPicker.SetItemHeight(ih: integer);
begin
  if FItemHeight <> ih then
  begin
    FItemHeight:= ih;
    ColorCombo.ItemHeight:= ih;
    Height:= ColorCombo.Height;
    ColorBtn.Height:= Height;
  end;
end;

procedure TColorPicker.SetItemWidth(iw: integer);
begin
  if FItemWidth <> iw then
  begin
    FItemWidth:= iw;
    ColorCombo.ItemWidth:= iw;
  end;
end;

procedure TColorPicker.SetColor(cl: TColor);
var
  i: integer;
  newcol: boolean;
begin
  if FColor <> cl then
  begin
    //ColorCombo.ItemIndex:=-1;
    newcol:=true;
    FColor:= cl;

    For i:= 0 to ColorCombo.Items.Count-1 do
      if ColorToString(cl)= ColorCombo.Items[i] then
      begin
        ColorCombo.ItemIndex:=i;
        newcol:= false;
      end;
    if newcol then
    begin
      ColorCombo.AddItem(ColorToString(cl), nil);
      ColorCombo.ItemIndex:= ColorCombo.Items.Count-1; ;

    end;

  end;
end;


procedure TColorPicker.DoResize(Sender: Tobject);
begin
  ColorCombo.Width:= width-28;
  ColorBtn.Top:= 0;
  ColorBtn.left:= width-Height;
  ColorBtn.Height:= Height;
  ColorBtn.Width:= Height;
  ColorCombo.Height:= height;
  height:= ColorCombo.Height;
end;

procedure TColorPicker.DoSelect(Sender: TObject);
begin
  FColor:= StringToColor(ColorCombo.Items[ColorCombo.ItemIndex]);
   if Assigned(FOnChange) then FOnChange(Self);
end;

// Owner draw paint combho with color

procedure TColorPicker.DoDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  ltRect: TRect;
  txtTop: integer;
  ColTop: Integer;
  flRect: TRect;
begin
  ColorCombo.Canvas.Font:= font;
  ColorCombo.Canvas.FillRect(ARect);                                                              //first paint normal background
  txtTop:= (ARect.Bottom-ARect.Top-ColorCombo.Canvas.TextHeight(ColorCombo.Items[Index])) div 2;  // To vertically center text
  ColTop:= (ARect.Bottom-ARect.Top-13) div 2;                                                     // Vertically center color square
  ColorCombo.Canvas.TextRect(ARect, 22, ARect.Top+txtTop, ColorCombo.Items[Index]);               //paint item text
  ltRect.Left   := ARect.Left+2;                                                                  //rectangle for color
  ltRect.Right  := ltRect.Left+13;
  ltRect.Top    := ARect.Top+ColTop;
  ltRect.Bottom := ltRect.Top+13 ;
  flrect.Left:= ltRect.Left+1;                                                                    //Reduce 1 pixel
  flRect.Right:= ltRect.Right-1;                                                                  //to see the lines around the colour
  flRect.Top:= ltRect.Top+1;
  flRect.Bottom:= ltRect.Bottom-1;                                                                //
  ColorCombo.Canvas.Rectangle(ltRect);
  ColorCombo.Canvas.Brush.Color := StringToColor(ColorCombo.Items[Index]);
  ColorCombo.Canvas.FillRect(flRect);
end;

procedure TColorPicker.DoBtnClick(Sender: TObject);
begin
  ColorDlg.Color:= FColor;
  if ColorDlg.Execute then
  SetColor(ColorDlg.Color);

end;

// implement Ctrl-C and Ctrl-V to copy and paste
procedure TColorPicker.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = Ord('C')) then
  begin
    MnuCopyClick(Sender);
    key:= 0;
  end;
  if (ssCtrl in Shift) and (Key = Ord('V')) then
  begin
    MnuPasteClick(Sender);
    key:= 0;
  end;
end;

// TCheckbox color

Destructor TCheckBoxX.Destroy;
Begin
inherited Destroy;
fBitMap.Free;
End;

Constructor TCheckBoxX.Create(AOwner : TComponent);
Begin
  inherited Create(AOwner);
Parent:= TWinControl(aOwner);
Height := 17;
Width := 97;
fChecked := False;
fColor := cldefault;
fState := cbUnChecked;
fFont := inherited Font;
fAllowGrayed := False;
fFocus := False;
fMouseState := msMouseUp;
fAlignment := taRightJustify;
TabStop := True; // Sorry
fBitMap:= TBitmap.Create;
fCaption:= 'ChecBoxX';
End;

Procedure TCheckBoxX.fSetAlignment(A : TAlignment);
Begin
If A <> fAlignment then
   Begin
   fAlignment := A;
   Invalidate;
   End;
End;

Procedure TCheckBoxX.fSetAllowGrayed(Bo : Boolean);
Begin
If fAllowGrayed <> Bo then
   Begin
   fAllowGrayed := Bo;
   If not fAllowGrayed then
      If fState = cbGrayed then
         Begin
         If fChecked then
            fState := cbChecked
         else
            fState := cbUnChecked;
         End;
   Invalidate;
   End;
End;

Procedure TCheckBoxX.fSetCaption(S : String);

Begin
If fCaption <> S then
   Begin
   fCaption := S;
   Invalidate;
   End;
End;

Procedure TCheckBoxX.fSetType(T : TType);
Begin
If fType <> T then
   Begin
   fType := T;
   Invalidate;
   End;
End;

Procedure TCheckBoxX.fSetCheckColor(C : TColor);
Begin
If fCheckColor <> C then
   Begin
   fCheckColor := C;
   Invalidate;
   End;
End;

Procedure TCheckBoxX.fSetChecked(Bo : Boolean);
Begin
If fChecked <> Bo then
   Begin
   fChecked := Bo;
   If fState <> cbGrayed then
      Begin
      If fChecked then
         fState := cbChecked
      else
         fState := cbUnChecked;
      End;
   Invalidate;
   End;
End;

Procedure TCheckBoxX.fSetColor(C : TColor);
Begin
If fColor <> C then
   Begin
   fColor := C;
   Invalidate;
   End;
End;

Procedure TCheckBoxX.fSetFont(cbFont : TFont);
Var
   FntChanged : Boolean;
Begin
FntChanged := False;
If fFont.Style <> cbFont.Style then
   Begin
   fFont.Style := cbFont.Style;
   FntChanged := True;
   End;
If fFont.CharSet <> cbFont.Charset then
   Begin
   fFont.Charset := cbFont.Charset;
   FntChanged := True;
   End;
If fFont.Size <> cbFont.Size then
   Begin
   fFont.Size := cbFont.Size;
   FntChanged := True;
   End;
If fFont.Name <> cbFont.Name then
   Begin
   fFont.Name := cbFont.Name;
   FntChanged := True;
   End;
If fFont.Color <> cbFont.Color then
   Begin
   fFont.Color := cbFont.Color;
   FntChanged := True;
   End;
If FntChanged then
   Invalidate;
End;

Procedure TCheckBoxX.fSetState(cbState : TState);
Begin
If fState <> cbState then
   Begin
   fState := cbState;
   If (fState = cbChecked) then
      fChecked := True;
   If (fState = cbGrayed) then
      fAllowGrayed := True;
   If fState = cbUnChecked then
      fChecked := False;
   Invalidate;
   End;
End;

Procedure TCheckBoxX.Paint;
Var
   I                             : Integer;
   fTextWidth,fTextHeight        : Integer;
Begin
  Canvas.Font.Size := Font.Size;
  Canvas.Font.Style := Font.Style;
  Canvas.Font.Color := Font.Color;
  Canvas.Font.Charset := Font.CharSet;

  fTextWidth := Canvas.TextWidth(Caption);
  fTextHeight := Canvas.TextHeight('Q');

  If fAlignment = taRightJustify then
  begin
    fBoxTop := (Height - fRBoxHeight) div 2;
    fBoxLeft := 0;
    fTextTop := (Height - fTextHeight) div 2;
    fTextLeft := fBoxLeft + fRBoxWidth + 4;
  end else
  begin
    fBoxTop := (Height - fRBoxHeight) div 2;
    fBoxLeft := Width - fRBoxWidth;
    fTextTop := (Height - fTextHeight) div 2;
    fTextLeft := 1;
  end;

  // Selected colors
  Canvas.Pen.Color := fColor;
  Canvas.Brush.Style:= bsClear;

  If (fState = cbGrayed) or (enabled= false) then
  Begin
    fAllowGrayed := True;
    Canvas.Font.color:= clGrayText;
    Canvas.Pen.Color:= clGrayText;
  End;
  // Write caption
  Canvas.TextOut(fTextLeft,fTextTop,Caption);
  // Now prepare the checkbox outline
  Canvas.Brush.Style:= bsSolid;
  If (fState = cbChecked) then Canvas.Brush.Color := clWindow;
  If (fState = cbUnChecked) then Canvas.Brush.Color := clWindow;

  // Make the box clBtnFace when the mouse is down
  // just like the "standard" CheckBox
  If fMouseState = msMouseDown then Canvas.Brush.Color := clBtnFace;
  // Now fill the box brush with default blank colour
  Canvas.FillRect(Rect(fBoxLeft + 1,
                     fBoxTop + 1,
                     fBoxLeft + fRBoxWidth - 1,
                     fBoxTop + fRBoxHeight - 1));

// Draw the rectangular checkbox  to be the same as Lazarus checkbox

Canvas.rectangle(fBoxLeft, fBoxTop, fBoxLeft + fRBoxWidth,fBoxTop + fRBoxHeight );

If fChecked then
   Begin
   Canvas.Pen.Color := fCheckColor; //clBlack;
   Canvas.Brush.Color := fCheckColor; //clBlack;

   // Paint the rectangle
   If fType = cbRect then
      Begin
      Canvas.FillRect(Rect(fBoxLeft + 1,fBoxTop + 1,
         fBoxLeft + fRBoxWidth - 1,fBoxTop + fRBoxHeight - 1));
      End;

   // Paint the bullet
   If fType = cbBullet then
      Begin
      Canvas.Ellipse(fBoxLeft + 3,fBoxTop + 3,
         fBoxLeft + fRBoxWidth - 3,fBoxTop + fRBoxHeight - 3);
      End;

   // Paint the cross
   If fType = cbCross then
      Begin
      {Right-top to left-bottom}
      Canvas.MoveTo(fBoxLeft + fRBoxWidth - 5,fBoxTop + 3);
         Canvas.LineTo(fBoxLeft + 2,fBoxTop + fRBoxHeight - 4);
      Canvas.MoveTo(fBoxLeft + fRBoxWidth - 4,fBoxTop + 3);
         Canvas.LineTo(fBoxLeft + 2,fBoxTop + fRBoxHeight - 3);
      Canvas.MoveTo(fBoxLeft + fRBoxWidth - 4,fBoxTop + 4);
         Canvas.LineTo(fBoxLeft + 3,fBoxTop + fRBoxHeight - 3);
      {Left-top to right-bottom}
      Canvas.MoveTo(fBoxLeft + 3,fBoxTop + 4);
         Canvas.LineTo(fBoxLeft + fRBoxWidth - 4,
            fBoxTop + fRBoxHeight - 3);
      Canvas.MoveTo(fBoxLeft + 3,fBoxTop + 3);
         Canvas.LineTo(fBoxLeft + fRBoxWidth - 3,
            fBoxTop + fRBoxHeight - 3);  //mid
      Canvas.MoveTo(fBoxLeft + 4,fBoxTop + 3);
         Canvas.LineTo(fBoxLeft + fRBoxWidth - 3,
            fBoxTop + fRBoxHeight - 4);
      End;

   // Paint the mark
   If fType = cbMark then
      For I := 0 to 2 do
         Begin
         {Left-mid to left-bottom}
         Canvas.MoveTo(fBoxLeft + 3,fBoxTop + 5 + I);
         Canvas.LineTo(fBoxLeft + 6,fBoxTop + 8 + I);
         {Left-bottom to right-top}
         Canvas.MoveTo(fBoxLeft + 6,fBoxTop + 6 + I);
         Canvas.LineTo(fBoxLeft + 10,fBoxTop + 2 + I);
         End;

   // Paint the diamond
   If fType = cbDiamond then
      Begin
      Canvas.Pixels[fBoxLeft + 06,fBoxTop + 03] := clBlack;
      Canvas.Pixels[fBoxLeft + 06,fBoxTop + 09] := clBlack;

      Canvas.MoveTo(fBoxLeft + 05,fBoxTop + 04);
      Canvas.LineTo(fBoxLeft + 08,fBoxTop + 04);

      Canvas.MoveTo(fBoxLeft + 05,fBoxTop + 08);
      Canvas.LineTo(fBoxLeft + 08,fBoxTop + 08);

      Canvas.MoveTo(fBoxLeft + 04,fBoxTop + 05);
      Canvas.LineTo(fBoxLeft + 09,fBoxTop + 05);

      Canvas.MoveTo(fBoxLeft + 04,fBoxTop + 07);
      Canvas.LineTo(fBoxLeft + 09,fBoxTop + 07);

      Canvas.MoveTo(fBoxLeft + 03,fBoxTop + 06);
      Canvas.LineTo(fBoxLeft + 10,fBoxTop + 06); // middle line
      End;
   // Paint the Bmp
   if fType = cbBmp then
   begin
     if assigned(Bitmap)then
     begin
       Canvas.Draw(fBoxLeft + 1,fBoxTop + 1, Bitmap);
     end;
   end;
   End;
End;

procedure TCheckBoxX.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
Begin
// The MouseDown procedure is only called when the mouse
// goes down WITHIN the control, so we don't have to check
// the X and Y values.
inherited MouseDown(Button, Shift, X, Y);
fMouseState := msMouseDown;
If fState <> cbGrayed then
   Begin
   SetFocus; // Set focus to this component
             // Windows sends a WM_KILLFOCUS message to all the
             // other components.
   fFocus := True;
   Invalidate;
   End;
End;

procedure TCheckBoxX.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
Begin
// The MouseUp procedure is only called when the mouse
// goes up WITHIN the control, so we don't have to check
// the X and Y values.
inherited MouseUp(Button, Shift, X, Y);
If fState <> cbGrayed then
begin
   fSetChecked(not fChecked); // Change the check
   if Assigned(FOnStateChange) then FOnStateChange(Self);
end;
   fMouseState := msMouseUp;
End;

Procedure TCheckBoxX.KeyDown(var Key : Word; Shift : TShiftState);
Begin
If fFocus then
   If Shift = [] then
      If Key = 0032 then
         Begin
         fMouseState := msMouseDown;
         If fState <> cbGrayed then
            Begin
            SetFocus; // Set focus to this component
                      // Windows sends a WM_KILLFOCUS message to all the
                      // other components.
            fFocus := True;
            Invalidate;
            End;
         End;
Inherited KeyDown(Key,Shift);
End;

Procedure TCheckBoxX.KeyUp(var Key : Word; Shift : TShiftState);
Begin
If fFocus then
   If Shift = [] then
      If Key = 0032 then
         Begin
           If fState <> cbGrayed then
           begin
             fSetChecked(not fChecked); // Change the check
             if Assigned(FOnStateChange) then FOnStateChange(Self);
           end;
           fMouseState := msMouseUp;
         End;
Inherited KeyUp(Key,Shift);
End;

Procedure TCheckBoxX.WMKillFocus(var Message : TWMKillFocus);
Begin
  fFocus := False; // Remove the focus rectangle of all the components,
                 // which doesn't have the focus.
  Invalidate;
End;

Procedure TCheckBoxX.WMSetFocus(var Message : TWMSetFocus);
begin
  fFocus := True;
  Invalidate;
End;

constructor TTitlePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent:= TWinControl(aOwner);
  fBorderLine:= bsSingle;
  Alignment:= taLeftJustify;
  FBorderColor:= clActiveBorder;
end;

procedure  TTitlePanel.setBorderLine(bl: TBorderStyle);
begin
  if bl <> fBorderLine then
  begin
    fBorderLine:= bl;
    Invalidate;
  end;
end;

procedure  TTitlePanel.SetBorderColor(bc: TColor);
begin
  if bc <> fBorderColor then
  begin
    fBorderColor:= bc;
    Invalidate;
  end;
end;

Procedure TTitlePanel.Paint;
var
  style: TTextStyle;
  txth, txtw: integer;
  lmrg: integer;
begin
  Canvas.Region.ClipRect;
  Canvas.Font:= Font;
  Style.SystemFont:= false;
  Canvas.Font.Style:= Font.Style;
  txth:= Canvas.TextHeight(caption);
  txtw:= Canvas.TextWidth(caption);
  lmrg:= 15;
  Case Alignment of
   taCenter: lmrg:= (width-txtw) div 2;
   taLeftJustify: lmrg:= 15;
   taRightJustify: lmrg:= width-txtw-15;
  end;
  // Remove background color on the full panel
  // replace with the parent/owner color
  Canvas.Brush.Color:= TWinControl(Parent).Color;
  Canvas.FillRect(Rect(0, 0, width, height));
  // Fill color on the used surface
  Canvas.Brush.Color:= Color;
  Canvas.FillRect(Rect(0, 8, width, height));
  Style.Opaque := True;

  //Top border with place for caption
  if BorderLine=bsSingle then
  begin
    Canvas.Pen.Color:= fBorderColor;
    Canvas.Line(0,8,lmrg,8);
    Canvas.Line(lmrg+txtw,8,width,8);
    Canvas.Line(width-1,8,width-1,height);
    Canvas.Line(0, height-1, width-1,height-1);
    Canvas.Line(0,8,0,height);
  end;
  Canvas.Brush.Style:= bsClear;

  Canvas.TextRect(Rect(lmrg, 0, txtw+lmrg, txth), lmrg ,0,caption, Style);

end;

constructor TSignalMeter.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     ControlStyle := ControlStyle + [csOpaque, csReplicatable, csSetCaption];
     width              := 100;
     height             := 200;
     fColorFore         := clRed;
     fColorBack         := clBtnFace;
     fMarkerColor       := clBlue;
     fValueMin          := 0;
     fValueMax          := 100;
     fLeftMeter         := 10;
     fIncrement         := 10;
     fShowIncrements    := true;
     fShowMarker        := true;
     fValue             := 0;
     fGapTop            := 10;
     fGapBottom         := 10;
     fBarThickness      := 5;
     fSignalUnit        := 'Units';
     fOrientation       := gmVertical;
end;

destructor TSignalMeter.Destroy;
begin
     inherited Destroy;
end;

procedure TSignalMeter.SetOrientation(Value: TSignalMeterOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Invalidate;
  end;
end;
procedure TSignalMeter.CMTextChanged(var Message: TMessage);
begin
     Invalidate;
end;

procedure TSignalMeter.SetValue(val : Double);
begin
     if (val <> fValue) and (val >= fValueMin) and (val <= fValueMax) then begin
        fValue := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetColorFore(val : TColor);
begin
     if val <> fColorFore then begin
        fColorFore := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetColorBack(val : TColor);
begin
     if val <> fColorBack then begin
        fColorBack := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetSignalUnit(val : ShortString);
begin
     if val <> fSignalUnit then begin
        fSignalUnit := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetValueMin(val : Double);
begin
     if (val <> fValueMin) and (val <= fValue) then begin
        fValueMin := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetValueMax(val : Double);
begin
     if (val <> fValueMax) and (val >= fValue) then begin
        fValueMax := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetDigits(val : Byte);
begin
     if (val <> fDigits) then begin
        fDigits := val;
        //Invalidate;
     end;
end;

procedure TSignalMeter.SetIncrement(val : Double);
begin
     if (val <> fIncrement) and (val > 0) then begin
        fIncrement := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetShowIncrements(val : Boolean);
begin
     if (val <> fShowIncrements) then begin
        fShowIncrements := val;
        Invalidate;
     end;
end;

function TSignalMeter.GetTransparent : Boolean;
begin
     Result := not (csOpaque in ControlStyle);
end;

procedure TSignalMeter.SetTransparent(Val : Boolean);
begin
  if Val <> Transparent then
  begin
    if Val then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TSignalMeter.SetGapTop(val : Word);
begin
     if (val <> fGapTop) then begin
        fGapTop := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetGapBottom(val : Word);
begin
     if (val <> fGapBottom) then begin
        fGapBottom := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetLeftMeter(val : Word);
begin
     if (val <> fLeftMeter) then begin
        fLeftMeter := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetBarThickness(val : Word);
begin
     if (val <> fBarThickness) and (val > 0) then begin
        fBarThickness := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetMarkerColor(val : TColor);
begin
     if (val <> fMarkerColor) then begin
        fMarkerColor := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetShowMarker(val : Boolean);
begin
     if (val <> fShowMarker) then begin
        fShowMarker := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetShowTopText(val : Boolean);
begin
     if (val <> fShowTopText) then begin
        fShowTopText := val;
        Invalidate;
     end ;
end;

procedure TSignalMeter.SetShowValueMin(val : Boolean);
begin
     if (val <> fShowValueMin) then begin
        fShowValueMin := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.SetShowValueMax(val : Boolean);
begin
     if (val <> fShowValueMax) then begin
        fShowValueMax := val;
        Invalidate;
     end;
end;

procedure TSignalMeter.DrawIncrements;
var
   i : Double;
   PosPixels : Word;
begin
     if fShowIncrements then begin
        With Canvas do begin
             i := fValueMin;
             While i <= fValueMax do begin
                   PosPixels := ValueToPixels(i);
                   pen.color := clGray;
                   MoveTo(LeftMeter + BarThickness + 3, PosPixels-1);
                   LineTo(LeftMeter + BarThickness + 7, PosPixels-1);
                   pen.color := clWhite;
                   MoveTo(LeftMeter + BarThickness + 3, PosPixels);
                   LineTo(LeftMeter + BarThickness + 7, PosPixels);
                   i := i+fIncrement;
             end;
        end;
     end;
end;

procedure TSignalMeter.DrawMarker;
begin
     If fShowMarker then begin
        With Canvas do begin
          pen.color := clWhite;
          Brush.Style := bsClear;
          MoveTo(LeftMeter - 2, ValueToPixels(fValue));
          LineTo(LeftMeter - 6, ValueToPixels(fValue)-4);
          LineTo(LeftMeter - 6, ValueToPixels(fValue)+4);
          pen.color := clGray;
          LineTo(LeftMeter - 2, ValueToPixels(fValue));

          pen.color := fMarkerColor;
          Brush.color := fMarkerColor;
          Brush.Style := bsSolid;
          Polygon([Point(LeftMeter - 3, ValueToPixels(fValue)),
                   Point(LeftMeter - 5, ValueToPixels(fValue)-2),
                   Point(LeftMeter - 5, ValueToPixels(fValue)+2),
                   Point(LeftMeter - 3, ValueToPixels(fValue))]);
        end;
     end;
end;

procedure TSignalMeter.DrawTopText;
begin
   If fShowTopText then begin
     With Canvas do begin
          DisplayValue := Caption;
          Brush.Style := bsClear;
          TheRect := ClientRect;
          DrawStyle := DT_SINGLELINE + DT_NOPREFIX + DT_CENTER + DT_TOP;
          Font.Style := [fsBold];
          TopTextHeight := DrawText(Handle, PChar(DisplayValue), Length(DisplayValue),
                                    TheRect, DrawStyle);

          Font.Style := [];
          TheRect.Top := TopTextHeight;
          DisplayValue := FloatToStrF(Value, ffFixed, 8, fDigits) + ' ' + fSignalUnit;
          TopTextHeight := TopTextHeight + DrawText(Handle, PChar(DisplayValue),
                           Length(DisplayValue), TheRect, DrawStyle);
          TopTextHeight := TopTextHeight + fGapTop;
     end;
   end else
   begin
     DisplayValue := '';
     TopTextHeight:= GapTop;
   end;
end;

procedure TSignalMeter.DrawValueMin;
begin
   If fShowValueMin then begin
     With Canvas do begin
          TheRect := ClientRect;
          TheRect.Left := LeftMeter + BarThickness + 10;
          TheRect.Top := TopTextHeight;
          TheRect.Bottom := Height - fGapBottom + 6;
          Brush.Style := bsClear;

          DrawStyle := DT_SINGLELINE + DT_NOPREFIX + DT_LEFT + DT_BOTTOM;
          DisplayValue := FloatToStrF(ValueMin, ffFixed, 8, fDigits) + ' ' + fSignalUnit;
          DrawText(Handle, PChar(DisplayValue), Length(DisplayValue), TheRect, DrawStyle);
     end;
   end;
end;

procedure TSignalMeter.DrawValueMax;
begin
   If fShowValueMax then begin
     With Canvas do begin
          TheRect := ClientRect;
          TheRect.Left := LeftMeter + BarThickness + 10;
          TheRect.Top := TopTextHeight - 6;
          Brush.Style := bsClear;

          DrawStyle := DT_SINGLELINE + DT_NOPREFIX + DT_LEFT + DT_TOP;
          DisplayValue := FloatToStrF(ValueMax, ffFixed, 8, fDigits) + ' ' + fSignalUnit;
          DrawText(Handle, PChar(DisplayValue), Length(DisplayValue), TheRect, DrawStyle);
     end;
   end;
end;

procedure TSignalMeter.DrawMeterBar;
begin
  Case fOrientation of
    gmHorizontal:
        With Canvas do begin
          pen.Color := fColorBack;
          Brush.Color := fColorBack;
          Brush.Style := bsSolid;
          //Rectangle(LeftMeter, ValueToPixels(fValueMax), LeftMeter + fBarThickness, ValueToPixels(fValueMin));
          Rectangle (GapBottom, LeftMeter, ValueToPixels(fValueMax), LeftMeter+ fBarThickness);
          pen.Color := fColorFore;
          Brush.Color := fColorFore;
          Brush.Style := bsSolid;
          //Rectangle(LeftMeter + 1, ValueToPixels(fValue), LeftMeter + fBarThickness, ValueToPixels(fValueMin));
          Rectangle (GapBottom , LeftMeter, ValueToPixels(fValue), LeftMeter+ fBarThickness);

          pen.color := clWhite;
          Brush.Style := bsClear;
          //MoveTo(LeftMeter + fBarThickness-1, ValueToPixels(fValueMax));
          MoveTo(GapBottom , LeftMeter+ fBarThickness);
          //LineTo(LeftMeter, ValueToPixels(fValueMax));
          LineTo(GapBottom, LeftMeter);
          //LineTo(LeftMeter, ValueToPixels(fValueMin)-1);
          LineTo(ValueToPixels(fValueMax),LeftMeter);
          pen.color := clGray;
          //LineTo(LeftMeter + fBarThickness, ValueToPixels(fValueMin)-1);
          LineTo(ValueToPixels(fValueMax), LeftMeter+ fBarThickness);
          //LineTo(LeftMeter + fBarThickness, ValueToPixels(fValueMax));
          LineTo(GapBottom, LeftMeter + fBarThickness );
          If (fValue > fValueMin) and (fValue < fValueMax) then begin
             pen.color := clWhite;
             //MoveTo(LeftMeter+1, ValueToPixels(fValue));
             MoveTo(ValueToPixels(fValue), LeftMeter+1);
             //LineTo(LeftMeter + fBarThickness, ValueToPixels(fValue));
             LineTo(ValueToPixels(fValue), LeftMeter+fBarThickness-1);
             pen.color := clGray;
             //MoveTo(LeftMeter+1, ValueToPixels(fValue)-1);
             MoveTo(ValueToPixels(fValue)+1, LeftMeter+1);
             //LineTo(LeftMeter + fBarThickness, ValueToPixels(fValue)-1);
             LineTo(ValueToPixels(fValue)+1, LeftMeter+fBarThickness-1);
          end;

        end;
    gmVertical:
        With Canvas do begin
          pen.Color := fColorBack;
          Brush.Color := fColorBack;
          Brush.Style := bsSolid;
          Rectangle(LeftMeter, ValueToPixels(fValueMax), LeftMeter + fBarThickness, ValueToPixels(fValueMin));

          pen.Color := fColorFore;
          Brush.Color := fColorFore;
          Brush.Style := bsSolid;
          Rectangle(LeftMeter + 1, ValueToPixels(fValue), LeftMeter + fBarThickness, ValueToPixels(fValueMin));

          pen.color := clWhite;
          Brush.Style := bsClear;
          MoveTo(LeftMeter + fBarThickness-1, ValueToPixels(fValueMax));
          LineTo(LeftMeter, ValueToPixels(fValueMax));
          LineTo(LeftMeter, ValueToPixels(fValueMin)-1);

          pen.color := clGray;
          LineTo(LeftMeter + fBarThickness, ValueToPixels(fValueMin)-1);
          LineTo(LeftMeter + fBarThickness, ValueToPixels(fValueMax));

          If (fValue > fValueMin) and (fValue < fValueMax) then begin
             pen.color := clWhite;
             MoveTo(LeftMeter+1, ValueToPixels(fValue));
             LineTo(LeftMeter + fBarThickness, ValueToPixels(fValue));
             pen.color := clGray;
             MoveTo(LeftMeter+1, ValueToPixels(fValue)-1);
             LineTo(LeftMeter + fBarThickness, ValueToPixels(fValue)-1);
          end;
       end;
     end;
end;

Function TSignalMeter.ValueToPixels(val : Double) : Integer;
var
   factor : Double;
begin
     Result := 0;
    Case fOrientation of
    gmHorizontal:
     If fValueMax > fValueMin then begin
        Factor := (Width-fGapBottom-fGapTop)/(fValueMax-fValueMin);
        Result := Round(Factor*val+fGapBottom);
     end;
    gmVertical:
      If fValueMax > fValueMin then begin
        Factor := (Height-fGapBottom-TopTextHeight)/(fValueMin-fValueMax);
        Result := Round(Factor*val -Factor*fValueMax+TopTextHeight);
     end;
  end;
end;


procedure TSignalMeter.Paint;
begin

     //if width < fLeftMeter+ fBarThickness + fLeftMeter then width := fLeftMeter+ fBarThickness + fLeftMeter;


     With Canvas do begin
          if not Transparent then begin
             Brush.Color := Self.Color;
             Brush.Style := bsSolid;
             FillRect(ClientRect);
          end;
          Brush.Style := bsClear;
          DrawTopText;
          DrawValueMin;
          DrawValueMax;
          DrawMeterBar;
          DrawMarker;
          DrawIncrements;
     end;
  end;





end.


