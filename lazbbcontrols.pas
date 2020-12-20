{******************************************************************************}
{ lazbbcontrols : Controls with properties unavailable in lazarus controls     }
{ Added to lazbbComponents palette                                             }
{ bb - sdtp - december 2020                                                    }
{ TSCrollButton : Speedbutton with scrolling caption                           }
{ TSCrollLabel ;: Label with scrolling caption                                 }
{   Scrolling (boolean): Enable or disable caption scrolling. When caption is  }
{       shorter than button width, scrolling is always disabled.               }
{   ScrollInterval (ms): Set the scroolling speed. A low interval means a high }
{       scrolling speed.                                                       }
{   ScrollAutoString (string): String added between caption text during        }
{       scrolling. Default is '...'                                            }
{   ScrollGraph (boolean): Enable or disable smooth scrolling (pixel by pixel  }
{       instead char by char).                                                 }
{   ScrollStep (integer): Increment scrolling step. default 1                  }
{   ScrollDirection (sdLeftToRight, sdRightToLeft)                             }
{ TColorPicker : Combine color combobox with color dialog                      }
{******************************************************************************}

unit lazbbcontrols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, LResources, Forms, Controls, Graphics, Dialogs, Buttons, PropEdits;

type
  TBidiMod = (Disabled);
  TSCrollDirection= (sdLeftToRight, sdRightToLeft);

  TSCrollButton = class(TSpeedButton)
  private
    FCaption: String;
    FScrolling: boolean;
    FSCrollInterval: integer;
    FScrollAutoString:string;
    FScrollGraph: Boolean;
    FSCrollStep: Integer;
    FScrollDirection: TSCrollDirection;
    FMargin: Integer;
    FBidiMode: TBidiMOd;
    FSpacing: Integer;
    CaptionBmp: Tbitmap;
    CaptionRect: TRect;
    FTimerScroll: TTimer;
    TimerGlyph:TTimer;
    PrevGlyphwidth: Integer;
    ScrollText: String;
    txtHeight, txtWidth: Integer;
    ScrollBmp: TBitMap;
    ScrollRect:Trect;
    BkGndBmp:Tbitmap;
    BkGndRect: Trect;
    ScrollIndex: Integer;
    BordersWidth:Integer;
    procedure ReInit;
    procedure OnTimerScrollL(Sender: TObject);       // Left to right
    procedure OnTimerScrollR(Sender: TObject);       // Right to left
    procedure OnTimerGlyph(Sender: TObject);
    procedure SetCaption(AValue: string);
    procedure SetMargin(AValue: integer);
    procedure SetSpacing(AValue: integer);
    procedure SetSCrolling(AValue: Boolean);
    procedure SetSCrollInterval(AValue:integer);
    procedure SetSCrollAutoString(AValue:string);
    procedure SetSCrollGraph(aValue: Boolean);
    procedure SetSCrollStep(aValue:Integer);
    procedure SetSCrollDirection(aValue: TSCrollDirection);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Déclarations publiées }
    property Caption: string read FCaption write SetCaption;
    property Margin: integer read FMargin write SetMargin default 4;
    property Spacing: integer read FSpacing write SetSpacing default 4;
    property BidiMode: TBiDiMod read FBidiMode default disabled;
    property Scrolling: Boolean read FScrolling write SetScrolling default false;
    property ScrollInterval: Integer read FSCrollInterval write SetSCrollInterval default 50;
    property ScrollAutoString: string read FScrollAutoString write SetSCrollAutoString;
    property ScrollGraph: Boolean read FScrollGraph write SetScrollGraph default true;
    property SCrollStep: Integer read FScrollStep write SetScrollStep default 1;
    property SCrollDirection: TSCrollDirection read FSCrollDirection write SetSCrollDirection default sdLeftToRight;
  end;

  // Scrolling label

  TSCrollLabel = class(TLabel)
  private
    FCaption: String;
    FScrolling: boolean;
    FSCrollInterval: integer;
    FScrollAutoString:string;
    FScrollGraph: Boolean;
    FSCrollStep: Integer;
    FScrollDirection: TSCrollDirection;
    FMargin: Integer;
    FBidiMode: TBiDiMod;  // Disable the property
    //Borderwidth: integer;
    CaptionBmp: Tbitmap;
    CaptionRect: TRect;
    FTimerScroll: TTimer;
    FTimerCanvas: TTimer;
    ScrollText: String;
    txtHeight, txtWidth: Integer;
    ScrollBmp: TBitMap;
    ScrollRect:Trect;
    BkGndBmp:Tbitmap;
    BkGndRect: Trect;
    ScrollIndex: Integer;
    BordersWidth:Integer;
    procedure ReInit;
    procedure OnTimerScrollL(Sender: TObject);         // Left to right
    procedure OnTimerScrollR(Sender: TObject);         // Right to left
    procedure OnTimerCanvas(Sender:Tobject);
    procedure SetCaption(AValue: string);
    procedure SetMargin(AValue: integer);
    procedure SetSCrolling(AValue: Boolean);
    procedure SetSCrollInterval(AValue:integer);
    procedure SetSCrollAutoString(AValue:string);
    procedure SetSCrollGraph(aValue: Boolean);
    procedure SetSCrollStep(aValue:Integer);
    procedure SetSCrollDirection(aValue: TSCrollDirection);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Déclarations publiées }
    property Caption: string read FCaption write SetCaption;
    property Margin: integer read FMargin write SetMargin default 0;
    property BidiMode: TBiDiMod read FBidiMode default disabled;
    property Scrolling: Boolean read FScrolling write SetScrolling default false;
    property ScrollInterval: Integer read FSCrollInterval write SetSCrollInterval default 50;
    property ScrollAutoString: string read FScrollAutoString write SetSCrollAutoString;
    property ScrollGraph: Boolean read FScrollGraph write SetScrollGraph default true;
    property SCrollStep: Integer read FScrollStep write SetScrollStep default 1;
    property SCrollDirection: TSCrollDirection read FSCrollDirection write SetSCrollDirection default sdLeftToRight;
  end;

  // TColorPicker
  // System color combo plus color dialog
  // ColorDialog title cannot change, so no title property

 TColorPicker = Class(TCustomControl)
   private
     FColor: Tcolor;
     FItemHeight: Integer;
     ColorCombo: TComboBox;
     ColorBtn: TSpeedButton;
     ColorDlg: TColorDialog;
   protected

   public
     constructor Create(AOwner: TComponent); override;
     procedure DoResize(Sender: TObject);
     procedure DoDrawItem (Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
     procedure DoSelect(Sender: TObject);
     procedure DoBtnClick(Sender: TObject);
     procedure SetItemHeight(ih: integer);
     procedure SetColor(cl: TColor);
   published
     property ItemHeight : integer  read FItemHeight write SetItemHeight;
     property Color: TColor read FColor write SetColor;
  end;

 const
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
               'clWhite',
               'clMoneyGreen',
               'clSkyBlue',
               'clCream',
               'clMedGray',
               'clNone',
               'clDefault');

procedure Register;

implementation

procedure Register;
begin
   {$I lazbbcontrols_icon.lrs}
   RegisterComponents('lazbbComponents',[TSCrollButton]);
   RegisterComponents('lazbbComponents',[TScrollLabel]);
   RegisterComponents('lazbbComponents',[TColorPicker]);
   // Hide some propertioes from
   {RegisterPropertyEditor(TypeInfo(Boolean), TColorPicker, 'Autosize', THiddenPropertyEditor); // Need IDEIntf packet }
end;


// TSCrollButton procedures

// TScrollButton creation

constructor TScrollButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent:= TWinControl(aOwner);
  FCaption:='ScrollButton';
  FScrolling:= true;
  AutoSize:= False;
  Width:= 75;
  FMargin:= 4;
  inherited Margin:= FMargin;
  FSpacing:= 4;
  inherited Spacing:= FSpacing;
  BordersWidth:= 3; // Arbitrary value for proper scrolling centering
  FScrollInterval:=50;
  FScrollAutoString:= '...';
  FScrollGraph:= true;
  FScrollStep:= 1;
  TimerGlyph:= TTimer.Create(self);
  TimerGlyph.Enabled:= True;
  TimerGlyph.OnTimer:= @OnTimerGlyph;
  TimerGlyph.Interval:= 100;
  FTimerSCroll:= TTimer.Create(self);
  FTimerScroll.Enabled:= False;
  FTimerScroll.OnTimer:= @OnTimerScrollL;
  FTimerScroll.Interval:= FScrollInterval;

  ScrollIndex:= 0;
  inherited Layout:= Layout;
  inherited Caption:= FCaption;
  if  csDesigning in ComponentState then
  begin
    Inherited Margin:= -1;
    Exit;
  end;
  ScrollText:= '';
  ScrollBmp:= Tbitmap.Create;
  ScrollBmp.PixelFormat:= pf8bit;
  BkGndBmp:= Tbitmap.Create;
  BkGndBmp.PixelFormat:= pf8bit;
  CaptionBmp:= Tbitmap.Create;
  PrevGlyphwidth:= Glyph.width;
  FBidiMode:= Disabled;
  inherited BiDiMode:= bdLeftToRight;
end;

procedure TScrollButton.SetCaption(AValue: string);
begin
  if fCaption=AValue then exit;
  FCaption:=AValue;
  //if csDesigning in ComponentState then
  inherited Caption:=FCaption;
  ReInit;
end;

procedure TScrollButton.SetMargin(AValue: integer);
begin
  if FMargin=AValue then exit;
  FMargin:=AValue;
  inherited Margin:= AValue;
  ReInit;
end;

procedure TScrollButton.SetSpacing(AValue: integer);
begin
  if FSpacing=AValue then exit;
  FSpacing:=AValue;
  inherited Spacing:= AValue;
  ReInit;
end;

procedure TScrollButton.SetSCrolling(AValue: Boolean);
begin
  if FScrolling= AValue then exit;
  FSCrolling:= AValue;
  ReInit;
end;

procedure TScrollButton.SetScrollInterval(AValue:integer);
begin
  if FScrollInterval= AValue then exit;
  FScrollInterval:= AValue;
  FTimerScroll.Interval:= AValue;
  //ReInit;  //Not needed, will be updated next timer tick
end;

procedure TScrollButton.SetSCrollAutoString(AValue:string);
begin
  if FSCrollAutoString= AValue then exit;
  FSCrollAutoString:= AValue;
  ReInit;
end;

procedure TScrollButton.SetSCrollGraph(AValue:Boolean);
begin
  if fScrollGraph= AValue then exit;
  fsCrollGraph:= AValue;
  Reinit;
end;

procedure TScrollButton.SetSCrollStep(AValue:Integer);
begin
  if fScrollStep=aValue then exit;
  fSCrollStep:= AValue;
  //ReInit;  //Not needed, will be updated next timer tick
end;

procedure TScrollButton.SetSCrollDirection(aValue: TSCrollDirection);
begin
  if FScrollDirection=aValue then exit;
  FScrollDirection:= aValue;
    if aValue=sdLeftToRight then FTimerScroll.OnTimer:= @OnTimerScrollL
  else FTimerScroll.OnTimer:= @OnTimerScrollR ;

  //ReInit;  //Not needed, will be updated next timer tick
end;

destructor TScrollButton.Destroy;
begin
  if assigned(BkGndBmp) then BkGndBmp.Free;
  if assigned(ScrollBmp) then ScrollBmp.Free;
  if assigned(CaptionBmp) then CaptionBmp.Free;
  if assigned(FTimerScroll) then FTimerSCroll.free;
  if assigned(TimerGlyph) then TimerGlyph.free;
  Inherited Destroy;
end;

procedure TScrollButton.ReInit;
var
  BegTxt: integer;
begin
  if csDesigning in ComponentState then exit;
  inherited Caption:= '';      // avoid some flickering
  inherited margin:= FMargin;  // if it was previously changed
  // Scroll désactivé
  FTimerScroll.enabled:= false;
  if (Glyph.width=0) or (layout=blGlyphTop) or (layout=blGlyphBottom)
  // if FCaption shorter than button free space, then exit.
  then BegTxt:= FMargin else BegTxt:= (Glyph.Width div NumGlyphs)+FSpacing+FMargin;
  Canvas.Font.Assign(Font);
  if (Canvas.TextWidth(FCaption) <= ClientWidth-BegTxt-FMargin-BordersWidth) or not FScrolling then
  begin
    Inherited Margin:= -1;
    Inherited Caption:= FCaption;
    Exit;
  end  ;
  if csDesigning in ComponentState then exit;
  // So, we scroll !
  ScrollText:= FCaption+FScrollAutoString;
  TxtHeight:= Canvas.TextHeight(ScrollText);
  TxtWidth:= Canvas.TextWidth(ScrollText);
  CaptionRect.Top:= (height-TxtHeight) div 2;
  CaptionRect.Bottom:= CaptionRect.Top+TxtHeight;
  BkGndBmp.Width:= 1;
  BkGndBmp.Height:= TxtHeight;
  BkGndRect:= Rect(0, 0, 1, TxtHeight);
  if (Glyph.width=0)  then
  begin
    CaptionRect.Left:= FMargin+1;
    CaptionRect.Right:= CaptionRect.Left+ClientWidth-FMargin*2-BordersWidth;
    BkGndBmp.Canvas.CopyRect(BkGndRect, Canvas,
                              Rect(CaptionRect.Left-1, CaptionRect.top, CaptionRect.Left, CaptionRect.Top+1));
  end else
  Case Layout of
    blGlyphLeft: begin
                   CaptionRect.Left:= FMargin+Fspacing+(glyph.width div NumGlyphs)+FMargin;
                   CaptionRect.Right:= ClientWidth+1-FMargin*2;
                   BkGndBmp.Canvas.CopyRect(BkGndRect, Canvas,
                              Rect(CaptionRect.Left-1, CaptionRect.top, CaptionRect.Left, CaptionRect.Bottom));
                 end;
    blGlyphRight:begin
                   CaptionRect.Left:= FMargin+1;
                   CaptionRect.Right:= ClientWidth-Fspacing-FMargin*2-(glyph.width div NumGlyphs);
                   BkGndBmp.Canvas.CopyRect(BkGndRect, Canvas,
                              Rect(CaptionRect.Right, CaptionRect.top, CaptionRect.Right+1, CaptionRect.Bottom));
                 end;
    blGlyphTop : begin
                   CaptionRect.Left:= FMargin+1;
                   CaptionRect.Right:= CaptionRect.Left+ClientWidth-FMargin*3;
                   CaptionRect.Top:=FMargin+glyph.height+ (ClientHeight-FMargin-glyph.height-Canvas.TextHeight(ScrollText) ) div 2;
                   CaptionRect.Bottom:= FMargin+glyph.height+ (ClientHeight-FMargin-glyph.height+Canvas.TextHeight(ScrollText) ) div 2;
                   BkGndBmp.Canvas.CopyRect(BkGndRect, Canvas,
                              Rect(CaptionRect.Left-1, CaptionRect.top, CaptionRect.Left, CaptionRect.Bottom));
                 end;
    blGlyphBottom: begin
                   CaptionRect.Left:= FMargin+1;
                   CaptionRect.Right:= CaptionRect.Left+ClientWidth-FMargin*3;
                   CaptionRect.Top:=(ClientHeight-FMargin-glyph.height-Canvas.TextHeight(ScrollText)) div 2;
                   CaptionRect.Bottom:=(ClientHeight-FMargin-glyph.height+Canvas.TextHeight(ScrollText) ) div 2;
                   BkGndBmp.Canvas.CopyRect(BkGndRect, Canvas,
                              Rect(CaptionRect.Left-1, CaptionRect.top, CaptionRect.Left, CaptionRect.Bottom));
                end;
  end;
  If fScrollGraph then
  begin
    ScrollBmp.Canvas.Font.Assign(Font);
    ScrollBmp.width:=  2*txtWidth;
    ScrollBmp.Height:= txtHeight;
    ScrollRect:= Rect(0,0,2*txtWidth,txtHeight);
    ScrollBmp.Canvas.StretchDraw(ScrollRect, BkGndBmp);
    ScrollBmp.Canvas.Brush.Style:= bsClear;
    ScrollBmp.Canvas.TextOut(0,0, ScrollText+ScrollText);
  end;
  FTimerScroll.Enabled:= FScrolling;
end;

// This timer detect if glyph size has changed and reinit.

procedure TScrollButton.OnTimerGlyph(Sender: TObject);
begin
  if csDesigning in ComponentState then exit;
  if PrevGlyphwidth=Glyph.width then exit
  else
  begin
    PrevGlyphwidth:=Glyph.width;
    ReInit;
  end;
end;


// Timer procedure for left to right
// separate procedures to reduce processing time in the timer event

procedure TScrollButton.OnTimerScrollL(Sender: TObject);
begin
  if fSCrollGraph then   // pixel by pixel
  begin
    if (ScrollIndex=0) then ReInit;
    if ScrollIndex < (ScrollBmp.Width div 2)-1 then Inc(ScrollIndex, FSCrollStep)
    else ScrollIndex:= 0;
    // Background change when button has focus
    BkGndBmp.Canvas.CopyRect(BkGndRect, Canvas,
                              Rect(CaptionRect.Left-1, CaptionRect.top, CaptionRect.Left, CaptionRect.Bottom));
    ScrollBmp.Canvas.StretchDraw(ScrollRect, BkGndBmp);
    ScrollBmp.Canvas.TextOut(0,0, ScrollText+ScrollText);
    Canvas.CopyRect(CaptionRect, ScrollBmp.Canvas, Rect(ScrollIndex,0,ScrollIndex+CaptionRect.Right-CaptionRect.left ,
         txtHeight));
  end else              // scroll char by char
  begin
    ScrollText:= Copy(ScrollText, 2, Length(ScrollText) - 1) + Copy(ScrollText,1,1) ;
    inherited Caption:= scrolltext;
  end;
end;

// Timer procedure for right to left
// separate procedures to reduce processing time in the timer event

procedure TScrollButton.OnTimerScrollR(Sender: TObject);
begin
    if fSCrollGraph then
  begin
    if (ScrollIndex=0) then ReInit;
    if ScrollIndex >0 then Dec(ScrollIndex, FSCrollStep)
    else ScrollIndex:= (ScrollBmp.Width div 2);
    // Background change when button has focus
    BkGndBmp.Canvas.CopyRect(BkGndRect, Canvas,
                              Rect(CaptionRect.Left-1, CaptionRect.top, CaptionRect.Left, CaptionRect.Bottom));
    ScrollBmp.Canvas.StretchDraw(ScrollRect, BkGndBmp);
    ScrollBmp.Canvas.TextOut(0,0, ScrollText+ScrollText);
    Canvas.CopyRect(CaptionRect, ScrollBmp.Canvas, Rect(ScrollIndex,0,ScrollIndex+CaptionRect.Right-CaptionRect.left ,
         txtHeight));
  end else                          // scroll char by char
  begin
    ScrollText:=Copy(ScrollText,Length(ScrollText),1)+Copy(ScrollText, 1, Length(ScrollText)-1);
    inherited Caption:= scrolltext;
  end;
end;


// TScrollLabel procedures

constructor TScrollLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent:= TWinControl(aOwner);
  FCaption:='ScrollButton';
  FScrolling:= true;
  AutoSize:= False;
  Width:= 75;
  FMargin:= 0;
  BordersWidth:= 3; // Arbitrary value for proper scrolling centering
  FScrollInterval:=50;
  FScrollAutoString:= '...';
  FScrollGraph:= true;
  FScrollStep:= 1;
  FTimerSCroll:= TTimer.Create(self);
  FTimerScroll.Enabled:= False;
  FTimerScroll.OnTimer:= @OnTimerScrollL;
  FTimerScroll.Interval:= FScrollInterval;
  ScrollIndex:= 0;
  inherited Layout:= Layout;
  inherited Caption:= FCaption;
  ScrollText:= '';
  ScrollBmp:= Tbitmap.Create;
  ScrollBmp.PixelFormat:= pf8bit;
  BkGndBmp:= Tbitmap.Create;
  BkGndBmp.PixelFormat:= pf8bit;
  CaptionBmp:= Tbitmap.Create;
  FTimerCanvas:= TTimer.Create(self);
  FTimerCanvas.Enabled:= true;
  FTimerCanvas.Interval:= 50;
  FTimerCanvas.OnTimer:= @OntimerCanvas;
  FBidiMode:= Disabled;
  inherited BiDiMode:= bdLeftToRight;
end;

// Canvas is not immediately created, so reinit until it is created
// Needed to use margin proeprty when scroll is disabled

procedure TScrollLabel.OnTimerCanvas(sender: TObject);
begin
  if assigned(Canvas) then
  begin
    FTimerCanvas.Enabled:= false;
    ReInit;
  end;
end;

procedure TScrollLabel.SetCaption(AValue: string);
begin
  if fCaption=AValue then exit;
  FCaption:=AValue;
  inherited Caption:=FCaption;
  ReInit;
end;

procedure TScrollLabel.SetMargin(AValue: integer);
begin
  if FMargin=AValue then exit;
  FMargin:=AValue;
  //inherited Margin:= AValue;
  ReInit;
end;

procedure TScrollLabel.SetSCrolling(AValue: Boolean);
begin
  if FScrolling= AValue then exit;
  FSCrolling:= AValue;
  ReInit;
end;

procedure TScrollLabel.SetScrollInterval(AValue:integer);
begin
  if FScrollInterval= AValue then exit;
  FScrollInterval:= AValue;
  FTimerScroll.Interval:= AValue;
  //ReInit;  //Not needed, will be updated next timer tick
end;

procedure TScrollLabel.SetSCrollAutoString(AValue:string);
begin
  if FSCrollAutoString= AValue then exit;
  FSCrollAutoString:= AValue;
  ReInit;
end;

procedure TScrollLabel.SetSCrollGraph(AValue:Boolean);
begin
  if fScrollGraph= AValue then exit;
  fsCrollGraph:= AValue;
  Reinit;
end;

procedure TScrollLabel.SetSCrollStep(AValue:Integer);
begin
  if fScrollStep=aValue then exit;
  fSCrollStep:= AValue;
  //ReInit;  //Not needed, will be updated next timer tick
end;

procedure TScrollLabel.SetSCrollDirection(aValue: TSCrollDirection);
begin
  if FScrollDirection=aValue then exit;
  FScrollDirection:= aValue;
  if aValue=sdLeftToRight then FTimerScroll.OnTimer:= @OnTimerScrollL
  else FTimerScroll.OnTimer:= @OnTimerScrollR ;
  //ReInit;  //Not needed, will be updated next timer tick
end;

destructor TScrollLabel.Destroy;
begin
  if assigned(BkGndBmp) then BkGndBmp.Free;
  if assigned(ScrollBmp) then ScrollBmp.Free;
  if assigned(CaptionBmp) then CaptionBmp.Free;
  if assigned(FTimerScroll) then FTimerSCroll.free;
  if assigned(FTimerCanvas) then FTimerCanvas.free;
  Inherited Destroy;
end;

procedure TSCrollLabel.ReInit;

begin
  if csDesigning in ComponentState then exit;
  inherited Caption:= '';   // avoid some flickering
  FTimerScroll.enabled:= false;  // before scrolling process
  Canvas.Font.Assign(Font);
  TxtHeight:= Canvas.TextHeight(ScrollText);
  if (Canvas.TextWidth(FCaption) < (ClientWidth-FMargin*2)+BordersWidth) or not FScrolling then
  begin
    inherited Caption:= FCaption;
    Exit;
  end;
  // So, we scroll !
  ScrollText:= FCaption+FScrollAutoString;
  TxtHeight:= Canvas.TextHeight(ScrollText);
  TxtWidth:= Canvas.TextWidth(ScrollText);
  CaptionRect.Top:= (height-TxtHeight) div 2;
  CaptionRect.Bottom:= CaptionRect.Top+TxtHeight;
  BkGndBmp.Width:= 1;
  BkGndBmp.Height:= 1;
  BkGndRect:= Rect(0, 0, 1, 1);
  CaptionRect.Left:= FMargin;
  CaptionRect.Right:= CaptionRect.Left+ClientWidth-FMargin*2;
  // Get label background color (pixel at top left)
  if color= clnone then BkGndBmp.Canvas.CopyRect(BkGndRect, Canvas,
          Rect(CaptionRect.Left-1, CaptionRect.top, CaptionRect.Left, CaptionRect.Top+1))
  else BkGndBmp.Canvas.CopyRect(BkGndRect, Canvas,
          Rect(CaptionRect.Left, CaptionRect.top, CaptionRect.Left+1, CaptionRect.Top+1)) ;
  If fScrollGraph then
   begin
     ScrollBmp.Canvas.Font.Assign(Font);
     ScrollBmp.width:=  2*txtWidth;
     ScrollBmp.Height:= txtHeight;
     ScrollRect:= Rect(0,0,2*txtWidth,txtHeight);
     ScrollBmp.Canvas.StretchDraw(ScrollRect, BkGndBmp);
     ScrollBmp.Canvas.Brush.Style:= bsClear;
     ScrollBmp.Canvas.FloodFill(0,0,Color, fssurface);
     ScrollBmp.Canvas.TextOut(0,0, ScrollText+ScrollText);
   end;
  FTimerScroll.Enabled:= FScrolling;
end;

// Timer procedure for left to right
// separate procedures to reduce processing time in the timer event

procedure TScrollLabel.OnTimerScrollL(Sender: TObject);
begin
  if fSCrollGraph then   // pixel by pixel
  begin
    if (ScrollIndex=0) then ReInit;
    if ScrollIndex < (ScrollBmp.Width div 2)-1 then Inc(ScrollIndex, FSCrollStep)
    else ScrollIndex:= 0;
    Canvas.CopyRect(CaptionRect, ScrollBmp.Canvas, Rect(ScrollIndex,0,ScrollIndex+CaptionRect.Right-CaptionRect.left ,
         txtHeight));
  end else              // scroll char by char
  begin
    ScrollText:= Copy(ScrollText, 2, Length(ScrollText) - 1) + Copy(ScrollText,1,1) ;
    inherited Caption:= scrolltext;
  end;
end;

// Timer procedure for right to left

procedure TScrollLabel.OnTimerScrollR(Sender: TObject);
begin
  if fSCrollGraph then
  begin
    if (ScrollIndex=0) then ReInit;
    if ScrollIndex >0 then Dec(ScrollIndex, FSCrollStep)
    else ScrollIndex:= (ScrollBmp.Width div 2);
    Canvas.CopyRect(CaptionRect, ScrollBmp.Canvas, Rect(ScrollIndex,0,ScrollIndex+CaptionRect.Right-CaptionRect.left ,
         txtHeight));
  end else                          // scroll char by char
  begin
    ScrollText:=Copy(ScrollText,Length(ScrollText),1)+Copy(ScrollText, 1, Length(ScrollText)-1);
    inherited Caption:= scrolltext;
  end;
end;

// TColorPicker

constructor TColorPicker.Create(AOwner: TComponent);
var
  AStr:String;
begin
  inherited;
  {$I lazbbcolorbtn.lrs}
  Caption:= '';
  Width:= 128;
  OnResize:= @DoResize;
  ColorCombo:= TcomboBox.Create(self);
  ColorCombo.Parent:= self;
  ColorCombo.Style:= csOwnerDrawFixed;
  ColorCombo.Left:= 0;
  ColorCombo.Top:= 0;
  Height:= 23;
  ItemHeight:= 15;
  ColorCombo.height:= height;
  ColorCombo.ItemHeight:= ItemHeight;
  ColorCombo.width:= 100;
  ColorCombo.BorderStyle:= bsNone;
  ColorCombo.visible:= true;
  ColorCombo.Items:= TstringList.Create;
  for AStr in ColorArr do ColorCombo.Items.Add (AStr);
  ColorCombo.ItemIndex:= ColorCombo.Items.Count-1;
  FColor:= clDefault;
  ColorCombo.OnDrawItem:= @DoDrawItem;
  ColorCombo.OnSelect:=  @DoSelect;
  ColorBtn:= TSpeedButton.Create(self);
  ColorBtn.Parent:= self;
  ColorBtn.left:= 105;
  ColorBtn.Top:= 0;
  ColorBtn.Height:= Height;
  ColorBtn.Width:= Height;
  ColorBtn.Visible:= true;
  ColorBtn.LoadGlyphFromLazarusResource('tcolorbtn');
  ColorBtn.OnClick:= @DoBtnClick;
  ColorDlg:= TColorDialog.Create(self);

end;


procedure TColorPicker.SetItemHeight(ih: integer);
begin
  if FItemHeight <> ih then
  FItemHeight:= ih;
  ColorCombo.ItemHeight:= ih;
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
  ColorBtn.left:= width-23;
  ColorCombo.Height:= height;
  height:= ColorCombo.Height;
end;

procedure TColorPicker.DoSelect(Sender: TObject);
begin
  FColor:= StringToColor(ColorCombo.Items[ColorCombo.ItemIndex]);
end;

procedure TColorPicker.DoDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  ltRect: TRect;
  flRect: TRect;
begin
  ColorCombo.Canvas.FillRect(ARect);                                         //first paint normal background
  ColorCombo.Canvas.TextRect(ARect, 22, ARect.Top, ColorCombo.Items[Index]);  //paint item text
  ltRect.Left   := ARect.Left   + 2;                                        //rectangle for color
  ltRect.Right  := ARect.Left   + 15;
  ltRect.Top    := ARect.Top    + 2;
  ltRect.Bottom := ARect.Bottom - 2;
  flrect.Left:= ltRect.Left+1;
  flRect.Right:= ltRect.Right-1;
  flRect.Top:= ltRect.Top+1;
  flRect.Bottom:= ltRect.Bottom-1;
  ColorCombo.Canvas.Pen.Color:= clBlack;
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

end.

