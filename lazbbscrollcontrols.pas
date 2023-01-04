{******************************************************************************
  lazbbscrollcontrols : Label and Button with scrolling caption
  Added to lazbbComponents palette
  bb - sdtp - february 2022
  TbbScrollButton : Speedbutton with scrolling caption
  TbbScrollLabel ;: Label with scrolling caption
   Scrolling (boolean): Enable or disable caption scrolling. When caption is
       shorter than button width, scrolling is always disabled.
   ScrollInterval (ms): Set the scroolling speed. A low interval means a high
       scrolling speed.
   ScrollAutoString (string): String added between caption text during
       scrolling. Default is '...'
   ScrollGraph (boolean): Enable or disable smooth scrolling (pixel by pixel
       instead char by char).
   ScrollStep (integer): Increment scrolling step. default 1
   ScrollDirection (sdLeftToRight, sdRightToLeft)
********************************************************************************}

unit lazbbscrollcontrols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  fpTimer, ExtCtrls, StdCtrls;


type
TScrollDirection= (sdLeftToRight, sdRightToLeft);
TBidiMod = (Disabled);

TbbScrollButton = class(TSpeedButton)
  private
    FCaption: String;
    FScrolling: boolean;
    FScrollInterval: integer;
    FScrollAutoString:string;
    FScrollGraph: Boolean;
    FScrollStep: Integer;
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
    procedure SetScrolling(AValue: Boolean);
    procedure SetScrollInterval(AValue:integer);
    procedure SetScrollAutoString(AValue:string);
    procedure SetScrollGraph(aValue: Boolean);
    procedure SetScrollStep(aValue:Integer);
    procedure SetScrollDirection(aValue: TSCrollDirection);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { D�clarations publi�es }
    property Caption: string read FCaption write SetCaption;
    property Margin: integer read FMargin write SetMargin default 4;
    property Spacing: integer read FSpacing write SetSpacing default 4;
    property BidiMode: TBiDiMod read FBidiMode default disabled;
    property Scrolling: Boolean read FScrolling write SetScrolling default false;
    property ScrollInterval: Integer read FScrollInterval write SetScrollInterval default 50;
    property ScrollAutoString: string read FScrollAutoString write SetScrollAutoString;
    property ScrollGraph: Boolean read FScrollGraph write SetScrollGraph default true;
    property ScrollStep: Integer read FScrollStep write SetScrollStep default 1;
    property ScrollDirection: TScrollDirection read FScrollDirection write SetScrollDirection default sdLeftToRight;
  end;

TbbScrollLabel = class(TCustomLabel)
private
  FScrollAutoString: String;
  FScrollDirection: TSCrollDirection;
  FSCrollGraph: Boolean;
  FScrolling: Boolean;
  FSCrollInterval: Integer;
  FScrollStep: Integer;
  //FTransparent: Boolean;
  //FWordWrap: Boolean;
  CaptionRect: TRect;
  TxtHeight, TxtWidth, CaptionWidth: Integer;
  ScrollBmp: TBitmap;
  ScrollIndex: Integer;
  ScrollRect: Trect;
  bkColor: TColor;
  ScrollText: String;
  TextScroll: String;
  TimerScroll:TFPTimer;
  First: Boolean;
  //IsLoaded: Boolean;
  xOff, yOff: Integer;

  NoScrolling: Boolean;
  function GetAlignment: TAlignment;
  procedure SetAlignment(al: TAlignment);
  function GetCaption: String;
  procedure SetCaption(s: String);
  function GetLayout: TTextLayout;
  procedure SetLayout(tl: TTextLayout);
  procedure SetScrollAutoString(s: String);
  procedure SetScrollDirection(sd: TScrollDirection);
  procedure SetScrollGraph(b: Boolean);
  procedure SetSCrolling(b: Boolean);
  procedure SetSCrollInterval(i: Integer);
  procedure SetScrollStep(i: Integer);
  procedure OnTimerScrollL(Sender: TObject);
  procedure OnTimerScrollR(Sender: TObject);         // Right to left
  procedure Init;
protected

public
  constructor Create(aOwner: Tcomponent); override;
  destructor Destroy; override;
  procedure Paint; override;
published
  property Align;
  property Alignment: TAlignment read GetAlignment write SetAlignment;
  property Anchors;
  property AutoSize;
  property BidiMode;
  property BorderSpacing;
  property Caption: String read GetCaption write SetCaption;
  property Color;
  property Constraints;
  property DragCursor;
  property DragKind;
  property DragMode;
  property Enabled;
  property FocusControl;
  property Font;
  property Layout: TTextLayout read GetLayout write SetLayout;
  property ParentBidiMode;
  property ParentColor;
  property ParentFont;
  property ParentShowHint;
  property PopupMenu;
  property ShowAccelChar;
  property ShowHint;
  property Transparent;
  property Visible;
  property WordWrap;
  property OnChangeBounds;
  property OnClick;
  property OnContextPopup;
  property OnDblClick;
  property OnDragDrop;
  property OnDragOver;
  property OnEndDrag;
  property OnMouseDown;
  property OnMouseEnter;
  property OnMouseLeave;
  property OnMouseMove;
  property OnMouseUp;
  property OnMouseWheel;
  property OnMouseWheelDown;
  property OnMouseWheelUp;
  property OnMouseWheelHorz;
  property OnMouseWheelLeft;
  property OnMouseWheelRight;
  property OnResize;
  property OnStartDrag;
  property OptimalFill;
  property ScrollAutoString: String read FScrollAutoString write SetScrollAutoString;
  property ScrollDirection: TSCrollDirection read FScrollDirection write SetScrollDirection;
  property SCrollGraph: Boolean read FSCrollGraph  write SetSCrollGraph;
  property Scrolling: Boolean read FScrolling write SetScrolling;
  property ScrollInterval: Integer read FSCrollInterval write SetSCrollInterval default 50;
  property ScrollStep: Integer read FScrollStep write SetScrollStep;
end;


TbbScrollLabel1 = class(TLabel)
  private
    FCaption: String;
    FScrolling: boolean;
    FScrollInterval: integer;
    FScrollAutoString:string;
    FScrollGraph: Boolean;
    FScrollStep: Integer;
    FScrollDirection: TSCrollDirection;
    FMargin: Integer;
    FBidiMode: TBiDiMod;  // Disable the property
    //FTransparent: Boolean; // disable property, use parentcolor and parentfont instead
    //Borderwidth: integer;
    CaptionBmp: Tbitmap;
    CaptionRect: TRect;
    //FTimerScroll: TTimer;
    FTimerScroll:TFPTimer;
    FTimerCanvas: TTimer;
    ScrollText: String;
    txtHeight, txtWidth: Integer;
    ScrollBmp: TBitMap;
    ScrollRect:Trect;
    //BkGndBmp:Tbitmap;
    //BkGndRect: Trect;
    ScrollIndex: Integer;
    BordersWidth:Integer;
    procedure ReInit;
    procedure OnTimerScrollL(Sender: TObject);         // Left to right
    procedure OnTimerScrollR(Sender: TObject);         // Right to left
    procedure OnTimerCanvas(Sender:Tobject);
    procedure SetCaption(AValue: string);
    procedure SetMargin(AValue: integer);
    procedure SetScrolling(AValue: Boolean);
    procedure SetScrollInterval(AValue:integer);
    procedure SetScrollAutoString(AValue:string);
    procedure SetScrollGraph(aValue: Boolean);
    procedure SetScrollStep(aValue:Integer);
    procedure SetScrollDirection(aValue: TScrollDirection);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { D�clarations publi�es }
    property Caption: string read FCaption write SetCaption;
    property Margin: integer read FMargin write SetMargin default 0;
    property BidiMode: TBiDiMod read FBidiMode default disabled;
    property Scrolling: Boolean read FScrolling write SetScrolling default true;
    property ScrollInterval: Integer read FScrollInterval write SetScrollInterval default 50;
    property ScrollAutoString: string read FScrollAutoString write SetScrollAutoString;
    property ScrollGraph: Boolean read FScrollGraph write SetScrollGraph default true;
    property ScrollStep: Integer read FScrollStep write SetScrollStep default 1;
    property ScrollDirection: TScrollDirection read FScrollDirection write SetScrollDirection default sdLeftToRight;
  end;

procedure Register;

implementation

procedure Register;
begin
   {$I lazbbscrollcontrols_icon.lrs}
   RegisterComponents('lazbbComponents',[TbbScrollButton]);
   RegisterComponents('lazbbComponents',[TbbScrollLabel]);
end;

// TbbScrollButton creation

constructor TbbScrollButton.Create(AOwner: TComponent);
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

procedure TbbScrollButton.SetCaption(AValue: string);
begin
  if fCaption=AValue then exit;
  FCaption:=AValue;
  //if csDesigning in ComponentState then
  inherited Caption:=FCaption;
  ReInit;
end;

procedure TbbScrollButton.SetMargin(AValue: integer);
begin
  if FMargin=AValue then exit;
  FMargin:=AValue;
  inherited Margin:= AValue;
  ReInit;
end;

procedure TbbScrollButton.SetSpacing(AValue: integer);
begin
  if FSpacing=AValue then exit;
  FSpacing:=AValue;
  inherited Spacing:= AValue;
  ReInit;
end;

procedure TbbScrollButton.SetSCrolling(AValue: Boolean);
begin
  if FScrolling= AValue then exit;
  FSCrolling:= AValue;
  ReInit;
end;

procedure TbbScrollButton.SetScrollInterval(AValue:integer);
begin
  if FScrollInterval= AValue then exit;
  FScrollInterval:= AValue;
  FTimerScroll.Interval:= AValue;
  //ReInit;  //Not needed, will be updated next timer tick
end;

procedure TbbScrollButton.SetSCrollAutoString(AValue:string);
begin
  if FSCrollAutoString= AValue then exit;
  FSCrollAutoString:= AValue;
  ReInit;
end;

procedure TbbScrollButton.SetSCrollGraph(AValue:Boolean);
begin
  if fScrollGraph= AValue then exit;
  fsCrollGraph:= AValue;
  Reinit;
end;

procedure TbbScrollButton.SetSCrollStep(AValue:Integer);
begin
  if fScrollStep=aValue then exit;
  fSCrollStep:= AValue;
  //ReInit;  //Not needed, will be updated next timer tick
end;

procedure TbbScrollButton.SetSCrollDirection(aValue: TSCrollDirection);
begin
  if FScrollDirection=aValue then exit;
  FScrollDirection:= aValue;
    if aValue=sdLeftToRight then FTimerScroll.OnTimer:= @OnTimerScrollL
  else FTimerScroll.OnTimer:= @OnTimerScrollR ;

  //ReInit;  //Not needed, will be updated next timer tick
end;

destructor TbbScrollButton.Destroy;
begin
  if assigned(BkGndBmp) then BkGndBmp.Free;
  if assigned(ScrollBmp) then ScrollBmp.Free;
  if assigned(CaptionBmp) then CaptionBmp.Free;
  if assigned(FTimerScroll) then FTimerSCroll.free;
  if assigned(TimerGlyph) then TimerGlyph.free;
  Inherited Destroy;
end;

procedure TbbScrollButton.ReInit;
var
  BegTxt: integer;
begin
  if csDesigning in ComponentState then exit;
  inherited Caption:= '';      // avoid some flickering
  inherited margin:= FMargin;  // if it was previously changed
  // Scroll d�sactiv�
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

procedure TbbScrollButton.OnTimerGlyph(Sender: TObject);
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

procedure TbbScrollButton.OnTimerScrollL(Sender: TObject);
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

procedure TbbScrollButton.OnTimerScrollR(Sender: TObject);
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

// New TbbScrollLabel

function  TbbScrollLabel.GetAlignment: TAlignment;
begin
  result:= Inherited Alignment;
end;

procedure TbbScrollLabel.SetAlignment(al: TAlignment);
begin
  if Alignment=al then exit;
  Inherited Alignment:= al;
  Init;
  Invalidate;
end;

function TbbScrollLabel.GetCaption: String;
begin
  result:= Inherited Caption;
end;

procedure TbbScrollLabel.SetCaption(s: String);
begin
  if Caption=s then exit;
  Inherited Caption:= s;
  ScrollText:= Caption+FScrollAutoString;
  ScrollIndex:= 0;
  Init;
  Invalidate;
end;

function TbbScrollLabel.GetLayout: TTextLayout;
begin
  result:= Inherited Layout;
end;

procedure TbbScrollLabel.SetLayout(tl: TTextLayout);
begin
  if Layout=tl then exit;
  Inherited Layout:= tl;
  Init;
  Invalidate;
end;

procedure TbbScrollLabel.SetScrollAutoString(s: String);
begin
  if FScrollAutoString=s then exit;
  FScrollAutoString:=s;
  ScrollText:= Caption+FScrollAutoString;
  Init;
  Invalidate;
end;

procedure TbbScrollLabel.SetScrollDirection(sd: TScrollDirection);
begin
  if FScrollDirection= sd then exit;
  FScrollDirection:= sd;
  if FScrollDirection=sdLeftToRight then TimerScroll.OnTimer:= @OnTimerScrollL
  else TimerScroll.OnTimer:= @OnTimerScrollR ;
  Init;
  Invalidate;
end;

procedure TbbScrollLabel.SetScrollGraph(b: Boolean);
begin
  if FSCrollGraph= b then exit;
  FSCrollGraph:= b;
  Init;
  Invalidate;
end;

procedure TbbScrollLabel.SetSCrolling(b: Boolean);
begin
  if FScrolling= b then exit;
  FSCrolling:= b;
  Init;
  Invalidate;
end;

procedure TbbScrollLabel.SetSCrollInterval(i: Integer);
begin
  if FSCrollInterval=i then exit;
  FSCrollInterval:= i;
  TimerSCroll.Interval:= i;
  Init;
  Invalidate;
end;

procedure TbbScrollLabel.SetScrollStep(i: Integer);
begin
  if FScrollStep=i then exit;
  FScrollStep:= i;
  Init;
  Invalidate;
end;

constructor TbbScrollLabel.Create(aOwner: Tcomponent);
begin
  inherited;
  parent:= TwinControl(aOwner);
  Height:= 15;
  Width:= 50;
  //Color:= parent.color;
  ScrollBmp:= TBitmap.Create;
  ScrollBmp.PixelFormat:= pf24bit;
  AutoSize:= false;
  ParentFont:= False;
  FScrollAutoString:= '...';
  FScrolling:= true;
  FSCrollGraph:= True;
  FScrollStep:= 1;
  TimerScroll:= TFPTimer.Create(self);
  TimerScroll.UseTimerThread:= true;
  FScrollInterval:= 50;
  TimerScroll.Interval:= FScrollInterval;
  TimerScroll.OnTimer:= @OnTimerScrollL;
  TimerScroll.Enabled:= False;
  ScrollText:= Caption+FScrollAutoString;
  TextScroll:= Caption+FScrollAutoString;
  First:= true;
end;

destructor TbbScrollLabel.Destroy;
begin
  if Assigned(TimerSCroll) then
  begin
    TimerScroll.Enabled:= False;
    TimerScroll.Free;
  end;
  if assigned(ScrollBmp) then ScrollBmp.Free;
  Inherited Destroy;
end;

// Procedure called when any property change to properly set variables


procedure TbbScrollLabel.Init;
begin
  TextScroll:= Caption+FScrollAutoString;
  ScrollText:= Caption+FScrollAutoString;
  CaptionRect.Top:= BorderSpacing.Top;
  CaptionRect.Left:= BorderSpacing.Left;
  CaptionRect.Right:= ClientWidth-BorderSpacing.Right;
  CaptionRect.Bottom:= Height-BorderSpacing.Bottom;
  if (color=cldefault) or (color=clnone) then bkcolor:= clForm //Parent.Color
  else bkColor:= color;
  if Assigned(Canvas) or (csDesigning in ComponentState) then    // Canvas are processed only when component is loaded to avoid crashes
  begin
    TxtWidth:= Canvas.TextWidth(ScrollText);
    TxtHeight:= Canvas.TextHeight(ScrollText);
    Case Layout of
      tlCenter: yOff:= (Height-txtHeight) div 2;
      tlBottom: yOff:= Height-txtHeight;
      else yOff:= 0;
    end;
    Case Alignment of
      taRightJustify: xOff:= Width-CaptionWidth;
      else xOff:= 0;
    end;
    CaptionWidth:= Canvas.TextWidth(Caption);
    NoScrolling:= (CaptionWidth<Clientwidth) or (not scrolling) ;
    //TimerScroll.Enabled:= NoSCrolling;
    Canvas.Brush.Style:= bssolid;
    Canvas.Brush.color:= bkColor;
    ScrollBmp.Canvas.pen.color:= Font.Color;
    ScrollBmp.Height:= Height;
    ScrollBmp.Width:= txtWidth*2;
    ScrollBmp.Canvas.Font.Assign(Canvas.Font);
    ScrollRect:= Rect(0,0,2*txtWidth,txtHeight);
    ScrollBmp.Canvas.Brush.Style:= bssolid;
    ScrollBmp.Canvas.Brush.color:= bkColor;
    ScrollBmp.Canvas.FillRect(0,0,ScrollBmp.Width, ScrollBmp.Height);
    ScrollBmp.Canvas.pen.color:= Font.Color;
    ScrollBmp.Canvas.TextOut(xOff,yOff, ScrollText+ScrollText);
    TimerScroll.Enabled:= true;
  end;

end;

procedure TbbScrollLabel.Paint;
begin
  if NoScrolling or (csDesigning in ComponentState) then                         // See in init procedure
  begin
    //Canvas.TextOut(xOff, yOff, Caption);
    inherited Paint;
    exit;
  end;
  if FSCrollGraph then
  begin
     Canvas.CopyRect(CaptionRect, ScrollBmp.Canvas, Rect(ScrollIndex,0,
                     ScrollIndex+CaptionRect.Right-CaptionRect.left, Height));
  end else
  begin
    Canvas.TextOut(xOff, yOff, TextScroll);
  end;
end;

// Timer procedure for left to right
// separate procedures to reduce processing time in the timer event

procedure TbbScrollLabel.OnTimerScrollL(Sender: TObject);
begin
  if fSCrollGraph then   // pixel by pixel
  begin
    if (ScrollIndex=0) then Invalidate;
    if ScrollIndex < (ScrollBmp.Width div 2)-1 then Inc(ScrollIndex, FSCrollStep)
    else ScrollIndex:= 0;
  end else              // scroll char by char
  begin
    TextScroll:= Copy(TextScroll, 2, Length(TextScroll) - 1) + Copy(TextScroll,1,1) ;
  end;
  Invalidate;;
end;

// Timer procedure for right to left

procedure TbbScrollLabel.OnTimerScrollR(Sender: TObject);
begin
  if fSCrollGraph then
  begin
    if (ScrollIndex=0) then Invalidate;
    if ScrollIndex >0 then Dec(ScrollIndex, FSCrollStep)
    else ScrollIndex:= (ScrollBmp.Width div 2);
  end else                          // scroll char by char
  begin
    TextScroll:=Copy(TextScroll,Length(TextScroll),1)+Copy(TextScroll, 1, Length(TextScroll)-1);
  end;
  Invalidate;
end;


// TbbScrollLabel1 procedures

constructor TbbScrollLabel1.Create(AOwner: TComponent);
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
  //FTimerSCroll:= TTimer.Create(self);
  FTimerSCroll:= TFPTimer.Create(self);
  FTimerScroll.UseTimerThread:= true;
  FTimerScroll.Enabled:= False;
  FTimerScroll.OnTimer:= @OnTimerScrollL;
  FTimerScroll.Interval:= FScrollInterval;
  //FTimerScroll.StartTimer;
  ScrollIndex:= 0;
  inherited Layout:= Layout;
  inherited Caption:= FCaption;
  ScrollText:= '';
  ScrollBmp:= Tbitmap.Create;
  ScrollBmp.PixelFormat:= pf8bit;
  //BkGndBmp:= Tbitmap.Create;
  //BkGndBmp.PixelFormat:= pf8bit;
  CaptionBmp:= Tbitmap.Create;
  FTimerCanvas:= TTimer.Create(self);
  FTimerCanvas.Enabled:= true;
  FTimerCanvas.Interval:= 50;
  FTimerCanvas.OnTimer:= @OntimerCanvas;
  FBidiMode:= Disabled;
  inherited BiDiMode:= bdLeftToRight;     end;

// Canvas is not immediately created, so reinit until it is created
// Needed to use margin proeprty when scroll is disabled

procedure TbbScrollLabel1.OnTimerCanvas(sender: TObject);
begin
  if assigned(Canvas) then
  begin
    FTimerCanvas.Enabled:= false;
    ReInit;
  end;
end;

procedure TbbScrollLabel1.SetCaption(AValue: string);
begin
  if fCaption=AValue then exit;
  FCaption:=AValue;
  inherited Caption:=FCaption;
  ReInit;
end;

procedure TbbScrollLabel1.SetMargin(AValue: integer);
begin
  if FMargin=AValue then exit;
  FMargin:=AValue;
  //inherited Margin:= AValue;
  ReInit;
end;

procedure TbbScrollLabel1.SetSCrolling(AValue: Boolean);
begin
  if FScrolling= AValue then exit;
  FSCrolling:= AValue;
  ReInit;
end;

procedure TbbScrollLabel1.SetScrollInterval(AValue:integer);
begin
  if FScrollInterval= AValue then exit;
  FScrollInterval:= AValue;
  FTimerScroll.Interval:= AValue;
  //ReInit;  //Not needed, will be updated next timer tick
end;

procedure TbbScrollLabel1.SetSCrollAutoString(AValue:string);
begin
  if FSCrollAutoString= AValue then exit;
  FSCrollAutoString:= AValue;
  ReInit;
end;

procedure TbbScrollLabel1.SetSCrollGraph(AValue:Boolean);
begin
  if fScrollGraph= AValue then exit;
  fsCrollGraph:= AValue;
  Reinit;
end;

procedure TbbScrollLabel1.SetSCrollStep(AValue:Integer);
begin
  if fScrollStep=aValue then exit;
  fSCrollStep:= AValue;
  ReInit;
end;

procedure TbbScrollLabel1.SetSCrollDirection(aValue: TSCrollDirection);
begin
  if FScrollDirection=aValue then exit;
  FScrollDirection:= aValue;
  if aValue=sdLeftToRight then FTimerScroll.OnTimer:= @OnTimerScrollL
  else FTimerScroll.OnTimer:= @OnTimerScrollR ;
  //ReInit;  //Not needed, will be updated next timer tick
end;

destructor TbbScrollLabel1.Destroy;
begin
  FTimerSCroll.StopTimer;
  FTimerCanvas.enabled:=false;
  //if assigned(BkGndBmp) then BkGndBmp.Free;
  //if assigned(ScrollBmp) then ScrollBmp.Free;
  //if assigned(CaptionBmp) then CaptionBmp.Free;
  if assigned(FTimerScroll) then FTimerSCroll.free;
  if assigned(FTimerCanvas) then FTimerCanvas.free;
  Inherited Destroy;
end;

procedure TbbScrollLabel1.ReInit;
var
  bkColor: TColor;
begin
  FTimerScroll.enabled:= false;  // before scrolling process
  if csDesigning in ComponentState then exit;
  inherited Caption:= '';   // avoid some flickering
  if (color=cldefault) or (color=clnone) then bkcolor:= clform
  else bkColor:= color;
  Canvas.Font.Assign(Font);
  TxtHeight:= Canvas.TextHeight(ScrollText);
  // So, we scroll !
  ScrollText:= FCaption+FScrollAutoString;
  TxtHeight:= Canvas.TextHeight(ScrollText);
  TxtWidth:= Canvas.TextWidth(ScrollText);
  CaptionRect.Top:= (height-TxtHeight) div 2;
  CaptionRect.Bottom:= CaptionRect.Top+TxtHeight;
  //BkGndBmp.Width:= 1;
  //BkGndBmp.Height:= 1;
  //BkGndRect:= Rect(0, 0, 1, 1);
  CaptionRect.Left:= FMargin;
  CaptionRect.Right:= CaptionRect.Left+ClientWidth-FMargin*2;
  // Get label background color (pixel at top left)
 { if color= clnone then BkGndBmp.Canvas.CopyRect(BkGndRect, Canvas,
          Rect(CaptionRect.Left-1, CaptionRect.top, CaptionRect.Left, CaptionRect.Top+1))
  else BkGndBmp.Canvas.CopyRect(BkGndRect, Canvas,
          Rect(CaptionRect.Left, CaptionRect.top, CaptionRect.Left+1, CaptionRect.Top+1)) ; }
  If fScrollGraph and (Canvas.TextWidth(FCaption)>=(ClientWidth-FMargin*2)+BordersWidth) then
  begin
    ScrollBmp.Canvas.Font.Assign(Font);
    ScrollBmp.width:=  2*txtWidth;
    ScrollBmp.Height:= txtHeight;
    ScrollRect:= Rect(0,0,2*txtWidth,txtHeight);
    //ScrollBmp.Canvas.StretchDraw(ScrollRect, BkGndBmp);
    ScrollBmp.Canvas.Brush.Style:= bssolid;
    ScrollBmp.Canvas.Brush.color:= bkColor;
    //ScrollBmp.Canvas.FloodFill(0,0,bkColor, fssurface);
    ScrollBmp.Canvas.TextOut(0,0, ScrollText+ScrollText);
  end;
  if (Canvas.TextWidth(FCaption) < (ClientWidth-FMargin*2)+BordersWidth) or not FScrolling then
  begin
    inherited Caption:= FCaption;
    exit;
  end;
  FTimerScroll.Enabled:= FScrolling;
end;

// Timer procedure for left to right
// separate procedures to reduce processing time in the timer event

procedure TbbScrollLabel1.OnTimerScrollL(Sender: TObject);
begin
  //if not FScrolling then exit;
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

procedure TbbScrollLabel1.OnTimerScrollR(Sender: TObject);
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

end.


