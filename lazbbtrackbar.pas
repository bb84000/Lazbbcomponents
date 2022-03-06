{******************************************************************************
  lazbbtrackbar : Customizable TTrackbar
  Added to lazbbComponents palette
  bb - sdtp - march 2022

  TbbTrackBar specific properties :
    SliderColor: Slider default color
    SliderColorDown: Slider color on mouse button down
    SliderColorHover: Slider color on mouse cursor over
    RulerColor: Ruler color
    RulerBorderColor: Ruler border color
    RulerSize: Ruler thickness
    ScaleMarks: Same as TTrackbar TickMark;
    ScaleSize: Min, max and zero scale ticks size. other ticks are half size
    ScaleColor: scale iicks Color;
    Frequency: divider for the scale ticks,
               also used as position step move when pgUp nd PgDn keys pressed
    KeyIncrement: position step move when arrow key pressed.
    Home key pressed set position to Min, End key pressed set position to Max
******************************************************************************}


unit lazbbtrackbar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLType, LMessages, FPCanvas;

type

  TbbTrackBar = class;

  TTBarOrientation = (tbHorizontal, tbVertical);
  TScaleMark = (tmBottomRight, tmTopLeft, tmBoth, tmNone);
  TTickStyle = (tsNone, tsAuto, tsManual);
  TTBarScalePos = (trLeft, trRight, trTop, trBottom);

  TSlider = class
  private
    FOrientation: TTBarOrientation;
    FMark: TScaleMark;
    procedure setOrientation(tb: TTBarOrientation);
    procedure setMark(tm: TScaleMark);
  protected
  public
    Parent: TbbTrackBar;
    Shape: array of TPoint;                 // defines slider polygon
    Rectngl: TRect;                         // define sllider rectangle for mouse trap
    constructor create(aOwner: TbbTrackBar);
    destructor Destroy; override;
    procedure ReInit;
    procedure Move(x, y: Integer; absol: Boolean= false);
    procedure Paint(col: TColor);
    property Orientation: TTBarOrientation read FOrientation write setOrientation;
    property Mark: TScaleMark read FMark write setMark;
  published
  end;

  TLMSetFocus = record
    Msg: Cardinal;
    {$ifdef cpu64}
      UnusedMsg: Cardinal;
    {$endif}
    FocusedWnd: HWND;
    Unused: LPARAM;
    Result: LRESULT;
  end;

  TbbTrackBar = class(TCustomControl)
  private
    FSliderColor: TColor;
    FSliderColorDown: Tcolor;
    FSliderColorHover: TColor;
    FRulerColor: TColor;
    FRulerBorderColor: Tcolor;
    FRulerSize: integer;
    FPosition: Integer;
    FMax, FMin : Integer;
    FOrientation: TTBarOrientation;
    FReversed: boolean;
    FScaleMarks: TScaleMark;
    FscaleSize: Integer;
    FScaleColor: TColor;
    FFrequency: Integer;
    FKeyIncrement: Integer;
    FGapMin, FGapMax: Integer;
    FOnPositionChange: TNotifyEvent;
    Slider: TSlider;
    SliderTop, SliderLeft: Integer;
    tlMargin, brMargin: Integer;
    MouseDwn: boolean;
    Xprev, Yprev: integer;
    Xdif, YDif: Integer;
    XMouse, YMouse: Integer;
    PrevWidth, PrevHeight: Integer;
    Ruler: Trect;
    topleftScale,botrightScale: array of integer;
    First: Boolean;
    function getPosition: integer;
    procedure setPosition(i: integer);
    procedure setSliderColor(cl: TColor);
    procedure setSliderColorDown(cl: TColor);
    procedure setSliderColorHover(cl: TColor);
    procedure setRulerColor(cl: Tcolor);
    procedure setRulerBorderColor(cl: Tcolor);
    procedure setRulerSize(i: integer);
    procedure setScaleColor(cl: Tcolor);
    procedure setScaleMarks(tm: TScaleMark);
    procedure setScaleSize(i: integer);
    procedure setMin(i: integer);
    procedure setMax(i: integer);
    procedure setFrequency(i: integer);
    procedure setKeyIncrement(i: integer);
    procedure setGapMin(i: Integer);
    procedure setGapMax(i: Integer);
    procedure setOrientation(tr: TTBarOrientation);
    procedure setReversed(b: boolean);
    procedure ScaleChange;
    procedure RulerChange;
    procedure PaintRuler;
    procedure PaintScale;
    procedure PositionChange(p: integer);
    Function PositionToPixels(pos : Integer) : Integer;
    function PixelsToPosition(px: Integer): Integer;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKILLFOCUS); message LM_KILLFOCUS;
  protected
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState);   override;
  public
    property Canvas;
    constructor Create(aOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure tbChangeBounds(Sender: TObject);
  published
    property OnPositionChange: TNotifyEvent read FOnPositionChange write FOnPositionChange;
    property BorderStyle;
    property enabled;
    property color;
    property Orientation: TTBarOrientation read FOrientation write setOrientation;
    property Reversed: Boolean read FReversed write setReversed default False;
    property Min: Integer read FMin write setMin;
    property Max: Integer read FMax write setMax;
    property Frequency: integer read FFrequency write setFrequency;
    property KeyIncrement: Integer read FKeyIncrement write setKeyIncrement;
    property GapMin: Integer read FGapMin write setGapMin;
    property GapMax: Integer read FGapMax write setGapMax;
    property Position: Integer read getPosition write setPosition default 0;
    property SliderColor: Tcolor read FSliderColor write setSliderColor;
    property SliderColorDown: TColor read FSliderColorDown write setSliderColorDown;
    property SliderColorHover: TColor read FSliderColorHover write SetSliderColorHover;
    property RulerColor: TColor read FRulerColor write setRulerColor;
    property RulerBorderColor: Tcolor read FRulerBorderColor write setRulerBorderColor;
    property RulerSize: Integer read FRulerSize write setRulerSize;
    property ScaleMarks: TSCaleMark read FScaleMarks write setScaleMarks;
    property ScaleColor: Tcolor read FScaleColor write setScaleColor;
    property ScaleSize: Integer read FScalesize write setScaleSize;
    property Visible;
    property TabOrder;
    property TabStop;
   end;

procedure Register;

implementation

procedure Register;
begin
  {$I lazbbtrackbar_icon.lrs}
  RegisterComponents('lazbbComponents',[TbbTrackBar]);
end;

// TSlider

constructor TSlider.create(aOwner: TbbTrackBar);
begin
  inherited Create;
  Parent:= TbbTrackBar(aOwner);
  Rectngl:= TRect.Create(0,0,0,0);
  FMark:= tmTopLeft;
  FOrientation:= tbVertical;
  ReInit;
end;

destructor TSlider.Destroy;
begin
  inherited;
end;

procedure TSlider.setOrientation(tb: TTBarOrientation);
begin
  if FOrientation=tb then exit;
  FOrientation:= tb;
  ReInit;
end;

procedure TSlider.setMark(tm: TScaleMark);
begin
  if FMark=tm then exit;
  FMark:= tm;
  ReInit;
end;

procedure TSlider.ReInit;
begin
  if Forientation=tbVertical then
  begin
    Rectngl.TopLeft:= point(0,0);
    Rectngl.BottomRight:= Point(20,10);
    case FMark of
      tmTopLeft: begin
        shape:= [Point(0,5), Point(5,0), Point(18,0), Point(18,10),  Point(5,10),  Point(0,5)];
        Rectngl.BottomRight:= Point(18,10);
      end;
      tmBottomRight: begin
        shape:= [Point(2,0), Point(15,0), Point(20,5), Point(15,10), Point(2,10), Point(2,0)];
        Rectngl.TopLeft:= point(2,0);
      end;
      tmBoth: shape:= [Point(0,5), Point(5,0), Point(15,0), Point(20,5), Point(15,10),  Point(5,10),  Point(0,5)] ;
      tmNone: shape:= [Point(0,0), Point(0,10), Point(20,10), Point(20,0), Point(0,0)];
    end;
  end else
  begin
    Rectngl.TopLeft:= point(0,0);
    Rectngl.BottomRight:= Point(10,20);
    case FMark of
      tmTopLeft: begin
        shape:= [Point(0,5), Point(5,0), Point(10,5), Point(10,18),  Point(0,18),  Point(0,5)];
        Rectngl.BottomRight:= Point(10, 18);
      end;
      tmBottomRight: begin
        shape:= [Point(0,2), Point(10,2), Point(10,15), Point(5,20), Point(0,15), Point(0,2)];
        Rectngl.TopLeft:= Point(0,2);
      end;
      tmBoth: shape:= [Point(0,5), Point(5,0), Point(10,5), Point(10,15), Point(5,20),  Point(0,15),  Point(0,5)];
      tmNone: shape:= [Point(0,0), Point(10,0), Point(10,20), Point(0,20), Point(0,0)];
    end;
  end;
end;

procedure TSlider.Move(x, y: Integer; absol: Boolean= false);
var
  i: Integer;
begin
  if absol then ReInit;
  // Move shape
  for i:= 0 to high(shape) do
  begin
    shape[i].X:= shape[i].X+x;
    shape[i].Y:= Shape[i].Y+y;
  end;
  // move Rectngl;
  Rectngl.Offset(x, y);
end;

procedure TSlider.Paint(col: TColor);
var
  PrevBrushStyle: TBrushStyle;
  PrevBrushColor: TColor;
  PrevPenStyle: TPenStyle;
  PrevPenColor: TColor;
begin

  // save current canvas settings
  PrevBrushStyle:= Parent.Canvas.Brush.style;
  PrevBrushColor:= Parent.Canvas.Brush.Color;
  PrevPenStyle:= Parent.Canvas.Pen.Style;
  PrevPenColor:= Parent.Canvas.pen.Color;
  // paint slider shape
  if not Parent.enabled then col:= $CCCCCC;
  Parent.Canvas.Brush.Style:= bsSolid;
  Parent.Canvas.brush.Color:= col;
  Parent.Canvas.Pen.Style:= psSolid;
  Parent.Canvas.Pen.Color:= col;
  Parent.Canvas.Polygon(Shape);
  // restore previous canvas settings
  Parent.Canvas.Brush.Style:= PrevBrushStyle;
  Parent.Canvas.brush.Color:= PrevBrushColor;
  Parent.Canvas.Pen.Style:= PrevPenStyle;
  Parent.Canvas.Pen.Color:= PrevPenColor;
end;

// TbbTRackbar

constructor TbbTrackBar.create(aOwner: Tcomponent);
begin
  inherited;
  parent:= TwinControl(aOwner);
  ControlStyle := ControlStyle + [csParentBackground, csClickEvents,
    csCaptureMouse, csDoubleClicks, csRequiresKeyboardInput, csopaque];
  Slider:= TSlider.Create(self);
  Width:= 30;
  Height:= 120;
  PrevWidth:=width;
  PrevHeight:=height;
  enabled:= true;
  Color:= clDefault;
  FMin:= 0;
  FMax:= 10;
  tlMargin:= 5;
  brMargin:= 10;
  FGapMin:= 10;
  FGapMax:= 10;
  FRulerSize:= 10;
  FScaleSize:= 5;
  FFrequency:= 1;
  FKeyIncrement:= 1;
  OnChangeBounds:= @tbChangeBounds;
  FReversed:= false;
  //Default slider vertical, left
  FOrientation:= tbVertical;
  Slider.Orientation:= tbVertical;
  FScaleMarks:=  tmTopLeft;
  Slider.Mark:= tmTopLeft;
  ScaleChange;
  Slider.ReInit;
   Slider.Move(SliderLeft, SliderTop);
  Xprev:= 5;
  Yprev:=0;
  FSliderColor:= clMenuHighlight;
  FSliderColorDown:= clActiveCaption;
  FSliderColorHover:= clBlue;
  FRulerColor:=clActiveBorder;;
  FRulerBorderColor:= cl3DLight;
  FScaleColor:= clGray;
  ParentColor:= false;
  Paint;
  Tabstop:= true;
  First:= true;
end;

destructor TbbTrackBar.Destroy;
begin
  inherited destroy;
end;

procedure TbbTrackBar.WMSetFocus(var Message: TLMSetFocus);
begin
  invalidate;
end;


procedure TbbTrackBar.WMKillFocus(var Message: TLMKILLFOCUS);
begin
  invalidate;
end;

// Keyboard keys can move slider if component has focus

procedure TbbTrackBar.KeyDown(var Key: Word; Shift: TShiftState);
var
  step: integer;
begin
  inherited KeyDown(Key, Shift);
  step:= 0;
  case Key of
    VK_TAB: exit;              // let the tab key move focus to another component
    VK_HOME: if not (ssCtrl in Shift) then SetPosition(FMin);            // home is always Min
    VK_END: if not (ssCtrl in Shift) then SetPOsition(Fmax);             // End is always Max value
    VK_PRIOR: if not (ssCtrl in Shift) then step:= -FFrequency;          // page Up always increase value
    VK_NEXT: if not (ssCtrl in Shift) then step:= FFrequency;            // page Down always decrease value
    VK_UP: if (not (ssCtrl in Shift)) and (FOrientation=tbVertical) then
      if Reversed then step:= FKeyIncrement else step:= -FKeyIncrement;
    VK_DOWN: if (not (ssCtrl in Shift)) and (FOrientation=tbVertical) then
      if Reversed then step:= -FKeyIncrement else step:= FKeyIncrement;
    VK_LEFT: if not (ssCtrl in Shift) and (FOrientation=tbHorizontal) then
      if Reversed then step:= FKeyIncrement else step:= -FKeyIncrement;
    VK_RIGHT: if not (ssCtrl in Shift) and (FOrientation=tbHorizontal) then
      if Reversed then step:= -FKeyIncrement else step:= FKeyIncrement;
  end;
  // check if we dont go out min and max limits
  if (Fposition+step>FMax) then setPosition(FMax) else
  begin
   if (Fposition+step<FMin) then setPosition(FMin) else setPosition(FPosition+step);
  end;
  Key:= 0;
end;


// Bounds changed in design mode it is not pertinent to change size automatically at runtime !

procedure TbbTrackBar.tbChangeBounds(Sender: TObject);
begin
  if csDesigning in ComponentState then
  begin
    ScaleChange;
    Slider.ReInit;
    Slider.Move(SliderLeft, SliderTop);
    if FOrientation= tbVertical then PositionChange(0) //SliderMove(Ruler.Top-5,FScaleSize)
    else PositionChange(0);
    invalidate;
  end;
end;


// Space between the component's top or left and the ruler's top or left

procedure TbbTrackBar.setGapMin(i: integer);
begin
  if FGapMin= i then exit;
  if i<0 then
  begin
    raise ELayoutException.CreateFmt('Negative value %d not allowed.', [i]);
    exit;
  end;
  FGapMin:= i;
  ScaleChange;
  Slider.ReInit;
  Slider.Move(SliderLeft, SliderTop);
  Invalidate;

end;

// Space between the component's bottom or right and the ruler's bottom or right

procedure TbbTrackBar.setGapMax(i: integer);
begin
  if FGapMax= i then exit;
  if i<0 then
  begin
    raise ELayoutException.CreateFmt('Negative value %d not allowed.', [i]);
    exit;
  end;
  FGapMax:= i;
  ScaleChange;
  Slider.ReInit;
  Slider.Move(SliderLeft, SliderTop);
  Invalidate;

end;

function TbbTrackBar.PixelsToPosition(px: Integer): Integer;
begin
  if FOrientation=tbVertical then result:= Round(((px-FGapMin)*(FMax-FMin))/(Height-FGapMin-FGapMax-10))
  else result:= Round(((px-FGapMin)*(FMax-FMin))/(Width-FGapMin-FGapMax-10));
  if Reversed then Result:= FMax-FMin-Result;
end;

// get trackbar position from slider position

function TbbTrackBar.getPosition: integer;
begin
  result:= Fposition;
  if csDesigning in ComponentState then
  begin
    exit;
  end else
  begin
    if FOrientation = tbVertical then result:= PixelsToPosition(Slider.Rectngl.Top)
    else result:= PixelsToPosition(Slider.Rectngl.Left);
    Result:= Result+FMin;
    if result>Fmax then Result:= FMax;
    if Result<Fmin then Result:= FMin;
  end;
end;

// Position value to absolute pixel position on component

Function TbbTrackBar.PositionToPixels(pos : Integer) : Integer;
begin;
  result:= FScaleSize;
  if (pos<FMin) or (pos>FMax) then exit;
  pos:= pos-FMin;
  if FOrientation=tbVertical then
  begin
    Result:= Round((pos*(Height-FGapMin-FGapMax-10))/(FMax-FMin))+FGapMin;
    if Reversed then Result:= Height-Result-FGapMax;
  end else
  begin
    Result:= Round((pos*(Width-FGapMin-FGapMax-10))/(FMax-FMin))+FGapMin;
    if Reversed then Result:= Width-Result-FGapMax;
  end;
end;

procedure TbbTrackBar.PositionChange(p: integer);
begin
  if (p<FMin) or (p>FMax) then exit;
  Slider.ReInit;
  if FOrientation=tbVertical then Slider.Move(SliderLeft, PositionToPixels(p))
  else Slider.Move(PositionToPixels(p), SliderTop);
  if Assigned(FOnPositionChange) then FOnPositionChange(self);
end;

procedure TbbTrackBar.setPosition(i: integer);
begin
  if FPosition= i then exit;
  if (i<Fmin) or (i>Fmax) then
  begin
    raise ELayoutException.CreateFmt('Position must be between Min (%d) and Max (%d).'+LineEnding+
                                     'Position value %d not allowed.', [FMin, FMax, i]);
    exit;
  end;
  FPosition:= i;
  PositionChange(i);
  Invalidate;
  if Assigned(FOnPositionChange) then FOnPositionChange(self);
end;

procedure TbbTrackBar.setOrientation(tr: TTBarOrientation);

begin
  if Forientation=tr then exit;
  Forientation:= tr;
  if csDesigning in ComponentState then
  begin
    if (Forientation= tbVertical) and (width>height) or
       (FOrientation=tbHorizontal) and (height>width) then
        SetBounds(left, top, height, width);
  end;
  ScaleChange;
  Slider.Orientation:= tr;
  Slider.ReInit;
  Slider.Move(SliderLeft,SliderTop);
  Positionchange(0);
  Invalidate;
end;

procedure TbbTrackBar.setReversed(b: boolean);
begin
  if FReversed= b then exit;
  fReversed:= b;
  Slider.ReInit;
  Slider.Move(SliderLeft,SliderTop);
  Positionchange(0);
  Invalidate;
end;

procedure TbbTrackBar.setScaleMarks(tm: TScaleMark);
begin
  if FScaleMarks= tm then exit;
  FScaleMarks:= tm;
  Slider.Mark:= tm;

  PositionChange(FPosition);
  Invalidate;
end;

procedure TbbTrackBar.setScaleSize(i: integer);
begin
  if FScaleSize= i then exit;
  if i<0 then
  begin
    raise ELayoutException.CreateFmt('Negative value %d not allowed.', [i]);
    exit;
  end;
  FscaleSize:= i;
  ScaleChange;
  Slider.ReInit;
  Slider.Move(SliderLeft, SliderTop);
  Invalidate;
end;

procedure TbbTrackBar.setRulerSize(i: integer);
begin
  if FRulerSize= i then exit;
  if (i<0) or (i>10) then
  begin
    raise ELayoutException.CreateFmt('RulerSize must be between 0 and 10.'+LineEnding+
                                     'RulerSize value %d not allowed.', [i]);
    exit;
  end;
  FRulerSize:= i;
  ScaleChange;
  Slider.ReInit;
  Slider.Move(SliderLeft, SliderTop);
  Invalidate;
end;

procedure TbbTrackBar.SetMin(i: integer);
begin
  if FMin= i then exit;
  if i>=FMax then
  begin
    raise ELayoutException.CreateFmt('Min Value %d not allowed.'+LineEnding+'Min must be lower than Max (%d).', [i, FMax]);
    exit;
  end;
  FMin:= i;
  PositionChange(FPosition);
  INvalidate;
end;

procedure TbbTrackBar.SetMax(i: integer);
begin
  if FMax= i then exit;
  if i<=Fmin then
  begin
    raise ELayoutException.CreateFmt('Max Value %d not allowed.'+LineEnding+' Max must be greater than Min (%d).', [i, FMin]);
    exit;
  end;
  FMax:= i;

  PositionChange(FPosition);
  Invalidate;
end;

procedure TbbTrackBar.setFrequency(i: integer);
begin
  if FFrequency= i then exit;
  if (i<1) or (i>FMax-Fmin) then
  begin
    raise ELayoutException.CreateFmt('Frequency must be between 1 and Max-Min (%d).'+LineEnding+
                                     'Frequency value %d not allowed.', [FMax-FMin, i]);
    exit;
  end;
  FFrequency:= i;
  Invalidate;
end;

procedure TbbTrackBar.setKeyIncrement(i: integer);
begin
  if FKeyIncrement= i then exit;
  if (i<1) or (i>FMax-Fmin) then
  begin
    raise ELayoutException.CreateFmt('KeyIncrement must be between 1 and Max-Min (%d).'+LineEnding+
                                     'KeyIncrement value %d not allowed.', [FMax-FMin, i]);
    exit;
  end;
  FKeyIncrement:= i;
  Invalidate;
end;

procedure TbbTrackBar.setSliderColor(cl: TColor);
begin
  if FSliderColor= cl then exit;
  FSliderColor:= cl;
  Invalidate;
end;

procedure TbbTrackBar.setSliderColorDown(cl: TColor);
begin
  if FSliderColorDown= cl then exit;
  FSliderColorDown:= cl;
  Invalidate;
end;

procedure TbbTrackBar.setSliderColorHover(cl: TColor);
begin
  if FSliderColorHover= cl then exit;
  FSliderColorHover:= cl;
  INvalidate;
end;

procedure TbbTrackBar.setRulerColor(cl: Tcolor);
begin
  if FRulerColor= cl then exit;
  FRulerColor:= cl;
  Invalidate;
end;

procedure TbbTrackBar.setRulerBorderColor(cl: Tcolor);
begin
  if FRulerBorderColor= cl then exit;
  FRulerBorderColor:= cl;
  if csDesigning in ComponentState then
  begin
     Invalidate;
  end;

end;

procedure TbbTrackBar.setScaleColor(cl: Tcolor);
begin
  if FScaleColor= cl then exit;
  FScaleColor:= cl;
  Invalidate;
end;

procedure TbbTrackBar.Scalechange;
begin
  SliderTop:= FGapMin;
  SliderLeft:= FScaleSize;
  if FOrientation=tbVertical then
  begin
    SliderTop:= FGapMin;
    SliderLeft:= FScaleSize;
    if (FScaleMarks=tmBottomRight) or (FScaleMarks=tmNone) then SliderLeft:=0;
    topleftScale:= [0, FGapMin, FSCaleSize, Height-FGapMax];   // top or left scale
    botrightScale:= [21+SliderLeft, FGapMin, 21+SliderLeft+FSCaleSize, Height-FGapMax]; //Bottom or right scale
    Ruler.Left:= tlMargin+SliderLeft;
    Ruler.Top:= FGapMin;
    Ruler.Right:= Ruler.Left+FRulerSize+1;
    Ruler.Bottom:= Height-FGapMax;
  end else
  begin
    SliderLeft:= FGapMin;
    SliderTop:= FScaleSize;
    if (FScaleMarks=tmBottomRight) or (FScaleMarks=tmNone) then SliderTop:= 0;
    topleftScale:= [FGapMin, 0, width-FGapMax, FScaleSize];   // top or left scale
    botrightScale:= [FGapMin, 21+SliderTop, width-FGapMax, 21+SliderTop+FScaleSize]; //Bottom or right scale
    Ruler.Left:= FGapMin;
    Ruler.Top:= tlMargin+SliderTop;
    Ruler.Right:= width-FGapMax;
    Ruler.Bottom:= Ruler.top+FRulersize+1;
  end;
end;

procedure TbbTrackBar.RulerChange;
begin
  if FOrientation=tbVertical then
  begin

  end else
  begin

  end;
end;

procedure TbbTrackBar.PaintScale;
var
  x1, y1, x2, y2: Integer;
  i: Integer;
begin
    Canvas.pen.style:= psSolid;
    if not Enabled then Canvas.pen.Color:= $C4C4C4
    else Canvas.pen.Color:= FScaleColor;
    for i:=Fmin to FMax  do
    begin
      if i mod FFrequency =0 then
      begin
        y1:= PositionToPixels(i)+5;
        if Forientation= tbVertical then
        begin
          // Paint top left scale
          if (i=0) or (i=Fmax) or (i=Fmin) then x1:= topleftScale[0] else x1:= topleftScale[0]+(ScaleSize div 2);
          if (i=0) or (i=Fmax) or (i=Fmin) then x2:= botrightScale[2] else x2:= botrightScale[2]-(ScaleSize div 2);
          if (FScaleMarks=tmTopLeft) or (FScaleMarks=tmBoth) then  Canvas.Line(x1, y1, topleftScale[2], y1);
          if (FScaleMarks=tmBottomRight) or (FScaleMarks=tmBoth) then Canvas.Line(botrightScale[0], y1, x2, y1);
        end else
        begin
          x1:= PositionToPixels(i)+5;
          if (i=0) or (i=Fmax) or (i=Fmin) then y1:= topleftScale[1] else y1:= topleftScale[1]+(ScaleSize div 2);
          if (i=0) or (i=Fmax) or (i=Fmin) then y2:= botrightScale[3] else y2:= botrightScale[3]-(ScaleSize div 2);
          if (FScaleMarks=tmTopLeft) or (FScaleMarks=tmBoth) then Canvas.Line(x1, y1, x1, topleftScale[3] );
          if (FScaleMarks=tmBottomRight) or (FScaleMarks=tmBoth) then Canvas.Line(x1, botrightScale[1], x1 , y2);
        end;
      end;
    end;
    Canvas.pen.style:= psClear;
end;

procedure TbbTrackBar.PaintRuler;
begin
  Canvas.pen.style:= psSolid;
  if not enabled then
  begin
    Canvas.pen.Color:=$CCCCCC;
    Canvas.Brush.Color:= $E7EAEA;
    Canvas.Rectangle(Ruler);
  end else
  begin
    Canvas.pen.Color:=FRulerBorderColor;
    Canvas.Brush.Color:= FRulerColor;
    Canvas.Rectangle(Ruler);
  end;
  Canvas.pen.style:= psClear;
end;

  procedure TbbTrackBar.paint;
begin
  inherited Paint;
  PaintScale;
  PaintRuler;
  Slider.Paint(FSliderColor);
  if focused then
  begin
    canvas.Brush.style:= bsclear;
    Canvas.pen.style:= psDot;
    Canvas.Pen.Cosmetic := false;
    Canvas.Pen.Color:= clBackground;
    canvas.Rectangle(0,0, width, height);
    Canvas.Pen.Cosmetic := true;
    Canvas.pen.style:= psClear;
  end;
  // Need to fire OnPosition change event on runtime activation
  // if position property has not changed since component creation
  if First= true then
  begin
    if not(csDesigning in ComponentState) then
    begin
      if (Position= 0) and Assigned(FOnPositionChange) then FOnPositionChange(self);
      first:= false;
    end;
  end;
end;

procedure TbbTrackBar.MouseDown(Button: TMouseButton; Shift:TShiftState; X, Y:Integer);
begin
  inherited;
  // Check if we are in the Slider rectangle
  MouseDwn:=false;
  if not Slider.Rectngl.Contains(Point(X,Y)) then exit;
  MouseDwn:= true;
  Slider.paint(FSliderColorDown);
  Xprev:= X;
  Yprev:= Y;
  Xdif:= X-Slider.Rectngl.Left;
  Ydif:= Y-Slider.Rectngl.Top;
end;


procedure TbbTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
 dy: integer;
 dx: Integer;
 col:Tcolor;
begin
  inherited;
  XMouse:= X;
  YMouse:= Y;
  if (not ParentColor) and (color= cldefault) then color:= clWhite;
  if Slider.Rectngl.Contains(Point(X, Y))then
  begin
     Slider.Paint(FSliderColorHover);
  end else
  begin
    MouseLeave;
  end;
  if not MouseDwn then exit;
  Case FOrientation of
    tbVertical: begin
      if ((Y-Ydif)<FGapMin) or ((Y-Ydif)>(height-GapMax-10)) then exit;   // 10 is slider height
      X:= 0;
      Slider.Paint(color);
      PaintRuler;
      dy:= Y-Yprev;
      Slider.Move(X, dy);

    end;
    tbHorizontal: begin
      if ((X-Xdif)<FGapMin) or ((X-Xdif)>width-FGapMax-10) then exit;
      Y:= 0;
      Slider.Paint(color);
      PaintRuler;
      dx:= X-Xprev;
      Slider.Move(dx, Y);
    end;
  end;
  Slider.Paint(FSliderColorDown);
  Xprev:= X;
  Yprev:= Y;
  Fposition:= getPosition;
  if Assigned(FOnPositionChange) then FOnPositionChange(Self);
end;

procedure TbbTrackBar.MouseUp(Button: TMouseButton; Shift:TShiftState; X, Y:Integer);
begin
  inherited;
  MouseDwn:= false;
  Slider.Paint(SliderColor);
end;

procedure TbbTrackBar.MouseEnter;
begin
end;

procedure TbbTrackBar.MouseLeave;
begin
  inherited;
  Slider.Paint(SliderColor);
end;

end.
