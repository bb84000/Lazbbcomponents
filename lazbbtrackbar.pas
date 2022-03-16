{******************************************************************************
  lazbbtrackbar : Customizable TTrackbar
  Added to lazbbComponents palette
  bb - sdtp - march 2022

  TbbTrackBar specific properties :
    SliderColor: Slider default color
    SliderColorDown: Slider color on mouse button down
    SliderColorHover: Slider color on mouse cursor over
    Slidersize : Slider's thickness
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

  TbbTrackBar = class;    // Forward declaration

  TTBarOrientation = (tbHorizontal, tbVertical);
  TScaleMark = (tmBottomRight, tmTopLeft, tmBoth, tmNone);
  TTickStyle = (tsNone, tsAuto, tsManual);
  TSliderStyle= (ssClassic, ssButton);
  TTBarScalePos = (trLeft, trRight, trTop, trBottom);
  TBtnState= (bsEnabled, bsDown, bsHover, bsDisabled, bsErase);

  // TSCale : Scale class set and paint scale(s)

  TSCale =class
  private
  protected
  public
    bmp: Tbitmap;
    Parent: TbbTrackBar;
    TopLeft, BotRight:  array of integer;
    constructor create(aOwner: TbbTrackBar);
    destructor Destroy; override;
    procedure ReInit;
    procedure Paint;
  published
  end;

  TSlider = class
  private
  protected
  public
    Parent: TbbTrackBar;
    Shape: array of TPoint;                 // defines slider polygon
    BtnShape: array of TPoint;
    Rectngl: TRect;                         // define sllider rectangle for mouse trap
    TopLeft: Tpoint;
    constructor create(aOwner: TbbTrackBar);
    destructor Destroy; override;
    procedure ReInit(origine: Boolean= false);
    procedure setShape;
    procedure Move(x, y: Integer; absol: Boolean= false);
    procedure Move(pt: Tpoint; absol: Boolean= false);
    procedure Paint(btnState: TBtnState);
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
    FSliderSize: Integer;
    FSliderStyle: TSliderStyle;
    FSliderBorderColor: TColor;
    FRulerColor: TColor;
    FRulerBorderColor: Tcolor;
    FRulerSize: integer;
    FPosition: Integer;
    FMax, FMin : Integer;
    FColorParent: Boolean;
    FOrientation: TTBarOrientation;
    FReversed: boolean;
    FScaleMarks: TScaleMark;
    FscaleSize: Integer;
    FScaleColor: TColor;
    FFrequency: Integer;
    FKeyIncrement: Integer;
    FOnPositionChange: TNotifyEvent;
    Slider: TSlider;
    Scale: TSCale;
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
    procedure setSliderSize(i: Integer);
    procedure setSliderStyle(ss: TSliderStyle);
    procedure setSliderBorderColor(cl: Tcolor);
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
    procedure setOrientation(tr: TTBarOrientation);
    procedure setReversed(b: boolean);
    procedure setColorParent(b: boolean);
    //function getColor: TColor;
    //procedure setColor(cl: Tcolor);
    procedure RulerChange;
    procedure PaintRuler;
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
    GapMin, GapMax: Integer;
    property Canvas;
    constructor Create(aOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure tbChangeBounds(Sender: TObject);
  published
    property OnPositionChange: TNotifyEvent read FOnPositionChange write FOnPositionChange;
    property enabled;
    property color; //: Tcolor read getcolor write setColor;
    property Orientation: TTBarOrientation read FOrientation write setOrientation;
    property Reversed: Boolean read FReversed write setReversed default False;
    property Min: Integer read FMin write setMin;
    property Max: Integer read FMax write setMax;
    property Frequency: integer read FFrequency write setFrequency;
    property KeyIncrement: Integer read FKeyIncrement write setKeyIncrement;
    property Position: Integer read getPosition write setPosition default 0;
    property SliderColor: Tcolor read FSliderColor write setSliderColor;
    property SliderBorderColor: Tcolor read FSliderBorderColor write setSliderBorderColor;
    property SliderColorDown: TColor read FSliderColorDown write setSliderColorDown;
    property SliderColorHover: TColor read FSliderColorHover write SetSliderColorHover;
    property SliderSize: INteger read FSliderSize write setSliderSize;
    property SliderStyle: TSliderStyle read FSliderStyle write setSliderStyle;
    property RulerColor: TColor read FRulerColor write setRulerColor;
    property RulerBorderColor: Tcolor read FRulerBorderColor write setRulerBorderColor;
    property RulerSize: Integer read FRulerSize write setRulerSize;
    property ScaleMarks: TSCaleMark read FScaleMarks write setScaleMarks;
    property ScaleColor: Tcolor read FScaleColor write setScaleColor;
    property ScaleSize: Integer read FScalesize write setScaleSize;
    property Visible;
    property ColorParent: Boolean read FColorParent write SetColorParent;
    property TabOrder;
    property ShowHint;
    property TabStop;
   end;

procedure Register;

implementation

procedure Register;
begin
  {$I lazbbtrackbar_icon.lrs}
  RegisterComponents('lazbbComponents',[TbbTrackBar]);
end;

// TSCale

constructor TScale.create(aOwner: TbbTrackBar);
begin
  inherited Create;
  Parent:= TbbTrackBar(aOwner);
  ReInit;
end;

destructor TScale.Destroy;
begin
  inherited;
end;

procedure TScale.Reinit;
begin
  With Parent do
  begin
    if Orientation=tbVertical then
    begin
      // Add 1 px to scale size to be sure slider don't delete the scale
      topleft:= [0, GapMin, ScaleSize, Height-GapMax];   // top or left scale
      if ScaleMarks=tmBoth then botright:= [21+ScaleSize, GapMin, 21+ScaleSize*2, Height-GapMax]
      else botright:= [21, GapMin, 21+ScaleSize, Height-GapMax]; //Bottom or right scale
    end else
    begin
      topleft:= [GapMin, 0, width-GapMax, ScaleSize];   // top or left scale
      if ScaleMarks=tmBoth then botright:= [GapMin, 21+ScaleSize, width-GapMax, 21+ScaleSize*2]
      else  botright:= [GapMin, 21, width-GapMax, 21+ScaleSize]  ; //Bottom or right scale
    end;
  end;
end;

procedure TScale.Paint;
  var
  x1, y1, x2, y2: Integer;
  i: Integer;
  SliderMid: Integer;
begin
  Reinit;
  With Parent do
  begin
    SliderMid:= SliderSize div 2 ;
    Canvas.pen.style:= psSolid;
    if not Enabled then Canvas.pen.Color:= $C4C4C4
    else Canvas.pen.Color:= ScaleColor;
    for i:=Min to Max  do
    begin
      if i mod Frequency =0 then
      begin
        y1:= PositionToPixels(i)+SliderMid;
        if Orientation= tbVertical then
        begin
          // Paint top left scale
          if (i=0) or (i=max) or (i=min) then x1:= topleft[0] else x1:= topleft[0]+(ScaleSize div 2);
          if (i=0) or (i=max) or (i=min) then x2:= botright[2] else x2:= botright[2]-(ScaleSize div 2);
          if (ScaleMarks=tmTopLeft) or (ScaleMarks=tmBoth) then  Canvas.Line(x1, y1, topleft[2], y1);
          if (ScaleMarks=tmBottomRight) or (ScaleMarks=tmBoth) then Canvas.Line(botright[0], y1, x2, y1);
        end else
        begin
          x1:= PositionToPixels(i)+SliderMid;
          if (i=0) or (i=max) or (i=min) then y1:= topleft[1] else y1:= topleft[1]+(ScaleSize div 2);
          if (i=0) or (i=max) or (i=min) then y2:= botright[3] else y2:= botright[3]-(ScaleSize div 2);
          if (ScaleMarks=tmTopLeft) or (ScaleMarks=tmBoth) then Canvas.Line(x1, y1, x1, topleft[3] );
          if (ScaleMarks=tmBottomRight) or (ScaleMarks=tmBoth) then Canvas.Line(x1, botright[1], x1 , y2);
        end;
      end;
    end;
    Canvas.pen.style:= psClear;
  end;
end;

// TSlider

constructor TSlider.create(aOwner: TbbTrackBar);
begin
  inherited Create;
  Parent:= TbbTrackBar(aOwner);
  Rectngl:= TRect.Create(0,0,0,0);
  ReInit;
end;

destructor TSlider.Destroy;
begin
  inherited;
end;

procedure TSlider.setShape;
  var
  Size: Integer;
  SliderMid: Integer;
begin
  Size:= Parent.SliderSize;
  SliderMid:= Size div 2;

  if Parent.Orientation=tbVertical then
  begin
    case Parent.ScaleMarks of
      tmTopLeft: begin
        shape:= [Point(Rectngl.Left, Rectngl.Top+SliderMid), Point(Rectngl.Left+5,Rectngl.Top),
                 Point(Rectngl.Right,Rectngl.Top), Rectngl.BottomRight,
                 Point(Rectngl.Left+5,Rectngl.Bottom),  Point(Rectngl.Left,Rectngl.Top+SliderMid)];
      end;
      tmBottomRight: begin
        shape:= [Rectngl.TopLeft, Point(Rectngl.Left+13,Rectngl.Top), Point(Rectngl.Right,Rectngl.Top+SliderMid),
                 Point(Rectngl.Left+13,Rectngl.Bottom), Point(Rectngl.Left,Rectngl.Bottom), Rectngl.TopLeft];
      end;
      tmBoth: shape:= [Point(Rectngl.Left,Rectngl.Top+SliderMid), Point(Rectngl.Left+5,Rectngl.Top),
                       Point(Rectngl.Left+15,Rectngl.Top), Point(Rectngl.Right,Rectngl.Top+SliderMid),
                       Point(Rectngl.Left+15,Rectngl.Bottom),  Point(Rectngl.Left+5,Rectngl.Bottom),
                       Point(Rectngl.Left,Rectngl.Top+SliderMid)] ;
      tmNone: begin
        shape:= [Rectngl.TopLeft, Point(Rectngl.Left,Rectngl.Bottom), Rectngl.BottomRight, Point(Rectngl.Right,Rectngl.Top), Rectngl.TopLeft];
      end;
    end;
    BtnShape:= [Point(Rectngl.Left+1, Rectngl.Top+1), Point(Rectngl.Right+1, Rectngl.Bottom),     // 0 and 1: Butgton Rectangle;
                Point(Rectngl.Left+1, Rectngl.Top), Point(Rectngl.Right, Rectngl.Top),            // 2 and 3: Button top line,
                Point(Rectngl.Left, Rectngl.Top+1), Point(Rectngl.Left, Rectngl.Bottom-1),        // 4 and 5: Buttonleft line;
                Point(Rectngl.Left+1, Rectngl.Bottom-1), Point(Rectngl.Right, Rectngl.Bottom-1),  // 6 and 7: Button 1st bottom line
                Point(Rectngl.Right, Rectngl.Top+1), Point(Rectngl.Right, Rectngl.Bottom-1),      // 8 and 9: Button 1st right line;
                Point(Rectngl.Left+3, Rectngl.Top+Rectngl.Height div 2),                          // 10 and 11 : index line;
                Point(Rectngl.Right-2, Rectngl.Top+Rectngl.Height div 2)];
  end else
  begin
    case Parent.ScaleMarks  of
      tmTopLeft: begin
        shape:= [Point(Rectngl.Left,Rectngl.Top+5), Point(Rectngl.Left+SliderMid,Rectngl.Top),
                 Point(Rectngl.Right,Rectngl.Top+5), Rectngl.BottomRight,  Point(Rectngl.Left,Rectngl.Bottom),
                 Point(Rectngl.Left,Rectngl.Top+5)];

      end;
      tmBottomRight: begin
        shape:= [Rectngl.TopLeft, Point(Rectngl.Right,Rectngl.Top), Point(Rectngl.Right,Rectngl.Top+13),
                 Point(Rectngl.Left+SliderMid,Rectngl.Top+18), Point(Rectngl.Left,Rectngl.Top+13), Rectngl.TopLeft];
      end;
      tmBoth: shape:= [Point(Rectngl.Left,Rectngl.Top+5), Point(Rectngl.Left+SliderMid,Rectngl.Top),
                       Point(Rectngl.Right,Rectngl.Top+5), Point(Rectngl.Right,Rectngl.Top+15),
                       Point(Rectngl.Left+SliderMid,Rectngl.Bottom), Point(Rectngl.Left,Rectngl.Top+15),
                       Point(Rectngl.Left,Rectngl.Top+5)];
      tmNone: begin
        shape:= [Rectngl.TopLeft, Point(Rectngl.Right,Rectngl.Top), Point(Rectngl.Right,Rectngl.Bottom),
                 Point(Rectngl.Left,Rectngl.Bottom), Rectngl.TopLeft];
      end;
    end;
    BtnShape:= [Point(Rectngl.Left+1, Rectngl.Top+1), Point(Rectngl.Right, Rectngl.Bottom+1),     // 0 and 1: Butgton Rectangle;
                Point(Rectngl.Left+1, Rectngl.Top), Point(Rectngl.Right-1, Rectngl.Top),            // 2 and 3: Button top line,
                Point(Rectngl.Left, Rectngl.Top+1), Point(Rectngl.Left, Rectngl.Bottom),        // 4 and 5: Buttonleft line;
                Point(Rectngl.Left+1, Rectngl.Bottom), Point(Rectngl.Right-1, Rectngl.Bottom),  // 6 and 7: Button 1st bottom line
                Point(Rectngl.Right-1, Rectngl.Top+1), Point(Rectngl.Right-1, Rectngl.Bottom),      // 8 and 9: Button 1st right line;
                Point(Rectngl.Left+Rectngl.Width div 2, Rectngl.Top+3),                           // 10 and 11 : index line;
                Point(Rectngl.Left+Rectngl.Width div 2,  Rectngl.Bottom-2)];
  end;
end;

procedure TSlider.ReInit(origine: Boolean= false);
var
  Size: Integer;
  //SliderMid: Integer;
begin
  Size:= Parent.SliderSize;
  //SliderMid:= Size div 2;
  if Parent.Orientation=tbVertical then
  begin
    TopLeft:= Point(Parent.ScaleSize, Parent.GapMin);
    Rectngl.TopLeft:= point(0,0);
    Rectngl.BottomRight:= Point(20,Size);
    case Parent.ScaleMarks of
      tmTopLeft: begin
        Rectngl.BottomRight:= Point(18,Size);
      end;
      tmBottomRight: begin
        Rectngl.TopLeft:= point(2,0);
        TopLeft.X:= 0;
      end;
      tmNone: begin
        TopLeft.X:= 0;
      end;
    end;
  end else
  begin
    TopLeft:= Point(Parent.GapMin, Parent.ScaleSize);
    Rectngl.TopLeft:= point(0,0);
    Rectngl.BottomRight:= Point(Size,20);
    case Parent.ScaleMarks  of
      tmTopLeft: begin
        Rectngl.BottomRight:= Point(Size, 18);
      end;
      tmBottomRight: begin
        Rectngl.TopLeft:= Point(0,2);
        TopLeft.Y:= 0;
      end;
      tmNone: begin
        TopLeft.Y:= 0;
      end;
    end;
  end;
  SetShape;
  if origine then move(TopLeft);
end;

procedure TSlider.Move(x, y: Integer; absol: Boolean= false);
begin
  if absol then ReInit;
  Rectngl.Offset(x, y);
  setShape;
end;

procedure TSlider.Move(pt: Tpoint; absol: Boolean= false);
begin
  // Move shape
  Move(pt.x, pt.y, absol);
end;

procedure TSlider.Paint(BtnState: TbtnState);
var
  PrevBrushStyle: TBrushStyle;
  PrevBrushColor: TColor;
  PrevPenStyle: TPenStyle;
  PrevPenColor: TColor;
  col: TColor;
  //BtnCol: TColor;
  BorderCol: TColor;
begin
  BorderCol:= Parent.SliderBorderColor;
  if (not Parent.enabled) or (BtnState=bsDisabled) then
  begin
    col:= $CCCCCC;
    BorderCol:= $CCCCCC;
  end;
  if Parent.Color= clDefault then col:= clForm;
  Case BtnState of
    bsEnabled: col:= Parent.SliderColor;
    bsDisabled: col:= $CCCCCC;
    bsDown: col:= Parent.SliderColorDown;
    bsHover: col:= Parent.SliderColorHover;
    bsErase: begin
      col:= Parent.Color;
      if Parent.Color= clDefault then col:= clForm;
    end;
  end;
  With Parent.Canvas do
  begin
    // save current canvas settings
    PrevBrushStyle:= Brush.style;
    PrevBrushColor:= Brush.Color;
    PrevPenStyle:= Pen.Style;
    PrevPenColor:= pen.Color;
    // paint slider shape
    if BtnState=bsErase then
    begin
      Brush.Style:= bsSolid;
      brush.Color:= col;
      Pen.Style:= psSolid;
      Pen.Color:= col;
      Rectangle(Rectngl.Left, Rectngl.Top, Rectngl.Left+Rectngl.Width+1, Rectngl.Top+Rectngl.Height+1);
    end else
    begin
      if parent.SliderStyle= ssClassic then        // TTrackbar slider style
      begin
        Brush.Style:= bsSolid;
        brush.Color:= col;
        Pen.Style:= psSolid;
        Pen.Color:= BorderCol;
        Polygon(Shape);
      end else
      begin                                       // Slider style : button
        Brush.Style:= bsSolid;
        brush.Color:= col;
        Rectangle(BtnShape[0].x, BtnShape[0].y, BtnShape[1].x, BtnShape[1].y);
        Pen.Style:= psSolid;
        Pen.Color:= BorderCol;
        Line(BtnShape[2].x, BtnShape[2].y, BtnShape[3].x, BtnShape[3].y );
        Line(BtnShape[4].x, BtnShape[4].y, BtnShape[5].x, BtnShape[5].y );
        Line(BtnShape[6].x, BtnShape[6].y, BtnShape[7].x, BtnShape[7].y );
        Line(BtnShape[8].x, BtnShape[8].y, BtnShape[9].x, BtnShape[9].y );
        if not (Parent.ScaleMarks= tmNone) then Line (BtnShape[10].x, BtnShape[10].y, BtnShape[11].x, BtnShape[11].y );
      end;
    end;
    // restore previous canvas settings
    Brush.Style:= PrevBrushStyle;
    brush.Color:= PrevBrushColor;
    Pen.Style:= PrevPenStyle;
    Pen.Color:= PrevPenColor;
  end;
end;

// TbbTRackbar

constructor TbbTrackBar.create(aOwner: Tcomponent);
begin
  inherited;
  parent:= TwinControl(aOwner);
  ControlStyle := ControlStyle + [csParentBackground, csClickEvents,
    csCaptureMouse, csDoubleClicks, csRequiresKeyboardInput, csopaque];
  Slider:= TSlider.Create(self);
  Scale:= TScale.Create(self);
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
  GapMin:= 5;
  GapMax:= 5;
  FSliderSize:= 10;
  FScaleSize:= 5;
  FRulerSize:= 10;
  FFrequency:= 1;
  FKeyIncrement:= 1;
  OnChangeBounds:= @tbChangeBounds;
  FReversed:= false;
  //Default slider vertical, left
  FOrientation:= tbVertical;
  FScaleMarks:=  tmTopLeft;
  Scale.ReInit;
  SliderStyle:= ssClassic;
  Slider.ReInit(true);
  Xprev:= 0;
  Yprev:=0;
  //FSliderColor:= clMenuHighlight;
  //FSliderColorDown:= clActiveCaption;
  //FSliderColorHover:= clBlue;
  //FSliderBorderColor:= clActiveBorder;
  FRulerColor:= cl3DLight;
  FRulerBorderColor:= clActiveBorder;
  FScaleColor:= clGray;
  FColorParent:= false;
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
    Slider.ReInit(true);
    if FOrientation= tbVertical then PositionChange(0) //SliderMove(Ruler.Top-5,FScaleSize)
    else PositionChange(0);
    invalidate;
  end;
end;


function TbbTrackBar.PixelsToPosition(px: Integer): Integer;
begin
  if FOrientation=tbVertical then result:= Round(((px-GapMin)*(FMax-FMin))/(Height-GapMin-GapMax-FSliderSize))
  else result:= Round(((px-GapMin)*(FMax-FMin))/(Width-GapMin-GapMax-SliderSize));
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
var
  SliderCourse: Integer;
begin;
  result:= FScaleSize;
  if (pos<FMin) or (pos>FMax) then exit;
  SliderCourse:= Height-GapMin-GapMax-SliderSize;
  pos:= pos-FMin;
  if FOrientation=tbVertical then
  begin
    SliderCourse:= Height-GapMin-GapMax-SliderSize;
    Result:= GapMin+Round((Pos*SliderCourse)/(FMax-FMin));
    if Reversed then Result:= Height-GapMax-SliderSize-Round((pos*SliderCourse)/(FMax-FMin));
  end else
  begin
    SliderCourse:= Width-GapMin-GapMax-SliderSize;
    Result:= GapMin+Round((Pos*SliderCourse)/(FMax-FMin));
    if Reversed then Result:= Width-GapMax-SliderSize-Round((pos*SliderCourse)/(FMax-FMin));   end;
end;

procedure TbbTrackBar.PositionChange(p: integer);
begin
  if (p<FMin) or (p>FMax) then exit;
  Slider.ReInit;
  if FOrientation=tbVertical then Slider.Move(Slider.TopLeft.X, PositionToPixels(p))
  else Slider.Move(PositionToPixels(p), Slider.TopLeft.Y);
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
  Slider.ReInit(true);
  Positionchange(0);
  Invalidate;
end;

procedure TbbTrackBar.setSliderSize(i: Integer);
begin
  if FSliderSize=1 then exit;
  FSliderSize:= i;
  Slider.ReInit(true);
  Positionchange(0);
  Invalidate;
end;

procedure TbbTrackBar.setReversed(b: boolean);
begin
  if FReversed= b then exit;
  fReversed:= b;
  Slider.ReInit(true);
  Positionchange(0);
  Invalidate;
end;

procedure TbbTrackBar.setScaleMarks(tm: TScaleMark);
begin
  if FScaleMarks= tm then exit;
  FScaleMarks:= tm;
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
  Slider.ReInit(true);
  PositionChange(0);
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
  Slider.ReInit(true);
  PositionChange(0);
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

procedure TbbTrackBar.setSliderBorderColor(cl: TColor);
begin
  if FSliderBorderColor=cl then exit;
  FSliderBorderColor:= cl;
  Invalidate;
end;

procedure TbbTrackBar.setSliderStyle(ss: TSliderStyle);
begin
  if FSliderStyle= ss then exit;
  FSliderStyle:= ss;
  if csDesigning in ComponentState then
  begin
    if Fsliderstyle=ssClassic then
    begin
      FSliderColor:=clHighlight;
      FSliderColorHover:= clBlue;
      FSliderColorDown:= clGradientInactiveCaption;
      FSliderBorderColor:= clHighlight;
    end else
    begin
      FSliderColor:= clBtnFace;
      FSliderColorHover:= clBtnHighlight;
      FSliderColorDown:= clGradientActiveCaption;
      FSliderBorderColor:= clActiveBorder;
    end;
  end;
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

procedure TbbTrackBar.setColorParent(b: Boolean);
begin
  if FColorParent= b then exit;
  FColorParent:= b;
  if FColorParent then setColor(GetColorResolvingParent);
  invalidate;
end;

{function TbbTrackBar.getColor: TColor;
begin
  result:= inherited Color;
end;

procedure TbbTrackBar.setColor(cl: Tcolor);
begin
  if Color=cl then exit;
  if Cl=clNone then Color:= clDefault else Color:=cl;
  invalidate;
end;  }

procedure TbbTrackBar.setScaleColor(cl: Tcolor);
begin
  if FScaleColor= cl then exit;
  FScaleColor:= cl;
  Invalidate;
end;

procedure TbbTrackBar.RulerChange;
begin
  if FOrientation=tbVertical then
  begin
    Ruler.TopLeft:= Point(Slider.TopLeft.X+10-(RulerSize div 2), GapMin);
    Ruler.BottomRight:= Point(Ruler.Left+FRulerSize+1, Height-GapMax);
  end else
  begin
    Ruler.Left:= GapMin;
    Ruler.Top:= Slider.TopLeft.Y+10-(RulerSize div 2);          // Center ruler on slider median
    Ruler.Right:= width-GapMax;
    Ruler.Bottom:= Ruler.top+FRulersize+1;
  end;
end;


procedure TbbTrackBar.PaintRuler;
begin
  RulerChange;
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
  // Inherited nothing. We paint from scratch
  if Color=clNone then color:= clDefault;
  // Default color is wrong in design mode after a color change
  if (csDesigning in ComponentState) and (color=clDefault) then
  begin
    Canvas.Brush.Color:= clform;
    canvas.Rectangle(0,0, width, height);
  end;
  Scale.Paint;
  PaintRuler;
  Slider.Paint(bsEnabled);
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
  Slider.paint(bsDown);
  Xprev:= X;
  Yprev:= Y;
  Xdif:= X-Slider.Rectngl.Left;
  Ydif:= Y-Slider.Rectngl.Top;
end;


procedure TbbTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
 dy: integer;
 dx: Integer;
 col: Tcolor;
begin
  inherited;
  if (Color=clDefault) then Col:= clForm
  else Col:= color;
  XMouse:= X;
  YMouse:= Y;
  if Slider.Rectngl.Contains(Point(X, Y))then
  begin
     Slider.Paint(bsHover);
  end else
  begin
    MouseLeave;
  end;
  if not MouseDwn then exit;
  Case FOrientation of
    tbVertical: begin
      if ((Y-Ydif)<GapMin) or ((Y-Ydif)>(height-GapMax-SliderSize)) then exit;   // 10 is slider height
      X:= 0;
      Slider.Paint(bsErase);
      PaintRuler;
      dy:= Y-Yprev;
      Slider.Move(X, dy);
    end;
    tbHorizontal: begin
      if ((X-Xdif)<GapMin) or ((X-Xdif)>width-GapMax-SliderSize) then exit;
      Y:= 0;
      Slider.Paint(bsErase);
      PaintRuler;
      dx:= X-Xprev;
      Slider.Move(dx, Y);
    end;
  end;
  Slider.Paint(bsDown);
  Xprev:= X;
  Yprev:= Y;
  Fposition:= getPosition;
  if Assigned(FOnPositionChange) then FOnPositionChange(Self);
end;

procedure TbbTrackBar.MouseUp(Button: TMouseButton; Shift:TShiftState; X, Y:Integer);
begin
  inherited;
  MouseDwn:= false;
  Slider.Paint(bsEnabled);
end;

procedure TbbTrackBar.MouseEnter;
begin
end;

procedure TbbTrackBar.MouseLeave;
begin
  inherited;
  Slider.Paint(bsEnabled);
end;

end.


