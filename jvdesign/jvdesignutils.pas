﻿{ Modified for Lazarus by Costas Velissariou (velissariouc@gmail.com) 04/01/2011}

{Modified the file by Pavel Duborkin (e-mail: 7bit@list.ru, mydataexpress@mail.ru).
The changes made are marked with comments that begin with "// 7bit" and end with "//"}

unit JvDesignUtils;

{$mode objfpc}{$H+}

interface
uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$ifdef windows}
  windows,
  win32proc,
  {$endif}
  SysUtils, LCLProc, LCLType, LResources, LCLIntf, LMessages, Classes, FPCanvas,
  Controls, Graphics, Forms, dialogs;

type
  TDesignerDCFlag = (
    ddcDCOriginValid,         // please comment
    ddcFormOriginValid,       //
    ddcFormClientOriginValid, //
    ddcSizeValid              //
  );
  TDesignerDCFlags = set of TDesignerDCFlag;

  { TDesignerDeviceContext }

  TDesignerDeviceContext = class
  private
    FCanvas: TCanvas;
    FDC: HDC;
    FDCControl: TWinControl;
    FDCOrigin: TPoint;   // DC origin on desktop
    FFlags: TDesignerDCFlags;
    FFormClientOrigin: TPoint; // Form client origin on desktop
    FFormOrigin: TPoint; // DC origin relative to designer Form
    FDcSize: TPoint;
    FForm: TCustomForm;
    FSavedDC: HDC;
    FPaintCount: integer;
    function GetDCOrigin: TPoint;
    function GetDCSize: TPoint;
    function GetFormClientOrigin: TPoint;
    function GetFormOrigin: TPoint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetDC(AForm: TCustomForm; ADCControl: TWinControl; ADC: HDC);
    procedure Clear;
    procedure BeginPainting;
    procedure EndPainting;
    function RectVisible(ALeft, ATop, ARight, ABottom: integer): boolean;
    property Canvas: TCanvas read FCanvas;
    property DC: HDC read FDC;
    property Form: TCustomForm read FForm;
    property FormOrigin: TPoint read GetFormOrigin;// DC origin relative to designer Form
    property DCOrigin: TPoint read GetDCOrigin; // DC origin on Desktop
    property FormClientOrigin: TPoint read GetFormClientOrigin;// Form Client Origin on desktop
    property DCSize: TPoint read GetDCSize;
  end;



function GetControlClientOffset(C: TControl): TPoint; // 7bit
function DesignClientToParent(const APt: TPoint; AControl, AParent: TControl): TPoint;

function DesignMin(AA, AB: Integer): Integer;
function DesignMax(AA, AB: Integer): Integer;

function DesignRectWidth(const ARect: TRect): Integer;
function DesignRectHeight(const ARect: TRect): Integer;
function DesignValidateRect(const ARect: TRect): TRect;

function DesignNameIsUnique(AOwner: TComponent; const AName: string): Boolean;
function DesignUniqueName(AOwner: TComponent; const AClassName: string): string;

procedure DesignPaintRubberbandRect(AContainer: TWinControl; ARect: TRect; APenStyle: TPenStyle);
procedure DesignPaintRubberbandRect(Container: TControl; Canvas: TCanvas; ARect: TRect); overload;
procedure DesignPaintGrid(ACanvas: TCanvas; const ARect: TRect;
  ABackColor: TColor = clBtnFace; AGridColor: TColor = clBlack;
  ADivPixels: Integer = 8);
procedure DesignPaintRules(ACanvas: TCanvas; const ARect: TRect;
  ADivPixels: Integer = 32; ASubDivs: Boolean = True);

procedure DesignSaveComponentToStream(AComp: TComponent; AStream: TStream);
function DesignLoadComponentFromStream(AComp: TComponent; AStream: TStream;
  AOnError: TReaderError): TComponent;

procedure DesignSaveComponentToFile(AComp: TComponent; const AFileName: string);
procedure DesignLoadComponentFromFile(AComp: TComponent;
  const AFileName: string; AOnError: TReaderError);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jvcl.svn.sourceforge.net/svnroot/jvcl/trunk/jvcl/run/JvDesignUtils.pas $';
    Revision: '$Revision: 12535 $';
    Date: '$Date: 2009-10-02 12:36:42 +0300 (Παρ, 02 Οκτ 2009) $';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  ComCtrls;

// 7bit
function GetControlClientOffset(C: TControl): TPoint;
var
  P, P2: TPoint;
begin
  Result := Point(0, 0);
  P := C.ClientToScreen(Point(0, 0));
  if C.Parent <> nil then
  begin
    P2 := C.Parent.ClientToScreen(Point(C.Left, C.Top));
    Result := Point(P.X - P2.X, P.Y - P2.Y);
  end;
end;
//

function DesignClientToParent(const APt: TPoint; AControl, AParent: TControl): TPoint;
var
  R: trect;
begin
  Result := APt;

  while (AControl <> AParent) and (AControl <> nil) do
  begin
    {$ifdef windows}
    //undo lcl hack
    if (AControl is TWinControl) then
    begin
      if GetLCLClientBoundsOffset(AControl, R) then
      begin
        Inc(result.x, R.Left);
        Inc(result.Y, R.Top);
      end;
    end;
    {$endif}


    Inc(Result.X, AControl.Left);
    Inc(Result.Y, AControl.Top);
    AControl := AControl.Parent;
  end;
end;

function DesignMin(AA, AB: Integer): Integer;
begin
  if AB < AA then
    Result := AB
  else
    Result := AA;
end;

function DesignMax(AA, AB: Integer): Integer;
begin
  if AB > AA then
    Result := AB
  else
    Result := AA;
end;

function DesignRectWidth(const ARect: TRect): Integer;
begin
  Result := ARect.Right - ARect.Left;
end;

function DesignRectHeight(const ARect: TRect): Integer;
begin
  Result := ARect.Bottom - ARect.Top;
end;

function DesignValidateRect(const ARect: TRect): TRect;
begin
  with Result do
  begin
    if ARect.Right < ARect.Left then
    begin
      Left := ARect.Right;
      Right := ARect.Left;
    end
    else
    begin
      Left := ARect.Left;
      Right := ARect.Right;
    end;
    if ARect.Bottom < ARect.Top then
    begin
      Top := ARect.Bottom;
      Bottom := ARect.Top;
    end
    else
    begin
      Top := ARect.Top;
      Bottom := ARect.Bottom;
    end;
  end;
end;

function DesignNameIsUnique(AOwner: TComponent; const AName: string): Boolean;
begin
  Result := True;
  while Result and (AOwner <> nil) do
  begin
    Result := AOwner.FindComponent(AName) = nil;
    AOwner := AOwner.Owner;
  end;
end;

function DesignUniqueName(AOwner: TComponent; const AClassName: string): string;
var
  Base: string;
  I: Integer;
begin
  Base := Copy(AClassName, 2, MAXINT);
  I := 0;
  repeat
    Inc(I);
    Result := Base + IntToStr(I);
  until DesignNameIsUnique(AOwner, Result);
end;

procedure DesignPaintRubberbandRect(AContainer: TWinControl; ARect: TRect;
  APenStyle: TPenStyle);
var
  DesktopWindow: HWND;
  DC: HDC;
  C: TCanvas;
begin
  begin
    if AContainer = nil then
      DesktopWindow := 0
    else
    begin
      DesktopWindow := AContainer.Handle;
      ARect.TopLeft := AContainer.ScreenToClient(ARect.TopLeft);
      ARect.BottomRight := AContainer.ScreenToClient(ARect.BottomRight);
    end;

    {$ifdef windows}
    DC:=GetDCEx(DesktopWindow,0,DCX_CACHE or DCX_CLIPSIBLINGS);
    {$else}
    DC:=GetDC(DesktopWindow);
    {$endif}

    try
      C := TCanvas.Create;
      with C do
      try
        Handle := DC;
        Pen.Style := APenStyle;
        Pen.Color := clWhite;
        Pen.Mode := pmXor;

        Brush.Style := bsClear;
        Rectangle(ARect);
      finally
        C.Free;
      end;
    finally
      ReleaseDC(DesktopWindow, DC);
    end;
  end;
end;

procedure DesignPaintRules(ACanvas: TCanvas; const ARect: TRect;
  ADivPixels: Integer; ASubDivs: Boolean);
var
  d, d2, w, h, I: Integer;
begin
  d := ADivPixels;
  d2 := d div 2;
  w := (ARect.Right - ARect.Left + d - 1) div d;
  h := (ARect.Bottom - ARect.Top + d - 1) div d;
  with ACanvas do
  begin
    Pen.Style := psDot;
    for I := 0 to w do
    begin
      Pen.Color := $DDDDDD;
      MoveTo(I * d, ARect.Top);
      LineTo(I * d, ARect.Bottom);
      if ASubDivs then
      begin
        Pen.Color := $F0F0F0;
        MoveTo(I * d + d2, ARect.Top);
        LineTo(I * d + d2, ARect.Bottom);
      end;
    end;
    for I := 0 to h do
    begin
      Pen.Color := $DDDDDD;
      MoveTo(ARect.Left, I * d);
      LineTo(ARect.Right, I * d);
      if ASubDivs then
      begin
        Pen.Color := $F0F0F0;
        MoveTo(ARect.Left, I * d + d2);
        LineTo(ARect.Right, I * d + d2);
      end;
    end;
  end;
end;

procedure DesignPaintRubberbandRect(Container: TControl; Canvas: TCanvas;
  ARect: TRect);
begin
  with Canvas do
  begin
    //Pen.Style := psDash;
    Pen.Color := clMaroon;
    //Pen.Color := clWhite;
    //Pen.Mode := pmXor;
    Brush.Style := bsClear;

    ARect.TopLeft := Container.ScreenToClient(ARect.TopLeft);
    ARect.BottomRight := Container.ScreenToClient(ARect.BottomRight);

    Rectangle(ARect);
    Container.Invalidate;
  end;
end;

procedure DesignPaintGrid(ACanvas: TCanvas; const ARect: TRect;
  ABackColor: TColor; AGridColor: TColor; ADivPixels: Integer);
var
  b: TBitmap;
  I: Integer;
begin
  b := TBitmap.Create;
  try
    b.Height := DesignRectHeight(ARect);
    b.Width := ADivPixels;
    b.Canvas.Brush.Color := ABackColor;
    b.Canvas.FillRect(Rect(0, 0, b.Width, b.Height));

    I := 0;
    repeat
      b.Canvas.Pixels[0, I] := AGridColor;
      Inc(I, ADivPixels);
    until (I >= b.Height);

    I := ARect.Left;
    repeat
      ACanvas.Draw(I, ARect.Top, b);
      Inc(I, ADivPixels);
    until I >= ARect.Right;
  finally
    b.Free;
  end;
end;

procedure DesignSaveComponentToStream(AComp: TComponent; AStream: TStream);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.WriteComponent(AComp);
    MS.Position := 0;
    ObjectBinaryToText(MS, AStream, oteLFM);
  finally
    MS.Free;
  end;
end;

type
  TAccessComponent = class(TComponent);

function DesignLoadComponentFromStream(AComp: TComponent; AStream: TStream;
  AOnError: TReaderError): TComponent;
var
  MemStream: TMemoryStream;
  CompDesigning: Boolean;
begin
  MemStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(AStream, MemStream);
    MemStream.Position := 0;
    with TReader.Create(MemStream, 4096) do
    try
      OnError := AOnError;
      { We have to set the container into design mode so all loaded components
        are in design mode. }
      CompDesigning := csDesigning in AComp.ComponentState;
      TAccessComponent(AComp).SetDesigning(True, False);
      try
        Result := ReadRootComponent(AComp);
      finally
        if not CompDesigning then
          TAccessComponent(AComp).SetDesigning(CompDesigning, False);
      end;
    finally
      Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure DesignSaveComponentToFile(AComp: TComponent; const AFileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmCreate);
  try
    DesignSaveComponentToStream(AComp, FS);
  finally
    FS.Free;
  end;
end;

procedure DesignLoadComponentFromFile(AComp: TComponent;
  const AFileName: string; AOnError: TReaderError);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead);
  try
    DesignLoadComponentFromStream(AComp, FS, AOnError);
  finally
    FS.Free;
  end;
end;

{ TDesignerDeviceContext }

function TDesignerDeviceContext.GetDCOrigin: TPoint;
begin

end;

function TDesignerDeviceContext.GetDCSize: TPoint;
begin

end;

function TDesignerDeviceContext.GetFormClientOrigin: TPoint;
begin

end;

function TDesignerDeviceContext.GetFormOrigin: TPoint;
begin

end;

constructor TDesignerDeviceContext.Create;
begin
  inherited Create;
  FCanvas:=TCanvas.Create;
end;

destructor TDesignerDeviceContext.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TDesignerDeviceContext.SetDC(AForm: TCustomForm;
  ADCControl: TWinControl; ADC: HDC);
begin
  Clear;
  FDC := ADC;
  FDCControl := ADCControl;
  FForm := AForm;
end;

procedure TDesignerDeviceContext.Clear;
begin
  if (FSavedDC<>0) or (FPaintCount>0) then
    //RaiseGDBException('');
    ShowMessage('RaiseGDBException');
  FDC := 0;
  FFlags := FFlags - [ddcFormOriginValid, ddcFormClientOriginValid, ddcDCOriginValid, ddcSizeValid];
end;

procedure TDesignerDeviceContext.BeginPainting;
begin

end;

procedure TDesignerDeviceContext.EndPainting;
begin

end;

function TDesignerDeviceContext.RectVisible(ALeft, ATop, ARight,
  ABottom: integer): boolean;
begin

end;


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
