{ Modified for Lazarus by Costas Velissariou (velissariouc@gmail.com) 04/01/2011}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDesingSurface.pas, released on 2005-08-21.

The Initial Developer of the Original Code is Scott J Miles
Portions created by Scott J Miles are Copyright (C) 2005 Scott J Miles.
All Rights Reserved.

Contributor(s): Olivier Sannier (JVCL Integration)

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
  Mantis 3963: When a design surface is active, the ENTIRE form where it is
               located suffers impacts from being in design mode. This can not
               be circumvented because the Designer property is to be set on
               the parent form and it MUST be set for the design mode to be
               effective. The only workaround is to not have anything else
               on the form being designed.

-----------------------------------------------------------------------------}
// $Id: JvDesignSurface.pas 12931 2010-11-28 13:36:50Z ahuser $

{Modified the file by Pavel Duborkin (e-mail: 7bit@list.ru, mydataexpress@mail.ru).
The changes made are marked with comments that begin with "// 7bit" and end with "//"}

unit JvDesignSurface;

{$mode objfpc}{$H+}
{$DEFINE NO_DESIGNHOOK}
interface

uses
  Classes, SysUtils,
  LCLProc, LCLType, LResources, LCLIntf,
  //Messages,
  Forms, Controls, Graphics,
  Dialogs,
  //Windows,
  {$ifdef windows}
  win32proc,
  Messages,
  {$endif}
  ExtCtrls, TypInfo, LMessages, Menus, Buttons, StdCtrls;

type
  TJvDesignSurface = class;

  TJvDesignMessage = function(ASender: TControl; var AMsg: TLMessage;
    const APt: TPoint): boolean of object;

  TJvDesignCustomMessenger = class(TObject)
  private
    FContainer: TWinControl;
    FOnDesignMessage: TJvDesignMessage;
    FOnChange: TNotifyEvent;
  protected
    procedure SetContainer(AValue: TWinControl); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsDesignMessage(ASender: TControl; var AMessage: TLMessage): boolean;
      virtual;
    procedure Clear; virtual;
    procedure DesignChildren(AContainer: TWinControl; ADesigning: boolean);
    procedure DesignComponent(AComponent: TComponent; ADesigning: boolean); virtual;
    property Container: TWinControl read FContainer write SetContainer;
    property OnDesignMessage: TJvDesignMessage
      read FOnDesignMessage write FOnDesignMessage;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvDesignCustomMessengerClass = class of TJvDesignCustomMessenger;

  TJvDesignMessageHook = class(TObject)
  private
    FClient: TWinControl;
    FOldProc: TWndMethod;
    FUser: TJvDesignCustomMessenger;
  protected
    procedure HookProc(var AMessage: TLMessage);
    procedure Unhook;
  public
    constructor Create(AUser: TJvDesignCustomMessenger; AClient: TWinControl);
    destructor Destroy; override;
    property Client: TWinControl read FClient;
  end;

  { TJvDesignCustomController }

  TJvDesignCustomController = class(TObject)
  private
    FSurface: TJvDesignSurface;

  protected
    function GetDragRect: TRect; virtual; abstract;
    //CV function GetShift: TShiftState;
    function KeyDown(AKeyCode: cardinal): boolean; virtual; abstract;
    function KeyUp(AKeyCode: cardinal): boolean; virtual; abstract;
    function MouseDown(Button: TMouseButton; X, Y: integer;
      TheMessage: TLMMouse): boolean; virtual; abstract;
    function MouseMove(X, Y: integer; TheMessage: TLMMouse): boolean; virtual; abstract;
    function MouseUp(Button: TMouseButton; X, Y: integer; TheMessage: TLMMouse): boolean;
      virtual; abstract;
  public
    constructor Create(ASurface: TJvDesignSurface); virtual;
    procedure Paint(Control: TControl; DC: HDC); virtual;
    property DragRect: TRect read GetDragRect;
    property Surface: TJvDesignSurface read FSurface;
  end;

  TJvDesignCustomControllerClass = class of TJvDesignCustomController;

  TJvDesignHandleId = (dhNone, dhLeftTop, dhMiddleTop, dhRightTop, dhLeftMiddle,
    dhRightMiddle, dhLeftBottom, dhMiddleBottom, dhRightBottom);

  { TJvDesignCustomSelector }

  TJvDesignCustomSelector = class(TComponent)
  private
    FSurface: TJvDesignSurface;
    function GetSelectionRect: TRect;
  protected
    function GetCount: integer; virtual; abstract;
    function GetSelection(AIndex: integer): TControl; virtual; abstract;
    procedure SetSelection(AIndex: integer; AValue: TControl); virtual; abstract;
  public
    constructor Create(ASurface: TJvDesignSurface); reintroduce; virtual;
    destructor Destroy; override;
    function IsSelected(AValue: TControl): boolean; virtual; abstract;
    function GetClientControl(AControl: TControl): TControl; virtual; abstract;
    function GetCursor(AX, AY: integer): TCursor; virtual; abstract;
    function GetHitHandle(AX, AY: integer): TJvDesignHandleId; virtual; abstract;
    procedure AddToSelection(AValue: TControl); virtual; abstract;
    procedure ClearSelection; virtual; abstract;
    procedure RemoveFromSelection(AValue: TControl); virtual; abstract;
    procedure ToggleSelection(AValue: TControl);
    procedure Update; virtual; abstract;
    property Count: integer read GetCount;
    property Selection[AIndex: integer]: TControl read GetSelection write SetSelection;
    property Surface: TJvDesignSurface read FSurface;
    property SelectionRect: TRect read GetSelectionRect; // 7bit
  end;

  TJvDesignCustomSelectorClass = class of TJvDesignCustomSelector;

  TJvDesignObjectArray = array of TObject;
  TJvDesignGetAddClassEvent = procedure(Sender: TObject; var ioClass: string) of object;
{
  TJvDesignOwnerDrawGridEvent = procedure(ASender: TObject; ACanvas: TCanvas;
    ARect: TRect) of object;
}

  TAddComponentEvent = procedure(Sender: TObject; aComponent: TComponent) of object;

  { TJvDesignSurface }

  TJvDesignSurface = class(TComponent)
  private
    FDesignCanvas: TCanvas; // 7bit
    FActive: boolean;
    FAddClass: string;
    FContainer: TWinControl;
    FContainerHook: TJvDesignMessageHook;
    FController: TJvDesignCustomController;
    FControllerClass: TJvDesignCustomControllerClass;
    FGridColor: TColor;
    FGridSizeX: Integer;
    FGridSizeY: Integer;
    FShowGrid: Boolean;
    FMessenger: TJvDesignCustomMessenger;
    FMessengerClass: TJvDesignCustomMessengerClass;
    FOnAddComponent: TAddComponentEvent;
    FOnDeleteComponent: TAddComponentEvent;
    FOnGetComponentName: TAddComponentEvent;
    FOnChangingComponents: TNotifyEvent;
    FSelector: TJvDesignCustomSelector;
    FSelectorClass: TJvDesignCustomSelectorClass;
    FStepX: Integer;
    FStepY: Integer;
    FUpdateOwner: TComponent;
    FOldState: Integer;

    FPopupMenu: TPopupMenu;

    procedure MessengerOnChange(Sender: TObject);
    // 7bit
    procedure BeginPaint(DC: HDC);
    procedure EndPaint;
    procedure PaintFieldNames(AContainer: TWinControl);
    procedure PaintClientGrid(AWinControl: TWinControl; DC: HDC);
    function PaintControls(Sender: TControl; TheMessage: TLMPaint): Boolean;
    procedure SetGridColor(AValue: TColor);
    procedure SetGridSizeX(AValue: Integer);
    procedure SetGridSizeY(AValue: Integer);
    //
  protected
    FOnChange: TNotifyEvent;
    FOnGetAddClass: TJvDesignGetAddClassEvent;
    //    FOnOwnerDrawGrid: TJvDesignOwnerDrawGridEvent;
    FOnSelectionChange: TNotifyEvent;
    function GetAddBounds: TRect;
    function GetCount: integer;
    function GetSelected: TJvDesignObjectArray;
    function GetSelectedContainer: TWinControl;
    function GetSelection(AIndex: integer): TControl;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NeedContainer;
    procedure NeedController;
    procedure NeedMessenger;
    procedure NeedSelector;
    //procedure PaintContainerBkgnd(ADC: HDC);
    procedure ReaderError(Reader: TReader; const Msg: string; var Handled: boolean);
    procedure SetActive(AValue: boolean);
    procedure SetContainer(AValue: TWinControl);
    procedure SetShowGrid(const Value: Boolean);
    procedure SetSelection(AIndex: integer; AValue: TControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Clear: TJvDesignSurface;
    function ContainerToSelectedContainer(const APt: TPoint): TPoint;
    function FindControl(AX, AY: integer): TControl; virtual;
    function GetCursor(AX, AY: integer): TCursor; virtual;
    function GetHitHandle(AX, AY: integer): TJvDesignHandleId; virtual;
    function IsDesignMessage(ASender: TControl; var AMsg: TLMessage;
      const APt: TPoint): boolean;
    function LoadFromFile(const AFileName: string): TJvDesignSurface;
    function LoadFromStream(AStream: TStream): TJvDesignSurface;
    procedure AddComponent;
    procedure Change;
    procedure ClearSelection;
    procedure CopyComponents;
    procedure CutComponents;
    procedure DeleteComponents;
    procedure GetAddClass;
    procedure GrowComponents(AGrowWidth, AGrowHeight: integer);
    procedure NudgeComponents(ANudgeLeft, ANudgeTop: integer);
    procedure PasteComponents;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure Select(AControl: TControl);
    procedure SelectionChange;
    procedure SelectParent;
    procedure SetSelected(const AValue: array of TObject);
    procedure UpdateDesigner; virtual;
    // 7bit
    procedure DoChangingComponents;

    property Active: boolean read FActive write SetActive;
    property AddClass: string read FAddClass write FAddClass;
    property Controller: TJvDesignCustomController read FController;
    property ControllerClass: TJvDesignCustomControllerClass
      read FControllerClass write FControllerClass;
    property Count: integer read GetCount;
    property Messenger: TJvDesignCustomMessenger read FMessenger;
    property MessengerClass: TJvDesignCustomMessengerClass
      read FMessengerClass write FMessengerClass;
    property Selected: TJvDesignObjectArray read GetSelected;
    property SelectedContainer: TWinControl read GetSelectedContainer;
    property Selection[AIndex: integer]: TControl read GetSelection write SetSelection;
    property Selector: TJvDesignCustomSelector read FSelector;
    property SelectorClass: TJvDesignCustomSelectorClass
      read FSelectorClass write FSelectorClass;
  published
    property Container: TWinControl read FContainer write SetContainer;
     // 7bit
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default True;
    property GridSizeX: Integer read FGridSizeX write SetGridSizeX;
    property GridSizeY: Integer read FGridSizeY write SetGridSizeY;
    property GridColor: TColor read FGridColor write SetGridColor;
    property StepX: Integer read FStepX;
    property StepY: Integer read FStepY;
    property DesignCanvas: TCanvas read FDesignCanvas;
    //
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetAddClass: TJvDesignGetAddClassEvent
      read FOnGetAddClass write FOnGetAddClass;
    //    property OnOwnerDrawGrid: TJvDesignOwnerDrawGridEvent read FOnOwnerDrawGrid write FOnOwnerDrawGrid;
    property OnSelectionChange: TNotifyEvent
      read FOnSelectionChange write FOnSelectionChange;
    property PopupMenu: TPopupMenu read fPopupMenu write FPopupMenu;
    // 7bit //
    property OnGetComponentName: TAddComponentEvent
      read FOnGetComponentName write FOnGetComponentName;
    property OnAddComponent: TAddComponentEvent
      read FOnAddComponent write FOnAddComponent;
    property OnDeleteComponent: TAddComponentEvent
      read FOnDeleteComponent write FOnDeleteComponent;
    property OnChangingComponents: TNotifyEvent
      read FOnChangingComponents write FOnChangingComponents;

  end;

  TJvDesignScrollBox = class(TScrollBox)
  protected
    procedure AutoScrollInView(AControl: TControl); //CV override;
  end;

  TJvDesignPanel = class(TPanel)
  private
    FSurface: TJvDesignSurface;
    FOnPaint: TNotifyEvent;
    FDrawRules: boolean;
    function GetActive: boolean;
    function GetOnChange: TNotifyEvent;
    function GetOnGetAddClass: TJvDesignGetAddClassEvent;
    function GetOnSelectionChange: TNotifyEvent;
    procedure SetActive(const Value: boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetOnGetAddClass(const Value: TJvDesignGetAddClassEvent);
    procedure SetOnSelectionChange(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure Paint; override;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure SetDrawRules(const Value: boolean);
    property Active: boolean read GetActive write SetActive;
    property Canvas;
    property Surface: TJvDesignSurface read FSurface;
  published
    property DrawRules: boolean read FDrawRules write SetDrawRules default True;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnGetAddClass: TJvDesignGetAddClassEvent
      read GetOnGetAddClass write SetOnGetAddClass;
    property OnSelectionChange: TNotifyEvent
      read GetOnSelectionChange write SetOnSelectionChange;
    property align;
    property BevelInner;
    property BevelOuter;
    property Caption;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jvcl.svn.sourceforge.net/svnroot/jvcl/trunk/jvcl/run/JvDesignSurface.pas $';
    Revision: '$Revision: 12931 $';
    Date: '$Date: 2010-11-28 15:36:50 +0200 (Κυρ, 28 Νοε 2010) $';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Clipbrd,
  Types, //CV
  JvDesignUtils, JvDesignClip, JvDesignImp, JvResources, JvTypes, DBCtrls;

type
  TWinControlAccess = class(TWinControl);

  TPaintRectData = class
  public
    Control: TControl;
    ClientRect: TRect;
    CtrlRect: TRect;
    PaintRect: TRect;
    FieldName: String;
  end;

  { TPaintRectList }

  TPaintRectList = class(TList)
  private
    FTopParent: TWinControl;
    FTopParentPos: TPoint;
    function GetAboveControl(A, B: TControl): TControl;
    function GetRects(Index: Integer): TPaintRectData;
    procedure InnerAddRect(AControl: TControl; AClientRect, APaintRect: TRect; const AFieldName: String);
  public
    constructor Create(ATopParent: TWinControl);
    procedure AddRect(AControl: TControl);
    procedure Clear; override;
    procedure DeleteRect(PRD: TPaintRectData);
    property Rects[Index: Integer]: TPaintRectData read GetRects; default;
  end;


function GetPropStr(C: TComponent; const PropName: String): String;
var
  pInf: PPropInfo;
begin
  Result := '';
  pInf := GetPropInfo(C, PropName);
  if pInf <> nil then
    Result := GetStrProp(C, pInf);
end;


{function GetAboveControl(A, B: TControl): TControl;
var
  Ai, Bi: Integer;
begin
  if A.Parent = B.Parent then
  begin
    Ai := A.Parent.GetControlIndex(A);
    Bi := B.Parent.GetControlIndex(B);
    if Ai > Bi then Result := A
    else Result := B;
  end
  else
    Result := GetAboveControl(A.Parent, B.Parent);
end;  }

// TR:
//     1 2 3
//     1 0 3
//     1 4 3
// 0 - это SR
procedure SplitPaintRect(TR, SR: TRect; out R1, R2, R3, R4: TRect);
begin
  // 1
  R1 := Rect(TR.Left, TR.Top, SR.Left, TR.Bottom);
  // 2
  R2 := Rect(SR.Left, TR.Top, SR.Right, SR.Top);
  // 3
  R3 := Rect(SR.Right, TR.Top, TR.Right, SR.Bottom);
  // 4
  R4 := Rect(SR.Left, SR.Bottom, SR.Right, TR.Bottom);
end;

{ TPaintRectList }

function TPaintRectList.GetAboveControl(A, B: TControl): TControl;
var
  PL, PL2: array [0..20] of Integer;
  P: TWinControl;
  i, m, n: Integer;
begin
  if A.Parent = B.Parent then
  begin
    if A.Parent.GetControlIndex(A) > B.Parent.GetControlIndex(B) then
    begin
      if A is TWinControl then Exit(A)
      else Exit(B);
    end
    else
    begin
      if B is TWinControl then Exit(B)
      else Exit(A);
    end;
  end;

  PL[0] := A.Parent.GetControlIndex(A);
  P := A.Parent;
  for i := 1 to 20 do
    if P <> FTopParent then
    begin
      PL[i] := P.Parent.GetControlIndex(P);
      P := P.Parent;
    end
    else
    begin
      m := i-1;
      Break;
    end;

  PL2[0] := B.Parent.GetControlIndex(B);
  P := B.Parent;
  for i := 1 to 20 do
    if P <> FTopParent then
    begin
      PL2[i] := P.Parent.GetControlIndex(P);
      P := P.Parent;
    end
    else
    begin
      n := i-1;
      Break;
    end;

  Result := A;
  while True do
  begin
    if PL[m] > PL2[n] then Exit(A)
    else if PL[m] < PL2[n] then Exit(B);
    Dec(m);
    Dec(n);
    if n < 0 then Exit(A)
    else if m < 0 then Exit(B);
  end;
end;

function TPaintRectList.GetRects(Index: Integer): TPaintRectData;
begin
  Result := TPaintRectData(Items[Index]);
end;

procedure TPaintRectList.InnerAddRect(AControl: TControl; AClientRect,
  APaintRect: TRect; const AFieldName: String);
var
  i: Integer;
  SR, R1, R2, R3, R4: TRect;
  PRD: TPaintRectData;
begin
  for i := Count - 1 downto 0 do
  begin
    PRD := Rects[i];
    SR := APaintRect.Intersect(APaintRect, PRD.PaintRect);

    if not SR.IsEmpty then
    begin
      if (AControl.Parent = PRD.Control) or (PRD.Control.Parent = AControl) then
      begin
        APaintRect := SR;
        Break;
      end
      else if GetAboveControl(AControl, PRD.Control) = AControl then
      begin
        SplitPaintRect(PRD.PaintRect, SR, R1, R2, R3, R4);

        Remove(PRD);

        if not R1.IsEmpty then
          InnerAddRect(PRD.Control, PRD.ClientRect, R1, PRD.FieldName);
        if not R2.IsEmpty then
          InnerAddRect(PRD.Control, PRD.ClientRect, R2, PRD.FieldName);
        if not R3.IsEmpty then
          InnerAddRect(PRD.Control, PRD.ClientRect, R3, PRD.FieldName);
        if not R4.IsEmpty then
          InnerAddRect(PRD.Control, PRD.ClientRect, R4, PRD.FieldName);

        PRD.Free;
        InnerAddRect(AControl, AClientRect, APaintRect, AFieldName);
      end
      else
      begin
        SplitPaintRect(APaintRect, SR, R1, R2, R3, R4);

        if not R1.IsEmpty then
          InnerAddRect(AControl, AClientRect, R1, AFieldName);
        if not R2.IsEmpty then
          InnerAddRect(AControl, AClientRect, R2, AFieldName);
        if not R3.IsEmpty then
          InnerAddRect(AControl, AClientRect, R3, AFieldName);
        if not R4.IsEmpty then
          InnerAddRect(AControl, AClientRect, R4, AFieldName);
      end;

      Exit;
    end;
  end;

  PRD := TPaintRectData.Create;
  PRD.Control := AControl;
  PRD.ClientRect := AClientRect;
  PRD.PaintRect := APaintRect;
  PRD.PaintRect.Intersect(AClientRect);
  PRD.FieldName := AFieldName;
  Add(PRD);
end;

constructor TPaintRectList.Create(ATopParent: TWinControl);
begin
  inherited Create;
  FTopParent := ATopParent;
  FTopParentPos := ATopParent.ClientToScreen(Point(0, 0));
end;

procedure TPaintRectList.AddRect(AControl: TControl);
var
  CtrlPos: TPoint;
  CtrlR, ClientR, ParentClientR: TRect;
  FieldName: String;
begin
  CtrlPos := AControl.Parent.ClientToScreen(Point(AControl.Left, AControl.Top));
  CtrlPos := CtrlPos.Subtract(FTopParentPos);
  CtrlR := Rect(CtrlPos.X, CtrlPos.Y, CtrlPos.X + AControl.Width,
    CtrlPos.Y + AControl.Height);

  CtrlPos := AControl.ClientToScreen(Point(0, 0));
  CtrlPos := CtrlPos.Subtract(FTopParentPos);
  ClientR := Rect(CtrlPos.X, CtrlPos.Y, CtrlPos.X + AControl.ClientWidth,
    CtrlPos.Y + AControl.ClientHeight);
  FieldName := GetPropStr(AControl, 'FieldName');
  if AControl is TCustomCheckBox then FieldName := '';

  CtrlPos := AControl.Parent.ClientToScreen(Point(0, 0));
  CtrlPos := CtrlPos.Subtract(FTopParentPos);
  ParentClientR := Rect(CtrlPos.X, CtrlPos.Y, CtrlPos.X + AControl.Parent.ClientWidth,
    CtrlPos.Y + AControl.Parent.ClientHeight);
  CtrlR.InterSect(ParentClientR);

  InnerAddRect(AControl, ClientR, CtrlR, FieldName);
end;

procedure TPaintRectList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Rects[i].Free;
  inherited Clear;
end;

procedure TPaintRectList.DeleteRect(PRD: TPaintRectData);
begin
  Remove(PRD);
  PRD.Free;
end;

//=== { TJvDesignCustomMessenger } ===========================================

constructor TJvDesignCustomMessenger.Create;
begin

end;

destructor TJvDesignCustomMessenger.Destroy;
begin

end;

procedure TJvDesignCustomMessenger.Clear;
begin

end;

procedure TJvDesignCustomMessenger.DesignComponent(AComponent: TComponent;
  ADesigning: boolean);
begin

end;

procedure TJvDesignCustomMessenger.DesignChildren(AContainer: TWinControl;
  ADesigning: boolean);
var
  I: integer;
  C: TControl;
begin
{  for I := 0 to AContainer.ControlCount - 1 do
  begin
    C := AContainer.Controls[I];
    DesignComponent(C, ADesigning);
    if C is TWinControl then
      DesignChildren(TWinControl(C), ADesigning);
  end; }
  DesignComponent(AContainer, ADesigning);
  //for I := 0 to AContainer.Controls[0].ComponentCount - 1 do
  //  DesignComponent(AContainer.Controls[0].Components[I], ADesigning);
end;

procedure TJvDesignCustomMessenger.SetContainer(AValue: TWinControl);
begin
  FContainer := AValue;
end;

function TJvDesignCustomMessenger.IsDesignMessage(ASender: TControl;
  var AMessage: TLMessage): boolean;

  function MousePoint: TPoint;
  begin
    with TLMMouse(AMessage) do
      MousePoint := Point(XPos, YPos);

    Result := DesignClientToParent(Result, ASender, Container{$ifdef linux}.Parent{$endif});

  end;

begin
  if not Assigned(FOnDesignMessage) then
    Result := False
  else
  if AMessage.Msg = CN_NOTIFY then
  begin
    Result := False;
    exit;
  end;

  case AMessage.Msg of
    // 7bit - skip LM_CLOSEQUERY //
      {LM_LCL..LM_EXIT, LM_DRAGSTART..LM_INTERFACELAST : result:=true;

      CM_BASE..CM_APPSHOWMENUGLYPHCHANGED:
      begin
       // result:=true;
        case AMessage.Msg of
          { 7bit чтобы форма закрывалась} CM_VISIBLECHANGED, {} CM_SHOWINGCHANGED,CM_HITTEST: result:=false;
        end;

      end;

      $bd11: result:=true;



      LM_MOVE: result:=false;
     // LM_NOTIFY: result:=true; //?
      LM_DESTROY: result:=false;

      LM_CAPTURECHANGED,LM_SYSCOMMAND: result:=false;
     // LM_CONTEXTMENU: result:=true;

      LM_SETFOCUS,LM_SIZE: result:=false;
      LM_ACTIVATE,LM_SHOWWINDOW,LM_KILLFOCUS,LM_SETCURSOR: result:=false;
      LM_NCMOUSEMOVE..LM_NCLBUTTONDBLCLK: result:=true;   }

    //CM_ENTER, CM_EXIT, LM_SELCHANGE: Result := True;      // 7bit для Linux
    LM_MOUSEFIRST..LM_MOUSELAST:
      Result := FOnDesignMessage(ASender, AMessage, MousePoint);
    LM_KEYDOWN..LM_KEYUP, LM_PAINT, LM_ERASEBKGND, LM_WINDOWPOSCHANGED,
    CN_KEYDOWN..CN_KEYUP:
      Result := FOnDesignMessage(ASender, AMessage, Point(0, 0));
    else
      Result := False;
  end;

end;

//=== { TJvDesignMessageHook } ===============================================

constructor TJvDesignMessageHook.Create(AUser: TJvDesignCustomMessenger;
  AClient: TWinControl);
begin
  FUser := AUser;
  FClient := AClient;
  FOldProc := FClient.WindowProc;
  FClient.WindowProc := @HookProc;
end;

destructor TJvDesignMessageHook.Destroy;
begin
  Unhook;
  inherited Destroy;
end;

procedure TJvDesignMessageHook.Unhook;
begin
  FClient.WindowProc := FOldProc;
end;

procedure TJvDesignMessageHook.HookProc(var AMessage: TLMessage);
begin
  if not FUser.IsDesignMessage(FClient, AMessage) then
    FOldProc(AMessage);
end;

//=== { TJvDesignCustomController } ==========================================

constructor TJvDesignCustomController.Create(ASurface: TJvDesignSurface);
begin
  FSurface := ASurface;
end;

procedure TJvDesignCustomController.Paint(Control: TControl; DC: HDC);
begin

end;

//CV
function KeyboardStateToShiftState(const KeyboardState: TKeyboardState): TShiftState;
begin
  Result := [];
  if KeyboardState[VK_SHIFT] and $80 <> 0 then
    Include(Result, ssShift);
  if KeyboardState[VK_CONTROL] and $80 <> 0 then
    Include(Result, ssCtrl);
  if KeyboardState[VK_MENU] and $80 <> 0 then
    Include(Result, ssAlt);
  if KeyboardState[VK_LBUTTON] and $80 <> 0 then
    Include(Result, ssLeft);
  if KeyboardState[VK_RBUTTON] and $80 <> 0 then
    Include(Result, ssRight);
  if KeyboardState[VK_MBUTTON] and $80 <> 0 then
    Include(Result, ssMiddle);
end;

{function TJvDesignCustomController.GetShift: TShiftState;
// obones: For C5/D5 compatibility, we must use a local variable
// as KeyboardStateToShiftState with no parameters was introduced
// in D6/C6
var
  KeyState: TKeyBoardState;
begin
  //CV GetKeyboardState(KeyState);
  //CV Result := KeyboardStateToShiftState(KeyState);
end;
 }
//=== { TJvDesignCustomSelector } ============================================

// 7bit
function TJvDesignCustomSelector.GetSelectionRect: TRect;
var
  i: Integer;
  C: TControl;
  R: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if Surface.Count = 0 then Exit;
  Result := Surface.Selection[0].BoundsRect;

  for i := 1 to Surface.Count - 1 do
  begin
    C := Surface.Selection[i];
    R := C.BoundsRect;
    if R.Left < Result.Left then Result.Left := R.Left;
    if R.Top < Result.Left then Result.Top := R.Top;
    if R.Right > Result.Right then Result.Right := R.Right;
    if R.Bottom > Result.Bottom then Result.Bottom := R.Bottom;
  end;
end;
//

constructor TJvDesignCustomSelector.Create(ASurface: TJvDesignSurface);
begin
  inherited Create(nil);
  FSurface := ASurface;
end;

destructor TJvDesignCustomSelector.Destroy;
begin
  inherited Destroy;
end;

procedure TJvDesignCustomSelector.ToggleSelection(AValue: TControl);
begin
  if IsSelected(AValue) then
    RemoveFromSelection(AValue)
  else
    AddToSelection(AValue);
end;

//=== { TJvDesignSurface } ===================================================

constructor TJvDesignSurface.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessengerClass := TJvDesignDesignerMessenger;
  FControllerClass := TJvDesignController;
  FSelectorClass := TJvDesignSelector;
  FDesignCanvas := TCanvas.Create;
  FShowGrid := True;
  GridSizeX := 8;
  GridSizeY := 8;
  FGridColor := clBlack;
end;

destructor TJvDesignSurface.Destroy;
begin
  FDesignCanvas.Free;
  FContainerHook.Free;
  Messenger.Free;
  Controller.Free;
  Selector.Free;
  inherited Destroy;
end;

procedure TJvDesignSurface.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvDesignSurface.SetContainer(AValue: TWinControl);
begin
  FContainer := AValue;
end;

procedure TJvDesignSurface.NeedContainer;
begin
  if (Container = nil) and (Owner is TWinControl) then
    Container := TWinControl(Owner);
  if Container = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Container']);
end;

procedure TJvDesignSurface.NeedController;
begin
  if (Controller = nil) and (ControllerClass <> nil) then
    FController := ControllerClass.Create(Self);
  if Controller = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Controller']);
end;

procedure TJvDesignSurface.MessengerOnChange(Sender: TObject);
begin
  change;
end;

procedure TJvDesignSurface.BeginPaint(DC: HDC);
begin
  FOldState := SaveDC(DC);
  FDesignCanvas.Handle := DC;
end;

procedure TJvDesignSurface.EndPaint;
begin
  RestoreDC(FDesignCanvas.Handle, FOldState);
  FDesignCanvas.Handle := 0;
end;

procedure TJvDesignSurface.PaintFieldNames(AContainer: TWinControl);
var
  i: Integer;
  C: TComponent;
  PaintRects: TPaintRectList;
  Canv: TCanvas;
  TS: TTextStyle;
  PRD: TPaintRectData;
  PR, CR: TRect;
  Space: Integer;
begin
  PaintRects := TPaintRectList.Create(AContainer);
  for i := 0 to AContainer.ComponentCount - 1 do
  begin
    C := AContainer.Components[i];
    if (C is TControl) and TControl(C).IsVisible then
      PaintRects.AddRect(TControl(C));
  end;
  Space := AContainer.Scale96ToScreen(2);

  Canv := FDesignCanvas;
  FillChar(TS, SizeOf(TS), 0);
  TS.SingleLine := True;
  TS.Clipping := True;

  for i := 0 to PaintRects.Count - 1 do
  begin
    PRD := PaintRects[i];
    if PRD.FieldName = '' then Continue;
    PR := PRD.PaintRect;
    CR := PRD.ClientRect;

    Canv.Font.Assign(PRD.Control.Font);
    Canv.TextRect(PR, CR.Left + Space, CR.Top + Space, PRD.FieldName, TS);
  end;

  PaintRects.Free;
end;

// 7bit begin
// взято из designer.pp

procedure TJvDesignSurface.PaintClientGrid(AWinControl: TWinControl; DC: HDC);
var
  Clip: integer;
  CtrlCount: integer;
  i: integer;
  //CurControl: TControl;
begin
  BeginPaint(DC);
  try
    // exclude all child control areas
    CtrlCount := AWinControl.ControlCount;
    for i := 0 to CtrlCount - 1 do
    begin
      with AWinControl.Controls[I] do
      begin
        if (Visible or ((csDesigning in ComponentState) and
          not (csNoDesignVisible in ControlStyle))) then
        begin
          Clip := ExcludeClipRect(DC, Left, Top, Left + Width, Top + Height);
          if Clip = NullRegion then
            exit;
        end;
      end;
    end;

    // paint points
    FDesignCanvas.Pen.Color := FGridColor;
    FDesignCanvas.Pen.Width := 1;
    FDesignCanvas.Pen.Style := psSolid;
    FDesignCanvas.Pen.Mode := pmCopy;
    DrawGrid(FDesignCanvas.Handle, TWinControlAccess(AWinControl).GetLogicalClientRect,
      FGridSizeX, FGridSizeY);

    {if ShowBorderSpacing then
    begin
      aDDC.Canvas.Brush.Color := clRed;
      for i := 0 to Count - 1 do
      begin
        CurControl := AWinControl.Controls[i];
        if csNoDesignSelectable in CurControl.ControlStyle then
          Continue;
        aDDC.Canvas.FrameRect(
          CurControl.Left-CurControl.BorderSpacing.GetSideSpace(akLeft),
          CurControl.Top-CurControl.BorderSpacing.GetSideSpace(akTop),
          CurControl.Left+CurControl.Width+CurControl.BorderSpacing.GetSideSpace(akRight),
          CurControl.Top+CurControl.Height+CurControl.BorderSpacing.GetSideSpace(akBottom)
          );
      end;
    end;}
  finally
    EndPaint;
  end;
end;

function TJvDesignSurface.PaintControls(Sender: TControl; TheMessage: TLMPaint
  ): Boolean;
begin
  Result := True;
  Sender.Dispatch(TheMessage);

  if FShowGrid and (Sender is TWinControl) and (csAcceptsControls in Sender.ControlStyle) then
  begin
    PaintClientGrid(TWinControl(Sender), TheMessage.DC);
  end;

  {$ifdef linux}
  if IsDesignerDC(FContainer.Parent.Handle, TheMessage.DC) then
  try
    BeginPaint(TheMessage.DC);
    Controller.Paint(FContainer.Parent, TheMessage.DC);
    PaintFieldNames(FContainer);
  finally
    EndPaint;
  end;
  {$endif}
end;

procedure TJvDesignSurface.SetGridColor(AValue: TColor);
begin
  if FGridColor=AValue then Exit;
  FGridColor:=AValue;
  if Active then Container.Invalidate;
end;

procedure TJvDesignSurface.SetGridSizeX(AValue: Integer);
begin
  if FGridSizeX=AValue then Exit;
  FGridSizeX:=AValue;
  if not Odd(AValue) then
    FStepX:=AValue div 2
  else
    FStepX:=AValue;
  if Active then Container.Invalidate;
end;

procedure TJvDesignSurface.SetGridSizeY(AValue: Integer);
begin
  if FGridSizeY=AValue then Exit;
  FGridSizeY:=AValue;
  if not Odd(AValue) then
    FStepY:=AValue div 2
  else
    FStepY:=AValue;
  if Active then Container.Invalidate;
end;

// 7bit end

procedure TJvDesignSurface.NeedMessenger;
begin
  if (Messenger = nil) and (MessengerClass <> nil) then
  begin
    FMessenger := MessengerClass.Create;
    Messenger.OnDesignMessage := @IsDesignMessage;
    Messenger.OnChange := @MessengerOnChange;
  end;
  if Messenger = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Messenger']);
end;

procedure TJvDesignSurface.NeedSelector;
begin
  if (Selector = nil) and (SelectorClass <> nil) then
    FSelector := SelectorClass.Create(Self);
  if Selector = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Selector']);
end;

procedure TJvDesignSurface.SetActive(AValue: boolean);

  procedure Activate;
  begin
    NeedContainer;
    NeedController;
    NeedSelector;
    NeedMessenger;
    Messenger.Container := Container;
    FContainerHook := TJvDesignMessageHook.Create(Messenger, Container);
  end;

  procedure Deactivate;
  begin
    FreeAndNil(FContainerHook);
    if selector <> nil then
      Selector.ClearSelection;
    FreeAndNil(FMessenger);
  end;

begin
  if FActive <> AValue then
  begin
    if AValue then
      Activate
    else
      Deactivate;
    FActive := AValue;
    SelectionChange;
    if Assigned(Container) then
      Container.Invalidate;
  end;
end;

procedure TJvDesignSurface.UpdateDesigner;
begin
  Selector.Update;
end;

procedure TJvDesignSurface.DoChangingComponents;
begin
  if FOnChangingComponents <> nil then
    FOnChangingComponents(Self);
end;

function TJvDesignSurface.GetCount: integer;
begin
  Result := Selector.Count;
end;

function TJvDesignSurface.GetSelection(AIndex: integer): TControl;
begin
  Result := Selector.Selection[AIndex];
end;

procedure TJvDesignSurface.SetSelection(AIndex: integer; AValue: TControl);
begin
  Selector.Selection[AIndex] := AValue;
end;

procedure TJvDesignSurface.ClearSelection;
begin
  Selector.ClearSelection;
end;

procedure TJvDesignSurface.SelectionChange;
begin
  if not (csDestroying in ComponentState) and Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

function TJvDesignSurface.GetSelected: TJvDesignObjectArray;
var
  I: integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Selector.Selection[I];
end;

procedure TJvDesignSurface.SetSelected(const AValue: array of TObject);
var
  I: integer;
begin
  ClearSelection;
  for I := 0 to Length(AValue) - 1 do
    if AValue[I] is TControl then
      Selector.AddToSelection(TControl(AValue[I]));
end;

procedure TJvDesignSurface.Select(AControl: TControl);
begin
  ClearSelection;
  if AControl <> nil then
    Selector.AddToSelection(AControl);
end;

function TJvDesignSurface.FindControl(AX, AY: integer): TControl;
var
  C, C0: TControl;
  P, Offset: TPoint;
  r: trect;
begin
  P := Point(AX, AY);
  //C := Container.ControlAtPos(P, True, True);
  c := Container.ControlAtPos(p, [capfAllowDisabled, capfAllowWinControls]);

  while (C <> nil) and (C is TWinControl) do
  begin
    {$ifdef windows}
    if GetLCLClientBoundsOffset(c, R) then
    begin
      Dec(p.x, R.Left);
      Dec(p.Y, R.Top);
    end;
    {$else}
    Offset := GetControlClientOffset(C);
    Dec(P.X, Offset.X);
    Dec(P.Y, Offset.Y);
    {$endif}

    Dec(P.X, C.Left);
    Dec(P.Y, C.Top);
    C0 := TWinControl(C).ControlAtPos(P, [capfAllowDisabled, capfAllowWinControls]);
    if (C0 = nil) or (C0.Owner <> C.Owner) then
      Break;
    C := C0;
  end;
  if C = nil then
    C := Container;
  Result := Selector.GetClientControl(C);
end;

function TJvDesignSurface.GetSelectedContainer: TWinControl;
begin
  if Count <> 1 then
    Result := Container
  else
  if (Selection[0] is TWinControl) and (csAcceptsControls in
    Selection[0].ControlStyle) then
    Result := TWinControl(Selection[0])
  else
    Result := Selection[0].Parent;
end;

function TJvDesignSurface.ContainerToSelectedContainer(const APt: TPoint): TPoint;
var
  C: TControl;
  r: trect;
  Offset: TPoint;
begin
  Result := APt;
  C := SelectedContainer;
  while (C <> Container) and (C <> nil) do
  begin
    {$ifdef windows}
    if (c is Twincontrol) then
    begin
      if GetLCLClientBoundsOffset(C, R) then
      begin
        Dec(Result.x, R.Left);
        Dec(Result.Y, R.Top);
      end;
    end;
    {$else}
    Offset := GetControlClientOffset(C);
    Dec(Result.X, Offset.X);
    Dec(Result.Y, Offset.Y);
    {$endif}
    Dec(Result.X, C.Left);
    Dec(Result.Y, C.Top);
    C := C.Parent;
  end;
end;

function TJvDesignSurface.GetAddBounds: TRect;
begin
  with Result, Controller do
  begin
    TopLeft := ContainerToSelectedContainer(DragRect.TopLeft);
    BottomRight := ContainerToSelectedContainer(DragRect.BottomRight);
  end;
end;

procedure TJvDesignSurface.GetAddClass;
begin
  if Assigned(FOnGetAddClass) then
    FOnGetAddClass(Self, FAddClass);
end;

procedure TJvDesignSurface.AddComponent;
var
  CC: TComponentClass;
  C: TComponent;
  CO: TControl;

  function GetBounds: TRect;
  begin
    Result := GetAddBounds;
    if DesignRectWidth(Result) = 0 then
      Result.Right := Result.Left + CO.Width;
    if DesignRectHeight(Result) = 0 then
      Result.Bottom := Result.Top + CO.Height;
  end;

begin
  CC := TComponentClass(GetClass(AddClass));
  if (CC <> nil) and (SelectedContainer <> nil) then
  begin
    //C := CC.Create(Owner);
    //C.Name := DesignUniqueName(Owner, AddClass);
    C := CC.Create(Container);
    // 7bit - имя компоненту присваиваем в обработчике OnAddComponent
    if FOnGetComponentName <> nil then
      FOnGetComponentName(Self, C);
    // C.Name := DesignUniqueName(Container, AddClass);
    if C is TControl then
    begin
      CO := TControl(C);
      CO.Parent := SelectedContainer;
      CO.BoundsRect := GetBounds;
      //CO.PopupMenu := container.PopupMenu;

      Select(CO);
    end;
    Messenger.DesignComponent(C, Active);
    // 7bit //
    if FOnAddComponent <> nil then
      FOnAddComponent(Self, C);

    SelectionChange;
    Change;
    AddClass := '';
  end;
end;

procedure TJvDesignSurface.NudgeComponents(ANudgeLeft, ANudgeTop: integer);
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    with Selection[I] do
    begin
      Left := Left + ANudgeLeft;
      Top := Top + ANudgeTop;
    end;
  Change;
end;

procedure TJvDesignSurface.GrowComponents(AGrowWidth, AGrowHeight: integer);
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    with Selection[I] do
    begin
      Width := DesignMax(1, Width + AGrowWidth);
      Height := DesignMax(1, Height + AGrowHeight);
    end;
  Change;
end;

procedure DeleteComponent(c: TObject);
var
  i: integer;
begin
  if (c is TWinControl) then
  begin
    while TWinControl(c).ControlCount > 0 do
      deleteComponent((c as TWinControl).Controls[0]);
  end;

  {if (c is TComponent) then
  begin
    //delete the possible children it might have
    while (c as tcomponent).ComponentCount>0 do
      deleteComponent((c as tcomponent).Components[0]);
  end; }

  c.Free;
end;

procedure TJvDesignSurface.DeleteComponents;
var
  I: integer;
  z: array of TObject;
begin
  setlength(z, Count);
  for i := 0 to Count - 1 do
    z[i] := selection[i];

  ClearSelection;
  SelectionChange;

  for i := 0 to length(z) - 1 do
  begin
    // 7bit //
    if FOnDeleteComponent <> nil then
      FOnDeleteComponent(Self, TComponent(z[I]));

    deleteComponent(z[i]); //.free;
  end;

{  if Count > 0 then
  begin
    for I := 0 to Count - 1 do
    begin
      z:=selection[i];
      z.free;
//      selection[i].Caption:='del me';
      //Selection[I].Free;
    end;}


  Change;

end;

procedure TJvDesignSurface.CopyComponents;
var
  I: integer;
begin
  with TJvDesignComponentClipboard.Create(Container) do
    try
      OpenWrite;
      try
        for I := 0 to Count - 1 do
          SetComponent(Selection[I]);
      finally
        CloseWrite;
      end;
    finally
      Free;
    end;
end;

procedure TJvDesignSurface.CutComponents;
begin
  CopyComponents;
  DeleteComponents;
end;

procedure TJvDesignSurface.PasteComponents;
var
  CO: TControl;
  C: TComponent;
  P: TWinControl;

  procedure KeepInParent;
  begin
    with P do
    begin
      if CO.Left > ClientWidth then
        CO.Left := ClientWidth - CO.Width;
      if CO.Top > ClientHeight then
        CO.Top := ClientHeight - CO.Height;
    end;
  end;

  // 7bit //
  {procedure ChangeOwner(Ctrl: TWinControl);
  var
    i: Integer;
    C: TComponent;
  begin
    for i := Ctrl.ComponentCount - 1 downto 0 do
    begin
      C := Ctrl.Components[i];
      if (C is TSpeedButton) or (C is TDBLookup) then Continue;
//      if (not (C is TSpeedButton)) and (not (C is TDBLookup)) then
  //    begin
        Ctrl.RemoveComponent(C);
        C.Name := DesignUniqueName(Owner, C.ClassName);
        Container.InsertComponent(C);
        Messenger.DesignComponent(C, Active);
//      end;
    end;
  end;  }



  procedure PasteComponent;
  begin
    // 7bit C.Name := DesignUniqueName(Container, C.ClassName);
    if FOnGetComponentName <> nil then
      FOnGetComponentName(Self, C);

    //p.InsertComponent(C);
    // 7bit //
    Container.InsertComponent(C);

    if C is TControl then
    begin
      // 7bit //
      Messenger.DesignComponent(C, Active);

      CO := TControl(C);
      //KeepInParent;
      CO.Parent := P;
      Selector.AddToSelection(CO);
      // 7bit //
      if FOnAddComponent <> nil then
        FOnAddComponent(Self, C);

    end;

  end;

begin
  with TJvDesignComponentClipboard.Create(Container) do
    try
      OpenRead;
      try
        C := GetComponent;
        if (C <> nil) then
        begin
          P := SelectedContainer;
          ClearSelection;
          repeat
            PasteComponent;
            // 7bit //
          {if C is TWinControl then
            ChangeOwner(TWinControl(C));}

            C := GetComponent;
          until C = nil;
          SelectionChange;
          Change;
        end;
      finally
        CloseRead;
      end;
    finally
      Free;
    end;

  // 7bit //
  //active:=false;
  //active:=true;

end;

procedure TJvDesignSurface.SelectParent;
begin
  if Count > 0 then
  begin
    if Selection[0] <> Container then
    begin
      Select(Selection[0].Parent);
      // 7bit //
      SelectionChange;
    end;
  end;
end;

{
procedure TJvDesignSurface.PaintContainerBkgnd(ADC: HDC);
var
  r: TRect;
  canvas: TCanvas;
begin
  if DrawGrid then
  begin
    canvas := TCanvas.Create;
    try
      SelectClipRgn(ADC, 0);
      canvas.Handle := ADC;
      canvas.Brush.Color := Container.Brush.Color;
      r := canvas.ClipRect;
      if Assigned(FOnOwnerDrawGrid) then
        FOnOwnerDrawGrid(Self, canvas, Container.ClientRect)
      else begin
        canvas.FillRect(Container.ClientRect);
        DesignPaintRules(canvas, Container.ClientRect);
      end;
    finally
      canvas.Free;
    end;
  end;
end;
}

type
  TAccessWinControl = class(TWinControl);

function TJvDesignSurface.IsDesignMessage(ASender: TControl;
  var AMsg: TLMessage; const APt: TPoint): boolean;

  function VirtKey: cardinal;
  begin
    Result := AMsg.WParam and $ffff;
  end;

{
  function HandlePaint: Boolean;
  begin
    Result := False;
  end;

  function HandleEraseBkgnd: Boolean;
  begin
    if (ASender <> Container) then
      Result := False
    else begin
       PaintContainerBkgnd(TWMPaint(AMsg).DC);
       AMsg.Result := 1;
       Result := True;
    end;
  end;
}
var
  PosChangedHandle: HWND;
  I: integer;
  Control: TAccessWinControl;

  p: tpoint;
  c: tcontrol;
begin
  if not Active then
    Result := False
  else
    case AMsg.Msg of
{
      WM_ERASEBKGND:
        Result := HandleEraseBkgnd;
      WM_PAINT:
        Result := HandlePaint;
}
      $bd01:
        //, LM_LBUTTONDBLCLK:
        Result := True;

      LM_MOUSEENTER: Result := True;
      LM_LBUTTONDBLCLK: Result := True;

      LM_RBUTTONDOWN:
      begin
        Result := Controller.MouseDown(mbRight, APt.X, APt.Y, TLMMOUSE(AMsg));

        if Result and (FPopupMenu <> nil) then
        begin
          FPopupMenu.Popup;

          {p := container.ClientToScreen(APt);

          FPopupMenu.PopupComponent := TComponent(FindControl(Apt.x, apt.y));
          FPopupMenu.PopUp(p.x, p.y); }
        end;

        //  result:=true;
      end;
      LM_RBUTTONUP:
      begin
        Result := Controller.MouseUp(mbRight, APt.X, APt.Y, TLMMouse(aMsg));
        // result:=true;
      end;


      LM_LBUTTONDOWN:
      begin
        Result := Controller.MouseDown(mbLeft, APt.X, APt.Y, TLMMOUSE(AMsg));
        Result := True;
      end;
      LM_LBUTTONUP:
      begin
        Result := Controller.MouseUp(mbLeft, APt.X, APt.Y, TLMMouse(aMsg));
        Result := True;
      end;

      LM_MOUSEMOVE:
      begin
        Result := Controller.MouseMove(APt.X, APt.Y, TLMMouse(aMsg));
        Result := True;
      end;

      LM_KEYDOWN{, CN_KEYDOWN}:
      begin
        Result := Controller.KeyDown(VirtKey);
        Result := True;
      end;

      LM_KEYUP:
      begin
        Result := Controller.KeyUp(VirtKey);
        Result := True;
      end;

     {LM_WINDOWPOSCHANGED:
        begin
          if AMsg.lParam <> 0 then
          begin
            //CVPosChangedHandle := PWindowPos(AMsg.lParam).hwnd;
            PosChangedHandle := PWindowPos(AMsg.lParam)^.hwnd;

            // If the window that has changed is a control owned by our container
            // then we must update the designer. This allows to programatically
            // change the location of a control while making the designer handles
            // follow it around (Mantis 4693).
            // For this to work properly, we MUST update the bounds of the
            // control before calling UpdateDesigner because the VCL has not yet
            // processed the WM_WINDOWPOSCHANGED message when this code executes.
            // If we did not, the designer would use the previous position of the
            // control to display the handles.
            // Additionnaly, we must not work with controls that don't have their
            // handle allocated. In some instances, creating the handle may trigger
            // a second WM_WINDOWPOSCHANGED message, thus leading to an infinite
            // loop and a crash (Mantis 5225)
            for I := 0 to Container.ComponentCount - 1 do
            begin
              if Container.Components[I] is TWinControl then
              begin
                Control := TAccessWinControl(Container.Components[I]);
                if Control.HandleAllocated and (PosChangedHandle = Control.Handle) then
                begin
                  if not (csDestroyingHandle in Control.ControlState) then
                   //$IFDEF DELPHI10_UP
                    //CV Control.UpdateBounds;
                    //$ELSE
                    Control.Dispatch(AMsg);
                    //$ENDIF DELPHI10_UP

                  UpdateDesigner;
                end;
              end;
            end;//for
          end;

          // Must return False to let the VCL do its own work of placing the window
          Result := False;
        end;   }

      LM_WINDOWPOSCHANGED, LM_ERASEBKGND: Result := False;
      LM_PAINT: Result := PaintControls(ASender, TLMPaint(AMsg));   // 7bit result:=false;
      {LM_RBUTTONDOWN,}LM_MBUTTONDOWN{,LM_RBUTTONUP}: Result := True;

      CN_KEYDOWN, CN_CHAR, CN_SYSKEYUP, CN_SYSKEYDOWN, CN_SYSCHAR: Result := True;

      else
        Result := False;
    end;

end;

function TJvDesignSurface.GetCursor(AX, AY: integer): TCursor;
begin
  // Using FindControl is inefficient.
  // All we really want to know is if Selected[0] contains (AX, AY)
  if (Count > 0) and (FindControl(AX, AY) = Selected[0]) then
    Result := Selector.GetCursor(AX, AY)
  else
    Result := crDefault;
end;

function TJvDesignSurface.GetHitHandle(AX, AY: integer): TJvDesignHandleId;
begin
  Result := Selector.GetHitHandle(AX, AY);
end;

procedure TJvDesignSurface.BeginUpdate;
begin
  Active := False;
  FUpdateOwner := Owner;
  Owner.RemoveComponent(Self);
end;

procedure TJvDesignSurface.EndUpdate;
begin
  FUpdateOwner.InsertComponent(Self);
  Active := True;
end;

procedure TJvDesignSurface.ReaderError(Reader: TReader; const Msg: string;
  var Handled: boolean);
begin
  Handled := True;
end;

function TJvDesignSurface.Clear: TJvDesignSurface;
begin
  BeginUpdate;
  try
    Container.DestroyComponents;
  finally
    EndUpdate;
  end;
  Result := Self;
end;

procedure TJvDesignSurface.SaveToStream(AStream: TStream);
begin
  BeginUpdate;
  try
    DesignSaveComponentToStream(Container, AStream);
  finally
    EndUpdate;
  end;
end;

function TJvDesignSurface.LoadFromStream(AStream: TStream): TJvDesignSurface;
var
  SavedName: string;
begin
  BeginUpdate;
  SavedName := Container.Name;
  try
    Container.DestroyComponents;
    DesignLoadComponentFromStream(Container, AStream, @ReaderError);
    Container.Name := SavedName;
  finally
    Container.Name := SavedName;
    EndUpdate;
  end;
  Result := Self;
end;

procedure TJvDesignSurface.SaveToFile(const AFileName: string);
begin
  BeginUpdate;
  try
    DesignSaveComponentToFile(Container, AFileName);
  finally
    EndUpdate;
  end;
end;

function TJvDesignSurface.LoadFromFile(const AFileName: string): TJvDesignSurface;
var
  SavedName: string;
begin
  BeginUpdate;
  SavedName := Container.Name;
  try
    Container.DestroyComponents;
    DesignLoadComponentFromFile(Container, AFileName, @ReaderError);
  finally
    Container.Name := SavedName;
    EndUpdate;
  end;
  Result := Self;
end;


procedure TJvDesignSurface.SetShowGrid(const Value: Boolean);
begin
  if FShowGrid = Value then Exit;
  FShowGrid := Value;
  if Active then
    Container.Invalidate;
end;


//=== { TJvDesignScrollBox } =================================================

procedure TJvDesignScrollBox.AutoScrollInView(AControl: TControl);
begin

end;

//=== { TJvDesignPanel } =====================================================

constructor TJvDesignPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrawRules := True;
  FSurface := TJvDesignSurface.Create(Self);
  Surface.Name := 'Surface';
  Surface.Container := Self;
end;

procedure TJvDesignPanel.SetDrawRules(const Value: boolean);
begin
  FDrawRules := Value;
  Invalidate;
end;

procedure TJvDesignPanel.Paint;
begin
  inherited Paint;
  if Surface.Active or (csDesigning in ComponentState) then
  begin
    if DrawRules then
      DesignPaintRules(Canvas, ClientRect);
    if Assigned(FOnPaint) then
      FOnPaint(Self);
  end;
end;

procedure TJvDesignPanel.Clear;
begin
  // DesignSurface property value is lost on clear.
  // Restore it with the value returned from Clear.
  FSurface := Surface.Clear;
end;

procedure TJvDesignPanel.SaveToStream(AStream: TStream);
begin
  Surface.SaveToStream(AStream);
end;

procedure TJvDesignPanel.LoadFromStream(AStream: TStream);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromStream.
  FSurface := Surface.LoadFromStream(AStream);
end;

procedure TJvDesignPanel.SaveToFile(const AFileName: string);
begin
  Surface.SaveToFile(AFileName);
end;

procedure TJvDesignPanel.LoadFromFile(const AFileName: string);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromFile.
  FSurface := Surface.LoadFromFile(AFileName);
end;

function TJvDesignPanel.GetActive: boolean;
begin
  Result := Surface.Active;
end;

function TJvDesignPanel.GetOnChange: TNotifyEvent;
begin
  Result := Surface.OnChange;
end;

function TJvDesignPanel.GetOnGetAddClass: TJvDesignGetAddClassEvent;
begin
  Result := Surface.OnGetAddClass;
end;

function TJvDesignPanel.GetOnSelectionChange: TNotifyEvent;
begin
  Result := Surface.OnSelectionChange;
end;

procedure TJvDesignPanel.SetActive(const Value: boolean);
begin
  Surface.Active := Value;
end;

procedure TJvDesignPanel.SetOnChange(const Value: TNotifyEvent);
begin
  Surface.OnChange := Value;
end;

procedure TJvDesignPanel.SetOnGetAddClass(const Value: TJvDesignGetAddClassEvent);
begin
  Surface.OnGetAddClass := Value;
end;

procedure TJvDesignPanel.SetOnSelectionChange(const Value: TNotifyEvent);
begin
  Surface.OnSelectionChange := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
