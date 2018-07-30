unit FlashCompat;

interface

uses Classes, Controls, SysUtils, Dialogs, Contnrs, StdCtrls, ExtCtrls, Utils,
  Menus, XMLDoc, XMLIntf, IdHttp, Variants, Graphics, ShellAPI, Messages,
  Windows;

type
//  String = String;
//  Boolean = Boolean;
  Number = Double;
  int = Integer;
  uint = Cardinal;

  RegExp = String;

  TTextFormat = TFont;
  TContextMenu = TPopupMenu;
  TContextMenuItem = TMenuItem;

  TXML = IXMLDocument;

  TURLRequest = class;
  
  TURLLoader = class
  private
    _HttpClient : TIdHTTP;
    _data : String;
    _OnCompleted: TNotifyEvent;
    function getData : String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(Request : TURLRequest);
    property OnCompleted : TNotifyEvent read _OnCompleted write _OnCompleted;
    property data : String read getData;
  end;

  TURLRequest = class
  Private
    _url : String;
  Public
    constructor Create(url : String);
    property url : String read _url;
  end;

  TFlashArray = class
  Private
    _ObjectList : TObjectList;
    _ColumnName : String;
    function GetObjectCount : Integer;
  Public
    constructor Create;
    destructor Destroy; override;
    property Count : Integer read getObjectCount;
    function GetObject(Ndx : Integer) : TObject;
    procedure SortOn(FieldName : String);
    procedure Sort(Compare: TListSortCompare);
    function Pop: TObject;
    procedure Push(AValue: TObject);
  end;

  TSprite = class(TShape)
  private
    function GetLeft: Integer;
    function GetTop: Integer;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    function GetCanvas: TCanvas;
  public
    ButtonMode : Boolean; // Dummy variable
    MouseChildren : Boolean; // DUmmy variable
    ScaleX : Integer;
    ScaleY : Integer;
    Alpha : Number;
    constructor Create(AOwner: TComponent); override;
    procedure DoPaint(Sender: TObject);
    property Canvas: TCanvas read GetCanvas;
    property X: Integer read GetLeft write SetLeft;
    property Y: Integer read GetTop write SetTop;
  end;

  TMovieClip = class(TPanel)
  private
    function GetMouseX: Integer;
    function GetMouseY: Integer;
  protected
    _Transparent: Boolean;
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    scaleX : Number;
    scaleY : Number;
    property mouseX : Integer read getMouseX;
    property mouseY : Integer read getMouseY;
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TComponent; Transparent: Boolean = false);
    procedure SetChildIndex(Spr : TSprite; Ndx : Integer);
  end;

  TTextField = class(TLabel)
  private
    function GetLeft: Integer;
    function GetTop: Integer;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
  public
    ButtonMode: Boolean; // Dummy variable
    MouseChildren: Boolean; // DUmmy variable
    EmbedFonts: Boolean; // Dummy variable
    Selectable: Boolean; // Dummy variable
    property X: Integer read GetLeft write SetLeft;
    property Y: Integer read GetTop write SetTop;
  end;

  procedure NavigateToURL(request : TURLRequest; target : String = '');
  function StringToColorEx(const S: OleVariant): TColor;

implementation

procedure NavigateToURL( request : TURLRequest; target : String);
begin
  // Target is neglected over here, its a JavaScript thing!
  if request<>nil then begin
    ShellExecuteW(GetDesktopWindow, 'open', PChar(request.URL), nil, nil, SW_SHOWNORMAL);
  end;
end;

function StringToColorEx(const S: OleVariant): TColor;
begin
  if (S<>Null) and (Length(S)>0) then begin
    result := StringToColor(S);
  end else begin
    result := clBlack;
  end;
end;

{ TURLRequest }

constructor TURLRequest.Create(URL: String);
begin
  Self._URL := trim(URL);
end;

{ TURLLoader }

constructor TURLLoader.Create;
Begin
  _HttpClient := TIdHTTP.Create(nil);
end;

destructor TURLLoader.Destroy;
begin
  _HttpClient.Free;
  inherited;
end;

function TURLLoader.GetData: String;
begin
  result := _Data;
end;

procedure TURLLoader.Load(Request: TURLRequest);
var
  StrStream : TStringStream;
  FileStream : TFileStream;
Begin
  if LowerCase(element(0, ':', Request.URL))='http' then begin
    StrStream := TStringStream.Create('');
    try
      _HttpClient.Get(Request.URL, StrStream);
      StrStream.Position := 0;
      _Data := Utf8Encode(StrStream.DataString);
      if Assigned(OnCompleted) then OnCompleted(Self);
    finally
      StrStream.Free;
    end;
  end else begin
    FileStream := TFileStream.Create(Request.URL, fmOpenRead);
    try
      FileStream.Position := 0;

      StrStream := TStringStream.Create('');
      try
        StrStream.Position := 0;
        StrStream.CopyFrom(FileStream, FileStream.Size);
        _Data := Utf8Encode(StrStream.DataString);
        if Assigned(OnCompleted) then OnCompleted(Self);
      finally
        StrStream.Free;
      end;
    finally
      FileStream.Free;
    end;  
  end;
end;

{ TFlashArray }

constructor TFlashArray.Create;
begin
  _ObjectList := TObjectList.Create;
  _ObjectList.OwnsObjects := true;
end;

destructor TFlashArray.Destroy;
begin
  _ObjectList.Clear;
  _ObjectList.Free;

  inherited;
end;

function TFlashArray.GetObject(Ndx: Integer): TObject;
begin
  if ndx>=_ObjectList.Count then begin
    result := nil;
  end else begin
    result := _ObjectList.Items[Ndx];
  end;
end;

function TFlashArray.GetObjectCount: Integer;
begin
  result := _ObjectList.Count;
end;

function TFlashArray.Pop: TObject;
begin
  if _ObjectList.Count=0 then begin
    result := nil;
  end else begin
    result := _ObjectList.Items[_ObjectList.Count-1];
    _ObjectList.Delete(_ObjectList.Count-1);
  end;
end;

procedure TFlashArray.Push(AValue: TObject);
begin
  _ObjectList.Add(AValue);
end;

procedure TFlashArray.Sort(Compare: TListSortCompare);
begin
  _ObjectList.Sort(Compare);
end;

procedure TFlashArray.SortOn(FieldName: String);
begin
	// TODO: Missing implementation
end;

{ TMovieClip }

procedure TMovieClip.CMHitTest(var Message: TCMHitTest);
begin
  Message.Result := Windows.HTNOWHERE;
end;

constructor TMovieClip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

constructor TMovieClip.CreateEx(AOwner: TComponent; Transparent: Boolean);
begin
  _Transparent := Transparent;
  inherited Create(AOwner);
end;

procedure TMovieClip.CreateParams(var Params: TCreateParams);
begin
  inherited;

  if _Transparent then begin
    if not (csDesigning in ComponentState) then
      Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
  end;
end;

function TMovieClip.GetMouseX: Integer;
begin
  result := ScreenToClient(Mouse.CursorPos).X;
end;

function TMovieClip.GetMouseY: Integer;
begin
  result := ScreenToClient(Mouse.CursorPos).Y;
end;

procedure TMovieClip.Paint;
begin
  inherited;
end;

procedure TMovieClip.SetChildIndex(Spr: TSprite; Ndx: Integer);
begin
	// TODO: Missing implementation
end;

(* Transparent panel tip by Serge Gubenko *)
procedure TMovieClip.WndProc(var Message: TMessage);
var
  XControl: TControl;
  XPos: TPoint;
begin
  if _Transparent and (not (csDesigning in ComponentState)) and ((Message.Msg >= WM_MOUSEFIRST)
    and (Message.Msg <= WM_MOUSELAST)) then begin
    XPos := ClientToScreen(
      POINT(TWMMouse(Message).XPos, TWMMouse(Message).YPos)
    );
    XControl := Parent.ControlAtPos(
      POINT(TWMMouse(Message).XPos + Left,
      TWMMouse(Message).YPos + Top),
      true, true
    );
    if Assigned(XControl) and (XControl is TWinControl) then
    begin
      XPos := TWinControl(XControl).ScreenToClient(XPos);
      TWMMouse(Message).XPos := XPos.X;
      TWMMouse(Message).YPos := XPos.Y;
      PostMessage(
        TWinControl(XControl).Handle, Message.Msg,
        Message.WParam, Message.LParam
      );
    end
    else
    begin
      XPos := Parent.ScreenToClient(XPos);
      TWMMouse(Message).XPos := XPos.X;
      TWMMouse(Message).YPos := XPos.Y;
      PostMessage(Parent.Handle, Message.Msg, Message.WParam, Message.LParam);
    end;
    Message.Result := 0;
  end else begin
    inherited WndProc(Message);
  end;
end;

{ TSprite }

constructor TSprite.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TSprite.DoPaint(Sender: TObject);
begin
//  if Self.visible then begin
//    Self.Canvas.Rectangle(0, 0, Self.Width, Self.Height);
//  end;
end;

function TSprite.GetCanvas: TCanvas;
begin
  Result := inherited Canvas;
end;

function TSprite.GetLeft: Integer;
begin
  Result := Left;
end;

function TSprite.GetTop: Integer;
begin
  Result := Top;
end;

procedure TSprite.SetLeft(const Value: Integer);
begin
  Left := Value;
end;

procedure TSprite.SetTop(const Value: Integer);
begin
  Top := Value;
end;

{ TTextField }

function TTextField.GetLeft: Integer;
begin
  Result := Left;
end;

function TTextField.GetTop: Integer;
begin
  Result := Top;
end;

procedure TTextField.SetLeft(const Value: Integer);
begin
  Left := Value;
end;

procedure TTextField.SetTop(const Value: Integer);
begin
  Top := Value;
end;

end.
