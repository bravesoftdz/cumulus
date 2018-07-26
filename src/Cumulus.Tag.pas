(*
		com.roytanck.wpcumulus.Tag
		Copyright 2009: Roy Tanck

		This file is part of WP-Cumulus.

    WP-Cumulus is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    WP-Cumulus is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with WP-Cumulus.  If not, see <http://www.gnu.org/licenses/>.
*)

unit Cumulus.Tag;

interface

uses Controls, Classes, Graphics, FlashCompat, ExtCtrls, SysUtils, StdCtrls,
   XMLDoc, Xml.XMLIntf, Windows;

type
	TTag = class(TSprite)
  private
		_Back    : TSprite;
		_Node    : IXMLNode;
		_Cx      : Number;
		_Cy      : Number;
		_Cz      : Number;
		_Color   : TColor;
		_Hicolor : TColor;
		_Active  : Boolean;
		_Tf      : TTextField;

    _Left    : Integer;
    _Top     : Integer;
    _Owner   : TWinControl;

		procedure MouseOverHandler(Sender: TObject);
	  procedure MouseOutHandler(Sender: TObject);
		procedure MouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

		function GetNumberFromString(const S :String): Number;

    procedure DoPaint(Sender: TObject);
    procedure _SetCx(const Value: Number);
    procedure _SetCy(const Value: Number);
    procedure _SetCz(const Value: Number);
  public
    ScaleX: Integer;
    ScaleY: Integer;
    Alpha: Number;

    constructor Create(owner: TWinControl; node: IXMLNode; color: TColor; hicolor: TColor);
    destructor Destroy; override;

    // setters and getters
    property Cx: Number read _Cx write _SetCx;
    property Cy: Number read _Cy write _SetCy;
    property Cz: Number read _Cz write _SetCz;
    property Active : Boolean read _Active;

    property X : Integer write _Left;
    property Y : Integer write _Top;
  end;

implementation

uses Cumulus.TagCloud, RegularExpressions, Variants;

procedure TTag.MouseOverHandler(Sender: TObject);
begin
  _Back.Visible := True;
  _Tf.Font.Color := _Hicolor;
  _Active := True;
end;

procedure TTag.MouseOutHandler(Sender: TObject);
begin
  _Back.Visible := False;
  _Tf.Font.Color := _Color;
  _Active := False;
end;

procedure TTag.MouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Request: TURLRequest;
  Target: String;
begin
  if Assigned(TTagCloud(Self._Owner).OnNavigateToURL) then begin
    Request := TURLRequest.Create(_Node.Attributes['href']);
    try
      if _Node.Attributes['target'] = Null then begin
        Target  := '_self';
      end else begin
        Target  := _Node.Attributes['target'];
      end;

      TTagCloud(Self._Owner).OnNavigateToURL(Self, Request, Target);
    finally
      Request.Free;
    end;
  end;

end;

procedure TTag._SetCx(const Value: Number);
begin
  _Cx := Value;
  DoPaint(Self);
end;

procedure TTag._SetCy(const Value: Number);
begin
  _Cy := Value;
  DoPaint(Self);
end;

procedure TTag._SetCz(const Value: Number);
begin
  _Cz := Value;
  DoPaint(Self);
end;

destructor TTag.Destroy;
begin
  inherited;
end;

function CrossFadeColor(FromColor, ToColor: TColor; Rate: Single): TColor;
var r,g,b : byte; 
begin 
  r := Round(GetRValue(FromColor)*Rate+GetRValue(ToColor)*(1-Rate));
  g := Round(GetGValue(FromColor)*Rate+GetGValue(ToColor)*(1-Rate));
  b := Round(GetBValue(FromColor)*Rate+GetBValue(ToColor)*(1-Rate));
  Result := RGB(r,g,b);
end;

procedure TTag.DoPaint(Sender: TObject);
var
  OffsetX,
  OffsetY : Integer;
begin
  OffsetX := _owner.Width div 2;
  OffsetY := _owner.Height div 2;

  _Left := _Left + round(cx);
  _Top := _Top + round(cy);
    
  _tf.Left := _Left div 10 + OffsetX;
  _tf.Top := _Top div 10 + OffsetY;

  _tf.Font.Size := scaley*2;
  if not _active then _tf.Font.Color := CrossFadeColor(_color, clWhite, alpha);

  _back.Left := _Left div 10 + OffsetX - 2;
  _back.Top := _Top div 10 + OffsetY - 2;

  _back.Height := _tf.Height + 4;
  _back.Width := _tf.Width + 4;
end;

function TTag.GetNumberFromString(const S: String): Number;
var
  regexpr : TRegEx;
  match   : TMatchCollection;
  i       : integer;
  buffer  : String;
begin
  regexpr := TRegEx.Create('(\d|\.|\,)', [roCompiled, roSingleLine]);
  match := regexpr.Matches(s);
  if match.Count=0 then begin
    result := 0;
  end else begin
    buffer := '';
    for I := 0 to match.Count-1 do begin
      buffer := buffer + match.Item[i].Value;
    end;
    buffer := StringReplace(buffer, ',', '.', [rfReplaceAll]);
    result := StrToFloatDef(buffer, 0);
  end;
end;

constructor TTag.Create(Owner: TWinControl; Node: IXMLNode; Color: TColor; Hicolor: TColor);
var
  Format: TTextFormat;
  SelfEx: TTextField;
begin
  inherited Create(Owner);
  _Owner := Owner;

  _Node := Node;
  _Color := Color;
  _Hicolor := Hicolor;
  _Active := False;

  // Create the text field
  _Tf := TTextField.Create(_Owner);
  _Tf.Parent := _Owner;
  _Tf.Autosize := True;
  _Tf.Alignment := taLeftJustify;
  _Tf.Transparent := True;
  _Tf.Selectable := False;

  // Set styles
  Format := TTextFormat.Create;
  Format.Name := 'Arial';
  Format.Style := [fsBold];
  Format.Color := Color;
  Format.Size := Round(2 * GetNumberFromString(node.Attributes['style']));
  try
    _Tf.Font.Assign(Format);
  finally
    Format.Free;
  end;
  _Tf.EmbedFonts := True;

  // Set text
  _Tf.Caption := Node.Text;
  _Tf.BringToFront; // TODO: addChild(_tf);

  // Scale and add
  _Tf.X := -Self.Width div 2;
  _Tf.Y := -Self.Height div 2;

  // Create the back
  _Back := TSprite.Create(_Owner);
  _Back.Parent := _Owner;
  _Back.Canvas.Brush.Color := _Hicolor;
  _Back.Canvas.Pen.Style := psSolid;
  _Back.Canvas.Pen.Color := _Hicolor;
  _Back.Canvas.Pen.Width := 0;
  _Back.Canvas.Rectangle(0, 0, _Tf.Width + 20, _Tf.Height + 5);

  _Back.SendToBack; // TODO: addChildAt( _back, 0 );
  _Back.X := -(_Tf.Width div 2) - 10;
  _Back.Y := -(_Tf.Height div 2) - 2;
  _Back.Visible := false;

  // Check for http links only
  if (_Node.Attributes['href']<>Null) and (Copy(_Node.Attributes['href'], 0, 4) = 'http') then begin
    // Force mouse cursor on rollover
    SelfEx := _Tf; // TODO: SelfEx should point to Self
    SelfEx.MouseChildren := false;
    SelfEx.ButtonMode := true;
    SelfEx.Cursor := crHandPoint;

    // Events
    SelfEx.OnMouseLeave := MouseOutHandler;
    SelfEx.OnMouseEnter := MouseOverHandler;
    SelfEx.OnMouseUp := MouseUpHandler;
  end;
end;

end.
