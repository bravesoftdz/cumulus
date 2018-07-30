unit untMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Cumulus.TagCloud, ExtCtrls, Utils, FlashCompat, Xml.XMLIntf, Xml.XMLDoc;

type
  TfrmCumulus = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    TagCloud : TTagCloud;
    procedure NavigateToURLHandler(Sender : TObject; Request : TURLRequest; const Target : String);
  end;

var
  frmCumulus: TfrmCumulus;

implementation

{$R *.dfm}

procedure TfrmCumulus.NavigateToURLHandler(Sender: TObject; Request: TURLRequest;
  const Target: String);
begin
  if length(trim(request.url))>0 then begin
    if MessageDlg(request.url, mtConfirmation, mbYesNo, 0)=mrYes then begin
      NavigateToURL(request, target);
    end;
  end;
end;

procedure TfrmCumulus.FormShow(Sender: TObject);
var
  MyXML : IXMLDocument;
begin
  TagCloud := TTagCloud.Create(Self);
  TagCloud.Parent := Self;
  TagCloud.OnNavigateToURL := NavigateToURLHandler;

  TagCloud.Align := alClient;

//  TagCloud.Left := 0;
//  TagCloud.Top := 0;
//  TagCloud.Width := 550;
//  TagCloud.Height := 375;

  TagCloud.TagColor := $ffffff;
  TagCloud.BackgroundColor := $333333;
  TagCloud.TransparentMode := False;
  TagCloud.RotationSpeed := 100;
  TagCloud.EvenlyDistributedTags := True;
  TagCloud.DisplayMode := dmTags;

  try
    MyXML := LoadXMLDocument('tagcloud.xml');
    TagCloud.Initialize(MyXML);
  except
    ShowMessage('Failed while attempting to load XML file.');
  end;
end;

procedure TfrmCumulus.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TagCloud.Free;
end;

end.
