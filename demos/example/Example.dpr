program Example;

uses
  Forms,
  untMain in 'untMain.pas' {frmCumulus},
  Cumulus.Tag in '..\..\src\Cumulus.Tag.pas',
  Cumulus.TagCloud in '..\..\src\Cumulus.TagCloud.pas',
  FlashCompat in '..\..\src\FlashCompat.pas',
  Utils in '..\..\src\Utils.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.CreateForm(TfrmCumulus, frmCumulus);
  Application.Run;
end.
