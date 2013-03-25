program test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main_test, libmupdf, purecode;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPreviewForm, PreviewForm);
  Application.Run;
end.

