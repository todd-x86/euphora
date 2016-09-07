unit frmPreview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OleCtrls, SHDocVw_EWB, EwbCore, EmbeddedWB;

type
  TfrmUIPreview = class(TForm)
    Contents: TEmbeddedWB;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmUIPreview: TfrmUIPreview;

implementation

{$R *.dfm}

end.
