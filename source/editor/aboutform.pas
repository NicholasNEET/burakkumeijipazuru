unit aboutform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Button_Okay: TButton;
    Image_Blkmage: TImage;
    Image_japname: TImage;
    Image_engname: TImage;
    Label_Version: TLabel;
    procedure Button_OkayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image_BlkmageClick(Sender: TObject);
  private

  public

  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  { load picture images from /assets }
  Image_Blkmage.Picture.LoadFromFile('assets/image/editor/blkmage.bmp');
  Image_japname.Picture.LoadFromFile('assets/image/menu/jap_name.bmp');
  Image_engname.Picture.LoadFromFile('assets/image/menu/eng_name.bmp');
end;

procedure TfrmAbout.Button_OkayClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.Image_BlkmageClick(Sender: TObject);
begin

end;

end.

