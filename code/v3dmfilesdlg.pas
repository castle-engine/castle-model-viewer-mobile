{
  Copyright 2017-2018 Michalis Kamburelis and Jan Adamec.

  This file is part of "view3dscene-mobile".

  "view3dscene-mobile" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "view3dscene-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit V3DMFilesDlg;

interface

uses Classes, SysUtils,
  CastleControls, CastleScene, CastleUIState, CastleKeysMouse;

type
  TFileSelectedEvent = procedure (Url : string) of object;

  TStateFilesDlg = class(TUIState)
  strict private
    type
      TFilesDialog = class(TCastleRectangleControl)
      strict private
        FileList: TStringList;
        procedure FileNameClick(Sender: TObject);
      public
        constructor Create(AOwner: TComponent); reintroduce;
        destructor Destroy; override;
        procedure DoAnswered;
      end;
    var
      Dialog: TFilesDialog;
  public
    FOnFileSelected: TFileSelectedEvent;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StateFilesDlg: TStateFilesDlg;

implementation

uses CastleColors, CastleWindow, CastleUIControls, CastleFilesUtils, CastleLog,
  CastleUtils, CastleVectors;

{ TStateFilesDlg.TFilesDialog ---------------------------------------------- }

constructor TStateFilesDlg.TFilesDialog.Create(AOwner: TComponent);
var
  InsideRect: TCastleRectangleControl;
  LabelWndTitle: TCastleLabel;
  I, FilesCount, NextTop: integer;
  FileBtn: TCastleButton;
begin
  inherited Create(AOwner);

  // fixed demo scenes, TODO: make it dynamic by enumerating in data/demo folder
  FileList := TStringList.Create();
  FileList.Add(ApplicationData('demo/castle_walk.wrl'));
  FileList.Add(ApplicationData('demo/chinchilla.wrl.gz'));
  FileList.Add(ApplicationData('demo/teapot (fresnel and toon shader).x3dv'));
  FileList.Add(ApplicationData('demo/teapot (time to shader).x3dv'));

  Width := 400;
  Height := 500;
  Color := Black;

  InsideRect := TCastleRectangleControl.Create(Self);
  InsideRect.Width := CalculatedWidth - 10;
  InsideRect.Height := CalculatedHeight - 10;
  InsideRect.Color := HexToColor('505050');
  InsideRect.Anchor(hpMiddle);
  InsideRect.Anchor(vpMiddle);
  InsertFront(InsideRect);

  LabelWndTitle := TCastleLabel.Create(Self);
  LabelWndTitle.Color := White;
  LabelWndTitle.Html := true;
  LabelWndTitle.Caption := '<b>Demo Scenes</b>';
  LabelWndTitle.Anchor(hpMiddle);
  LabelWndTitle.Anchor(vpTop, 0);
  InsideRect.InsertFront(LabelWndTitle);

  NextTop := -40; // title size

  FilesCount := FileList.Count;
  for I := 0 to FilesCount - 1 do
  begin
    FileBtn := TCastleButton.Create(Self);
    FileBtn.Caption := ExtractFileName(FileList[I]);
    FileBtn.OnClick := @FileNameClick;
    FileBtn.Tag := I;
    FileBtn.AutoSizeWidth := false;
    FileBtn.Width := InsideRect.Width - 20;
    FileBtn.Anchor(hpMiddle);
    FileBtn.Anchor(vpTop, NextTop);
    InsideRect.InsertFront(FileBtn);
    NextTop := NextTop - FileBtn.CalculatedHeight;
  end;

  Height := -NextTop + 20;
  InsideRect.Height := CalculatedHeight - 10;
end;

destructor TStateFilesDlg.TFilesDialog.Destroy;
begin
  FileList.Free;
  inherited;
end;

procedure TStateFilesDlg.TFilesDialog.FileNameClick(Sender: TObject);
var
  FileIdx: integer;
begin
  FileIdx := (Sender as TCastleButton).Tag;
  if Assigned(StateFilesDlg.FOnFileSelected) then
    StateFilesDlg.FOnFileselected(FileList[FileIdx]);

  DoAnswered;
end;

procedure TStateFilesDlg.TFilesDialog.DoAnswered;
begin
  TUIState.Pop(StateFilesDlg);
end;

{ TStateFilesDlg ------------------------------------------------------------ }

procedure TStateFilesDlg.Start;
var
  TransparentBackground: TCastleRectangleControl;
begin
  inherited;

  InterceptInput := true;

  TransparentBackground := TCastleRectangleControl.Create(FreeAtStop);
  TransparentBackground.Color := Theme.BackgroundColor;
  TransparentBackground.FullSize := true;
  InsertFront(TransparentBackground);

  Dialog := TFilesDialog.Create(FreeAtStop);
  Dialog.Anchor(hpMiddle);
  Dialog.Anchor(vpMiddle);
  InsertFront(Dialog);
end;

function TStateFilesDlg.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

  // end dialog if clicked outside dialog
  if Event.IsMouseButton(mbLeft) and (not Dialog.ScreenRect.Contains(Event.Position)) then
  begin
    Dialog.DoAnswered;
    Result := true;
  end;
end;

end.
