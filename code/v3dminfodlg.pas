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

unit V3DMInfoDlg;

interface

uses Classes,
  CastleControls, CastleScene, CastleUIState, CastleKeysMouse;

type
  TStateInfoDlg = class(TUIState)
  strict private
    type
      TInfoDialog = class(TCastleRectangleControl)
      strict private
        procedure BtnDonateClick(Sender: TObject);
        procedure BtnWebClick(Sender: TObject);
        procedure BtnOpenGlInfoClick(Sender: TObject);
        procedure BtnDoneClick(Sender: TObject);
      public
        constructor Create(AOwner: TComponent); reintroduce;
        procedure DoAnswered;
      end;
    var
      Dialog: TInfoDialog;
  public
    FScene: TCastleScene;
    FStatistics: string;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StateInfoDlg: TStateInfoDlg;

const
  SmallFontScale = 0.8;

implementation

uses
  SysUtils,
  CastleColors, CastleWindow, CastleUIControls, CastleFilesUtils, CastleLog,
  CastleGLUtils, CastleUtils, CastleVectors, CastleOpenDocument,
  CastleDialogStates;

{ TStateInfoDlg.TInfoDialog ---------------------------------------------- }

constructor TStateInfoDlg.TInfoDialog.Create(AOwner: TComponent);
var
  HeaderRect: TCastleRectangleControl;
  LabelWndTitle, LabelSceneStats, LabelAbout: TCastleLabel;
  ImgLogo: TCastleImageControl;
  BtnOpenGlInfo, BtnDonate, BtnWeb, BtnDone: TCastleButton;
begin
  inherited Create(AOwner);

  Width := 320;
  Height := 520;
  ThemeImage := tiWindow;

  HeaderRect := TCastleRectangleControl.Create(Self);
  HeaderRect.Width := CalculatedWidth - 4;
  HeaderRect.Color := HexToColor('5A5A5A');
  HeaderRect.Anchor(hpMiddle);
  HeaderRect.Anchor(vpTop, -2);
  InsertFront(HeaderRect);

  LabelWndTitle := TCastleLabel.Create(Self);
  LabelWndTitle.Color := White;
  LabelWndTitle.Html := true;
  LabelWndTitle.Caption := '<b>About view3Dscene</b>';
  LabelWndTitle.Anchor(hpMiddle);
  LabelWndTitle.Anchor(vpTop, -12);
  InsertFront(LabelWndTitle);

  BtnDone := TCastleButton.Create(Self);
  BtnDone.Caption := 'Done';
  BtnDone.OnClick := @BtnDoneClick;
  BtnDone.Anchor(vpTop, -7);
  BtnDone.Anchor(hpRight, -7);
  InsertFront(BtnDone);

  HeaderRect.Height := BtnDone.CalculatedHeight + 12;

  LabelSceneStats := TCastleLabel.Create(Self);
  LabelSceneStats.Color := Silver;
  LabelSceneStats.FontScale := SmallFontScale;
  LabelSceneStats.Caption := StateInfoDlg.FStatistics;
  LabelSceneStats.Width := Width - 24;
  LabelSceneStats.Alignment := hpLeft;
  //LabelSceneStats.AutoSizeWidth := false;
  LabelSceneStats.Anchor(hpLeft, 10);
  LabelSceneStats.Anchor(vpTop, -54);
  InsertFront(LabelSceneStats);

  BtnOpenGlInfo := TCastleButton.Create(Self);
  BtnOpenGlInfo.Caption := 'OpenGL information';
  BtnOpenGlInfo.Anchor(hpMiddle);
  BtnOpenGlInfo.Anchor(vpBottom, 390);
  BtnOpenGlInfo.OnClick := @BtnOpenGlInfoClick;
  InsertFront(BtnOpenGlInfo);

  ImgLogo := TCastleImageControl.Create(Self);
  ImgLogo.URL := ApplicationData('castle_game_engine_icon.png');
  ImgLogo.Anchor(hpMiddle);
  ImgLogo.Anchor(vpBottom, 120);
  InsertFront(ImgLogo);

  LabelAbout := TCastleLabel.Create(Self);
  LabelAbout.Color := Silver;
  LabelAbout.FontScale := SmallFontScale;
  LabelAbout.Caption := 'This application uses Castle Game Engine,' + NL
                     +  'open-source multi-platform 3D engine' + NL
                     +  'written in Modern Pascal.';
  LabelAbout.Width := Width - 44;
  //LabelSceneStats.AutoSizeWidth := false;
  LabelAbout.Anchor(hpMiddle);
  LabelAbout.Anchor(vpBottom, 60);
  InsertFront(LabelAbout);

  BtnDonate := TCastleButton.Create(Self);
  BtnDonate.Caption := 'Donate';
  BtnDonate.Anchor(hpLeft, 10);
  BtnDonate.Anchor(vpBottom, 10);
  BtnDonate.OnClick := @BtnDonateClick;
  InsertFront(BtnDonate);

  BtnWeb := TCastleButton.Create(Self);
  BtnWeb.Caption := 'Website';
  BtnWeb.Anchor(hpRight, -10);
  BtnWeb.Anchor(vpBottom, 10);
  BtnWeb.OnClick := @BtnWebClick;
  InsertFront(BtnWeb);
end;

procedure TStateInfoDlg.TInfoDialog.BtnDonateClick(Sender: TObject);
begin
  OpenURL('https://www.patreon.com/castleengine');
end;

procedure TStateInfoDlg.TInfoDialog.BtnWebClick(Sender: TObject);
begin
  OpenURL('https://castle-engine.sourceforge.io');
end;

procedure TStateInfoDlg.TInfoDialog.BtnOpenGlInfoClick(Sender: TObject);
var
  Dlg: TStateDialogOK;
begin
  // open new UIState
  Dlg := TStateDialogOK.Create(Self);
  Dlg.Caption := GLInformationString;
  TUIState.Push(Dlg);
end;

procedure TStateInfoDlg.TInfoDialog.BtnDoneClick(Sender: TObject);
begin
  DoAnswered;
end;

procedure TStateInfoDlg.TInfoDialog.DoAnswered;
begin
  TUIState.Pop(StateInfoDlg);
end;

{ TStateInfoDlg ------------------------------------------------------------ }

procedure TStateInfoDlg.Start;
var
  TransparentBackground: TCastleRectangleControl;
begin
  inherited;

  InterceptInput := true;

  TransparentBackground := TCastleRectangleControl.Create(FreeAtStop);
  TransparentBackground.Color := Theme.BackgroundColor;
  TransparentBackground.FullSize := true;
  InsertFront(TransparentBackground);

  Dialog := TInfoDialog.Create(FreeAtStop);
  Dialog.Anchor(hpMiddle);
  Dialog.Anchor(vpMiddle);
  InsertFront(Dialog);
end;

function TStateInfoDlg.Press(const Event: TInputPressRelease): boolean;
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
