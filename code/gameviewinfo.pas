{
  Copyright 2017-2020 Michalis Kamburelis and Jan Adamec.

  This file is part of "view3dscene-mobile".

  "view3dscene-mobile" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "view3dscene-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit GameViewInfo;

interface

uses Classes,
  CastleUIControls, CastleControls, CastleScene, CastleUIState, CastleKeysMouse;

type
  TViewInfo = class(TCastleView)
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
  ViewInfo: TViewInfo;

const
  SmallFontScale = 0.8;

implementation

uses
  Math, SysUtils,
  CastleColors, CastleWindow, CastleFilesUtils, CastleLog,
  CastleGLUtils, CastleUtils, CastleVectors, CastleOpenDocument,
  CastleDialogViews;

{ TViewInfo.TInfoDialog ---------------------------------------------- }

constructor TViewInfo.TInfoDialog.Create(AOwner: TComponent);
var
  HeaderRect: TCastleRectangleControl;
  LabelWndTitle, LabelSceneStats, LabelAbout: TCastleLabel;
  ImgLogo: TCastleImageControl;
  BtnOpenGlInfo, BtnDonate, BtnWeb, BtnDone: TCastleButton;
  NextTop, NextBottom, LogoHeight: Single;
begin
  inherited Create(AOwner);

  Width := Min(320, ViewInfo.Container.UnscaledWidth - 20);
  Height := Min(480, ViewInfo.Container.UnscaledHeight - 20);
  ThemeImage := tiWindow;
  UseThemeImage := true;

  HeaderRect := TCastleRectangleControl.Create(Self);
  HeaderRect.Width := EffectiveWidth - 4;
  HeaderRect.Color := HexToColor('5A5A5A');
  HeaderRect.Anchor(hpMiddle);
  HeaderRect.Anchor(vpTop, -2);
  InsertFront(HeaderRect);

  LabelWndTitle := TCastleLabel.Create(Self);
  LabelWndTitle.Color := White;
  LabelWndTitle.Html := true;
  LabelWndTitle.Caption := '<b>About view3Dscene</b>';
  LabelWndTitle.Anchor(hpMiddle);
  LabelWndTitle.Anchor(vpTop, -14);
  InsertFront(LabelWndTitle);

  BtnDone := TCastleButton.Create(Self);
  BtnDone.Caption := 'Done';
  BtnDone.OnClick := @BtnDoneClick;
  BtnDone.Anchor(vpTop, -7);
  BtnDone.Anchor(hpRight, -7);
  InsertFront(BtnDone);

  HeaderRect.Height := BtnDone.EffectiveHeight + 12;
  NextTop := HeaderRect.Height + 8;

  LabelSceneStats := TCastleLabel.Create(Self);
  LabelSceneStats.Color := Silver;
  LabelSceneStats.FontScale := SmallFontScale;
  LabelSceneStats.Caption := ViewInfo.FStatistics;
  LabelSceneStats.MaxWidth := Width - 16;
  LabelSceneStats.Alignment := hpLeft;
  LabelSceneStats.VerticalAlignment := vpTop;
  LabelSceneStats.Anchor(hpLeft, 10);
  LabelSceneStats.Anchor(vpTop, -NextTop);
  InsertFront(LabelSceneStats);
  NextTop := NextTop + LabelSceneStats.EffectiveHeight + 10;

  BtnOpenGlInfo := TCastleButton.Create(Self);
  BtnOpenGlInfo.Caption := 'OpenGL information';
  BtnOpenGlInfo.Anchor(hpMiddle);
  BtnOpenGlInfo.Anchor(vpTop, -NextTop);
  BtnOpenGlInfo.OnClick := @BtnOpenGlInfoClick;
  InsertFront(BtnOpenGlInfo);
  NextTop := NextTop + BtnOpenGlInfo.EffectiveHeight + 10;

  // from the bottom
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
  NextBottom := 10 + BtnWeb.EffectiveHeight + 10;

  LabelAbout := TCastleLabel.Create(Self);
  LabelAbout.Color := Silver;
  LabelAbout.FontScale := SmallFontScale;
  LabelAbout.Caption := 'This application uses Castle Game Engine, open-source multi-platform 3D engine written in Modern Pascal.';
  LabelAbout.MaxWidth := Width - 16;
  LabelAbout.Anchor(hpLeft, 10);
  LabelAbout.Anchor(vpBottom, NextBottom);
  InsertFront(LabelAbout);
  NextBottom := NextBottom + LabelAbout.EffectiveHeight + 10;

  // put logo in the center of the remaining space
  LogoHeight := Height - NextBottom - NextTop;
  if LogoHeight > 50 then
  begin
    ImgLogo := TCastleImageControl.Create(Self);
    ImgLogo.URL := 'castle-data:/castle_game_engine_icon.png';
    ImgLogo.Stretch := true;
    ImgLogo.ProportionalScaling := psFit;
    ImgLogo.Width := Width - 14;
    ImgLogo.Height := LogoHeight;
    ImgLogo.Anchor(hpMiddle);
    ImgLogo.Anchor(vpBottom, NextBottom);
    InsertFront(ImgLogo);
  end;
end;

procedure TViewInfo.TInfoDialog.BtnDonateClick(Sender: TObject);
begin
  OpenURL('https://www.patreon.com/castleengine');
end;

procedure TViewInfo.TInfoDialog.BtnWebClick(Sender: TObject);
begin
  OpenURL('https://castle-engine.io');
end;

procedure TViewInfo.TInfoDialog.BtnOpenGlInfoClick(Sender: TObject);
var
  Dlg: TViewDialogOK;
begin
  // open new UIState
  Dlg := TViewDialogOK.Create(Self);
  Dlg.Caption := GLInformationString;
  Container.PushView(Dlg);
end;

procedure TViewInfo.TInfoDialog.BtnDoneClick(Sender: TObject);
begin
  DoAnswered;
end;

procedure TViewInfo.TInfoDialog.DoAnswered;
begin
  Container.PopView(ViewInfo);
end;

{ TViewInfo ------------------------------------------------------------ }

procedure TViewInfo.Start;
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

function TViewInfo.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

  // end dialog if clicked outside dialog
  if Event.IsMouseButton(buttonLeft) and (not Dialog.RenderRect.Contains(Event.Position)) then
  begin
    Dialog.DoAnswered;
    Result := true;
  end;
end;

end.
