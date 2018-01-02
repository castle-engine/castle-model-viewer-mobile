{
  Copyright 2017-2017 Michalis Kamburelis and Jan Adamec.

  This file is part of "view3dscene-mobile".

  "view3dscene-mobile" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "view3dscene-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit V3DMOptionsDlg;

interface

uses Classes, SysUtils,
  CastleControls, CastleScene, CastleUIState, CastleKeysMouse;

type
  TStateOptionsDlg = class(TUIState)
  strict private
    type
      TOptionsDialog = class(TCastleRectangleControl)
      strict private
        //ChkShowBBox: TCastleCheckbox;
        ChkShowBBox, ChkShowFps, ChkHeadlight, ChkCollisions: TCastleButton;
        procedure ChkShowBBoxClick(Sender: TObject);
        procedure ChkShowFpsClick(Sender: TObject);
        procedure ChkHeadlightClick(Sender: TObject);
        procedure ChkCollisionsClick(Sender: TObject);
      public
        constructor Create(AOwner: TComponent); reintroduce;
        procedure DoAnswered;
      end;
    var
      Dialog: TOptionsDialog;
  public
    FScene: TCastleScene;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StateOptionsDlg: TStateOptionsDlg;

implementation

uses CastleColors, CastleWindow, CastleUIControls, CastleFilesUtils, CastleLog,
  CastleUtils, CastleVectors,
  V3DMOptions;

{ TStateOptionsDlg.TOptionsDialog ---------------------------------------------- }

constructor TStateOptionsDlg.TOptionsDialog.Create(AOwner: TComponent);
var
  InsideRect: TCastleRectangleControl;
  LabelWndTitle: TCastleLabel;
begin
  inherited Create(AOwner);

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
  LabelWndTitle.Caption := '<b>Options</b>';
  LabelWndTitle.Anchor(hpMiddle);
  LabelWndTitle.Anchor(vpTop, -10);
  InsideRect.InsertFront(LabelWndTitle);

  ChkShowBBox := TCastleButton.Create(Self);
  ChkShowBBox.Caption := 'Show Bounding Box';
  ChkShowBBox.Anchor(hpLeft, 10);
  ChkShowBBox.Anchor(vpTop, -50);
  ChkShowBBox.PaddingHorizontal := 10;
  ChkShowBBox.OnClick := @ChkShowBBoxClick;
  InsideRect.InsertFront(ChkShowBBox);

  ChkShowFps := TCastleButton.Create(Self);
  ChkShowFps.Caption := 'Show FPS';
  ChkShowFps.Anchor(hpLeft, 10);
  ChkShowFps.Anchor(vpTop, -100);
  ChkShowFps.PaddingHorizontal := 10;
  ChkShowFps.OnClick := @ChkShowFpsClick;
  InsideRect.InsertFront(ChkShowFps);

  ChkHeadlight := TCastleButton.Create(Self);
  ChkHeadlight.Caption := 'Headlight';
  ChkHeadlight.Anchor(hpLeft, 10);
  ChkHeadlight.Anchor(vpTop, -150);
  ChkHeadlight.PaddingHorizontal := 10;
  ChkHeadlight.OnClick := @ChkHeadlightClick;
  ChkHeadlight.Enabled := Assigned(StateOptionsDlg.FScene);
  //if Assigned(StateOptionsDlg.FScene) then
  //  ChkHeadlight.Checked := StateOptionsDlg.FScene.HeadLightOn;
  InsideRect.InsertFront(ChkHeadlight);

  ChkCollisions := TCastleButton.Create(Self);
  ChkCollisions.Caption := 'Collisions';
  ChkCollisions.Anchor(hpLeft, 10);
  ChkCollisions.Anchor(vpTop, -200);
  ChkCollisions.PaddingHorizontal := 10;
  ChkCollisions.OnClick := @ChkCollisionsClick;
  InsideRect.InsertFront(ChkCollisions);

end;

procedure TStateOptionsDlg.TOptionsDialog.ChkShowBBoxClick(Sender: TObject);
begin
  AppOptions.ShowBBox := not AppOptions.ShowBBox;
end;

procedure TStateOptionsDlg.TOptionsDialog.ChkShowFpsClick(Sender: TObject);
begin
  AppOptions.ShowFps := not AppOptions.ShowFps;
end;

procedure TStateOptionsDlg.TOptionsDialog.ChkHeadlightClick(Sender: TObject);
begin
  if Assigned(StateOptionsDlg.FScene) then
  //  StateOptionsDlg.FScene.HeadLightOn := ChkHeadlight.Checked;
    StateOptionsDlg.FScene.HeadLightOn := not StateOptionsDlg.FScene.HeadLightOn;
end;

procedure TStateOptionsDlg.TOptionsDialog.ChkCollisionsClick(Sender: TObject);
begin
  AppOptions.CollisionsOn := not AppOptions.CollisionsOn;

  if Assigned(StateOptionsDlg.FScene) then
    StateOptionsDlg.FScene.Collides := not AppOptions.CollisionsOn;
end;

procedure TStateOptionsDlg.TOptionsDialog.DoAnswered;
begin
  TUIState.Pop(StateOptionsDlg);
  AppOptions.Save;
end;

{ TStateOptionsDlg ------------------------------------------------------------ }

procedure TStateOptionsDlg.Start;
var
  TransparentBackground: TCastleRectangleControl;
begin
  inherited;

  InterceptInput := true;

  TransparentBackground := TCastleRectangleControl.Create(FreeAtStop);
  TransparentBackground.Color := Vector4(0.1, 0.1, 0.1, 0.5);
  TransparentBackground.FullSize := true;
  InsertFront(TransparentBackground);

  Dialog := TOptionsDialog.Create(FreeAtStop);
  Dialog.Anchor(hpMiddle);
  Dialog.Anchor(vpMiddle);
  InsertFront(Dialog);
end;

function TStateOptionsDlg.Press(const Event: TInputPressRelease): boolean;
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
