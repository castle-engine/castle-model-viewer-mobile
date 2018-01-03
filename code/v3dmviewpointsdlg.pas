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

unit V3DMViewpointsDlg;

interface

uses Classes, SysUtils,
  CastleControls, CastleScene, CastleUIState, CastleKeysMouse;

type
  TViewpointSelectedEvent = procedure (ViewpointIdx : integer) of object;

  TStateViewpointsDlg = class(TUIState)
  strict private
    type
      TViewpointsDialog = class(TCastleRectangleControl)
      strict private
        procedure ViewpointNameClick(Sender: TObject);
      public
        constructor Create(AOwner: TComponent); reintroduce;
        procedure DoAnswered;
      end;
    var
      Dialog: TViewpointsDialog;
  public
    FScene: TCastleScene;
    FCurrentViewpointIdx: integer;
    FOnViewpointSelected: TViewpointSelectedEvent;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StateViewpointsDlg: TStateViewpointsDlg;

implementation

uses CastleColors, CastleWindow, CastleUIControls, CastleFilesUtils, CastleLog,
  CastleUtils, CastleVectors;

{ TStateViewpointsDlg.TViewpointsDialog ---------------------------------------------- }

constructor TStateViewpointsDlg.TViewpointsDialog.Create(AOwner: TComponent);
var
  InsideRect: TCastleRectangleControl;
  LabelWndTitle: TCastleLabel;
  I, ViewpointCount, NextTop: integer;
  ViewpointBtn: TCastleButton;
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
  LabelWndTitle.Caption := '<b>Viewpoints</b>';
  LabelWndTitle.Anchor(hpMiddle);
  LabelWndTitle.Anchor(vpTop, -10);
  InsideRect.InsertFront(LabelWndTitle);

  NextTop := -50; // title size

  if Assigned(StateViewpointsDlg.FScene) then
     ViewpointCount := StateViewpointsDlg.FScene.ViewpointsCount
  else
     ViewpointCount := 0;

  for I := 0 to ViewpointCount - 1 do
  begin
    ViewpointBtn := TCastleButton.Create(Self);
    ViewpointBtn.Caption := StateViewpointsDlg.FScene.GetViewpointName(I);
    ViewpointBtn.OnClick := @ViewpointNameClick;
    ViewpointBtn.Tag := I;
    ViewpointBtn.Toggle := true;
    ViewpointBtn.Pressed := (I = StateViewpointsDlg.FCurrentViewpointIdx);
    ViewpointBtn.AutoSizeWidth := false;
    ViewpointBtn.Width := InsideRect.Width - 20;
    ViewpointBtn.Anchor(hpMiddle);
    ViewpointBtn.Anchor(vpTop, NextTop);
    InsideRect.InsertFront(ViewpointBtn);
    NextTop := NextTop - ViewpointBtn.CalculatedHeight;
  end;

  Height := -NextTop + 20;
  InsideRect.Height := CalculatedHeight - 10;
end;

procedure TStateViewpointsDlg.TViewpointsDialog.ViewpointNameClick(Sender: TObject);
var
  ViewpointIdx: integer;
begin
  ViewpointIdx := (Sender as TCastleButton).Tag;
  if Assigned(StateViewpointsDlg.FOnViewpointSelected) then
    StateViewpointsDlg.FOnViewpointSelected(ViewpointIdx);

  DoAnswered;
end;

procedure TStateViewpointsDlg.TViewpointsDialog.DoAnswered;
begin
  TUIState.Pop(StateViewpointsDlg);
end;

{ TStateViewpointsDlg ------------------------------------------------------------ }

procedure TStateViewpointsDlg.Start;
var
  TransparentBackground: TCastleRectangleControl;
begin
  inherited;

  InterceptInput := true;

  TransparentBackground := TCastleRectangleControl.Create(FreeAtStop);
  TransparentBackground.Color := Vector4(0.1, 0.1, 0.1, 0.5);
  TransparentBackground.FullSize := true;
  InsertFront(TransparentBackground);

  Dialog := TViewpointsDialog.Create(FreeAtStop);
  Dialog.Anchor(hpMiddle);
  Dialog.Anchor(vpMiddle);
  InsertFront(Dialog);
end;

function TStateViewpointsDlg.Press(const Event: TInputPressRelease): boolean;
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
