{
  Copyright 2017-2018 Michalis Kamburelis and Jan Adamec.

  This file is part of "view3dscene-mobile".

  "view3dscene-mobile" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "view3dscene-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "view3dscene-mobile"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Implements the application logic, independent from Android / standalone. }
unit Game;

interface

uses CastleWindowTouch;

var
  Window: TCastleWindowTouch;

implementation

uses Classes, SysUtils, Math,
  CastleControls, CastleKeysMouse, CastleFilesUtils, Castle2DSceneManager,
  CastleVectors, Castle3D, CastleSceneCore, CastleUtils, CastleColors,
  CastleUIControls, CastleUIState, CastleMessaging, CastleLog, CastleImages,
  CastleCameras, CastleWindow, CastleScene,
  CastleGLImages,
  X3DNodes,
  V3DMInfoDlg, V3DMOptions, V3DMOptionsDlg, V3DMViewpointsDlg;

{ main game stuff ------------------------------------------------------------ }
type
  TButtonsHandler = class
    class procedure BtnNavClick(Sender: TObject);
    class procedure BtnOptionsClick(Sender: TObject);
    class procedure BtnInfoClick(Sender: TObject);
    class procedure BtnViewpointNextClick(Sender: TObject);
    class procedure BtnViewpointListClick(Sender: TObject);
    class procedure ViewpointSelected(ViewpointIdx: integer);
  end;

var
  BtnNavWalk, BtnNavFly, BtnNavExamine, BtnNavTurntable, BtnOptions,
    BtnViewpointPrev, BtnViewpointNext, BtnViewpointList,
    BtnInfo: TCastleButton;
  Status: TCastleLabel;

  CurrentViewpointIdx: integer;
  SceneBoundingBox: TShapeNode;

{ One-time initialization. }
procedure ApplicationInitialize;
const
  ButtonPadding = 20;
begin
  AppOptions := TAppOptions.Create;
  AppOptions.Load;

  SceneBoundingBox := nil;
  CurrentViewpointIdx := 0;
  StateInfoDlg := TStateInfoDlg.Create(Application);
  StateOptionsDlg := TStateOptionsDlg.Create(Application);
  StateViewpointsDlg := TStateViewpointsDlg.Create(Application);

  // Create UI
  BtnNavWalk := TCastleButton.Create(Window);
  BtnNavWalk.Caption := 'Walk';
  BtnNavWalk.OnClick := @TButtonsHandler(nil).BtnNavClick;
  BtnNavWalk.Left := 0;
  BtnNavWalk.Bottom := 0;
  BtnNavWalk.PaddingHorizontal := ButtonPadding;
  BtnNavWalk.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(BtnNavWalk);

  BtnNavFly := TCastleButton.Create(Window);
  BtnNavFly.Caption := 'Fly';
  BtnNavFly.OnClick := @TButtonsHandler(nil).BtnNavClick;
  BtnNavFly.Left := 150;
  BtnNavFly.Bottom := 0;
  BtnNavFly.PaddingHorizontal := ButtonPadding;
  BtnNavFly.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(BtnNavFly);

  BtnNavExamine := TCastleButton.Create(Window);
  BtnNavExamine.Caption := 'Examine';
  BtnNavExamine.OnClick := @TButtonsHandler(nil).BtnNavClick;
  BtnNavExamine.Left := 300;
  BtnNavExamine.Bottom := 0;
  BtnNavExamine.PaddingHorizontal := ButtonPadding;
  BtnNavExamine.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(BtnNavExamine);

  BtnNavTurntable := TCastleButton.Create(Window);
  BtnNavTurntable.Caption := 'Turntable';
  BtnNavTurntable.OnClick := @TButtonsHandler(nil).BtnNavClick;
  BtnNavTurntable.Left := 450;
  BtnNavTurntable.Bottom := 0;
  BtnNavTurntable.PaddingHorizontal := ButtonPadding;
  BtnNavTurntable.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(BtnNavTurntable);

  BtnOptions := TCastleButton.Create(Window);
  BtnOptions.Caption := 'Options';
  BtnOptions.OnClick := @TButtonsHandler(nil).BtnOptionsClick;
  BtnOptions.Left := 600;
  BtnOptions.Bottom := 0;
  BtnOptions.PaddingHorizontal := ButtonPadding;
  BtnOptions.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(BtnOptions);

  BtnInfo := TCastleButton.Create(Window);
  BtnInfo.Caption := '(i)';
  BtnInfo.OnClick := @TButtonsHandler(nil).BtnInfoClick;
  BtnInfo.Left := 700;
  BtnInfo.Bottom := 0;
  BtnInfo.PaddingHorizontal := ButtonPadding;
  BtnInfo.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(BtnInfo);

  BtnViewpointPrev := TCastleButton.Create(Window);
  BtnViewpointPrev.Caption := '<';
  BtnViewpointPrev.OnClick := @TButtonsHandler(nil).BtnViewpointNextClick;
  BtnViewpointPrev.Left := 400;
  BtnViewpointPrev.Bottom := 80;
  BtnViewpointPrev.PaddingHorizontal := ButtonPadding;
  BtnViewpointPrev.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(BtnViewpointPrev);

  BtnViewpointList := TCastleButton.Create(Window);
  BtnViewpointList.Caption := 'Viewpoints';
  BtnViewpointList.OnClick := @TButtonsHandler(nil).BtnViewpointListClick;
  BtnViewpointList.Left := 450;
  BtnViewpointList.Bottom := 80;
  BtnViewpointList.PaddingHorizontal := ButtonPadding;
  BtnViewpointList.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(BtnViewpointList);

  BtnViewpointNext := TCastleButton.Create(Window);
  BtnViewpointNext.Caption := '>';
  BtnViewpointNext.OnClick := @TButtonsHandler(nil).BtnViewpointNextClick;
  BtnViewpointNext.Left := 600;
  BtnViewpointNext.Bottom := 80;
  BtnViewpointNext.PaddingHorizontal := ButtonPadding;
  BtnViewpointNext.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(BtnViewpointNext);

  Status := TCastleLabel.Create(Window);
  Status.Padding := 5;
  Status.Color := Red;
  Status.Left := 10;
  Status.Anchor(vpTop, -50);
  Window.Controls.InsertFront(Status);
end;

procedure InitializeSceneBoundingBox;
var
  BoxNode: TBoxNode;
  Material: TMaterialNode;
begin
  BoxNode := TBoxNode.Create;
  BoxNode.Size := Window.MainScene.BoundingBox.Size;

  Material := TMaterialNode.Create;
  Material.ForcePureEmissive;
  Material.EmissiveColor := GreenRGB;

  SceneBoundingBox := TShapeNode.Create;
  SceneBoundingBox.Geometry := BoxNode;
  SceneBoundingBox.Shading := shWireframe;
  SceneBoundingBox.Material := Material;
  SceneBoundingBox.Appearance.ShadowCaster := false;

  Window.MainScene.RootNode.AddChildren(SceneBoundingBox);
end;

procedure WindowDropFiles(Container: TUIContainer; const FileNames: array of string);
var
  Url: string;
  ViewpointsPresent: boolean;
begin
  if Length(FileNames) = 0 then Exit;
  Url := FileNames[0];

  SceneBoundingBox := nil;

  Application.Log(etInfo, 'Opened ' + Url);

  Window.Load(Url);
  Window.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  Window.MainScene.ProcessEvents := true;

  Window.MainScene.Collides := AppOptions.CollisionsOn;

  { // TODO: not implemented in the engine for OpenGLES yet
  Window.MainScene.Attributes.WireframeEffect := weWireframeOnly;
  Window.MainScene.Attributes.WireframeColor  := RedRGB;}

  CurrentViewpointIdx := 0;
  ViewpointsPresent := Window.MainScene.ViewpointsCount > 0;
  BtnViewpointPrev.Exists := ViewpointsPresent;
  BtnViewpointList.Exists := ViewpointsPresent;
  BtnViewpointNext.Exists := ViewpointsPresent;
end;

procedure WindowUpdate(Container: TUIContainer);
begin
  if Status.Exists <> AppOptions.ShowFps then
    Status.Exists := AppOptions.ShowFps;

  if Status.Exists then
    Status.Caption := Format('FPS: %f (real : %f)',
      [Window.Fps.OnlyRenderFps, Window.Fps.RealFps]);

  if (Window.MainScene <> nil) and (Assigned(SceneBoundingBox) <> AppOptions.ShowBBox) then
  begin
    if AppOptions.ShowBBox then
      InitializeSceneBoundingBox
    else begin
      Window.MainScene.RootNode.RemoveChildren(SceneBoundingBox);
      SceneBoundingBox := nil;
    end;
  end;

end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(K_F5) then
    Window.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
  if Event.IsKey(K_Escape) then
    Application.Terminate;

end;

class procedure TButtonsHandler.BtnNavClick(Sender: TObject);
begin
  if Sender = BtnNavWalk then
     Window.NavigationType := ntWalk
  else if Sender = BtnNavFly then
    Window.NavigationType := ntFly
  else if Sender = BtnNavExamine then
    Window.NavigationType := ntExamine
  else if Sender = BtnNavTurntable then
    Window.NavigationType := ntTurntable;
end;

class procedure TButtonsHandler.BtnOptionsClick(Sender: TObject);
begin
  StateOptionsDlg.FScene := Window.MainScene;
  TUIState.Push(StateOptionsDlg);
end;

class procedure TButtonsHandler.BtnInfoClick(Sender: TObject);
begin
  StateInfoDlg.FScene := Window.MainScene;
  TUIState.Push(StateInfoDlg);
end;

class procedure TButtonsHandler.BtnViewpointNextClick(Sender: TObject);
begin
  if Window.MainScene = nil then exit;
  if Sender = BtnViewpointNext then
    Inc(CurrentViewpointIdx)
  else
    Dec(CurrentViewpointIdx);

  if CurrentViewpointIdx < 0 then
    CurrentViewpointIdx := Window.MainScene.ViewpointsCount;
  if CurrentViewpointIdx > Window.MainScene.ViewpointsCount - 1 then
    CurrentViewpointIdx := 0;

  Window.MainScene.MoveToViewpoint(CurrentViewpointIdx);
end;

class procedure TButtonsHandler.BtnViewpointListClick(Sender: TObject);
begin
  StateViewpointsDlg.FScene := Window.MainScene;
  StateViewpointsDlg.FOnViewpointSelected := @TButtonsHandler(nil).ViewpointSelected;
  TUIState.Push(StateViewpointsDlg);
end;

class procedure TButtonsHandler.ViewpointSelected(ViewpointIdx: integer);
begin
  CurrentViewpointIdx := ViewpointIdx;
  Window.MainScene.MoveToViewpoint(CurrentViewpointIdx);
end;

function MyGetApplicationName: string;
begin
  Result := 'view3dscene-mobile';
end;

initialization
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowTouch.Create(Application);
  Window.OnPress := @WindowPress;
  Window.OnUpdate := @WindowUpdate;
  Window.OnDropFiles := @WindowDropFiles;
  Window.FpsShowOnCaption := false;
  Window.AutomaticTouchInterface := true;
  Window.AutoRedisplay := false;
  Application.MainWindow := Window;

  OptimizeExtensiveTransformations := true;

finalization
end.
