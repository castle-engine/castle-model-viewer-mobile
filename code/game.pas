{
  Copyright 2017-2017 Michalis Kamburelis and Jan Adamec.

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
  CastleCameras, CastleWindow,
  CastleGLImages, CastleGLContainer,
  X3DNodes,
  V3DMOptions, V3DMOptionsDlg;

{ main game stuff ------------------------------------------------------------ }
type
  TButtonsHandler = class
    class procedure BtnNavWalkClick(Sender: TObject);
    class procedure BtnNavFlyClick(Sender: TObject);
    class procedure BtnNavExamineClick(Sender: TObject);
    class procedure BtnNavTurntableClick(Sender: TObject);
    class procedure BtnOptionsClick(Sender: TObject);
  end;

var
  BtnNavWalk, BtnNavFly, BtnNavExamine, BtnNavTurntable, BtnOptions: TCastleButton;
  Status: TCastleLabel;

{ One-time initialization. }
procedure ApplicationInitialize;
const
  ButtonPadding = 30;
begin
  BtnNavWalk := TCastleButton.Create(Window);
  BtnNavWalk.Caption := 'Walk';
  BtnNavWalk.OnClick := @TButtonsHandler(nil).BtnNavWalkClick;
  BtnNavWalk.Left := 0;
  BtnNavWalk.Bottom := 0;
  BtnNavWalk.PaddingHorizontal := ButtonPadding;
  BtnNavWalk.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(BtnNavWalk);

  BtnNavFly := TCastleButton.Create(Window);
  BtnNavFly.Caption := 'Fly';
  BtnNavFly.OnClick := @TButtonsHandler(nil).BtnNavFlyClick;
  BtnNavFly.Left := 150;
  BtnNavFly.Bottom := 0;
  BtnNavFly.PaddingHorizontal := ButtonPadding;
  BtnNavFly.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(BtnNavFly);

  BtnNavExamine := TCastleButton.Create(Window);
  BtnNavExamine.Caption := 'Examine';
  BtnNavExamine.OnClick := @TButtonsHandler(nil).BtnNavExamineClick;
  BtnNavExamine.Left := 300;
  BtnNavExamine.Bottom := 0;
  BtnNavExamine.PaddingHorizontal := ButtonPadding;
  BtnNavExamine.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(BtnNavExamine);

  BtnNavTurntable := TCastleButton.Create(Window);
  BtnNavTurntable.Caption := 'Turntable';
  BtnNavTurntable.OnClick := @TButtonsHandler(nil).BtnNavTurntableClick;
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

  Status := TCastleLabel.Create(Window);
  Status.Padding := 5;
  Status.Color := Red;
  Status.Left := 10;
  Status.Anchor(vpTop, -10);
  Window.Controls.InsertFront(Status);
end;

procedure WindowDropFiles(Container: TUIContainer; const FileNames: array of string);
var
  Url: string;
begin
  if Length(FileNames) = 0 then Exit;
  Url := FileNames[0];

  Application.Log(etInfo, 'Opened ' + Url);

  Window.Load(Url);
  Window.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  Window.MainScene.ProcessEvents := true;
end;

procedure WindowUpdate(Container: TUIContainer);
begin
  if Status.Exists <> AppOptions.ShowFps then
    Status.Exists := AppOptions.ShowFps;

  if Status.Exists then
    Status.Caption := Format('FPS: %f (real : %f)',
      [Window.Fps.OnlyRenderFps, Window.Fps.RealFps]);

end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(K_F5) then
    Window.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
  if Event.IsKey(K_Escape) then
    Application.Terminate;

end;

class procedure TButtonsHandler.BtnNavWalkClick(Sender: TObject);
begin
  Window.NavigationType := ntWalk;
end;

class procedure TButtonsHandler.BtnNavFlyClick(Sender: TObject);
begin
  Window.NavigationType := ntFly;
end;

class procedure TButtonsHandler.BtnNavExamineClick(Sender: TObject);
begin
  Window.NavigationType := ntExamine;
end;

class procedure TButtonsHandler.BtnNavTurntableClick(Sender: TObject);
begin
  Window.NavigationType := ntTurntable;
end;

class procedure TButtonsHandler.BtnOptionsClick(Sender: TObject);
begin
  TUIState.Push(StateOptionsDlg);
end;

function MyGetApplicationName: string;
begin
  Result := 'view3dscene-mobile';
end;

initialization
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;

  InitializeLog;

  AppOptions := TAppOptions.Create;
  AppOptions.Load;

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

  StateOptionsDlg := TStateOptionsDlg.Create(Application);

  OptimizeExtensiveTransformations := true;

finalization
end.
