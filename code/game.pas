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

uses SysUtils, Math,
  CastleControls, CastleKeysMouse, CastleFilesUtils, Castle2DSceneManager,
  CastleVectors, Castle3D, CastleSceneCore, CastleUtils, CastleColors,
  CastleUIControls, CastleMessaging, CastleLog,
  CastleCameras, CastleWindow;

{ main game stuff ------------------------------------------------------------ }

var
  ShowAchievements: TCastleButton;
  Status: TCastleLabel;

type
  TButtonsHandler = class
    class procedure ShowAchievementsClick(Sender: TObject);
  end;

{ One-time initialization. }
procedure ApplicationInitialize;
const
  ButtonPadding = 30;
begin
  ShowAchievements := TCastleButton.Create(Window);
  ShowAchievements.Caption := 'Show Achievements';
  ShowAchievements.OnClick := @TButtonsHandler(nil).ShowAchievementsClick;
  ShowAchievements.Left := 10;
  ShowAchievements.Bottom := 190;
  ShowAchievements.PaddingHorizontal := ButtonPadding;
  ShowAchievements.PaddingVertical := ButtonPadding;
  Window.Controls.InsertFront(ShowAchievements);

  Status := TCastleLabel.Create(Window);
  Status.Padding := 5;
  Status.Color := Red;
  Status.Left := 10;
  Status.Anchor(vpTop, -10);
  Window.Controls.InsertFront(Status);
end;


procedure WindowUpdate(Container: TUIContainer);
begin
  Status.Caption := Format('FPS: %f (real : %f)',
    [Window.Fps.FrameTime, Window.Fps.RealTime]);


end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
var
  S: TVector3;
begin
  if Event.IsKey(K_F5) then
    Window.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
  if Event.IsKey(K_Escape) then
    Application.Terminate;

end;


class procedure TButtonsHandler.ShowAchievementsClick(Sender: TObject);
begin
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
  Window.FpsShowOnCaption := true;
  Application.MainWindow := Window;

  OptimizeExtensiveTransformations := true;
finalization
end.
