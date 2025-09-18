{
  Copyright 2017-2024 Michalis Kamburelis and Jan Adamec.

  This file is part of "castle-model-viewer-mobile".

  "castle-model-viewer-mobile" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle-model-viewer-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle-model-viewer-mobile"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Initialize the application, independent from the platform. }
unit GameInitialize;

interface

implementation

uses Classes, SysUtils, Math,
  CastleWindow, CastleControls, CastleFilesUtils,
  CastleVectors, CastleUtils, CastleMessages, CastleLog,
  CastleDownload, CastleFileFilters, CastleUIControls, CastleColors, CastleImages,
  CastleApplicationProperties, CastleSceneCore, CastleHttps,
  GameOptions, GameScreenShotArbitrarySize
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameViewDisplayScene
  , GameViewOptions
  , GameViewFiles
  , GameViewAbout
  , GameViewChoice
  , GameViewNavigation
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindow;

{ One-time initialization. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  OptimizeExtensiveTransformations := true;

  AppOptions := TAppOptions.Create;
  AppOptions.Load;

  EnableBlockingDownloads := AppOptions.EnableBlockingDownloads;

  // We use MessageOK here, we need MessageOKPushesView to make it work on iOS
  MessageOKPushesView := true;

  { Create views (see https://castle-engine.io/views ). }
  {$region 'Castle View Creation'}
  // The content here may be automatically updated by CGE editor.
  ViewDisplayScene := TViewDisplayScene.Create(Application);
  ViewOptions := TViewOptions.Create(Application);
  ViewFiles := TViewFiles.Create(Application);
  ViewAbout := TViewAbout.Create(Application);
  ViewChoice := TViewChoice.Create(Application);
  ViewNavigation := TViewNavigation.Create(Application);
  {$endregion 'Castle View Creation'}

  Window.Container.View := ViewDisplayScene;

  // uncomment to use ScreenShotArbitrarySize
  // ScreenShotArbitrarySize := TScreenShotArbitrarySize.Create(Application);
  // Window.Container.Controls.InsertFront(ScreenShotArbitrarySize);
end;

initialization
  { This initialization section configures:
    - Application.OnInitialize
    - Application.MainWindow
    - determines initial window size

    You should not need to do anything more in this initialization section.
    Most of your actual application initialization (in particular, any file reading)
    should happen inside ApplicationInitialize. }

  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;
end.
