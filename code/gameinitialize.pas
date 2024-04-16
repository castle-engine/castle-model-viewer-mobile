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

uses Classes, SysUtils, Math, Zipper,
  CastleWindow, CastleControls, CastleFilesUtils,
  CastleVectors, CastleUtils, CastleMessages, CastleLog,
  CastleDownload, CastleFileFilters, CastleUIControls, CastleColors, CastleImages,
  CastleApplicationProperties,
  GameViewInfo, GameOptions, GameViewOptions, GameViewViewpoints, GameViewFiles,
  GameNavToolbar, GameViewDisplayScene;

var
  Window: TCastleWindow;

{ One-time initialization. }
procedure ApplicationInitialize;
//var
  //CustomUIFont: TFontFamily;
begin
  AppOptions := TAppOptions.Create;
  AppOptions.Load;

  EnableBlockingDownloads := AppOptions.EnableBlockingDownloads;

  ViewDisplayScene := TViewDisplayScene.Create(Application);
  ViewInfo := TViewInfo.Create(Application);
  ViewOptions := TViewOptions.Create(Application);
  ViewViewpoints := TViewViewpoints.Create(Application);
  ViewFiles := TViewFiles.Create(Application);
  ViewNavToolbar := TViewNavToolbar.Create(Application);

  // Create UI
  Window.Container.UIExplicitScale := Window.Container.Dpi / 96.0;
  Window.Container.UIScaling := usExplicitScale;

  Theme.BackgroundColor := Vector4(0.1, 0.1, 0.1, 0.5);
  Theme.MessageTextColor := Silver;
  Theme.TextColor := Black;
  Theme.DisabledTextColor := Gray;

  Theme.ImagesPersistent[tiWindow].Url := 'castle-data:/theme_window.png'; // dialog background color and frame
  Theme.ImagesPersistent[tiButtonNormal].Url := 'castle-data:/theme_btnNormal.png';
  Theme.ImagesPersistent[tiButtonDisabled].Url := 'castle-data:/theme_btnDisabled.png';
  Theme.ImagesPersistent[tiButtonFocused].Url := 'castle-data:/theme_btnFocused.png';
  Theme.ImagesPersistent[tiButtonPressed].Url := 'castle-data:/theme_btnPressed.png';
  Theme.ImagesPersistent[tiScrollbarSlider].Url := 'castle-data:/theme_btnNormal.png';

  Theme.ImagesPersistent[tiButtonNormal].ProtectedSides.AllSides := 3;
  Theme.ImagesPersistent[tiButtonDisabled].ProtectedSides.AllSides := 3;
  Theme.ImagesPersistent[tiButtonFocused].ProtectedSides.AllSides := 3;
  Theme.ImagesPersistent[tiButtonPressed].ProtectedSides.AllSides := 3;
  Theme.ImagesPersistent[tiScrollbarSlider].ProtectedSides.AllSides := 3;

  { prepare TFontFamily with font varians for bold, italic }
  (* TODO: Assign custom font to use Html:=true on some labels.
     Or just don't use Html:=true, seems not really necessary for this application UI.

  CustomUIFont := TFontFamily.Create(Window);
  CustomUIFont.RegularFont := TTextureFont.Create(CustomUIFont);
  (CustomUIFont.RegularFont as TTextureFont).Load(TextureFont_DejaVuSans_20);
  CustomUIFont.BoldFont := TTextureFont.Create(CustomUIFont);
  (CustomUIFont.BoldFont as TTextureFont).Load(TextureFont_DejaVuSansBold_20);
  CustomUIFont.ItalicFont := TTextureFont.Create(CustomUIFont);
  (CustomUIFont.ItalicFont as TTextureFont).Load(TextureFont_DejaVuSansOblique_20);
  CustomUIFont.BoldItalicFont := TTextureFont.Create(CustomUIFont);
  (CustomUIFont.BoldItalicFont as TTextureFont).Load(TextureFont_DejaVuSansBoldOblique_20);
  CustomUIFont.Size := 15;
  Container.DefaultFont := CustomUIFont;
  *)

  Window.Container.View := ViewDisplayScene;
end;

initialization
  ApplicationProperties.ApplicationName := 'castle-model-viewer-mobile';

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Window.FpsShowOnCaption := false;
  Application.MainWindow := Window;

  // TODO: buggy now in CGE
  //OptimizeExtensiveTransformations := true;
end.
