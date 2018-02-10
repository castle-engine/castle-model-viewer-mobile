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

procedure OpenScene(const Url: string);
procedure ShowHideNavigationButtons(UpdateToobar: boolean);

implementation

uses Classes, SysUtils, Math,
  CastleControls, CastleKeysMouse, CastleFilesUtils, Castle2DSceneManager,
  CastleVectors, CastleBoxes, Castle3D, CastleSceneCore, CastleUtils, CastleColors,
  CastleUIControls, CastleUIState, CastleMessaging, CastleLog, CastleImages,
  CastleCameras, CastleApplicationProperties, CastleWindow, CastleScene,
  CastleGLImages, CastleFonts,
  CastleDialogStates,
  CastlePhotoService,
  X3DNodes,
  V3DMInfoDlg, V3DMOptions, V3DMOptionsDlg, V3DMViewpointsDlg, V3DMFilesDlg,
  V3DMNavToolbar;

{ main game stuff ------------------------------------------------------------ }
type
  TButtonsHandler = class
    class procedure BtnNavPopupClick(Sender: TObject);
    class procedure BtnNavClick(Sender: TObject);
    class procedure BtnScreenshotClick(Sender: TObject);
    class procedure BtnOptionsClick(Sender: TObject);
    class procedure BtnInfoClick(Sender: TObject);
    class procedure BtnFilesClick(Sender: TObject);
    class procedure FileSelected(Url: string);
    class procedure BtnViewpointNextClick(Sender: TObject);
    class procedure BtnViewpointListClick(Sender: TObject);
    class procedure ViewpointSelected(ViewpointIdx: integer);
    class procedure NavigationTypeInPopupSelected(NavType: TNavigationType);
    class procedure BoundNavigationInfoChanged(Sender: TObject);
    class procedure OnWarningHandle(Sender: TObject; const Category, S: string);
  end;

var
  BtnNavPopup, BtnNavWalk, BtnNavFly, BtnNavExamine, BtnNavTurntable, BtnOptions,
    BtnViewpointPrev, BtnViewpointNext, BtnViewpointList,
    BtnScreenshot, BtnInfo, BtnFiles: TCastleButton;
  ToolbarPanel: TCastlePanel;
  Status: TCastleLabel;

  CurrentViewpointIdx: integer;
  SceneBoundingBox: TCastleScene;
  BBoxTransform: TTransformNode;
  BBoxGeometry: TBoxNode;
  SceneWarnings: TStringList;
  SceneWarningsDlg: TStateDialogOK;
  AvailableNavTypes: TNavTypeList;
  ShowNavButtonsOnMainToolbar: boolean;

{ One-time initialization. }
procedure ApplicationInitialize;
const
  ButtonPadding = 3;
var
  ButtonsHeight: Cardinal;
  I: Integer;
  ToolButton: TCastleButton;
  ImgTriangle: TCastleImageControl;
begin
  AppOptions := TAppOptions.Create;
  AppOptions.Load;

  SceneBoundingBox := nil;
  CurrentViewpointIdx := 0;
  StateInfoDlg := TStateInfoDlg.Create(Application);
  StateOptionsDlg := TStateOptionsDlg.Create(Application);
  StateViewpointsDlg := TStateViewpointsDlg.Create(Application);
  StateFilesDlg := TStateFilesDlg.Create(Application);
  StateNavToolbarDlg := TStateNavToolbarDlg.Create(Application);
  SceneWarningsDlg := TStateDialogOK.Create(Application);

  SceneWarnings := TStringList.Create;

  AvailableNavTypes := TNavTypeList.Create;
  AvailableNavTypes.Add(ntExamine);
  ShowNavButtonsOnMainToolbar := true;

  Window.SceneManager.OnBoundNavigationInfoChanged := @TButtonsHandler(nil).BoundNavigationInfoChanged;
  ApplicationProperties.OnWarning.Add(@TButtonsHandler(nil).OnWarningHandle);

  // Create UI
  Window.Container.UIExplicitScale := Window.Dpi / 96.0;
  Window.Container.UIScaling := usExplicitScale;

  Theme.BackgroundColor := Vector4(0.1, 0.1, 0.1, 0.5);
  Theme.MessageTextColor := Silver;
  Theme.TextColor := Black;
  Theme.DisabledTextColor := Gray;

  Theme.Images[tiWindow] := CastleImages.LoadImage(ApplicationData('theme_window.png')); // dialog background color and frame
  Theme.Images[tiButtonNormal] := CastleImages.LoadImage(ApplicationData('theme_btnNormal.png'));
  Theme.Images[tiButtonDisabled] := CastleImages.LoadImage(ApplicationData('theme_btnDisabled.png'));
  Theme.Images[tiButtonFocused] := CastleImages.LoadImage(ApplicationData('theme_btnFocused.png'));
  Theme.Images[tiButtonPressed] := CastleImages.LoadImage(ApplicationData('theme_btnPressed.png'));
  Theme.OwnsImages[tiWindow] := true;
  Theme.OwnsImages[tiButtonNormal] := true;
  Theme.OwnsImages[tiButtonDisabled] := true;
  Theme.OwnsImages[tiButtonFocused] := true;
  Theme.OwnsImages[tiButtonPressed] := true;
  Theme.Corners[tiButtonNormal] := Vector4Integer(3, 3, 3, 3);
  Theme.Corners[tiButtonDisabled] := Vector4Integer(3, 3, 3, 3);
  Theme.Corners[tiButtonFocused] := Vector4Integer(3, 3, 3, 3);
  Theme.Corners[tiButtonPressed] := Vector4Integer(3, 3, 3, 3);

  Theme.Images[tiScrollbarSlider] := CastleImages.LoadImage(ApplicationData('theme_btnNormal.png'));
  Theme.OwnsImages[tiScrollbarSlider] := true;
  Theme.Corners[tiScrollbarSlider] := Vector4Integer(3, 3, 3, 3);

  UIFont.Size := 15;

  // toolbar
  ToolbarPanel := TCastlePanel.Create(Application);
  Window.Controls.InsertFront(ToolbarPanel);

  // add buttons to toolbar - Tag=1 marks button should not add space after it
  BtnNavPopup := TCastleButton.Create(ToolbarPanel);
  BtnNavPopup.Caption := ' ';  // leave space for triangle
  BtnNavPopup.Tooltip := 'Navigation type';
  BtnNavPopup.OnClick := @TButtonsHandler(nil).BtnNavPopupClick;
  ToolbarPanel.InsertFront(BtnNavPopup);
  ImgTriangle := TCastleImageControl.Create(BtnNavPopup);
  ImgTriangle.Image := CastleImages.LoadImage(ApplicationData('popup_triangle.png'));
  ImgTriangle.OwnsImage := true;
  ImgTriangle.Anchor(hpRight, -6);
  ImgTriangle.Anchor(vpMiddle);
  BtnNavPopup.InsertFront(ImgTriangle);

  BtnNavWalk := TCastleButton.Create(ToolbarPanel);
  BtnNavWalk.Tooltip := 'Walk';
  BtnNavWalk.Image := CastleImages.LoadImage(ApplicationData('nav_walk.png'));
  BtnNavWalk.OwnsImage := true;
  BtnNavWalk.OnClick := @TButtonsHandler(nil).BtnNavClick;
  BtnNavWalk.Toggle := true;
  BtnNavWalk.PaddingHorizontal := ButtonPadding;
  BtnNavWalk.PaddingVertical := ButtonPadding;
  BtnNavWalk.Tag := 1;
  ToolbarPanel.InsertFront(BtnNavWalk);
  ButtonsHeight := BtnNavWalk.CalculatedHeight;

  BtnNavFly := TCastleButton.Create(ToolbarPanel);
  BtnNavFly.Tooltip := 'Fly';
  BtnNavFly.Image := CastleImages.LoadImage(ApplicationData('nav_fly.png'));
  BtnNavFly.OwnsImage := true;
  BtnNavFly.OnClick := @TButtonsHandler(nil).BtnNavClick;
  BtnNavFly.Toggle := true;
  BtnNavFly.Tag := 1;
  ToolbarPanel.InsertFront(BtnNavFly);

  BtnNavExamine := TCastleButton.Create(ToolbarPanel);
  BtnNavExamine.Tooltip := 'Examine';
  BtnNavExamine.Image := CastleImages.LoadImage(ApplicationData('nav_examine.png'));
  BtnNavExamine.OwnsImage := true;
  BtnNavExamine.OnClick := @TButtonsHandler(nil).BtnNavClick;
  BtnNavExamine.Toggle := true;
  ToolbarPanel.InsertFront(BtnNavExamine);

  BtnNavTurntable := TCastleButton.Create(ToolbarPanel);
  BtnNavTurntable.Tooltip := 'Turntable';
  BtnNavTurntable.Image := CastleImages.LoadImage(ApplicationData('nav_turntable.png'));
  BtnNavTurntable.OwnsImage := true;
  BtnNavTurntable.OnClick := @TButtonsHandler(nil).BtnNavClick;
  BtnNavTurntable.Toggle := true;
  ToolbarPanel.InsertFront(BtnNavTurntable);

  BtnNavPopup.Image := BtnNavExamine.Image;
  BtnNavPopup.OwnsImage := false;

  BtnViewpointPrev := TCastleButton.Create(ToolbarPanel);
  BtnViewpointPrev.Tooltip := 'Previous viewpoint';
  BtnViewpointPrev.Image := CastleImages.LoadImage(ApplicationData('arrow-left-b.png'));
  BtnViewpointPrev.OwnsImage := true;
  BtnViewpointPrev.OnClick := @TButtonsHandler(nil).BtnViewpointNextClick;
  BtnViewpointPrev.Tag := 1;
  ToolbarPanel.InsertFront(BtnViewpointPrev);

  BtnViewpointList := TCastleButton.Create(ToolbarPanel);
  BtnViewpointList.Caption := 'Viewpoints';
  BtnViewpointList.OnClick := @TButtonsHandler(nil).BtnViewpointListClick;
  BtnViewpointList.Tag := 1;
  ToolbarPanel.InsertFront(BtnViewpointList);

  BtnViewpointNext := TCastleButton.Create(ToolbarPanel);
  BtnViewpointNext.Tooltip := 'Next viewpoint';
  BtnViewpointNext.Image := CastleImages.LoadImage(ApplicationData('arrow-right-b.png'));
  BtnViewpointNext.OwnsImage := true;
  BtnViewpointNext.OnClick := @TButtonsHandler(nil).BtnViewpointNextClick;
  ToolbarPanel.InsertFront(BtnViewpointNext);

  BtnScreenshot := TCastleButton.Create(ToolbarPanel);
  BtnScreenshot.Tooltip := 'Screenshot';
  BtnScreenshot.Image := CastleImages.LoadImage(ApplicationData('screenshot.png'));
  BtnScreenshot.OwnsImage := true;
  BtnScreenshot.OnClick := @TButtonsHandler(nil).BtnScreenshotClick;
  ToolbarPanel.InsertFront(BtnScreenshot);

  BtnOptions := TCastleButton.Create(ToolbarPanel);
  BtnOptions.Tooltip := 'Options';
  BtnOptions.Image := CastleImages.LoadImage(ApplicationData('gear-b.png'));
  BtnOptions.OwnsImage := true;
  BtnOptions.OnClick := @TButtonsHandler(nil).BtnOptionsClick;
  ToolbarPanel.InsertFront(BtnOptions);

  BtnFiles := TCastleButton.Create(ToolbarPanel);
  BtnFiles.Tooltip := 'Saved scenes';
  BtnFiles.Image := CastleImages.LoadImage(ApplicationData('file.png'));
  BtnFiles.OwnsImage := true;
  BtnFiles.OnClick := @TButtonsHandler(nil).BtnFilesClick;
  ToolbarPanel.InsertFront(BtnFiles);

  BtnInfo := TCastleButton.Create(ToolbarPanel);
  BtnInfo.Tooltip := 'About';
  BtnInfo.Image := CastleImages.LoadImage(ApplicationData('info-circle.png'));
  BtnInfo.OwnsImage := true;
  BtnInfo.OnClick := @TButtonsHandler(nil).BtnInfoClick;
  ToolbarPanel.InsertFront(BtnInfo);

  // style all toolbar buttons - make them flat (no background), apart from PressedState
  for I := 0 to ToolbarPanel.ControlsCount - 1 do
  begin
    if ToolbarPanel.Controls[I] is TCastleButton then
    begin
      ToolButton := ToolbarPanel.Controls[I] as TCastleButton;
      ToolButton.CustomBackground := true;
      ToolButton.CustomBackgroundPressed := Theme.Images[tiButtonPressed];
      ToolButton.CustomBackgroundCorners := Vector4Integer(3, 3, 3, 3);
      ToolButton.PaddingHorizontal := ButtonPadding;
      ToolButton.PaddingVertical := ButtonPadding;
      ToolButton.Height := ButtonsHeight;
      ToolButton.AutoSizeHeight := false;
    end;
  end;

  Status := TCastleLabel.Create(Window);
  Status.Caption := ' ';
  Status.Padding := 5;
  Status.Color := Red;
  Status.FontScale := SmallFontScale;
  Window.Controls.InsertFront(Status);

  // decide if to show all navigation buttons on the toolbar or not (i.e. hide on phones, show on tablets)
  ShowNavButtonsOnMainToolbar := (Min(Window.Container.UnscaledWidth, Window.Container.UnscaledHeight)
                                   > BtnNavWalk.CalculatedWidth * 10);
  if ShowNavButtonsOnMainToolbar then
    BtnNavPopup.Exists := false
  else begin
    BtnNavWalk.Exists := false;
    BtnNavFly.Exists := false;
    BtnNavExamine.Exists := false;
    BtnNavTurntable.Exists := false;
  end;

  // TODO: do not always open demo scene
  OpenScene(ApplicationData('demo/castle_walk.wrl'));
end;

procedure WindowResize(Container: TUIContainer);
const
  ToolbarMargin = 2;  {< between buttons and toolbar panel }
  ButtonsMargin = 3;  {< between buttons }
var
  ToolButton: TCastleButton;
  NextLeft1, NextLeft2, ButtonsHeight: Integer;
  I, ViewpointCount: Integer;
  SpaceForButtons: Integer;
  TwoLineToolbar: boolean;
begin
  if not ToolbarPanel.Exists then exit;

  ViewpointCount := Window.MainScene.ViewpointsCount;
  BtnViewpointPrev.Exists := (ViewpointCount > 1);
  BtnViewpointList.Exists := (ViewpointCount > 0);
  BtnViewpointNext.Exists := (ViewpointCount > 1);

  SpaceForButtons := 0;
  for I := 0 to ToolbarPanel.ControlsCount - 1 do
  begin
    if ToolbarPanel.Controls[I] is TCastleButton then
    begin
      ToolButton := ToolbarPanel.Controls[I] as TCastleButton;
      if ToolButton.Exists then
      begin
        SpaceForButtons := SpaceForButtons + ToolButton.CalculatedWidth;
        if ToolButton.Tag = 0 then
          SpaceForButtons := SpaceForButtons + ButtonsMargin;
      end;
    end;
  end;

  ButtonsHeight := Max(BtnNavExamine.CalculatedHeight, BtnOptions.CalculatedHeight);

  // test if all buttons fit on one line
  TwoLineToolbar := (SpaceForButtons + 2*ToolbarMargin > Container.UnscaledWidth);
  if TwoLineToolbar then
    ToolbarPanel.Height := 2*ButtonsHeight + 3*ToolbarMargin + Container.StatusBarHeight { window extends below top status bar (clock, battery)}
  else
    ToolbarPanel.Height := ButtonsHeight + 2*ToolbarMargin + Container.StatusBarHeight;

  // toolbar
  ToolbarPanel.Left := 0;
  ToolbarPanel.Width := Container.UnscaledWidth;
  ToolbarPanel.Bottom := Container.UnscaledHeight - ToolbarPanel.Height;

  NextLeft1 := ToolbarMargin;
  NextLeft2 := ToolbarMargin;

  for I := 0 to ToolbarPanel.ControlsCount - 1 do
  begin
    if ToolbarPanel.Controls[I] is TCastleButton then
    begin
      ToolButton := ToolbarPanel.Controls[I] as TCastleButton;
      if ToolButton.Exists then
      begin
        // put viewpoints on next line, let's hope it's enough
        if TwoLineToolbar and ((ToolButton = BtnViewpointPrev) or (ToolButton = BtnViewpointList) or (ToolButton = BtnViewpointNext)) then
        begin
           // 2nd line
          ToolButton.Left := NextLeft2;
          ToolButton.Bottom := ToolbarMargin;

          NextLeft2 := NextLeft2 + ToolButton.CalculatedWidth;
          if ToolButton.Tag = 0 then
            NextLeft2 := NextLeft2 + ButtonsMargin;
        end
        else begin
           // 1st line
           ToolButton.Left := NextLeft1;
           if TwoLineToolbar then
             ToolButton.Bottom := ToolbarMargin + ButtonsHeight + ToolbarMargin
           else
             ToolButton.Bottom := ToolbarMargin;

           NextLeft1 := NextLeft1 + ToolButton.CalculatedWidth;
           if ToolButton.Tag = 0 then
             NextLeft1 := NextLeft1 + ButtonsMargin;
        end;

      end;
    end;
  end;

  // status text (FPS)
  Status.Left := 10;
  Status.Bottom := ToolbarPanel.Bottom - ButtonsMargin - Status.CalculatedHeight;
end;

procedure InitializeSceneBoundingBox;
var
  RootNode: TX3DRootNode;
  BBox: TBox3D;
  ShapeNode: TShapeNode;
  Material: TMaterialNode;
begin
  BBox := Window.MainScene.BoundingBox;
  BBoxGeometry := TBoxNode.Create;
  BBoxGeometry.Size := BBox.Size;

  Material := TMaterialNode.Create;
  Material.ForcePureEmissive;
  Material.EmissiveColor := GreenRGB;

  ShapeNode := TShapeNode.Create;
  ShapeNode.Geometry := BBoxGeometry;
  ShapeNode.Shading := shWireframe;
  ShapeNode.Material := Material;
  ShapeNode.Appearance.ShadowCaster := false;

  BBoxTransform := TTransformNode.Create;
  BBoxTransform.Translation := BBox.Center;
  BBoxTransform.AddChildren(ShapeNode);

  RootNode := TX3DRootNode.Create;
  RootNode.AddChildren(BBoxTransform);

  SceneBoundingBox := TCastleScene.Create(Window.MainScene);
  SceneBoundingBox.Load(RootNode, true);

  Window.MainScene.Add(SceneBoundingBox);
  SceneBoundingBox.Exists := AppOptions.ShowBBox;
  SceneBoundingBox.ExcludeFromStatistics := true;
  SceneBoundingBox.Collides := false;
end;

class procedure TButtonsHandler.OnWarningHandle(Sender: TObject; const Category, S: string);
begin
  SceneWarnings.Add(Category + ': ' + S);

  // show only when not other warnings arrive within that timer
  if TUIState.Current = SceneWarningsDlg then
    TUIState.Pop;
  SceneWarningsDlg.Caption := SceneWarnings.Text;
  SceneWarningsDlg.KeepInFront := true;
  TUIState.Push(SceneWarningsDlg);
end;

procedure OpenScene(const Url: string);
begin
  SceneBoundingBox := nil;
  SceneWarnings.Clear;

  Application.Log(etInfo, 'Opened ' + Url);

  Window.Load(Url);
  Window.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  Window.MainScene.ProcessEvents := true;

  Window.MainScene.Collides := AppOptions.CollisionsOn;

  CurrentViewpointIdx := 0;

  ShowHideNavigationButtons(false);

  WindowResize(Window.Container); // to hide viewpoints, etc

  InitializeSceneBoundingBox;
end;

procedure WindowDropFiles(Container: TUIContainer; const FileNames: array of string);
begin
  if Length(FileNames) <> 0 then
    OpenScene(FileNames[0]);
end;

procedure WindowUpdate(Container: TUIContainer);
var
  BBox: TBox3D;
begin
  if Status.Exists <> AppOptions.ShowFps then
    Status.Exists := AppOptions.ShowFps;

  if Status.Exists then
    Status.Caption := 'FPS: ' + Window.Fps.ToString;

  if (Window.MainScene <> nil) and (SceneBoundingBox <> nil) then
  begin
    SceneBoundingBox.Exists := AppOptions.ShowBBox;
    if AppOptions.ShowBBox then
    begin
      BBox := Window.MainScene.BoundingBox;
      BBoxGeometry.Size := BBox.Size;
      BBoxTransform.Translation := BBox.Center
    end;
  end;
end;

class procedure TButtonsHandler.BtnNavPopupClick(Sender: TObject);
begin
  StateNavToolbarDlg.FAvailableNavTypes := AvailableNavTypes;
  StateNavToolbarDlg.FSelectedNavType := Window.NavigationType;
  StateNavToolbarDlg.FOnNavTypeSelected := @TButtonsHandler(nil).NavigationTypeInPopupSelected;
  StateNavToolbarDlg.FShowAtPositionLeft := BtnNavPopup.Left;
  StateNavToolbarDlg.FShowAtPositionTop := Window.Container.UnscaledHeight - ToolbarPanel.Bottom + 2;
  TUIState.Push(StateNavToolbarDlg);
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

class procedure TButtonsHandler.NavigationTypeInPopupSelected(NavType: TNavigationType);
begin
  Window.NavigationType := NavType;
end;

class procedure TButtonsHandler.BoundNavigationInfoChanged(Sender: TObject);
var
  NavType: TNavigationType;
begin
  { this may be called when Window, and everythig it owned (like BtnNavWalk)
    is getting destroyed }
  if csDestroying in Window.ComponentState then
    Exit;

  NavType := Window.NavigationType;
  BtnNavWalk.Pressed := (NavType = ntWalk);
  BtnNavFly.Pressed := (NavType = ntFly);
  BtnNavExamine.Pressed := (NavType = ntExamine);
  BtnNavTurntable.Pressed := (NavType = ntTurntable);

  case NavType of
    ntWalk:    BtnNavPopup.Image := BtnNavWalk.Image;
    ntFly:     BtnNavPopup.Image := BtnNavFly.Image;
    ntExamine: BtnNavPopup.Image := BtnNavExamine.Image;
    ntTurntable: BtnNavPopup.Image := BtnNavTurntable.Image;
  end;

  ShowHideNavigationButtons(true);
end;

procedure ShowHideNavigationButtons(UpdateToobar: boolean);
var
  CurrentNavNode: TNavigationInfoNode;
  AnyTypePresent, WalkPresent, FlyPresent, ExaminePresent, TurntablePresent: boolean;
  I: Integer;
  TypeString: string;
  NeedsToolbarUpdate: boolean;

  function ShowNavButton(Btn: TCastleButton; ShowButton: boolean): boolean;
  begin
    Result := (Btn.Exists <> ShowButton);
    if Result then
      Btn.Exists := ShowButton;
  end;

begin
  if Window.MainScene = nil then
    exit;

  AnyTypePresent := false;
  WalkPresent := false;
  FlyPresent := false;
  ExaminePresent := false;
  TurntablePresent := false;

  CurrentNavNode := Window.MainScene.NavigationInfoStack.Top;
  if CurrentNavNode = nil then
    ExaminePresent := true
  else begin
    for I := 0 to CurrentNavNode.FdType.Items.Count - 1 do
    begin
      TypeString := CurrentNavNode.FdType.ItemsSafe[I];
      if CompareText(TypeString, 'ANY') = 0 then AnyTypePresent := true
      else if CompareText(TypeString, 'WALK') = 0 then WalkPresent := true
      else if CompareText(TypeString, 'FLY') = 0 then FlyPresent := true
      else if CompareText(TypeString, 'EXAMINE') = 0 then ExaminePresent := true
      else if CompareText(TypeString, 'TURNTABLE') = 0 then TurntablePresent := true;
    end;
  end;

  { When only default VRML 97 types set, it probably means the 3D file did not
    contain any navigationInfo node. IMHO it's better to show only Examine. }
  if ExaminePresent and AnyTypePresent and (not WalkPresent)
      and (not FlyPresent) and (not TurntablePresent) then
    AnyTypePresent := false;

  { Option to show all navigation types }
  if AppOptions.ShowAllNavgationButtons then
    AnyTypePresent := true;

  AvailableNavTypes.Clear;
  if WalkPresent or AnyTypePresent then AvailableNavTypes.Add(ntWalk);
  if FlyPresent or AnyTypePresent then AvailableNavTypes.Add(ntFly);
  if ExaminePresent or AnyTypePresent then AvailableNavTypes.Add(ntExamine);
  if TurntablePresent or AnyTypePresent then AvailableNavTypes.Add(ntTurntable);

  { Update the visibility of toolbar buttons }
  if ShowNavButtonsOnMainToolbar then
  begin
    NeedsToolbarUpdate := ShowNavButton(BtnNavWalk, AvailableNavTypes.Contains(ntWalk));
    NeedsToolbarUpdate := ShowNavButton(BtnNavFly, AvailableNavTypes.Contains(ntFly)) or NeedsToolbarUpdate;
    NeedsToolbarUpdate := ShowNavButton(BtnNavExamine, AvailableNavTypes.Contains(ntExamine)) or NeedsToolbarUpdate;
    NeedsToolbarUpdate := ShowNavButton(BtnNavTurntable, AvailableNavTypes.Contains(ntTurntable)) or NeedsToolbarUpdate;
    if UpdateToobar and NeedsToolbarUpdate then
      WindowResize(Window.Container);
  end;
end;

class procedure TButtonsHandler.BtnScreenshotClick(Sender: TObject);
var
  Image: TRGBImage;
  Filename: string;
  RestoreCtls: TUIControlList;
  I: Integer;
  C: TUIControl;
begin
  RestoreCtls := TUIControlList.Create(false);
  try
    // hide everything except SceneManager
    for I := 0 to Window.Controls.Count - 1 do
    begin
      C := Window.Controls[I];
      if C.Exists and (C <> Window.SceneManager) then
      begin
        C.Exists := false;
        RestoreCtls.InsertFront(C);
      end;
    end;
    // make screenshot
    Image := Window.SaveScreen;
    try
      Filename := ApplicationConfig('screenshot.png');
      SaveImage(Image, Filename);
      TPhotoService.StoreImage(Filename);
    finally FreeAndNil(Image) end;
    // restore hidden controls
    for I := 0 to RestoreCtls.Count - 1 do
      RestoreCtls[I].Exists := true;
  finally FreeAndNil(RestoreCtls) end;
end;

class procedure TButtonsHandler.BtnOptionsClick(Sender: TObject);
begin
  StateOptionsDlg.FScene := Window.MainScene;
  TUIState.Push(StateOptionsDlg);
end;

class procedure TButtonsHandler.BtnInfoClick(Sender: TObject);

  function SceneVertexTriangleInfo(const Scene: TCastleScene): string;
  const
    SSceneInfoTriVertCounts_Same = 'Scene contains %d triangles and %d ' +
      'vertices (with and without over-triangulating).';
    SSceneInfoTriVertCounts_1 =
      'When we don''t use over-triangulating (e.g. when we do collision '+
      'detection or ray tracing) scene has %d triangles and %d vertices.';
    SSceneInfoTriVertCounts_2 =
      'When we use over-triangulating (e.g. when we do OpenGL rendering) '+
      'scene has %d triangles and %d vertices.';
  begin
    if (Scene.VerticesCount(false) = Scene.VerticesCount(true)) and
       (Scene.TrianglesCount(false) = Scene.TrianglesCount(true)) then
      Result := Format(SSceneInfoTriVertCounts_Same,
        [Scene.TrianglesCount(false), Scene.VerticesCount(false)]) + NL else
    begin
      Result :=
        Format(SSceneInfoTriVertCounts_1,
          [Scene.TrianglesCount(false), Scene.VerticesCount(false)]) + NL +
        Format(SSceneInfoTriVertCounts_2,
          [Scene.TrianglesCount(true), Scene.VerticesCount(true)]) + NL;
    end;
  end;

  function SceneBoundingBoxInfo(const Scene: TCastleScene): string;
  var
    BBox: TBox3D;
  begin
    BBox := Scene.BoundingBox;
    Result := 'Bounding box : ' + BBox.ToString;
    if not BBox.IsEmpty then
    begin
      Result := Result + Format(', average size : %f', [BBox.AverageSize]);
    end;
    Result := Result + NL;
  end;

  function SceneRenderedShapes: string;
  var
    Statistics: TRenderStatistics;
  begin
    Statistics := Window.SceneManager.Statistics;
    Result := Format('Rendered shapes: %d / %d',
           [Statistics.ShapesRendered, Statistics.ShapesVisible]);
  end;

begin

  StateInfoDlg.FScene := Window.MainScene;
  StateInfoDlg.FStatistics := 'Scene information:' + NL
                           + SceneVertexTriangleInfo(Window.MainScene)
                           + SceneBoundingBoxInfo(Window.MainScene)
                           + SceneRenderedShapes;
  TUIState.Push(StateInfoDlg);
end;

class procedure TButtonsHandler.BtnFilesClick(Sender: TObject);
begin
  StateFilesDlg.FOnFileSelected := @TButtonsHandler(nil).FileSelected;
  TUIState.Push(StateFilesDlg);
end;

class procedure TButtonsHandler.FileSelected(Url: string);
begin
  OpenScene(Url);
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
  StateViewpointsDlg.FCurrentViewpointIdx := CurrentViewpointIdx;
  StateViewpointsDlg.FOnViewpointSelected := @TButtonsHandler(nil).ViewpointSelected;
  TUIState.Push(StateViewpointsDlg);
end;

class procedure TButtonsHandler.ViewpointSelected(ViewpointIdx: integer);
begin
  CurrentViewpointIdx := ViewpointIdx;
  Window.MainScene.MoveToViewpoint(CurrentViewpointIdx);
end;

initialization
  ApplicationProperties.ApplicationName := 'view3dscene-mobile';

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowTouch.Create(Application);
  Window.OnResize := @WindowResize;
  Window.OnUpdate := @WindowUpdate;
  Window.OnDropFiles := @WindowDropFiles;
  Window.FpsShowOnCaption := false;
  Window.AutomaticTouchInterface := true;
  Window.AutoRedisplay := false;
  Application.MainWindow := Window;

  OptimizeExtensiveTransformations := true;

finalization
  FreeAndNil(SceneWarnings);
  FreeAndNil(AvailableNavTypes);
end.
