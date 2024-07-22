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

{ Main view that displays 3D model. }
unit GameViewDisplayScene;

interface

uses Classes,
  CastleUIControls, CastleControls, CastleScene, X3DNodes, CastleViewport,
  CastleDialogViews;

type
  TViewDisplayScene = class(TCastleView)
  private
    BtnNavWalk, BtnNavFly, BtnNavExamine, BtnNavTurntable, BtnOptions,
      BtnViewpointPrev, BtnViewpointNext, BtnViewpointList,
      BtnScreenshot, BtnInfo, BtnFiles: TCastleButton;
    ToolbarPanel: TCastlePanel;
    Status: TCastleLabel;

    CurrentViewpointIdx: integer;
    BBoxScene: TCastleScene;
    BBoxTransform: TTransformNode;
    BBoxGeometry: TBoxNode;
    SceneWarnings: TStringList;
    AvailableNavTypes: TNavTypeList;

    MainViewport: TCastleAutoNavigationViewport;
    TouchNavigation: TCastleTouchNavigation;

    { CGE event handlers }
    { }
    procedure BtnNavClick(Sender: TObject);
    procedure BtnScreenshotClick(Sender: TObject);
    procedure BtnOptionsClick(Sender: TObject);
    procedure BtnInfoClick(Sender: TObject);
    procedure BtnFilesClick(Sender: TObject);
    procedure FileSelected(Url: string);
    procedure BtnViewpointNextClick(Sender: TObject);
    procedure BtnViewpointListClick(Sender: TObject);
    procedure ViewpointSelected(ViewpointIdx: integer);
    procedure NavigationTypeInPopupSelected(NavType: TNavigationType);
    procedure BoundNavigationInfoChanged(Sender: TObject);
    procedure OnWarningHandle(const Category, S: string);

    { Currently loaded scene. May be @nil only before first OpenScene from Start. }
    function MainScene: TCastleScene;
    function GetSceneUnpackDir: string;
    procedure OpenZippedScene(const Url: string);
    procedure DropFiles(const FileNames: array of string);
    { Call each frame to update BBox* values to match current MainScene.BoundingBox. }
    procedure UpdateBBox;
  public
    procedure Start; override;
    procedure Stop; override;
    procedure Resize; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;

    procedure ShowHideNavigationButtons(UpdateToobar: boolean);
    procedure OpenScene(const Url: string);
  end;

var
  ViewDisplayScene: TViewDisplayScene;

implementation

uses SysUtils, Math, Zipper,
  CastleImages, CastleFilesUtils, CastleWindow, CastleColors, CastleBoxes,
  CastleApplicationProperties, CastleUtils, CastlePhotoService, CastleLog,
  CastleMessages, CastleFileFilters, X3DLoad, CastleParameters,
  CastleRenderOptions, CastleUriUtils, CastleVectors,
  GameViewInfo, GameOptions, GameViewOptions, GameViewFiles, GameViewViewpoints;

procedure GlobalDropFiles(AContainer: TCastleContainer;
  const FileNames: array of string);
begin
  if ViewDisplayScene <> nil then
    ViewDisplayScene.DropFiles(FileNames);
end;

{ TViewDisplayScene ---------------------------------------------------------- }

procedure TViewDisplayScene.Start;

  { Create BBox* stuff. }
  procedure InitializeBBox;
  var
    RootNode: TX3DRootNode;
    BBoxShape: TShapeNode;
    Appearance: TAppearanceNode;
    Material: TUnlitMaterialNode;
  begin
    BBoxGeometry := TBoxNode.Create;

    Material := TUnlitMaterialNode.Create;
    Material.EmissiveColor := GreenRGB;

    Appearance := TAppearanceNode.Create;
    Appearance.Material := Material;

    BBoxShape := TShapeNode.Create;
    BBoxShape.Geometry := BBoxGeometry;
    BBoxShape.Shading := shWireframe;
    BBoxShape.Appearance := Appearance;
    BBoxShape.Appearance.ShadowCaster := false;

    BBoxTransform := TTransformNode.Create;
    BBoxTransform.AddChildren(BBoxShape);

    RootNode := TX3DRootNode.Create;
    RootNode.AddChildren(BBoxTransform);

    BBoxScene := TCastleScene.Create(FreeAtStop);
    BBoxScene.Load(RootNode, true);

    { Note: It's critical that BBoxScene is *not* child of MainScene,
      so that it doesn't influence MainScene.BoundingBox,
      preventing bbox visualization from getting smaller. }
    MainViewport.Items.Add(BBoxScene);
    BBoxScene.Collides := false;
  end;

const
  ButtonPadding = 3;
var
  ButtonsHeight: Single;
  I: Integer;
  ToolButton: TCastleButton;
  ImgTriangle: TCastleImageControl;
begin
  inherited;

  CurrentViewpointIdx := 0;

  SceneWarnings := TStringList.Create;

  AvailableNavTypes := TNavTypeList.Create;
  AvailableNavTypes.Add(ntExamine);

  { TODO: Design UI in editor }

  MainViewport := TCastleAutoNavigationViewport.Create(FreeAtStop);
  MainViewport.FullSize := true;
  MainViewport.AutoCamera := true;
  MainViewport.AutoNavigation := true;
  MainViewport.OnBoundNavigationInfoChanged := {$ifdef FPC}@{$endif} BoundNavigationInfoChanged;
  MainViewport.PreventInfiniteFallingDown := true;
  InsertFront(MainViewport);

  TouchNavigation := TCastleTouchNavigation.Create(FreeAtStop);
  TouchNavigation.FullSize := true;
  TouchNavigation.Viewport := MainViewport;
  TouchNavigation.AutoTouchInterface := true;
  TouchNavigation.AutoWalkTouchInterface := tiWalkRotate; // show both walk and rotate gizmos, more obvious for users
  TouchNavigation.AutoExamineTouchInterface := tiNone; // use 2-finger gesture to pan, not touchControl
  MainViewport.InsertFront(TouchNavigation);

  // toolbar
  ToolbarPanel := TCastlePanel.Create(FreeAtStop);
  InsertFront(ToolbarPanel);

  // add buttons to toolbar (Tag=1 marks button should not add space after it)
  BtnNavWalk := TCastleButton.Create(ToolbarPanel);
  BtnNavWalk.Tooltip := 'Walk';
  BtnNavWalk.Image.Url := 'castle-data:/nav_walk.png';
  BtnNavWalk.OnClick := {$ifdef FPC}@{$endif} BtnNavClick;
  BtnNavWalk.Toggle := true;
  BtnNavWalk.PaddingHorizontal := ButtonPadding;
  BtnNavWalk.PaddingVertical := ButtonPadding;
  BtnNavWalk.Tag := 1;
  ToolbarPanel.InsertFront(BtnNavWalk);
  ButtonsHeight := BtnNavWalk.EffectiveHeight;

  BtnNavFly := TCastleButton.Create(ToolbarPanel);
  BtnNavFly.Tooltip := 'Fly';
  BtnNavFly.Image.Url := 'castle-data:/nav_fly.png';
  BtnNavFly.OnClick := {$ifdef FPC}@{$endif} BtnNavClick;
  BtnNavFly.Toggle := true;
  BtnNavFly.Tag := 1;
  ToolbarPanel.InsertFront(BtnNavFly);

  BtnNavExamine := TCastleButton.Create(ToolbarPanel);
  BtnNavExamine.Tooltip := 'Examine';
  BtnNavExamine.Image.Url := 'castle-data:/nav_examine.png';
  BtnNavExamine.OnClick := {$ifdef FPC}@{$endif} BtnNavClick;
  BtnNavExamine.Toggle := true;
  ToolbarPanel.InsertFront(BtnNavExamine);

  BtnNavTurntable := TCastleButton.Create(ToolbarPanel);
  BtnNavTurntable.Tooltip := 'Turntable';
  BtnNavTurntable.Image.Url := 'castle-data:/nav_turntable.png';
  BtnNavTurntable.OnClick := {$ifdef FPC}@{$endif} BtnNavClick;
  BtnNavTurntable.Toggle := true;
  ToolbarPanel.InsertFront(BtnNavTurntable);

  BtnViewpointPrev := TCastleButton.Create(ToolbarPanel);
  BtnViewpointPrev.Tooltip := 'Previous viewpoint';
  BtnViewpointPrev.Image.Url := 'castle-data:/arrow-left-b.png';
  BtnViewpointPrev.OnClick := {$ifdef FPC}@{$endif} BtnViewpointNextClick;
  BtnViewpointPrev.Tag := 1;
  ToolbarPanel.InsertFront(BtnViewpointPrev);

  BtnViewpointList := TCastleButton.Create(ToolbarPanel);
  BtnViewpointList.Caption := 'Viewpoints';
  BtnViewpointList.OnClick := {$ifdef FPC}@{$endif} BtnViewpointListClick;
  BtnViewpointList.Tag := 1;
  ToolbarPanel.InsertFront(BtnViewpointList);

  BtnViewpointNext := TCastleButton.Create(ToolbarPanel);
  BtnViewpointNext.Tooltip := 'Next viewpoint';
  BtnViewpointNext.Image.Url := 'castle-data:/arrow-right-b.png';
  BtnViewpointNext.OnClick := {$ifdef FPC}@{$endif} BtnViewpointNextClick;
  ToolbarPanel.InsertFront(BtnViewpointNext);

  BtnScreenshot := TCastleButton.Create(ToolbarPanel);
  BtnScreenshot.Tooltip := 'Screenshot';
  BtnScreenshot.Image.Url := 'castle-data:/screenshot.png';
  BtnScreenshot.OnClick := {$ifdef FPC}@{$endif} BtnScreenshotClick;
  ToolbarPanel.InsertFront(BtnScreenshot);

  BtnOptions := TCastleButton.Create(ToolbarPanel);
  BtnOptions.Tooltip := 'Options';
  BtnOptions.Image.Url := 'castle-data:/gear-b.png';
  BtnOptions.OnClick := {$ifdef FPC}@{$endif} BtnOptionsClick;
  ToolbarPanel.InsertFront(BtnOptions);

  BtnFiles := TCastleButton.Create(ToolbarPanel);
  BtnFiles.Tooltip := 'Saved scenes';
  BtnFiles.Image.Url := 'castle-data:/file.png';
  BtnFiles.OnClick := {$ifdef FPC}@{$endif} BtnFilesClick;
  ToolbarPanel.InsertFront(BtnFiles);

  BtnInfo := TCastleButton.Create(ToolbarPanel);
  BtnInfo.Tooltip := 'About';
  BtnInfo.Image.Url := 'castle-data:/info-circle.png';
  BtnInfo.OnClick := {$ifdef FPC}@{$endif} BtnInfoClick;
  ToolbarPanel.InsertFront(BtnInfo);

  // style all toolbar buttons - make them flat (no background), apart from PressedState
  for I := 0 to ToolbarPanel.ControlsCount - 1 do
  begin
    if ToolbarPanel.Controls[I] is TCastleButton then
    begin
      ToolButton := ToolbarPanel.Controls[I] as TCastleButton;
      ToolButton.CustomBackground := true;
      ToolButton.CustomBackgroundPressed.Image := Theme.ImagesPersistent[tiButtonPressed].Image;
      ToolButton.CustomBackgroundPressed.OwnsImage := false;
      ToolButton.CustomBackgroundPressed.ProtectedSides.AllSides := 3;
      ToolButton.PaddingHorizontal := ButtonPadding;
      ToolButton.PaddingVertical := ButtonPadding;
      ToolButton.Height := ButtonsHeight;
      ToolButton.AutoSizeHeight := false;
    end;
  end;

  Status := TCastleLabel.Create(FreeAtStop);
  Status.Caption := ' ';
  Status.Padding := 5;
  Status.Color := Red;
  Status.FontScale := SmallFontScale;
  InsertFront(Status);

  Application.MainWindow.OnDropFiles := {$ifdef FPC}@{$endif} GlobalDropFiles;

  InitializeBBox;
  UpdateBBox;

  { Do not use Parameters[1] on Android and iOS.

    E.g. on Android, Parameters[1] may contain "ene.mobile"
    -- suffix of our qualified name. I don't know why,
    but these are not a useful URL to open naturally. }
  if (not IsLibrary) and (Parameters.High >= 1) then
    OpenScene(Parameters[1])
  else
    OpenScene('castle-data:/demo/castle_walk.wrl');
end;

procedure TViewDisplayScene.Stop;
begin
  FreeAndNil(SceneWarnings);
  FreeAndNil(AvailableNavTypes);
  BBoxScene := nil; // freed by FreeAtStop
  inherited;
end;

procedure TViewDisplayScene.DropFiles(const FileNames: array of string);
begin
  if Length(FileNames) <> 0 then
    OpenScene(FileNames[0]);
end;

function TViewDisplayScene.MainScene: TCastleScene;
begin
  Result := MainViewport.Items.MainScene;
end;

procedure TViewDisplayScene.Resize;
const
  ToolbarMargin = 2;  {< between buttons and toolbar panel }
  ButtonsMargin = 3;  {< between buttons }
var
  ToolButton: TCastleButton;
  NextLeft1, NextLeft2, ButtonsHeight: Single;
  I, ViewpointCount: Integer;
  SpaceForButtons: Single;
  TwoLineToolbar: boolean;
begin
  inherited;
  if not ToolbarPanel.Exists then exit;

  ViewpointCount := MainScene.ViewpointsCount;
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
        SpaceForButtons := SpaceForButtons + ToolButton.EffectiveWidth;
        if ToolButton.Tag = 0 then
          SpaceForButtons := SpaceForButtons + ButtonsMargin;
      end;
    end;
  end;

  ButtonsHeight := Max(BtnNavExamine.EffectiveHeight, BtnOptions.EffectiveHeight);

  // test if all buttons fit on one line
  TwoLineToolbar := (SpaceForButtons + 2*ToolbarMargin > Container.UnscaledWidth);
  if TwoLineToolbar then
    ToolbarPanel.Height := 2*ButtonsHeight + 3*ToolbarMargin + Container.StatusBarHeight { window extends below top status bar (clock, battery)}
  else
    ToolbarPanel.Height := ButtonsHeight + 2*ToolbarMargin + Container.StatusBarHeight;

  // toolbar
  ToolbarPanel.Translation := Vector2(0, Container.UnscaledHeight - ToolbarPanel.Height);
  ToolbarPanel.WidthFraction := 1;

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
          ToolButton.Translation := Vector2(NextLeft2, ToolbarMargin);
          NextLeft2 := NextLeft2 + ToolButton.EffectiveWidth;
          if ToolButton.Tag = 0 then
            NextLeft2 := NextLeft2 + ButtonsMargin;
        end else
        begin
          // 1st line
          if TwoLineToolbar then
            ToolButton.Translation := Vector2(NextLeft1, ToolbarMargin + ButtonsHeight + ToolbarMargin)
          else
            ToolButton.Translation := Vector2(NextLeft1, ToolbarMargin);
          NextLeft1 := NextLeft1 + ToolButton.EffectiveWidth;
          if ToolButton.Tag = 0 then
            NextLeft1 := NextLeft1 + ButtonsMargin;
        end;
      end;
    end;
  end;

  // status text (FPS)
  Status.Translation := Vector2(10,
    ToolbarPanel.EffectiveRect.Bottom - ButtonsMargin - Status.EffectiveHeight);
end;

procedure TViewDisplayScene.OnWarningHandle(const Category, S: string);
begin
  SceneWarnings.Add(Category + ': ' + S);
end;

procedure TViewDisplayScene.OpenScene(const Url: string);

  { Every CGE warning that happens within this procedure will be captured
    and later shown to player. }
  procedure LoadSceneAndCaptureWarnings;
  var
    NewScene: TCastleScene;
  begin
    Application.Log(etInfo, 'Opened ' + Url);

    NewScene := TCastleScene.Create(FreeAtStop);
    NewScene.Load(Url);
    NewScene.PreciseCollisions := true;
    NewScene.ProcessEvents := true;

    MainViewport.Items.MainScene.Free;
    MainViewport.Items.MainScene := NewScene;
    MainViewport.Items.Add(MainScene);

    { Do not take BBoxScene (with outdated now sizes)
      into account by MainViewport.AssignDefaultCamera.
      We will set BBoxScene.Exists correctly by UpdateBBox later in this routine. }
    BBoxScene.Exists := false;

    MainViewport.AssignDefaultCamera;
    MainViewport.AssignDefaultNavigation;

    // adjust MainViewport.BlendingSort, makes blending for Spine models always good
    if (NewScene.NavigationInfoStack.Top <> nil) and
       (NewScene.NavigationInfoStack.Top.BlendingSort <> sortAuto) then
      MainViewport.BlendingSort := NewScene.NavigationInfoStack.Top.BlendingSort
    else
      MainViewport.BlendingSort := sortAuto;

    // start 1st animation, if any; looks good on Spine models
    if NewScene.AnimationsList.Count > 0 then
    begin
      NewScene.PlayAnimation(NewScene.AnimationsList[0], true);
      WritelnLog('Animation', 'Playing animation ' + NewScene.AnimationsList[0]);
    end;

    MainScene.Collides := AppOptions.CollisionsOn;

    CurrentViewpointIdx := 0;

    ShowHideNavigationButtons(false);

    Resize; // to hide viewpoints, etc
    UpdateBBox;
  end;

begin
  if LowerCase(ExtractFileExt(Url)) = '.zip' then
  begin
    OpenZippedScene(Url); // this may call OpenScene in turn
    Exit;
  end;

  SceneWarnings.Clear;
  ApplicationProperties.OnWarning.Add({$ifdef FPC}@{$endif} OnWarningHandle);
  try
    LoadSceneAndCaptureWarnings;
  finally
    ApplicationProperties.OnWarning.Remove({$ifdef FPC}@{$endif} OnWarningHandle);
  end;

  { Show warnings, if any occurred during loading.
    We do not catch warnings that occurred outside of LoadSceneAndCaptureWarnings
    -- they could disrupt UI at any place.
    They'll be in application log for developer. }
  if SceneWarnings.Count <> 0 then
    MessageOK(Application.MainWindow, SceneWarnings.Text);
end;

procedure TViewDisplayScene.UpdateBBox;
var
  BBox: TBox3D;
begin
  BBoxScene.Exists := (MainScene <> nil) and AppOptions.ShowBBox;
  if not BBoxScene.Exists then Exit;

  BBox := MainScene.BoundingBox;
  BBoxScene.Exists := not BBox.IsEmpty;
  if not BBoxScene.Exists then Exit;

  BBoxGeometry.Size := BBox.Size;
  BBoxTransform.Translation := BBox.Center;
end;

procedure TViewDisplayScene.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;

  Status.Exists := AppOptions.ShowFps;
  if Status.Exists then
    Status.Caption := 'FPS: ' + Container.Fps.ToString;

  UpdateBBox;
end;

procedure TViewDisplayScene.BtnNavClick(Sender: TObject);
begin
  if Sender = BtnNavWalk then
    MainViewport.NavigationType := ntWalk
  else if Sender = BtnNavFly then
    MainViewport.NavigationType := ntFly
  else if Sender = BtnNavExamine then
    MainViewport.NavigationType := ntExamine
  else if Sender = BtnNavTurntable then
    MainViewport.NavigationType := ntTurntable;
end;

procedure TViewDisplayScene.NavigationTypeInPopupSelected(NavType: TNavigationType);
begin
  MainViewport.NavigationType := NavType;
end;

procedure TViewDisplayScene.BoundNavigationInfoChanged(Sender: TObject);
var
  NavType: TNavigationType;
begin
  { this may be called when Window, and everythig it owned (like BtnNavWalk)
    is getting destroyed.
    TODO: Is this safeguard still necessary? }
  if csDestroying in Application.ComponentState then
    Exit;

  NavType := MainViewport.NavigationType;
  BtnNavWalk.Pressed := (NavType = ntWalk);
  BtnNavFly.Pressed := (NavType = ntFly);
  BtnNavExamine.Pressed := (NavType = ntExamine);
  BtnNavTurntable.Pressed := (NavType = ntTurntable);

  ShowHideNavigationButtons(true);
end;

procedure TViewDisplayScene.ShowHideNavigationButtons(UpdateToobar: boolean);
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
  if MainScene = nil then
    exit;

  AnyTypePresent := false;
  WalkPresent := false;
  FlyPresent := false;
  ExaminePresent := false;
  TurntablePresent := false;

  CurrentNavNode := MainScene.NavigationInfoStack.Top;
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
  NeedsToolbarUpdate := ShowNavButton(BtnNavWalk, AvailableNavTypes.Contains(ntWalk));
  NeedsToolbarUpdate := ShowNavButton(BtnNavFly, AvailableNavTypes.Contains(ntFly)) or NeedsToolbarUpdate;
  NeedsToolbarUpdate := ShowNavButton(BtnNavExamine, AvailableNavTypes.Contains(ntExamine)) or NeedsToolbarUpdate;
  NeedsToolbarUpdate := ShowNavButton(BtnNavTurntable, AvailableNavTypes.Contains(ntTurntable)) or NeedsToolbarUpdate;
  if UpdateToobar and NeedsToolbarUpdate then
    Resize;
end;

procedure TViewDisplayScene.BtnScreenshotClick(Sender: TObject);
var
  Image: TRGBImage;
  Filename: string;
  RestoreCtls: TCastleUserInterfaceList;
  I: Integer;
  C: TCastleUserInterface;
begin
  RestoreCtls := TCastleUserInterfaceList.Create(false);
  try
    // hide everything except MainViewport
    for I := 0 to ControlsCount - 1 do
    begin
      C := Controls[I];
      if C.Exists and (C <> MainViewport) then
      begin
        C.Exists := false;
        RestoreCtls.InsertFront(C);
      end;
    end;
    // make screenshot
    Image := Container.SaveScreen;
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

procedure TViewDisplayScene.BtnOptionsClick(Sender: TObject);
begin
  ViewOptions.FScene := MainScene;
  Container.PushView(ViewOptions);
end;

procedure TViewDisplayScene.BtnInfoClick(Sender: TObject);

  function SceneVertexTriangleInfo(const Scene: TCastleScene): string;
  begin
    Result := Format('Scene contains %d triangles and %d vertices.', [
      Scene.TrianglesCount,
      Scene.VerticesCount
    ]);
  end;

  function BBoxInfo(const Scene: TCastleScene): string;
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

begin
  ViewInfo.FScene := MainScene;
  ViewInfo.FStatistics :=
    'Scene information:' + NL +
    SceneVertexTriangleInfo(MainScene) +
    BBoxInfo(MainScene) +
    MainViewport.Statistics.ToString;
  Container.PushView(ViewInfo);
end;

procedure TViewDisplayScene.BtnFilesClick(Sender: TObject);
begin
  Container.PushView(ViewFiles);
end;

procedure TViewDisplayScene.FileSelected(Url: string);
begin
  OpenScene(Url);
end;

procedure TViewDisplayScene.BtnViewpointNextClick(Sender: TObject);
begin
  if MainScene = nil then exit;
  if Sender = BtnViewpointNext then
    Inc(CurrentViewpointIdx)
  else
    Dec(CurrentViewpointIdx);

  if CurrentViewpointIdx < 0 then
    CurrentViewpointIdx := MainScene.ViewpointsCount;
  if CurrentViewpointIdx > MainScene.ViewpointsCount - 1 then
    CurrentViewpointIdx := 0;

  MainScene.MoveToViewpoint(CurrentViewpointIdx);
end;

procedure TViewDisplayScene.BtnViewpointListClick(Sender: TObject);
begin
  ViewViewpoints.FScene := MainScene;
  ViewViewpoints.FCurrentViewpointIdx := CurrentViewpointIdx;
  ViewViewpoints.FOnViewpointSelected := {$ifdef FPC}@{$endif} ViewpointSelected;
  Container.PushView(ViewViewpoints);
end;

procedure TViewDisplayScene.ViewpointSelected(ViewpointIdx: integer);
begin
  CurrentViewpointIdx := ViewpointIdx;
  MainScene.MoveToViewpoint(CurrentViewpointIdx);
end;

function TViewDisplayScene.GetSceneUnpackDir: string;
var
  UnpackDirUrl: string;
begin
  UnpackDirUrl := ApplicationConfig('unpack');
  Result := UriToFilenameSafe(UnpackDirUrl);
  WritelnLog('Unpack directory URL "%s", directory "%s"', [
    UnpackDirUrl,
    Result
  ]);
  if Result = '' then
    raise Exception.CreateFmt('Cannot determine unpack directory from URL "%s". It must be a regular directory, since unzip code only supports directories', [
      UnpackDirUrl
    ]);
end;

procedure TViewDisplayScene.OpenZippedScene(const Url: string);

  { Remove a (potentially non-empty) directory, but make no errors or
    even warnings if it does not exist. }
  procedure ClearDir(const UnpackDir: string);
  begin
    if DirectoryExists(UnpackDir) then
      RemoveNonEmptyDir(UnpackDir, true);
  end;

var
  ZippedFile, UnpackDir, SceneFileCandidate1, SceneFileCandidate2: string;
  UnpackedFile, UnpackedFilePart: string;
  UnZipper: TUnZipper;
  I: Integer;
  Message: string;
begin
  ZippedFile := UriToFilenameSafe(Url);
  if ZippedFile = '' then
    raise Exception.CreateFmt('Cannot determine filename from URL "%s". It must be a regular filename, since our unzip code only supports files for now.', [Url]);

  UnpackDir := GetSceneUnpackDir;
  ClearDir(UnpackDir);

  SceneFileCandidate1 := '';
  SceneFileCandidate2 := '';

  // unzip everything
  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := ZippedFile;
    UnZipper.OutputPath := UnpackDir;
    UnZipper.Examine;
    UnZipper.UnZipAllFiles;
  except
    on E: Exception do
         Application.Log(etError, 'Unzip error: ' + E.ClassName + #13#10 + E.Message);
  end;

  // find the scene file
  for I := UnZipper.Entries.Count-1 downto 0 do
  begin
    UnpackedFilePart := UnZipper.Entries.Entries[I].DiskFileName;
    UnpackedFile := IncludeTrailingPathDelimiter(UnpackDir) + UnpackedFilePart;

    // check if it is file and is not inside a subdir
    if FileExists(UnpackedFile) and
       (ExtractFileDir(UnpackedFilePart) = '') and
       TFileFilterList.Matches(LoadScene_FileFilters, UnpackedFile) then
    begin
      { Prefer other filenames than 'library'
        to allow opening archive with Room Arranger data,
        where library.wrl is an empty collection of PROTOs. }
      if ChangeFileExt(ExtractFileName(UnpackedFile), '') = 'library' then
        SceneFileCandidate2 := UnpackedFile
      else
        SceneFileCandidate1 := UnpackedFile;
    end;
  end;

  // open it (or show error message)
  if SceneFileCandidate1 <> '' then
    OpenScene(SceneFileCandidate1)
  else
  if SceneFileCandidate2 <> '' then
    OpenScene(SceneFileCandidate2)
  else
  begin
    Message := 'No supported scene file found inside the zip file.' + NL + NL
             + 'We enable opening scenes from ZIP archives to easily ship the main geometry file '
             + 'together with material files and textures inside one file.' + NL + NL
             + 'Zip contains:';
    for I := UnZipper.Entries.Count-1 downto 0 do
      Message := Message + NL + '  ' + UnZipper.Entries.Entries[I].DiskFileName;
    MessageOKPushesView := true;
    MessageOK(Application.MainWindow, Message);
  end;

  // cannot delete all unpacked files now, textures load a bit later

  FreeAndNil(UnZipper);
end;

end.