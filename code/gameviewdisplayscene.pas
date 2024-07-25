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

uses Classes, Generics.Collections,
  CastleUIControls, CastleControls, CastleScene, X3DNodes, CastleViewport,
  CastleDialogViews;

type
  TNavTypeList = class(specialize TList<TNavigationType>) end;

  { Actions to do when we have Resume, possibly coming back from a dialog
    to choose new viewpoint / animation to play. }
  TResumeAction = (raNone, raMoveViewpoint, raPlayAnimation, raChangeNavigation);

  TViewDisplayScene = class(TCastleView)
  private
    BtnNavigations, BtnOptions, BtnViewpoints, BtnAnimations,
      BtnScreenshot, BtnInfo, BtnFiles: TCastleButton;
    ToolbarPanel: TCastleHorizontalGroup;
    Status: TCastleLabel;

    BBoxScene: TCastleScene;
    BBoxTransform: TTransformNode;
    BBoxGeometry: TBoxNode;
    SceneWarnings: TStringList;
    ResumeAction: TResumeAction;

    {$warnings off}
    { Knowingly using deprecated TCastleAutoNavigationViewport:
      for X3D browser, it is still the best choice, to auto-adjust navigation
      based on X3D nodes. }
    MainViewport: TCastleAutoNavigationViewport;
    {$warnings on}
    TouchNavigation: TCastleTouchNavigation;

    { CGE event handlers }
    { }
    procedure BtnScreenshotClick(Sender: TObject);
    procedure BtnOptionsClick(Sender: TObject);
    procedure BtnInfoClick(Sender: TObject);
    procedure BtnFilesClick(Sender: TObject);
    procedure ClickViewpoints(Sender: TObject);
    procedure ClickAnimations(Sender: TObject);
    procedure ClickNavigations(Sender: TObject);
    procedure FileSelected(Url: string);
    procedure NavigationTypeInPopupSelected(NavType: TNavigationType);
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
    procedure Resume; override;
    procedure Resize; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;

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
  GameViewAbout, GameOptions, GameViewOptions, GameViewFiles, GameViewChoice,
  GameViewNavigation;

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
begin
  inherited;

  ResumeAction := raNone;
  SceneWarnings := TStringList.Create;

  { TODO: Design UI in editor }

  {$warnings off}
  { Knowingly using deprecated TCastleAutoNavigationViewport and AutoCamera:
    for X3D browser, it is still the best choice, to:
    - auto-adjust navigation based on X3D nodes.
    - follow X3D camera animations. }
  MainViewport := TCastleAutoNavigationViewport.Create(FreeAtStop);
  MainViewport.AutoCamera := true;
  {$warnings on}
  MainViewport.FullSize := true;
  MainViewport.AutoNavigation := true;
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
  ToolbarPanel := TCastleHorizontalGroup.Create(FreeAtStop);
  ToolbarPanel.Anchor(vpTop, 0);
  InsertFront(ToolbarPanel);

  // add buttons to toolbar
  BtnNavigations := TCastleButton.Create(ToolbarPanel);
  BtnNavigations.Caption := 'Navigations';
  BtnNavigations.OnClick := {$ifdef FPC}@{$endif} ClickNavigations;
  BtnNavigations.PaddingHorizontal := ButtonPadding;
  BtnNavigations.PaddingVertical := ButtonPadding;
  ToolbarPanel.InsertFront(BtnNavigations);

  ButtonsHeight := BtnNavigations.EffectiveHeight;

  BtnViewpoints := TCastleButton.Create(ToolbarPanel);
  BtnViewpoints.Caption := 'Viewpoints';
  BtnViewpoints.OnClick := {$ifdef FPC}@{$endif} ClickViewpoints;
  ToolbarPanel.InsertFront(BtnViewpoints);

  BtnAnimations := TCastleButton.Create(ToolbarPanel);
  BtnAnimations.Caption := 'Animations';
  BtnAnimations.OnClick := {$ifdef FPC}@{$endif} ClickAnimations;
  ToolbarPanel.InsertFront(BtnAnimations);

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
  Status.FontScale := 0.8;
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
    OpenScene('castle-data:/demo/cat_final.x3dv');
end;

procedure TViewDisplayScene.Stop;
begin
  FreeAndNil(SceneWarnings);
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
  {$warnings off}
  { Knowingly using deprecated MainScene, it is necessary for
    TCastleViewport.AutoCamera and TCastleAutoNavigationViewport
    which in turn are necessary for X3D browser to synchronize camera/navigation
    ideally. }
  Result := MainViewport.Items.MainScene;
  {$warnings on}
end;

procedure TViewDisplayScene.Resize;
const
  ButtonsMargin = 5;  {< between buttons }
begin
  inherited;

  BtnViewpoints.Exists := MainScene.ViewpointsCount > 0;
  BtnAnimations.Exists := MainScene.AnimationsList.Count > 0;

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

    {$warnings off}
    { Knowingly using deprecated MainScene, it is necessary for
      TCastleViewport.AutoCamera and TCastleAutoNavigationViewport
      which in turn are necessary for X3D browser to synchronize camera/navigation
      ideally. }
    MainViewport.Items.MainScene.Free;
    MainViewport.Items.MainScene := NewScene;
    MainViewport.Items.Add(MainScene);
  {$warnings on}

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

    Resize; // to hide viewpoints, animations buttons etc.
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

procedure TViewDisplayScene.ClickNavigations(Sender: TObject);
begin
  ViewNavigation.Navigation := MainViewport.NavigationType;
  ResumeAction := raChangeNavigation;
  Container.PushView(ViewNavigation);
end;

procedure TViewDisplayScene.NavigationTypeInPopupSelected(NavType: TNavigationType);
begin
  MainViewport.NavigationType := NavType;
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
      Result := Result + Format(', average size : %f', [BBox.AverageSize]);
  end;

begin
  ViewAbout.StatsText :=
    'Scene information:' + NL +
    SceneVertexTriangleInfo(MainScene) + NL +
    BBoxInfo(MainScene) + NL +
    MainViewport.Statistics.ToString;
  Container.PushView(ViewAbout);
end;

procedure TViewDisplayScene.BtnFilesClick(Sender: TObject);
begin
  Container.PushView(ViewFiles);
end;

procedure TViewDisplayScene.FileSelected(Url: string);
begin
  OpenScene(Url);
end;

procedure TViewDisplayScene.ClickViewpoints(Sender: TObject);
var
  I: Integer;
begin
  ViewChoice.ChoicesCaption := 'Viewpoints';
  ViewChoice.ChoiceCurrent := -1; // initially
  ViewChoice.Choices.Clear;

  for I := 0 to MainScene.ViewpointsCount - 1 do
  begin
    if MainScene.GetViewpointNode(I).Bound then
      ViewChoice.ChoiceCurrent := I;
    ViewChoice.Choices.Add(MainScene.GetViewpointName(I));
  end;

  ResumeAction := raMoveViewpoint;
  Container.PushView(ViewChoice);
end;

procedure TViewDisplayScene.ClickAnimations(Sender: TObject);
var
  I: Integer;
begin
  ViewChoice.ChoicesCaption := 'Animations';
  ViewChoice.ChoiceCurrent := -1; // initially
  ViewChoice.Choices.Clear;

  for I := 0 to MainScene.AnimationsList.Count - 1 do
  begin
    if MainScene.AnimationTimeSensor(I) = MainScene.CurrentAnimation then
      ViewChoice.ChoiceCurrent := I;
    ViewChoice.Choices.Add(MainScene.AnimationsList[I]);
  end;

  ResumeAction := raPlayAnimation;
  Container.PushView(ViewChoice);
end;

procedure TViewDisplayScene.Resume;
begin
  inherited;

  case ResumeAction of
    raNone: ;
    raMoveViewpoint:
      if ViewChoice.Answer <> -1 then
        MainScene.MoveToViewpoint(ViewChoice.Answer);
    raPlayAnimation:
      if ViewChoice.Answer <> -1 then
        MainScene.PlayAnimation(ViewChoice.Choices[ViewChoice.Answer], true);
    raChangeNavigation:
      MainViewport.NavigationType := ViewNavigation.Navigation;
    else raise EInternalError.Create('TViewDisplayScene.Resume: ResumeAction not implemented');
  end;
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
    MessageOK(Application.MainWindow, Message);
  end;

  // cannot delete all unpacked files now, textures load a bit later

  FreeAndNil(UnZipper);
end;

end.