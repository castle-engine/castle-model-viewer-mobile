{
  Copyright 2017-2025 Michalis Kamburelis and Jan Adamec.

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
  CastleDialogViews, CastleNotifications, CastleCameras, CastleZip;

type
  { Actions to do when we have Resume, possibly coming back from a dialog
    to choose new viewpoint / animation to play. }
  TResumeAction = (raNone, raMoveViewpoint, raPlayAnimation, raChangeNavigation);

  TViewDisplayScene = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonNavigations, ButtonOptions, ButtonViewpoints, ButtonAnimations,
      ButtonScreenshot, ButtonInfo, ButtonFiles, ButtonDonate: TCastleButton;
    LabelFpsStats: TCastleLabel;
    ViewportContainer: TCastleUserInterface;
    OnScreenNotifications: TCastleNotifications;
    Toolbar: TCastleUserInterface;
  private
    BBoxScene: TCastleScene;
    BBoxTransform: TTransformNode;
    BBoxGeometry: TBoxNode;
    SceneWarnings: TStringList;
    ResumeAction: TResumeAction;
    ToolbarBaseHeight: Single;
    SceneZip: TCastleZip;

    {$warnings off}
    { Knowingly using deprecated TCastleAutoNavigationViewport:
      for X3D browser, it is still the best choice, to auto-adjust navigation
      based on X3D nodes. }
    MainViewport: TCastleAutoNavigationViewport;
    {$warnings on}
    TouchNavigation: TCastleTouchNavigation;

    { Event handlers }
    procedure ClickScreenshot(Sender: TObject);
    procedure ClickOptions(Sender: TObject);
    procedure ClickInfo(Sender: TObject);
    procedure ClickFiles(Sender: TObject);
    procedure ClickViewpoints(Sender: TObject);
    procedure ClickAnimations(Sender: TObject);
    procedure ClickNavigations(Sender: TObject);
    procedure ClickDonate(Sender: TObject);
    procedure WarningHandle(const Category, S: string);
    procedure SafeBorderChanged(Sender: TObject);

    { Currently loaded scene. May be @nil only before first OpenScene from Start. }
    function MainScene: TCastleScene;
    procedure OpenZippedScene(const Url: string);
    procedure DropFiles(const FileNames: array of string);
    { Call each frame to update BBox* values to match current MainScene.BoundingBox. }
    procedure UpdateBBox;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Resume; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;

    procedure OpenScene(const Url: string);
  end;

var
  ViewDisplayScene: TViewDisplayScene;

implementation

uses SysUtils, Math,
  CastleImages, CastleFilesUtils, CastleWindow, CastleColors, CastleBoxes,
  CastleApplicationProperties, CastleUtils, CastlePhotoService, CastleLog,
  CastleMessages, CastleFileFilters, X3DLoad, CastleParameters,
  CastleRenderOptions, CastleUriUtils, CastleVectors, CastleOpenDocument,
  GameViewAbout, GameOptions, GameViewOptions, GameViewFiles, GameViewChoice,
  GameViewNavigation;

procedure GlobalDropFiles(AContainer: TCastleContainer;
  const FileNames: array of string);
begin
  if ViewDisplayScene <> nil then
    ViewDisplayScene.DropFiles(FileNames);
end;

{ TViewDisplayScene ---------------------------------------------------------- }

constructor TViewDisplayScene.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewdisplayscene.castle-user-interface';
end;

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

begin
  inherited;

  ResumeAction := raNone;
  SceneWarnings := TStringList.Create;

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
  { We allow movement using TouchNavigation and so disable movement by
    dragging elsewhere on the screen, it would be too easy to move by accident then.
    So change MouseDragMode from mdWalkRotate to mdRotate.
    Note that we don't change to mdNone -- it is still nice to allow user to rotate
    by draging on the screen, even though TouchNavigation also allows it by gizmo.
    See https://castle-engine.io/touch_input . }
  MainViewport.InternalWalkNavigation.MouseDragMode := mdRotate;
  ViewportContainer.InsertFront(MainViewport);

  TouchNavigation := TCastleTouchNavigation.Create(FreeAtStop);
  TouchNavigation.FullSize := true;
  TouchNavigation.Viewport := MainViewport;
  TouchNavigation.AutoTouchInterface := true;
  TouchNavigation.AutoWalkTouchInterface := tiWalkRotate; // show both walk and rotate gizmos, more obvious for users
  TouchNavigation.AutoExamineTouchInterface := tiNone; // use 2-finger gesture to pan, not touchControl
  MainViewport.InsertFront(TouchNavigation);

  ButtonNavigations.OnClick := @ClickNavigations;
  ButtonOptions.OnClick := @ClickOptions;
  ButtonScreenshot.OnClick := @ClickScreenshot;
  ButtonInfo.OnClick := @ClickInfo;
  ButtonFiles.OnClick := @ClickFiles;
  ButtonViewpoints.OnClick := @ClickViewpoints;
  ButtonAnimations.OnClick := @ClickAnimations;
  ButtonDonate.OnClick := @ClickDonate;

  Application.MainWindow.OnDropFiles := {$ifdef FPC}@{$endif} GlobalDropFiles;

  InitializeBBox;
  UpdateBBox;

  ToolbarBaseHeight := Toolbar.EffectiveHeight;
  Container.OnSafeBorderChanged := {$ifdef FPC}@{$endif} SafeBorderChanged;

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
  FreeAndNil(SceneZip);
  BBoxScene := nil; // freed by FreeAtStop
  Container.OnSafeBorderChanged := nil;
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

procedure TViewDisplayScene.WarningHandle(const Category, S: string);
begin
  SceneWarnings.Add(Category + ': ' + S);
end;

procedure TViewDisplayScene.OpenScene(const Url: string);

  { Every CGE warning that happens within this procedure will be captured
    and later shown to player. }
  procedure LoadSceneAndCaptureWarnings;

    procedure AdjustIconColorFromEnabled(const Button: TCastleButton);
    begin
      if Button.Enabled then
        Button.Image.Color := Black
      else
        Button.Image.Color := Gray;
    end;

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
      // Hack to play Walk, not Death, when Bunny loads
      if NewScene.HasAnimation('Walk') then
        NewScene.PlayAnimation('Walk', true)
      else
        NewScene.PlayAnimation(NewScene.AnimationsList[0], true);
      WritelnLog('Animation', 'Playing animation ' + NewScene.AnimationsList[0]);
    end;

    MainScene.Collides := true; // always start with Collisions = on

    // update buttons enabled/captions based on scene counts
    ButtonViewpoints.Enabled := MainScene.ViewpointsCount > 0;
    ButtonViewpoints.Tooltip := 'Viewpoints (' + IntToStr(MainScene.ViewpointsCount) + ')';
    AdjustIconColorFromEnabled(ButtonViewpoints);
    ButtonAnimations.Enabled := MainScene.AnimationsList.Count > 0;
    ButtonAnimations.Tooltip := 'Animations (' + IntToStr(MainScene.AnimationsList.Count) + ')';
    AdjustIconColorFromEnabled(ButtonAnimations);

    UpdateBBox;
  end;

begin
  if LowerCase(ExtractFileExt(Url)) = '.zip' then
  begin
    OpenZippedScene(Url); // this may call OpenScene in turn
    Exit;
  end;

  SceneWarnings.Clear;
  ApplicationProperties.OnWarning.Add({$ifdef FPC}@{$endif} WarningHandle);
  try
    LoadSceneAndCaptureWarnings;
  finally
    ApplicationProperties.OnWarning.Remove({$ifdef FPC}@{$endif} WarningHandle);
  end;

  { Show warnings, if any occurred during loading.
    We do not catch warnings that occurred outside of LoadSceneAndCaptureWarnings
    -- they could disrupt UI at any place.
    They'll be in application log for developer.

    Later: hide the warnings. Too disruptive to show this when opening the model.
    At least for house_floors.ifcjson (which has a number of known warnings).
    TODO: Allow showing them later -- by a button UI, when user requests.
  }
  // if SceneWarnings.Count <> 0 then
  //   MessageOK(Application.MainWindow, SceneWarnings.Text);
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

  LabelFpsStats.Exists := AppOptions.ShowFps;
  if LabelFpsStats.Exists then
    LabelFpsStats.Caption := 'FPS: ' + Container.Fps.ToString;

  UpdateBBox;
end;

procedure TViewDisplayScene.ClickNavigations(Sender: TObject);
begin
  ViewNavigation.Navigation := MainViewport.NavigationType;
  ResumeAction := raChangeNavigation;
  Container.PushView(ViewNavigation);
end;

procedure TViewDisplayScene.ClickDonate(Sender: TObject);
begin
  OpenUrl('https://www.patreon.com/castleengine');
end;

procedure TViewDisplayScene.ClickScreenshot(Sender: TObject);
var
  Image: TRGBImage;
  SavedLabelFpsStatsExists: Boolean;
  SavedViewportContainerBorderBottom: Single;
  ImageUrl: String;
begin
  SavedLabelFpsStatsExists := LabelFpsStats.Exists;
  SavedViewportContainerBorderBottom := ViewportContainer.Border.Bottom;

  // hide everything except viewport, and make viewport fill container
  Toolbar.Exists := false;
  OnScreenNotifications.Exists := false;
  LabelFpsStats.Exists := false;
  TouchNavigation.Exists := false;
  ViewportContainer.Border.Bottom := 0;
  Container.InternalTooltipHide;

  // make screenshot
  Image := Container.SaveScreen;
  try
    ImageUrl := FileNameAutoInc('castle-config:/screenshots/', 'screenshot_%d.png');
    SaveImage(Image, ImageUrl);
    TPhotoService.StoreImage(ResolveCastleConfigUrl(ImageUrl));
  finally FreeAndNil(Image) end;

  // restore hidden controls
  Toolbar.Exists := true;
  OnScreenNotifications.Exists := true;
  LabelFpsStats.Exists := SavedLabelFpsStatsExists;
  TouchNavigation.Exists := true;
  ViewportContainer.Border.Bottom := SavedViewportContainerBorderBottom;

  OnScreenNotifications.Show('Screenshot saved to ' + ExtractUriName(ImageUrl));
end;

procedure TViewDisplayScene.ClickOptions(Sender: TObject);
begin
  ViewOptions.FScene := MainScene;
  Container.PushView(ViewOptions);
end;

procedure TViewDisplayScene.ClickInfo(Sender: TObject);

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

procedure TViewDisplayScene.ClickFiles(Sender: TObject);
begin
  Container.PushView(ViewFiles);
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
  ResumeAction := raNone;
end;

procedure TViewDisplayScene.OpenZippedScene(const Url: string);
var
  SceneFileCandidate1, SceneFileCandidate2, SceneFileInZip, FileInZip: String;
  NewZip: TCastleZip;
begin
  NewZip := TCastleZip.Create;

  try
    NewZip.Open(Url);
  except
    on E: Exception do
    begin
      // This pushes new view with error message
      MessageOK(Application.MainWindow, 'Cannot open ZIP: ' + ExceptMessage(E));
      Exit;
    end;
  end;

  SceneFileCandidate1 := '';
  SceneFileCandidate2 := '';

  // find the scene file
  for FileInZip in NewZip.Files do
  begin
    // check if it is not inside a subdir
    if (Pos('/', FileInZip) = 0) and
       TFileFilterList.Matches(LoadScene_FileFilters, FileInZip) then
    begin
      { Prefer other filenames than 'library'
        to allow opening archive with Room Arranger data,
        where library.wrl is an empty collection of PROTOs. }
      if ChangeFileExt(FileInZip, '') = 'library' then
        SceneFileCandidate2 := FileInZip
      else
        SceneFileCandidate1 := FileInZip;
    end;
  end;

  // calculate SceneFileInZip
  if SceneFileCandidate1 <> '' then
    SceneFileInZip := SceneFileCandidate1
  else
  if SceneFileCandidate2 <> '' then
    SceneFileInZip := SceneFileCandidate2
  else
    SceneFileInZip := '';

  // open SceneFileInZip (or show error message)
  if SceneFileInZip <> '' then
  begin
    FreeAndNil(SceneZip);
    NewZip.RegisterUrlProtocol('model-viewer-zip');
    OpenScene('model-viewer-zip:/' + UrlEncode(SceneFileInZip));
  end else
  begin
    MessageOK(Application.MainWindow, 'No supported scene file found inside the zip file.' + NL +
      NL +
      'We enable opening scenes from ZIP archives to easily ship the main geometry file together with material files and textures inside one file.' + NL +
      NL +
      'Zip contents:' + NL +
      NL +
      NewZip.Files.Text);
    Exit;
  end;
end;

procedure TViewDisplayScene.SafeBorderChanged(Sender: TObject);
begin
  Toolbar.Height := ToolbarBaseHeight + Container.SafeBorder.Bottom;

  // update Border.Bottom border, to move touch gizmos up
  ViewportContainer.Border.Bottom := Toolbar.EffectiveHeight;

  // on Android in landscape mode, make touch navigation (e.g. in dungeon level)
  // not overlap with the buttons (back, home...) on the left
  TouchNavigation.Border.Left := Container.SafeBorder.Left;
  TouchNavigation.Border.Right := Container.SafeBorder.Right;
end;

end.