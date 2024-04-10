{
  Copyright 2017-2024 Michalis Kamburelis and Jan Adamec.

  This file is part of "view3dscene-mobile".

  "view3dscene-mobile" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "view3dscene-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Propose user to open a different model. }
unit GameViewFiles;

interface

uses Classes, SysUtils,
  CastleUIControls, CastleControls, CastleScene, CastleKeysMouse,
  CastleStringUtils;

type
  TViewFiles = class(TCastleView)
  published
    ButtonClose: TCastleButton;
    TransparentBackground: TCastleButton;
    ButtonOpenOwnLink: TCastleButton;
    ButtonTemplate: TCastleButton;
  strict private
    Models: TStringStringMap;
    procedure ClickClose(Sender: TObject);
    procedure ClickOpenOwnLink(Sender: TObject);
    procedure ClickOpenModel(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
  end;

var
  ViewFiles: TViewFiles;

implementation

uses
  Math, Generics.Collections,
  CastleColors, CastleWindow, CastleFilesUtils, CastleLog,
  CastleUtils, CastleVectors, CastleComponentSerialize, CastleOpenDocument,
  GameViewDisplayScene;

{ TViewFiles ------------------------------------------------------------ }

constructor TViewFiles.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewfiles.castle-user-interface';

  Models := TStringStringMap.Create;
  Models.Add('Castle Walk (VRML)', 'castle-data:/demo/castle_walk.wrl');
  Models.Add('Chinchilla (VRML)', 'castle-data:/demo/chinchilla.wrl');
  Models.Add('Teapot (fresnel and toon shader) (X3D)', 'castle-data:/demo/teapot (fresnel and toon shader).x3dv');
  Models.Add('Teapot (time to shader) (X3D)', 'castle-data:/demo/teapot (time to shader).x3dv');
  Models.Add('Animated 2D Dragon (Spine)', 'castle-data:/demo/dragon-spine/dragon.json');
  Models.Add('Duck (glTF)', 'castle-data:/demo/gltf-duck/duck.gltf');
end;

destructor TViewFiles.Destroy;
begin
  FreeAndNil(Models);
  inherited;
end;

procedure TViewFiles.Start;
var
  ModelPair: {$ifdef FPC} TStringStringMap.TDictionaryPair {$else} TPair<string, string> {$endif};
  ButtonOpenFactory: TCastleComponentFactory;
  TemplateIndex: Integer;
  ButtonOpen: TCastleButton;
begin
  inherited;
  InterceptInput := true;

  ButtonClose.OnClick := @ClickClose;
  ButtonOpenOwnLink.OnClick := @ClickOpenOwnLink;
  TransparentBackground.OnClick := @ClickClose;

  ButtonOpenFactory := TCastleComponentFactory.Create(nil);
  try
    ButtonOpenFactory.LoadFromComponent(ButtonTemplate);
    TemplateIndex := ButtonTemplate.Parent.IndexOfControl(ButtonTemplate);
    for ModelPair in Models do
    begin
      ButtonOpen := ButtonOpenFactory.ComponentLoad(FreeAtStop) as TCastleButton;
      ButtonOpen.Caption := ModelPair.Key;
      ButtonOpen.OnClick := @ClickOpenModel;
      ButtonOpen.Exists := true; // because ButtonTemplate has Exists=false
      Inc(TemplateIndex);
      ButtonTemplate.Parent.InsertControl(TemplateIndex, ButtonOpen);
      // do not store ModelPair.Value, we depend that it is unique
    end;
  finally FreeAndNil(ButtonOpenFactory) end;
end;

procedure TViewFiles.ClickClose(Sender: TObject);
begin
  Container.PopView(Self);
end;

procedure TViewFiles.ClickOpenOwnLink(Sender: TObject);
begin
  OpenUrl('https://github.com/castle-engine/view3dscene-mobile/blob/master/README.md#view3dscene-mobile');
  ClickClose(nil);
end;

procedure TViewFiles.ClickOpenModel(Sender: TObject);
var
  SenderButton: TCastleButton;
  ModelUrl: String;
begin
  SenderButton := Sender as TCastleButton;
  ModelUrl := Models[SenderButton.Caption];
  ViewDisplayScene.OpenScene(ModelUrl);
  ClickClose(nil);
end;

end.
