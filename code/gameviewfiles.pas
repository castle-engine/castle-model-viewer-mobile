{
  Copyright 2017-2025 Michalis Kamburelis and Jan Adamec.

  This file is part of "castle-model-viewer-mobile".

  "castle-model-viewer-mobile" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "castle-model-viewer-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Propose user to open a different model. }
unit GameViewFiles;

interface

uses Classes, SysUtils, Generics.Collections,
  CastleUIControls, CastleControls, CastleScene, CastleKeysMouse,
  CastleStringUtils,
  GameAbstractViewDialog;

type
  TViewFiles = class(TAbstractViewDialog)
  published
    ButtonOpenOwnLink: TCastleButton;
    ButtonTemplate: TCastleButton;
  strict private
    type
      TDemoModel = class
        Caption: String;
        Url: String;
      end;
      TDemoModelList = class({$ifdef FPC} specialize {$endif} TObjectList<TDemoModel>)
        procedure Add(const ACaption, AUrl: String); reintroduce;
      end;
    var
      Models: TDemoModelList;

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
  Math,
  CastleColors, CastleWindow, CastleFilesUtils, CastleLog,
  CastleUtils, CastleVectors, CastleComponentSerialize, CastleOpenDocument,
  GameViewDisplayScene;

{ TViewFiles.TDemoModelList ----------------------------------------------- }

procedure TViewFiles.TDemoModelList.Add(const ACaption, AUrl: String);
var
  Model: TDemoModel;
begin
  Model := TDemoModel.Create;
  Model.Caption := ACaption;
  Model.Url := AUrl;
  inherited Add(Model);
end;

{ TViewFiles ------------------------------------------------------------ }

constructor TViewFiles.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewfiles.castle-user-interface';

  Models := TDemoModelList.Create(true);
  Models.Add('Animated Cat (glTF + X3D, viewpoints)', 'castle-data:/demo/cat_final.x3dv');
  Models.Add('Dungeon (glTF + X3D, walk, viewpoints)', 'castle-data:/demo/dungeon_final.x3dv');
  //Models.Add('Steampunk Explorer (glTF)', 'castle-data:/demo/steampunk_underwater_explorer.glb');
  Models.Add('Bunny (glTF, animations)', 'castle-data:/demo/quaternius_monsters/Bunny.gltf');
  Models.Add('Animated 2D Dragon (Spine, animations)',
    'castle-data:/demo/dragon_spine_final.x3dv');
    //'castle-data:/demo/dragon-spine/dragon.json');
  // Models.Add('Teapot (fresnel and toon shader) (X3D)', 'castle-data:/demo/teapot_fresnel_toon.x3dv');
  Models.Add('House Floors (IFC)', 'castle-data:/demo/ifc/house_floors_viewpoints.x3dv');
  Models.Add('Teapot (time to shader) (X3D)', 'castle-data:/demo/teapot_time.x3dv');
  //Models.Add('Test Downloading Resources (X3D)', 'castle-data:/demo/needs_download_network_resources.x3dv');
end;

destructor TViewFiles.Destroy;
begin
  FreeAndNil(Models);
  inherited;
end;

procedure TViewFiles.Start;
var
  Model: TDemoModel;
  ButtonOpenFactory: TCastleComponentFactory;
  TemplateIndex: Integer;
  ButtonOpen: TCastleButton;
  I: Integer;
begin
  inherited;

  ButtonOpenOwnLink.OnClick := @ClickOpenOwnLink;

  ButtonOpenFactory := TCastleComponentFactory.Create(nil);
  try
    ButtonOpenFactory.LoadFromComponent(ButtonTemplate);
    TemplateIndex := ButtonTemplate.Parent.IndexOfControl(ButtonTemplate);
    for I := 0 to Models.Count - 1 do
    begin
      Model := Models[I];
      ButtonOpen := ButtonOpenFactory.ComponentLoad(FreeAtStop) as TCastleButton;
      ButtonOpen.Caption := Model.Caption;
      ButtonOpen.OnClick := @ClickOpenModel;
      ButtonOpen.Exists := true; // because ButtonTemplate has Exists=false
      ButtonOpen.Tag := I;
      Inc(TemplateIndex);
      ButtonTemplate.Parent.InsertControl(TemplateIndex, ButtonOpen);
    end;
  finally FreeAndNil(ButtonOpenFactory) end;
end;

procedure TViewFiles.ClickOpenOwnLink(Sender: TObject);
begin
  OpenUrl('https://castle-engine.io/castle-model-viewer-mobile');
  Container.PopView(Self);
end;

procedure TViewFiles.ClickOpenModel(Sender: TObject);
var
  SenderButton: TCastleButton;
  ModelUrl: String;
begin
  SenderButton := Sender as TCastleButton;
  ModelUrl := Models[SenderButton.Tag].Url;
  ViewDisplayScene.OpenScene(ModelUrl);
  Container.PopView(Self);
end;

end.
