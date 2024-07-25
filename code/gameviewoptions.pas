{
  Copyright 2017-2024 Michalis Kamburelis and Jan Adamec.

  This file is part of "castle-model-viewer-mobile".

  "castle-model-viewer-mobile" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "castle-model-viewer-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ View to change options (FPS display etc.). }
unit GameViewOptions;

interface

uses Classes, SysUtils,
  CastleUIControls, CastleControls, CastleScene, CastleUIState, CastleKeysMouse,
  GameInitialize, GameAbstractViewDialog;

type
  TViewOptions = class(TAbstractViewDialog)
  published
    CheckboxBBox: TCastleCheckbox;
    CheckboxFps: TCastleCheckbox;
    CheckboxHeadlight: TCastleCheckbox;
    CheckboxCollisions: TCastleCheckbox;
    CheckboxEnableBlockingDownloads: TCastleCheckbox;
  strict private
    procedure CheckboxBBoxChange(Sender: TObject);
    procedure CheckboxFpsChange(Sender: TObject);
    procedure CheckboxHeadlightChange(Sender: TObject);
    procedure CheckboxCollisionsChange(Sender: TObject);
    procedure CheckboxEnableBlockingDownloadsChange(Sender: TObject);
  public
    FScene: TCastleScene;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewOptions: TViewOptions;

implementation

uses
  Math,
  CastleColors, CastleWindow, CastleFilesUtils, CastleLog,
  CastleUtils, CastleVectors, CastleDownload,
  GameOptions, GameViewDisplayScene;

{ TViewOptions ------------------------------------------------------------- }

constructor TViewOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DesignUrl := 'castle-data:/gameviewoptions.castle-user-interface';
end;

procedure TViewOptions.Start;
begin
  inherited;

  CheckboxBBox.Checked := AppOptions.ShowBBox;
  CheckboxFps.Checked := AppOptions.ShowFps;
  CheckboxHeadlight.Checked := Assigned(FScene) and FScene.HeadLightOn;
  CheckboxCollisions.Checked := AppOptions.CollisionsOn;
  CheckboxEnableBlockingDownloads.Checked := AppOptions.EnableBlockingDownloads;

  CheckboxBBox.OnChange := @CheckboxBBoxChange;
  CheckboxFps.OnChange := @CheckboxFpsChange;
  CheckboxHeadlight.OnChange := @CheckboxHeadlightChange;
  CheckboxCollisions.OnChange := @CheckboxCollisionsChange;
  CheckboxEnableBlockingDownloads.OnChange := @CheckboxEnableBlockingDownloadsChange;
end;

procedure TViewOptions.CheckboxBBoxChange(Sender: TObject);
begin
  AppOptions.ShowBBox := CheckboxBBox.Checked;
end;

procedure TViewOptions.CheckboxFpsChange(Sender: TObject);
begin
  AppOptions.ShowFps := CheckboxFps.Checked;
end;

procedure TViewOptions.CheckboxHeadlightChange(Sender: TObject);
begin
  if Assigned(FScene) then
    FScene.HeadLightOn := CheckboxHeadlight.Checked;
end;

procedure TViewOptions.CheckboxCollisionsChange(Sender: TObject);
begin
  AppOptions.CollisionsOn := CheckboxCollisions.Checked;
  if Assigned(FScene) then
    FScene.Collides := AppOptions.CollisionsOn;
end;

procedure TViewOptions.CheckboxEnableBlockingDownloadsChange(Sender: TObject);
begin
  AppOptions.EnableBlockingDownloads := CheckboxEnableBlockingDownloads.Checked;
  EnableBlockingDownloads := AppOptions.EnableBlockingDownloads;
end;

end.
