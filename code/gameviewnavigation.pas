{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "castle-model-viewer-mobile".

  "castle-model-viewer-mobile" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "castle-model-viewer-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ View to change navigation type. }
unit GameViewNavigation;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleViewport,
  GameAbstractViewDialog;

type
  TViewNavigation = class(TAbstractViewDialog)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonExamine: TCastleButton;
    ButtonWalk: TCastleButton;
    ButtonFly: TCastleButton;
    Button2D: TCastleButton;
    ImageExamineMark: TCastleImageControl;
    ImageWalkMark: TCastleImageControl;
    ImageFlyMark: TCastleImageControl;
    Image2DMark: TCastleImageControl;
  private
    procedure ClickButtonChangeNavigation(Sender: TObject);
  public
    { Set this before Start, upon Stop it will be set to the chosen navigation. }
    Navigation: TNavigationType;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewNavigation: TViewNavigation;

implementation

constructor TViewNavigation.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewnavigation.castle-user-interface';
end;

procedure TViewNavigation.Start;

  procedure ConfigNavigationUi(const Button: TCastleButton;
    const ImageMark: TCastleImageControl; const UiNavigation: TNavigationType);
  begin
    Button.Tag := Ord(UiNavigation);
    Button.OnClick := @ClickButtonChangeNavigation;
    ImageMark.Exists := Navigation = UiNavigation;
  end;

begin
  inherited;
  ConfigNavigationUi(ButtonExamine, ImageExamineMark, ntExamine);
  ConfigNavigationUi(ButtonWalk, ImageWalkMark, ntWalk);
  ConfigNavigationUi(ButtonFly, ImageFlyMark, ntFly);
  ConfigNavigationUi(Button2D, Image2DMark, nt2D);
end;

procedure TViewNavigation.ClickButtonChangeNavigation(Sender: TObject);
begin
  Navigation := TNavigationType((Sender as TComponent).Tag);
  Container.PopView(Self);
end;

end.
