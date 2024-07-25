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

{ Allow user a choice between different values, and one value is marked as current. }
unit GameViewChoice;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  GameAbstractViewDialog;

type
  TViewChoice = class(TAbstractViewDialog)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonChoiceTemplate: TCastleButton;
    LabelCaption: TCastleLabel;
  private
    procedure ClickChoice(Sender: TObject);
  public
    { Set this before Start.
      Choices is initially created by empty.
      This class doesn't modify contents of Choices, ever. }
    Choices: TStringList;
    ChoicesCaption: String;
    // Set to -1 if nothing is current, or to index on Choices.
    ChoiceCurrent: Integer;

    { Answer is set to the index of the choice that user selected.
      It's -1 if user canceled. }
    Answer: Integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
  end;

var
  ViewChoice: TViewChoice;

implementation

uses SysUtils,
  CastleComponentSerialize;

constructor TViewChoice.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewchoice.castle-user-interface';
  Choices := TStringList.Create;
end;

destructor TViewChoice.Destroy;
begin
  FreeAndNil(Choices);
  inherited;
end;

procedure TViewChoice.Start;
const
  // TODO: Would be better to display all choices, and just make a scrollable list.
  MaxChoicesCount = 10;
var
  ButtonChoiceFactory: TCastleComponentFactory;
  TemplateIndex: Integer;
  ButtonChoice: TCastleButton;
  ButtonChoiceOwner: TComponent;
  I: Integer;
  ImageCurrentMark: TCastleImageControl;
begin
  inherited;

  // default Answer, in case user cancels
  Answer := -1;

  LabelCaption.Caption := ChoicesCaption;

  ButtonChoiceFactory := TCastleComponentFactory.Create(nil);
  try
    ButtonChoiceFactory.LoadFromComponent(ButtonChoiceTemplate);
    ButtonChoiceTemplate.Exists := false; // hide the template
    TemplateIndex := ButtonChoiceTemplate.Parent.IndexOfControl(ButtonChoiceTemplate);
    for I := 0 to Choices.Count - 1 do
    begin
      ButtonChoiceOwner := TComponent.Create(FreeAtStop);

      ButtonChoice := ButtonChoiceFactory.ComponentLoad(ButtonChoiceOwner) as TCastleButton;
      ButtonChoice.Caption := Choices[I];
      ButtonChoice.OnClick := @ClickChoice;
      ButtonChoice.Exists := true; // ignore ButtonChoiceTemplate.Exists, makes it easier to experiment at designing
      ButtonChoice.Tag := I;

      ImageCurrentMark := ButtonChoiceOwner.FindRequiredComponent('ImageCurrentMark') as TCastleImageControl;
      ImageCurrentMark.Exists := I = ChoiceCurrent;

      Inc(TemplateIndex);
      ButtonChoiceTemplate.Parent.InsertControl(TemplateIndex, ButtonChoice);

      if I = MaxChoicesCount then
        Break;
    end;
  finally FreeAndNil(ButtonChoiceFactory) end;
end;

procedure TViewChoice.ClickChoice(Sender: TObject);
begin
  Answer := (Sender as TCastleButton).Tag;
  Container.PopView(Self);
end;

end.
