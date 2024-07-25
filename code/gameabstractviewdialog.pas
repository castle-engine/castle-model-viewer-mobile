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

{ Abstract implementation of a dialog box, with transparent background
  and "Close" button. }
unit GameAbstractViewDialog;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TAbstractViewDialog = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonClose: TCastleButton;
    TransparentBackground: TCastleButton;
  private
    procedure ClickClose(Sender: TObject);
    procedure ClickTransparentBackground(Sender: TObject);
  public
    procedure Start; override;
  end;

implementation

procedure TAbstractViewDialog.ClickClose(Sender: TObject);
begin
  Container.PopView(Self);
end;

procedure TAbstractViewDialog.ClickTransparentBackground(Sender: TObject);
begin
  { Only react to clicks directly on TransparentBackground,
    not on children like colored rectangles showing dialog inside. }
  if (Container.Focus.Count <> 0) and
     (Container.Focus.Last = TransparentBackground) then
    ClickClose(nil);
end;

procedure TAbstractViewDialog.Start;
begin
  inherited;
  InterceptInput := true;
  TransparentBackground.OnClick := @ClickTransparentBackground;
  ButtonClose.OnClick := @ClickClose;
end;

end.

