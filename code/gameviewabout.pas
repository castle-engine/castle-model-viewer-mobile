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

{ About dialog, showing scene information, application and engine version. }
unit GameViewAbout;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewAbout = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonClose: TCastleButton;
    TransparentBackground: TCastleButton;
    ButtonDonate: TCastleButton;
    ButtonEngineWebsite: TCastleButton;
    ButtonRendererInfo: TCastleButton;
    LabelStats: TCastleLabel;
  private
    procedure ClickClose(Sender: TObject);
    procedure ClickDonate(Sender: TObject);
    procedure ClickEngineWebsite(Sender: TObject);
    procedure ClickRendererInfo(Sender: TObject);
  public
    // Set this before Start to display it in the dialog
    StatsText: String;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewAbout: TViewAbout;

implementation

uses CastleOpenDocument, CastleDialogViews, CastleGLUtils;

constructor TViewAbout.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewabout.castle-user-interface';
end;

procedure TViewAbout.Start;
begin
  inherited;
  LabelStats.Caption := StatsText;
  ButtonRendererInfo.OnClick := @ClickRendererInfo;
  ButtonEngineWebsite.OnClick := @ClickEngineWebsite;
  ButtonDonate.OnClick := @ClickDonate;
  ButtonClose.OnClick := @ClickClose;
  TransparentBackground.OnClick := @ClickClose;
end;

procedure TViewAbout.ClickClose(Sender: TObject);
begin
  Container.PopView(Self);
end;

procedure TViewAbout.ClickDonate(Sender: TObject);
begin
  OpenUrl('https://www.patreon.com/castleengine');
end;

procedure TViewAbout.ClickEngineWebsite(Sender: TObject);
begin
  OpenUrl('https://castle-engine.io');
end;

procedure TViewAbout.ClickRendererInfo(Sender: TObject);
var
  Dlg: TViewDialogOK;
begin
  // open new view with dialog
  Dlg := TViewDialogOK.Create(Self);
  Dlg.Caption := GLInformationString;
  Container.PushView(Dlg);
end;

end.
