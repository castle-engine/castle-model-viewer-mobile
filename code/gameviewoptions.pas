{
  Copyright 2017-2020 Michalis Kamburelis and Jan Adamec.

  This file is part of "view3dscene-mobile".

  "view3dscene-mobile" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "view3dscene-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit GameViewOptions;

interface

uses Classes, SysUtils,
  CastleUIControls, CastleControls, CastleScene, CastleUIState, CastleKeysMouse,
  GameInitialize, GameTable;

type
  TViewOptions = class(TCastleView)
  strict private
    type
      TOptionsDialog = class(TCastleRectangleControl, ICastleTableViewDataSource)
      strict private
        procedure TableViewSwitchChanged(Sender: TObject);
        procedure BtnDoneClick(Sender: TObject);
      public
        constructor Create(AOwner: TComponent); reintroduce;
        procedure DoAnswered;

        function TableViewNumberOfRows(Sender: TCastleTableView): Integer;
        procedure TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
      end;
    var
      Dialog: TOptionsDialog;
  public
    FScene: TCastleScene;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  ViewOptions: TViewOptions;

implementation

uses
  Math,
  CastleColors, CastleWindow, CastleFilesUtils, CastleLog,
  CastleUtils, CastleVectors, CastleDownload,
  GameOptions, GameViewDisplayScene;

const
  OptCellTagShowBBox    = 0;
  OptCellTagShowFps     = 1;
  OptCellTagHeadlight   = 2;
  OptCellTagCollisions  = 3;
  OptCellTagAllNavTypes = 4;
  OptCellTagDownloadRes = 5;

  OptCellCount          = 6;

{ TViewOptions.TOptionsDialog ---------------------------------------------- }

constructor TViewOptions.TOptionsDialog.Create(AOwner: TComponent);
var
  LabelWndTitle: TCastleLabel;
  BtnDone: TCastleButton;
  TableTop, Diff: Single;
  TableView: TCastleTableView;
begin
  inherited Create(AOwner);

  Width := Min(400, ViewOptions.StateContainer.UnscaledWidth - 20);
  Height := Min(500, ViewOptions.StateContainer.UnscaledHeight - 20);
  ThemeImage := tiWindow;
  UseThemeImage := true;

  LabelWndTitle := TCastleLabel.Create(Self);
  LabelWndTitle.Color := White;
  LabelWndTitle.Html := true;
  LabelWndTitle.Caption := '<b>Options</b>';
  LabelWndTitle.Anchor(hpMiddle);
  LabelWndTitle.Anchor(vpTop, -14);
  InsertFront(LabelWndTitle);

  BtnDone := TCastleButton.Create(Self);
  BtnDone.Caption := 'Done';
  BtnDone.OnClick := @BtnDoneClick;
  BtnDone.Anchor(vpTop, -7);
  BtnDone.Anchor(hpRight, -7);
  InsertFront(BtnDone);

  TableTop := -(BtnDone.EffectiveHeight + 14);

  TableView := TCastleTableView.Create(Self);
  TableView.EnableDragging := true;
  TableView.Width := Width - 14;
  TableView.Height := Height - 7 + TableTop;
  TableView.Anchor(hpMiddle);
  TableView.Anchor(vpTop, TableTop);
  InsertFront(TableView);
  TableView.DataSource := Self;

  // when tableView contents take less space, make the window smaller
  if TableView.ScrollArea.Height < TableView.Height then
  begin
    Diff := TableView.Height - TableView.ScrollArea.Height;
    TableView.Height := TableView.ScrollArea.Height;
    Height := Height - Diff;
  end;
end;

function TViewOptions.TOptionsDialog.TableViewNumberOfRows(Sender: TCastleTableView): Integer;
begin
  Result := OptCellCount;
end;

procedure TViewOptions.TOptionsDialog.TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
var
  CellCaption: string;
  SwitchState, CellEnabled: boolean;
  SwitchCtl: TCastleSwitchControl;
begin
  Cell.Color := Vector4(0.2, 0.2, 0.2, 1.0);
  CellEnabled := true;
  SwitchState := true;

  case Row of
    OptCellTagShowBBox:
      begin
        CellCaption := 'Show bounding box';
        SwitchState := AppOptions.ShowBBox;
      end;
    OptCellTagShowFps:
      begin
        CellCaption := 'Show FPS';
        SwitchState := AppOptions.ShowFps;
      end;
    OptCellTagHeadlight:
      begin
        CellCaption := 'Headlight';
        CellEnabled := Assigned(ViewOptions.FScene);
        if Assigned(ViewOptions.FScene) then
          SwitchState := ViewOptions.FScene.HeadLightOn
        else
          SwitchState := false;
      end;
    OptCellTagCollisions:
      begin
        CellCaption := 'Collisions';
        SwitchState := AppOptions.CollisionsOn;
      end;
    OptCellTagAllNavTypes:
      begin
        CellCaption := 'Enable all navigation types';
        SwitchState := AppOptions.ShowAllNavgationButtons;
      end;
    OptCellTagDownloadRes:
      begin
        CellCaption := 'Download resources from network';
        SwitchState := AppOptions.DownloadResourcesFromNetwork;
      end;
  end;
  Cell.TextLabel.Caption := CellCaption;
  SwitchCtl := TCastleSwitchControl.Create(Cell);
  SwitchCtl.Checked := SwitchState;
  SwitchCtl.Enabled := CellEnabled;
  SwitchCtl.Tag := Row;
  SwitchCtl.OnChange := @TableViewSwitchChanged;
  Cell.AccessoryControl := SwitchCtl;
end;

procedure TViewOptions.TOptionsDialog.TableViewSwitchChanged(Sender: TObject);
var
  SwitchCtl: TCastleSwitchControl;
begin
  if not (Sender is TCastleSwitchControl) then Exit;
  SwitchCtl := (Sender as TCastleSwitchControl);
  case SwitchCtl.Tag of
    OptCellTagShowBBox:  AppOptions.ShowBBox := SwitchCtl.Checked;

    OptCellTagShowFps:   AppOptions.ShowFps  := SwitchCtl.Checked;

    OptCellTagHeadlight:
      begin
        if Assigned(ViewOptions.FScene) then
          ViewOptions.FScene.HeadLightOn := SwitchCtl.Checked;
      end;

    OptCellTagCollisions:
      begin
        AppOptions.CollisionsOn := SwitchCtl.Checked;

        if Assigned(ViewOptions.FScene) then
          ViewOptions.FScene.Collides := AppOptions.CollisionsOn;
      end;

    OptCellTagAllNavTypes:
      begin
        AppOptions.ShowAllNavgationButtons := SwitchCtl.Checked;
        ViewDisplayScene.ShowHideNavigationButtons(true);
      end;

      OptCellTagDownloadRes:
      begin
        AppOptions.DownloadResourcesFromNetwork := SwitchCtl.Checked;
        EnableBlockingDownloads := AppOptions.DownloadResourcesFromNetwork;
      end;
  end;
end;

procedure TViewOptions.TOptionsDialog.BtnDoneClick(Sender: TObject);
begin
  DoAnswered;
end;

procedure TViewOptions.TOptionsDialog.DoAnswered;
begin
  Container.PopView(ViewOptions);
  AppOptions.Save;
end;

{ TViewOptions ------------------------------------------------------------ }

procedure TViewOptions.Start;
var
  TransparentBackground: TCastleRectangleControl;
begin
  inherited;

  InterceptInput := true;

  TransparentBackground := TCastleRectangleControl.Create(FreeAtStop);
  TransparentBackground.Color := Theme.BackgroundColor;
  TransparentBackground.FullSize := true;
  InsertFront(TransparentBackground);

  Dialog := TOptionsDialog.Create(FreeAtStop);
  Dialog.Anchor(hpMiddle);
  Dialog.Anchor(vpMiddle);
  InsertFront(Dialog);
end;

function TViewOptions.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

  // end dialog if clicked outside dialog
  if Event.IsMouseButton(buttonLeft) and (not Dialog.RenderRect.Contains(Event.Position)) then
  begin
    Dialog.DoAnswered;
    Result := true;
  end;
end;

end.
