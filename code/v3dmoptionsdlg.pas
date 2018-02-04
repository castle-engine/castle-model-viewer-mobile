{
  Copyright 2017-2018 Michalis Kamburelis and Jan Adamec.

  This file is part of "view3dscene-mobile".

  "view3dscene-mobile" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "view3dscene-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit V3DMOptionsDlg;

interface

uses Classes, SysUtils,
  CastleControls, CastleScene, CastleUIState, CastleKeysMouse,
  CastleControls_Switch, CastleControls_TableView,
  Game;

type
  TStateOptionsDlg = class(TUIState)
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
  StateOptionsDlg: TStateOptionsDlg;

implementation

uses
  Math,
  CastleColors, CastleWindow, CastleUIControls, CastleFilesUtils, CastleLog,
  CastleUtils, CastleVectors,
  V3DMOptions;

const
  OptCellTagShowBBox    = 0;
  OptCellTagShowFps     = 1;
  OptCellTagHeadlight   = 2;
  OptCellTagCollisions  = 3;
  OptCellTagAllNavTypes = 4;

  OptCellCount          = 5;

{ TStateOptionsDlg.TOptionsDialog ---------------------------------------------- }

constructor TStateOptionsDlg.TOptionsDialog.Create(AOwner: TComponent);
var
  InsideRect: TCastleRectangleControl;
  LabelWndTitle: TCastleLabel;
  BtnDone: TCastleButton;
  TableTop, Diff: integer;
  TableView: TCastleTableView;
begin
  inherited Create(AOwner);

  Width := Min(400, StateOptionsDlg.StateContainer.UnscaledWidth - 20);
  Height := Min(500, StateOptionsDlg.StateContainer.UnscaledHeight - 20);
  Color := Black;

  InsideRect := TCastleRectangleControl.Create(Self);
  InsideRect.Width := CalculatedWidth - 4;
  InsideRect.Height := CalculatedHeight - 4;
  InsideRect.Color := HexToColor('505050');
  InsideRect.Anchor(hpMiddle);
  InsideRect.Anchor(vpMiddle);
  InsertFront(InsideRect);

  LabelWndTitle := TCastleLabel.Create(Self);
  LabelWndTitle.Color := White;
  LabelWndTitle.Html := true;
  LabelWndTitle.Caption := '<b>Options</b>';
  LabelWndTitle.Anchor(hpMiddle);
  LabelWndTitle.Anchor(vpTop, -8);
  InsideRect.InsertFront(LabelWndTitle);

  BtnDone := TCastleButton.Create(Self);
  BtnDone.Caption := 'Done';
  BtnDone.OnClick := @BtnDoneClick;
  BtnDone.Anchor(vpTop, -4);
  BtnDone.Anchor(hpRight, -4);
  InsideRect.InsertFront(BtnDone);

  TableTop := -(BtnDone.CalculatedHeight + 14);

  TableView := TCastleTableView.Create(Self);
  TableView.EnableDragging := true;
  TableView.Width := InsideRect.Width - 10;
  TableView.Height := InsideRect.Height - 5 + TableTop;
  TableView.Anchor(hpMiddle);
  TableView.Anchor(vpTop, TableTop);
  InsideRect.InsertFront(TableView);
  TableView.DataSource := Self;

  // when tableView contents take less space, make the window smaller
  if TableView.ScrollArea.Height < TableView.Height then
  begin
    Diff := TableView.Height - TableView.ScrollArea.Height;
    TableView.Height := TableView.ScrollArea.Height;
    Height := Height - Diff;
    InsideRect.Height := CalculatedHeight - 4;
  end;
end;

function TStateOptionsDlg.TOptionsDialog.TableViewNumberOfRows(Sender: TCastleTableView): Integer;
begin
  Result := OptCellCount;
end;

procedure TStateOptionsDlg.TOptionsDialog.TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
var
  CellCaption: string;
  SwitchState, CellEnabled: boolean;
  SwitchCtl: TCastleSwitch;
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
        CellEnabled := Assigned(StateOptionsDlg.FScene);
        if Assigned(StateOptionsDlg.FScene) then
          SwitchState := StateOptionsDlg.FScene.HeadLightOn
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
  end;
  Cell.TextLabel.Caption := CellCaption;
  SwitchCtl := TCastleSwitch.Create(Cell);
  SwitchCtl.IsOn := SwitchState;
  SwitchCtl.Enabled := CellEnabled;
  SwitchCtl.Tag := Row;
  SwitchCtl.OnChange := @TableViewSwitchChanged;
  Cell.AccessoryControl := SwitchCtl;
end;

procedure TStateOptionsDlg.TOptionsDialog.TableViewSwitchChanged(Sender: TObject);
var
  SwitchCtl: TCastleSwitch;
begin
  if not (Sender is TCastleSwitch) then Exit;
  SwitchCtl := (Sender as TCastleSwitch);
  case SwitchCtl.Tag of
    OptCellTagShowBBox:  AppOptions.ShowBBox := SwitchCtl.IsOn;

    OptCellTagShowFps:   AppOptions.ShowFps  := SwitchCtl.IsOn;

    OptCellTagHeadlight:
      begin
        if Assigned(StateOptionsDlg.FScene) then
          StateOptionsDlg.FScene.HeadLightOn := SwitchCtl.IsOn;
      end;

    OptCellTagCollisions:
      begin
        AppOptions.CollisionsOn := SwitchCtl.IsOn;

        if Assigned(StateOptionsDlg.FScene) then
          StateOptionsDlg.FScene.Collides := AppOptions.CollisionsOn;
      end;

    OptCellTagAllNavTypes:
      begin
        AppOptions.ShowAllNavgationButtons := SwitchCtl.IsOn;
        ShowHideNavigationButtons(true);
      end;
  end;
end;

procedure TStateOptionsDlg.TOptionsDialog.BtnDoneClick(Sender: TObject);
begin
  DoAnswered;
end;

procedure TStateOptionsDlg.TOptionsDialog.DoAnswered;
begin
  TUIState.Pop(StateOptionsDlg);
  AppOptions.Save;
end;

{ TStateOptionsDlg ------------------------------------------------------------ }

procedure TStateOptionsDlg.Start;
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

function TStateOptionsDlg.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

  // end dialog if clicked outside dialog
  if Event.IsMouseButton(mbLeft) and (not Dialog.ScreenRect.Contains(Event.Position)) then
  begin
    Dialog.DoAnswered;
    Result := true;
  end;
end;

end.
