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

unit GameViewViewpoints;

interface

uses Classes, SysUtils,
  CastleUIControls, CastleControls, CastleScene, CastleUIState, CastleKeysMouse,
  GameTable;

type
  TViewpointSelectedEvent = procedure (ViewpointIdx : integer) of object;

  TViewViewpoints = class(TCastleView)
  strict private
    type
      TViewpointsDialog = class(TCastleRectangleControl, ICastleTableViewDataSource)
      strict private
        procedure TableViewDidSelectCell(Row: Integer; Sender: TCastleTableView);
        procedure BtnCloseClick(Sender: TObject);
      public
        constructor Create(AOwner: TComponent); reintroduce;
        procedure DoAnswered;

        function TableViewNumberOfRows(Sender: TCastleTableView): Integer;
        procedure TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
      end;
    var
      Dialog: TViewpointsDialog;
  public
    FScene: TCastleScene;
    FCurrentViewpointIdx: integer;
    FOnViewpointSelected: TViewpointSelectedEvent;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  ViewViewpoints: TViewViewpoints;

implementation

uses
  Math,
  CastleColors, CastleWindow, CastleFilesUtils, CastleLog,
  CastleUtils, CastleVectors;

{ TViewViewpoints.TViewpointsDialog ---------------------------------------------- }

constructor TViewViewpoints.TViewpointsDialog.Create(AOwner: TComponent);
var
  LabelWndTitle: TCastleLabel;
  BtnClose: TCastleButton;
  TableTop, Diff: Single;
  TableView: TCastleTableView;
begin
  inherited Create(AOwner);

  Width := Min(400, ViewViewpoints.StateContainer.UnscaledWidth - 20);
  Height := Min(500, ViewViewpoints.StateContainer.UnscaledHeight - 20);
  ThemeImage := tiWindow;
  UseThemeImage := true;

  LabelWndTitle := TCastleLabel.Create(Self);
  LabelWndTitle.Color := White;
  LabelWndTitle.Html := true;
  LabelWndTitle.Caption := '<b>Viewpoints</b>';
  LabelWndTitle.Anchor(hpMiddle);
  LabelWndTitle.Anchor(vpTop, -14);
  InsertFront(LabelWndTitle);

  BtnClose := TCastleButton.Create(Self);
  BtnClose.Caption := 'Close';
  BtnClose.OnClick := @BtnCloseClick;
  BtnClose.Anchor(vpTop, -7);
  BtnClose.Anchor(hpRight, -7);
  InsertFront(BtnClose);

  TableTop := -(BtnClose.EffectiveHeight + 14);

  TableView := TCastleTableView.Create(Self);
  TableView.EnableDragging := true;
  TableView.OnSelectCell := @TableViewDidSelectCell;
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

function TViewViewpoints.TViewpointsDialog.TableViewNumberOfRows(Sender: TCastleTableView): Integer;
begin
  if Assigned(ViewViewpoints.FScene) then
     Result := ViewViewpoints.FScene.ViewpointsCount
  else
     Result := 0;
end;

procedure TViewViewpoints.TViewpointsDialog.TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
begin
  Cell.Color := Vector4(0.2, 0.2, 0.2, 1.0);
  Cell.TextLabel.Caption := ViewViewpoints.FScene.GetViewpointName(Row);
  if Row = ViewViewpoints.FCurrentViewpointIdx then
    Cell.AccessoryType := tvcaCheckmark;
end;

procedure TViewViewpoints.TViewpointsDialog.TableViewDidSelectCell(Row: Integer; Sender: TCastleTableView);
begin
  if Assigned(ViewViewpoints.FOnViewpointSelected) then
    ViewViewpoints.FOnViewpointSelected(Row);

  DoAnswered;
end;

procedure TViewViewpoints.TViewpointsDialog.BtnCloseClick(Sender: TObject);
begin
  DoAnswered;
end;

procedure TViewViewpoints.TViewpointsDialog.DoAnswered;
begin
  Container.PopView(ViewViewpoints);
end;

{ TViewViewpoints ------------------------------------------------------------ }

procedure TViewViewpoints.Start;
var
  TransparentBackground: TCastleRectangleControl;
begin
  inherited;

  InterceptInput := true;

  TransparentBackground := TCastleRectangleControl.Create(FreeAtStop);
  TransparentBackground.Color := Theme.BackgroundColor;
  TransparentBackground.FullSize := true;
  InsertFront(TransparentBackground);

  Dialog := TViewpointsDialog.Create(FreeAtStop);
  Dialog.Anchor(hpMiddle);
  Dialog.Anchor(vpMiddle);
  InsertFront(Dialog);
end;

function TViewViewpoints.Press(const Event: TInputPressRelease): boolean;
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
