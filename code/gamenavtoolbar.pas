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

unit GameNavToolbar;

interface

uses Classes, SysUtils, Generics.Collections,
  CastleControls, CastleUIState, CastleKeysMouse, CastleCameras,
  CastleViewport, CastleUIControls,
  GameInitialize,
  GameTable;

type
  TNavTypeList = class(specialize TList<TNavigationType>) end;
  TNavTypeSelectedEvent = procedure (NavType: TNavigationType) of object;

  TViewNavToolbar = class(TCastleView)
  strict private
    type
      TNavToolbarDialog = class(TCastleRectangleControl, ICastleTableViewDataSource)
      strict private
        procedure TableViewDidSelectCell(Row: Integer; Sender: TCastleTableView);
      public
        constructor Create(AOwner: TComponent); reintroduce;
        procedure DoAnswered;

        function TableViewNumberOfRows(Sender: TCastleTableView): Integer;
        procedure TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
      end;
    var
      Dialog: TNavToolbarDialog;
  public
    FShowAtPositionTop, FShowAtPositionLeft: Single;
    FSelectedNavType: TNavigationType;
    FAvailableNavTypes: TNavTypeList;
    FOnNavTypeSelected: TNavTypeSelectedEvent;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  ViewNavToolbar: TViewNavToolbar;

implementation

uses
  Math,
  CastleColors, CastleWindow, CastleImages, CastleFilesUtils,
  CastleUtils, CastleVectors;

{ TViewNavToolbar.TNavToolbarDialog ---------------------------------------------- }

constructor TViewNavToolbar.TNavToolbarDialog.Create(AOwner: TComponent);
var
  TableView: TCastleTableView;
begin
  inherited Create(AOwner);

  Width := 160;
  Height := Min(300, ViewNavToolbar.StateContainer.UnscaledHeight - 20);
  ThemeImage := tiPanel;
  UseThemeImage := true;

  TableView := TCastleTableView.Create(Self);
  TableView.CellHeight := 40;
  TableView.EnableDragging := true;
  TableView.OnSelectCell := @TableViewDidSelectCell;
  TableView.Width := Width - 4;
  TableView.Height := Height - 4;
  TableView.Anchor(hpMiddle);
  TableView.Anchor(vpMiddle);
  InsertFront(TableView);
  TableView.DataSource := Self;

  // when tableView contents take less space, make the window smaller
  if TableView.ScrollArea.Height < TableView.Height then
  begin
    TableView.Height := TableView.ScrollArea.Height;
    Height := TableView.Height + 4;
  end;
end;

function TViewNavToolbar.TNavToolbarDialog.TableViewNumberOfRows(Sender: TCastleTableView): Integer;
begin
  Result := ViewNavToolbar.FAvailableNavTypes.Count;
end;

procedure TViewNavToolbar.TNavToolbarDialog.TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
var
  NavType: TNavigationType;
  Title, IconUrl: String;
begin
  NavType := ViewNavToolbar.FAvailableNavTypes.Items[Row];
  case NavType of
    ntWalk:
      begin
        Title := 'Walk';
        IconUrl := 'castle-data:/nav_walk.png';
      end;
    ntFly:
      begin
        Title := 'Fly';
        IconUrl := 'castle-data:/nav_fly.png';
      end;
    ntExamine:
      begin
        Title := 'Examine';
        IconUrl := 'castle-data:/nav_examine.png';
      end;
    ntTurntable:
      begin
        Title := 'Turntable';
        IconUrl := 'castle-data:/nav_turntable.png';
      end;
  end;

  Cell.Color := Vector4(1.0, 1.0, 1.0, 0.3);
  Cell.TextLabel.Caption := Title;
  Cell.TextLabel.Color := Black;
  Cell.ImageIcon.Url := IconUrl;
  if NavType = ViewNavToolbar.FSelectedNavType then
    Cell.AccessoryType := tvcaCheckmark;
end;

procedure TViewNavToolbar.TNavToolbarDialog.TableViewDidSelectCell(Row: Integer; Sender: TCastleTableView);
var
  NavType: TNavigationType;
begin
  NavType := ViewNavToolbar.FAvailableNavTypes.Items[Row];

  if Assigned(ViewNavToolbar.FOnNavTypeSelected) then
    ViewNavToolbar.FOnNavTypeSelected(NavType);

  DoAnswered;
end;

procedure TViewNavToolbar.TNavToolbarDialog.DoAnswered;
begin
  Container.PopView(ViewNavToolbar);
end;

{ TViewNavToolbar ------------------------------------------------------------ }

procedure TViewNavToolbar.Start;
var
  TransparentBackground: TCastleRectangleControl;
begin
  inherited;

  InterceptInput := true;

  TransparentBackground := TCastleRectangleControl.Create(FreeAtStop);
  TransparentBackground.Color := Theme.BackgroundColor;
  TransparentBackground.FullSize := true;
  InsertFront(TransparentBackground);

  Dialog := TNavToolbarDialog.Create(FreeAtStop);
  Dialog.Anchor(hpLeft, FShowAtPositionLeft);
  Dialog.Anchor(vpTop, -FShowAtPositionTop);
  InsertFront(Dialog);
end;

function TViewNavToolbar.Press(const Event: TInputPressRelease): boolean;
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
