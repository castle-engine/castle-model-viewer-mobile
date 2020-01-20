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

unit V3DMNavToolbar;

interface

uses Classes, SysUtils, Generics.Collections,
  CastleControls, CastleUIState, CastleKeysMouse, CastleCameras,
  Game;

type
  TNavTypeList = class(specialize TList<TNavigationType>) end;
  TNavTypeSelectedEvent = procedure (NavType: TNavigationType) of object;

  TStateNavToolbarDlg = class(TUIState)
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
  StateNavToolbarDlg: TStateNavToolbarDlg;

implementation

uses
  Math,
  CastleColors, CastleWindow, CastleUIControls, CastleImages, CastleFilesUtils,
  CastleUtils, CastleVectors;

{ TStateNavToolbarDlg.TNavToolbarDialog ---------------------------------------------- }

constructor TStateNavToolbarDlg.TNavToolbarDialog.Create(AOwner: TComponent);
var
  TableView: TCastleTableView;
begin
  inherited Create(AOwner);

  Width := 160;
  Height := Min(300, StateNavToolbarDlg.StateContainer.UnscaledHeight - 20);
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

function TStateNavToolbarDlg.TNavToolbarDialog.TableViewNumberOfRows(Sender: TCastleTableView): Integer;
begin
  Result := StateNavToolbarDlg.FAvailableNavTypes.Count;
end;

procedure TStateNavToolbarDlg.TNavToolbarDialog.TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
var
  NavType: TNavigationType;
  Title, Icon: string;
begin
  NavType := StateNavToolbarDlg.FAvailableNavTypes.Items[Row];
  case NavType of
    ntWalk:
      begin
        Title := 'Walk';
        Icon := 'nav_walk.png';
      end;
    ntFly:
      begin
        Title := 'Fly';
        Icon := 'nav_fly.png';
      end;
    ntExamine:
      begin
        Title := 'Examine';
        Icon := 'nav_examine.png';
      end;
    ntTurntable:
      begin
        Title := 'Turntable';
        Icon := 'nav_turntable.png';
      end;
  end;

  Cell.Color := Vector4(1.0, 1.0, 1.0, 0.3);
  Cell.TextLabel.Caption := Title;
  Cell.TextLabel.Color := Black;
  Cell.ImageIcon.Image := CastleImages.LoadImage(ApplicationData(Icon));
  Cell.ImageIcon.OwnsImage := true;
  if NavType = StateNavToolbarDlg.FSelectedNavType then
    Cell.AccessoryType := tvcaCheckmark;
end;

procedure TStateNavToolbarDlg.TNavToolbarDialog.TableViewDidSelectCell(Row: Integer; Sender: TCastleTableView);
var
  NavType: TNavigationType;
begin
  NavType := StateNavToolbarDlg.FAvailableNavTypes.Items[Row];

  if Assigned(StateNavToolbarDlg.FOnNavTypeSelected) then
    StateNavToolbarDlg.FOnNavTypeSelected(NavType);

  DoAnswered;
end;

procedure TStateNavToolbarDlg.TNavToolbarDialog.DoAnswered;
begin
  TUIState.Pop(StateNavToolbarDlg);
end;

{ TStateNavToolbarDlg ------------------------------------------------------------ }

procedure TStateNavToolbarDlg.Start;
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

function TStateNavToolbarDlg.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

  // end dialog if clicked outside dialog
  if Event.IsMouseButton(mbLeft) and (not Dialog.RenderRect.Contains(Event.Position)) then
  begin
    Dialog.DoAnswered;
    Result := true;
  end;
end;

end.
