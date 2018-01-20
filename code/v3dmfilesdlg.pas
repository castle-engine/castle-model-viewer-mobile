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

unit V3DMFilesDlg;

interface

uses Classes, SysUtils,
  CastleControls, CastleScene, CastleUIState, CastleKeysMouse,
  CastleControls_TableView;

type
  TFileSelectedEvent = procedure (Url : string) of object;

  TStateFilesDlg = class(TUIState)
  strict private
    type
      TFilesDialog = class(TCastleRectangleControl, ICastleTableViewDataSource)
      strict private
        FileList: TStringList;
        procedure TableViewDidSelectCell(Row: Integer; Sender: TCastleTableView);
        procedure BtnDoneClick(Sender: TObject);
      public
        constructor Create(AOwner: TComponent); reintroduce;
        destructor Destroy; override;
        procedure DoAnswered;

        function TableViewNumberOfRows(Sender: TCastleTableView): Integer;
        procedure TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
      end;
    var
      Dialog: TFilesDialog;
  public
    FOnFileSelected: TFileSelectedEvent;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StateFilesDlg: TStateFilesDlg;

implementation

uses
  Math,
  CastleColors, CastleWindow, CastleUIControls, CastleFilesUtils, CastleLog,
  CastleUtils, CastleVectors;

{ TStateFilesDlg.TFilesDialog ---------------------------------------------- }

constructor TStateFilesDlg.TFilesDialog.Create(AOwner: TComponent);
var
  InsideRect: TCastleRectangleControl;
  LabelWndTitle: TCastleLabel;
  BtnDone: TCastleButton;
  TableTop, Diff: integer;
  TableView: TCastleTableView;
begin
  inherited Create(AOwner);

  // fixed demo scenes, TODO: make it dynamic by enumerating in data/demo folder
  FileList := TStringList.Create();
  FileList.Add(ApplicationData('demo/castle_walk.wrl'));
  FileList.Add(ApplicationData('demo/chinchilla.wrl.gz'));
  FileList.Add(ApplicationData('demo/teapot (fresnel and toon shader).x3dv'));
  FileList.Add(ApplicationData('demo/teapot (time to shader).x3dv'));

  Width := Min(400, StateFilesDlg.StateContainer.UnscaledWidth - 20);
  Height := Min(500, StateFilesDlg.StateContainer.UnscaledHeight - 20);
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
  LabelWndTitle.Caption := '<b>Demo Scenes</b>';
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
  TableView.OnSelectCell := @TableViewDidSelectCell;
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

destructor TStateFilesDlg.TFilesDialog.Destroy;
begin
  FileList.Free;
  inherited;
end;

function TStateFilesDlg.TFilesDialog.TableViewNumberOfRows(Sender: TCastleTableView): Integer;
begin
  Result := FileList.Count;
end;

procedure TStateFilesDlg.TFilesDialog.TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
begin
  Cell.Color := Vector4(0.2, 0.2, 0.2, 1.0);
  Cell.TextLabel.Caption := ExtractFileName(FileList[Row]);
end;

procedure TStateFilesDlg.TFilesDialog.TableViewDidSelectCell(Row: Integer; Sender: TCastleTableView);
begin
  if Assigned(StateFilesDlg.FOnFileSelected) then
    StateFilesDlg.FOnFileselected(FileList[Row]);

  DoAnswered;
end;

procedure TStateFilesDlg.TFilesDialog.BtnDoneClick(Sender: TObject);
begin
  DoAnswered;
end;

procedure TStateFilesDlg.TFilesDialog.DoAnswered;
begin
  TUIState.Pop(StateFilesDlg);
end;

{ TStateFilesDlg ------------------------------------------------------------ }

procedure TStateFilesDlg.Start;
var
  TransparentBackground: TCastleRectangleControl;
begin
  inherited;

  InterceptInput := true;

  TransparentBackground := TCastleRectangleControl.Create(FreeAtStop);
  TransparentBackground.Color := Theme.BackgroundColor;
  TransparentBackground.FullSize := true;
  InsertFront(TransparentBackground);

  Dialog := TFilesDialog.Create(FreeAtStop);
  Dialog.Anchor(hpMiddle);
  Dialog.Anchor(vpMiddle);
  InsertFront(Dialog);
end;

function TStateFilesDlg.Press(const Event: TInputPressRelease): boolean;
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
