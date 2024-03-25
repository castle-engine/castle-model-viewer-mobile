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

unit GameViewFiles;

interface

uses Classes, SysUtils,
  CastleUIControls, CastleControls, CastleScene, CastleKeysMouse,
  GameTable;

type
  TFileSelectedEvent = procedure (Url : string) of object;

  TViewFiles = class(TCastleView)
  strict private
    type
      TFilesDialog = class(TCastleRectangleControl, ICastleTableViewDataSource)
      strict private
        FileList: TStringList;
        procedure TableViewDidSelectCell(Row: Integer; Sender: TCastleTableView);
        procedure BtnCloseClick(Sender: TObject);
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
  ViewFiles: TViewFiles;

implementation

uses
  Math,
  CastleColors, CastleWindow, CastleFilesUtils, CastleLog,
  CastleUtils, CastleVectors;

{ TViewFiles.TFilesDialog ---------------------------------------------- }

constructor TViewFiles.TFilesDialog.Create(AOwner: TComponent);
var
  LabelWndTitle: TCastleLabel;
  BtnClose: TCastleButton;
  TableTop, Diff: Single;
  TableView: TCastleTableView;
begin
  inherited Create(AOwner);

  // fixed demo scenes, TODO: make it dynamic by enumerating in data/demo folder
  FileList := TStringList.Create();
  FileList.Add('castle-data:/demo/castle_walk.wrl');
  FileList.Add('castle-data:/demo/chinchilla.wrl');
  FileList.Add('castle-data:/demo/teapot (fresnel and toon shader).x3dv');
  FileList.Add('castle-data:/demo/teapot (time to shader).x3dv');
  FileList.Add('castle-data:/demo/dragon-spine/dragon.json');
  FileList.Add('castle-data:/demo/gltf-duck/duck.gltf');

  Width := Min(400, ViewFiles.StateContainer.UnscaledWidth - 20);
  Height := Min(500, ViewFiles.StateContainer.UnscaledHeight - 20);
  ThemeImage := tiWindow;
  UseThemeImage := true;

  LabelWndTitle := TCastleLabel.Create(Self);
  LabelWndTitle.Color := White;
  LabelWndTitle.Html := true;
  LabelWndTitle.Caption := '<b>Demo Scenes</b>';
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

destructor TViewFiles.TFilesDialog.Destroy;
begin
  FileList.Free;
  inherited;
end;

function TViewFiles.TFilesDialog.TableViewNumberOfRows(Sender: TCastleTableView): Integer;
begin
  Result := FileList.Count;
end;

procedure TViewFiles.TFilesDialog.TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
begin
  Cell.Color := Vector4(0.2, 0.2, 0.2, 1.0);
  Cell.TextLabel.Caption := ExtractFileName(FileList[Row]);
end;

procedure TViewFiles.TFilesDialog.TableViewDidSelectCell(Row: Integer; Sender: TCastleTableView);
begin
  if Assigned(ViewFiles.FOnFileSelected) then
    ViewFiles.FOnFileselected(FileList[Row]);

  DoAnswered;
end;

procedure TViewFiles.TFilesDialog.BtnCloseClick(Sender: TObject);
begin
  DoAnswered;
end;

procedure TViewFiles.TFilesDialog.DoAnswered;
begin
  Container.PopView(ViewFiles);
end;

{ TViewFiles ------------------------------------------------------------ }

procedure TViewFiles.Start;
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

function TViewFiles.Press(const Event: TInputPressRelease): boolean;
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
