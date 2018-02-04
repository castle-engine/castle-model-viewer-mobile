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

unit CastleControls_TableView;

interface

uses
  Classes, Generics.Collections,
  CastleControls, CastleUIControls, CastleColors, CastleKeysMouse, CastleVectors;

type
  TCastleTableView = class;

  TCastleTableViewCellAccessoryType = (tvcaNone, tvcaCheckmark, tvcaDisclosureIndicator);

  TCastleTableViewCell = class(TCastleRectangleControl)
    private
      FTextLabel: TCastleLabel;
      FTag: Integer;
      FAccessoryType: TCastleTableViewCellAccessoryType;
      FAccessoryTypeImage: TCastleImageControl;
      FAccessoryControl: TUIControl;

      procedure SetAccessoryControl(AControl: TUIControl);
      procedure ReflectUIControls;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure MakeEmpty;

      { Use TextLabel.Caption to set the text visible on the cell. }
      property TextLabel: TCastleLabel read FTextLabel;
      { Decoration shown at the right cell border. }
      property AccessoryType: TCastleTableViewCellAccessoryType
                              read FAccessoryType write FAccessoryType default tvcaNone;
      { Any custom control added at the right cell border (e.g. SwitchControl, ImageControl) }
      property AccessoryControl: TUIControl
                              read FAccessoryControl write SetAccessoryControl default nil;
      { Custom data. }
      property Tag: Integer read FTag write FTag default 0;
  end;

  TCastleTableViewCellList = specialize TObjectList<TCastleTableViewCell>;

  ICastleTableViewDataSource = interface
      { Return number of cells (rows). }
      function TableViewNumberOfRows(Sender: TCastleTableView): Integer;
      { Called when TableView needs the contents for the cell. TableView manages
        its cells automatically. For example set Cell.TextLabel.Caption to
        define the text displayed in the cell. }
      procedure TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
  end;

  TTableViewDidSelectCellEvent = procedure(Row: Integer; Sender: TCastleTableView) of object;

  TCastleTableView = class(TCastleScrollView)
    strict private
      FDataSource: ICastleTableViewDataSource;
      FCells: TCastleTableViewCellList;
      FCellHeight: Integer;

      FOnDidSelectCell: TTableViewDidSelectCellEvent;

      FPressPos: TVector2;
      FMotionIsClick: boolean;
    public
      const
        DefaultCellHeight = 50;

      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure ReloadData;
      procedure SetDataSource(ADataSource: ICastleTableViewDataSource);

      function Press(const Event: TInputPressRelease): boolean; override;
      function Release(const Event: TInputPressRelease): boolean; override;
      function Motion(const Event: TInputMotion): boolean; override;
      procedure Update(const SecondsPassed: Single;
        var HandleInput: boolean); override;

      { TableView needs data source set in order to show some contents. }
      property DataSource: ICastleTableViewDatasource read FDataSource write SetDataSource;
      { Event called when cell was selected by user. }
      property OnSelectCell: TTableViewDidSelectCellEvent
                             read FOnDidSelectCell write FOnDidSelectCell;
      { Height of all cells }
      property CellHeight: Integer read FCellHeight write FCellHeight default DefaultCellHeight;
  end;

implementation

uses
  SysUtils, Math,
  CastleRectangles, CastleLog, CastleWindow,
  CastleControls_TableView_Img;

constructor TCastleTableViewCell.Create(AOwner: TComponent);
begin
  inherited;
  FTextLabel := TCastleLabel.Create(Self);
  FTextLabel.Anchor(hpLeft, 5);
  FTextLabel.Anchor(vpMiddle);
  InsertFront(FTextLabel);

  FAccessoryTypeImage := nil;
  FAccessoryControl := nil;

  MakeEmpty;
end;

destructor TCastleTableViewCell.Destroy;
begin
  inherited;
end;

procedure TCastleTableViewCell.MakeEmpty;
begin
  TextLabel.Caption := '';
  AccessoryType := tvcaNone;
  AccessoryControl := nil;
  Tag := 0;
  Color := Vector4(0.0, 0.0, 0.0, 0.0); // transparent
end;

procedure TCastleTableViewCell.SetAccessoryControl(AControl: TUIControl);
begin
  if AControl = FAccessoryControl then
    Exit;
  // remove old
  if FAccessoryControl <> nil then
  begin
    RemoveControl(FAccessoryControl);
    RemoveComponent(FAccessoryControl);
    FreeAndNil(FAccessoryControl);
  end;

  // set new
  if AControl <> nil then
  begin
    AControl.Anchor(vpMiddle);    // horizontal posistion is set in ReflectUIControls
    InsertFront(AControl);
    FAccessoryControl := AControl;
  end;
end;

procedure TCastleTableViewCell.ReflectUIControls;
var
  NextRight, Spacing: Integer;
begin
  if FAccessoryType = tvcaNone then
  begin
    if FAccessoryTypeImage <> nil then
      FAccessoryTypeImage.Exists := false;
  end
  else begin
    if FAccessoryTypeImage <> nil then
      FAccessoryTypeImage.Exists := true
    else begin
      FAccessoryTypeImage := TCastleImageControl.Create(Self);
      FAccessoryTypeImage.Anchor(vpMiddle);
      InsertFront(FAccessoryTypeImage);
    end;

    if FAccessoryType = tvcaCheckmark then
    begin
      FAccessoryTypeImage.OwnsImage := false;
      FAccessoryTypeImage.Image := TableViewImages.tviCheckmark;
      FAccessoryTypeImage.Color := FTextLabel.Color;
    end
    else if FAccessoryType = tvcaDisclosureIndicator then begin
      FAccessoryTypeImage.OwnsImage := false;
      FAccessoryTypeImage.Image := TableViewImages.tviDisclosureIndicator;
      FAccessoryTypeImage.Color := Theme.DisabledTextColor;
    end;
  end;

  // update horizontal positions
  NextRight := -5;
  Spacing := 8;

  if (FAccessoryTypeImage <> nil) and FAccessoryTypeImage.Exists then
  begin
    FAccessoryTypeImage.Anchor(hpRight, NextRight);
    NextRight := NextRight - FAccessoryTypeImage.CalculatedWidth - Spacing;
  end;

  if (FAccessoryControl <> nil) and (FAccessoryControl.Exists) then
  begin
    FAccessoryControl.Anchor(hpRight, NextRight);
    NextRight := NextRight - FAccessoryControl.CalculatedWidth - Spacing;
  end;

  FTextLabel.MaxWidth := CalculatedWidth - 5 + NextRight;
end;

constructor TCastleTableView.Create(AOwner: TComponent);
begin
  inherited;
  FCells := TCastleTableViewCellList.Create;
  FCells.OwnsObjects := false;
  FDataSource := nil;
  FOnDidSelectCell := nil;
  FMotionIsClick := false;
  FCellHeight := DefaultCellHeight;
end;

destructor TCastleTableView.Destroy;
begin
  FreeAndNil(FCells);
  inherited;
end;

procedure TCastleTableView.ReloadData;
var
  I, ItemCount, OldCount: Integer;
  Cell: TCastleTableViewCell;
  CellWidth, SeparatorHeight: Integer;
begin
  if ScrollbarVisible then
    CellWidth := Width - ScrollBarWidth
  else
    CellWidth := Width;
  SeparatorHeight := 1;

  if Assigned(FDataSource) then
    ItemCount := FDataSource.TableViewNumberOfRows(Self)
  else
    ItemCount := 3;
  OldCount := FCells.Count;
  for I := 0 to ItemCount - 1 do
  begin
    if I < OldCount then
    begin
      Cell := FCells.Items[I];
      Cell.MakeEmpty;
    end
    else begin
      Cell := TCastleTableViewCell.Create(ScrollArea);
      ScrollArea.InsertFront(Cell);
      FCells.Add(Cell);
    end;

    if Assigned(FDataSource) then
      FDataSource.TableViewUpdateCell(Cell, I, Self)
    else
      Cell.TextLabel.Caption := Format('Cell %d', [I+1]);

    Cell.Left := 0;
    Cell.Width := CellWidth;
    Cell.Height := CellHeight;
    Cell.Anchor(vpTop, -I * (CellHeight + SeparatorHeight));
    Cell.ReflectUIControls;
  end;

  // remove unused cells
  for I := OldCount - 1 downto ItemCount do
  begin
    Cell := FCells.Items[I];
    ScrollArea.RemoveControl(Cell);
    ScrollArea.RemoveComponent(Cell);
    FCells.Delete(I);
    FreeAndNil(Cell);
  end;

  ScrollArea.Height := ItemCount * (CellHeight + SeparatorHeight);
  ScrollArea.Width := CalculatedWidth;
end;

procedure TCastleTableView.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  WasScrollbar: boolean;
  Cell: TCastleTableViewCell;
  I, CellWidth: Integer;
begin
  WasScrollbar := ScrollbarVisible;

  inherited;

  { update the cell width when scrollbar changed visibility }
  if WasScrollbar <> ScrollbarVisible then
  begin
    if ScrollbarVisible then
      CellWidth := Width - ScrollBarWidth
    else
      CellWidth := Width;
    for I := 0 to FCells.Count - 1 do
    begin
      Cell := FCells.Items[I];
      Cell.Width := CellWidth;
      Cell.ReflectUIControls;
    end;
  end;
end;

procedure TCastleTableView.SetDataSource(ADataSource: ICastleTableViewDataSource);
begin
  FDataSource := ADataSource;
  ReloadData;
end;

function TCastleTableView.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

  if Assigned(FOnDidSelectCell) then
  begin
    FPressPos := Event.Position;
    FMotionIsClick := true;
    Result := ExclusiveEvents;   // be sure to get messages
  end;
end;

function TCastleTableView.Release(const Event: TInputPressRelease): boolean;
var
  I: Integer;
  CellRect: TRectangle;
begin
  Result := inherited;
  if Assigned(FOnDidSelectCell) and FMotionIsClick then
  begin
    //Application.Log(etInfo, Format('Touch at %d, %d', [Integer(FPressPos.X), Integer(FPressPos.Y)]));
    for I := 0 to FCells.Count - 1 do
    begin
      CellRect := FCells.Items[I].ScreenRect;
      //Application.Log(etInfo, Format('Rect at %d, %d, sz: %dx%d', [CellRect.Left, CellRect.Top, CellRect.Width, CellRect.Height]));
      if CellRect.Contains(FPressPos) then
      begin
        FOnDidSelectCell(I, Self);
        break;
      end;
    end;
  end;
  FMotionIsClick := false;
end;

function TCastleTableView.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;

  if Assigned(FOnDidSelectCell) and FMotionIsClick and (PointsDistance(Event.Position, FPressPos) > 4.0) then
    FMotionIsClick := false;
end;

end.

