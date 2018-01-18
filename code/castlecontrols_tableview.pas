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
      FTextLabel, FAccessoryTypeLabel: TCastleLabel;
      FTag: Integer;
      FAccessoryType: TCastleTableViewCellAccessoryType;
      FAccessoryControl: TUIControl;

      procedure SetAccessoryControl(AControl: TUIControl);
      procedure ReflectUIControls;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure MakeEmpty;

      property TextLabel: TCastleLabel read FTextLabel;
      property AccessoryType: TCastleTableViewCellAccessoryType
                              read FAccessoryType write FAccessoryType default tvcaNone;
      property AccessoryControl: TUIControl
                              read FAccessoryControl write SetAccessoryControl default nil;
      property Tag: Integer read FTag write FTag default 0;
  end;

  TCastleTableViewCellList = specialize TObjectList<TCastleTableViewCell>;

  ICastleTableViewDataSource = interface
      function TableViewNumberOfRows(Sender: TCastleTableView): Integer;
      procedure TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
  end;

  TTableViewDidSelectCellEvent = procedure(Row: Integer; Sender: TCastleTableView) of object;

  TCastleTableView = class(TCastleScrollView)
    strict private
      FDataSource: ICastleTableViewDataSource;
      FCells: TCastleTableViewCellList;

      FOnDidSelectCell: TTableViewDidSelectCellEvent;

      FPressPos: TVector2;
      FMotionIsClick: boolean;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure ReloadData;
      procedure SetDataSource(ADataSource: ICastleTableViewDataSource);

      function Press(const Event: TInputPressRelease): boolean; override;
      function Release(const Event: TInputPressRelease): boolean; override;
      function Motion(const Event: TInputMotion): boolean; override;

      property DataSource: ICastleTableViewDatasource read FDataSource write SetDataSource;
      property OnSelectCell: TTableViewDidSelectCellEvent
                             read FOnDidSelectCell write FOnDidSelectCell;
  end;

implementation

uses
  SysUtils, Math,
  CastleRectangles, CastleLog, CastleWindow;

constructor TCastleTableViewCell.Create(AOwner: TComponent);
begin
  inherited;
  FTextLabel := TCastleLabel.Create(Self);
  FTextLabel.Anchor(hpLeft, 5);
  FTextLabel.Anchor(vpMiddle);
  InsertFront(FTextLabel);

  FAccessoryTypeLabel := nil;
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
    if FAccessoryTypeLabel <> nil then
      FAccessoryTypeLabel.Exists := false;
  end
  else begin
    if FAccessoryTypeLabel <> nil then
      FAccessoryTypeLabel.Exists := true
    else begin
      FAccessoryTypeLabel := TCastleLabel.Create(Self);
      FAccessoryTypeLabel.Anchor(vpMiddle);
      InsertFront(FAccessoryTypeLabel);
    end;

    if FAccessoryType = tvcaCheckmark then
    begin
      FAccessoryTypeLabel.Caption := 'V';  // TODO: checkmark U+2714 or 2713
      FAccessoryTypeLabel.Color := FTextLabel.Color;
    end
    else if FAccessoryType = tvcaDisclosureIndicator then begin
      FAccessoryTypeLabel.Caption := '>';
      FAccessoryTypeLabel.Color := Theme.DisabledTextColor;
    end;
  end;

  // update horizontal positions
  NextRight := -5;
  Spacing := 8;

  if (FAccessoryTypeLabel <> nil) and FAccessoryTypeLabel.Exists then
  begin
    FAccessoryTypeLabel.Anchor(hpRight, NextRight);
    NextRight := NextRight - FAccessoryTypeLabel.CalculatedWidth - Spacing;
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
  FDataSource := nil;
  FOnDidSelectCell := nil;
  FMotionIsClick := false;
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
  CellHeight, SeparatorHeight: Integer;
begin
  CellHeight := 50; // TODO
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
    Cell.Width := Width - ScrollBarWidth;
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

