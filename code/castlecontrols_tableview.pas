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
  CastleControls, CastleColors, CastleKeysMouse;

type
  TCastleTableView = class;

  TCastleTableViewCellAccessoryType = (tvcaNone, tvcaCheckmark, tvcaDisclosureIndicator);

  TCastleTableViewCell = class(TCastleRectangleControl)
    public
      FText: string;
      FAccessoryType: TCastleTableViewCellAccessoryType;
      FTag: Integer;
      FBackgroundColor: TCastleColor;
    private
      FTextLabel, FAccessoryTypeLabel: TCastleLabel;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure MakeEmpty;
    private
      procedure ReflectUIControls;
  end;

  TCastleTableViewCellList = specialize TObjectList<TCastleTableViewCell>;

  ICastleTableViewDelegate = interface
      function TableViewNumberOfRows(Sender: TCastleTableView): Integer;
      procedure TableViewUpdateCellForRow(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
      procedure TableViewDidSelectCellAtRow(Row: Integer; Sender: TCastleTableView);
  end;

  TCastleTableView = class(TCastleScrollView)
    strict private
      FDelegate: ICastleTableViewDelegate;
      FCells: TCastleTableViewCellList;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure ReloadData;
      procedure SetDelegate(ADelegate: ICastleTableViewDelegate);

      function Press(const Event: TInputPressRelease): boolean; override;
      function Release(const Event: TInputPressRelease): boolean; override;
      function Motion(const Event: TInputMotion): boolean; override;

      property Delegate: ICastleTableViewDelegate read FDelegate write SetDelegate;
  end;

implementation

uses
  SysUtils, Math,
  CastleUIControls, CastleVectors;

constructor TCastleTableViewCell.Create(AOwner: TComponent);
begin
  inherited;
  FTextLabel := nil;
  FAccessoryTypeLabel := nil;
  MakeEmpty;
end;

destructor TCastleTableViewCell.Destroy;
begin
  inherited;
end;

procedure TCastleTableViewCell.MakeEmpty;
begin
  FText := '';
  FAccessoryType := tvcaNone;
  FTag := 0;
  FBackgroundColor := Vector4(0.0, 0.0, 0.0, 0.0); // transparent
end;

procedure TCastleTableViewCell.ReflectUIControls;
begin
  Self.Color := FBackgroundColor;

  if FTextLabel = nil then
  begin
    FTextLabel := TCastleLabel.Create(Self);
    FTextLabel.Anchor(hpLeft, 0);
    FTextLabel.Anchor(vpMiddle);
    InsertFront(FTextLabel);
  end;

  FTextLabel.Caption := FText;

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
      FAccessoryTypeLabel.Anchor(hpRight, 0);
      FAccessoryTypeLabel.Anchor(vpMiddle);
      InsertFront(FAccessoryTypeLabel);
    end;

    if FAccessoryType = tvcaCheckmark then
      FAccessoryTypeLabel.Caption := 'V'  // TODO
    else if FAccessoryType = tvcaDisclosureIndicator then
      FAccessoryTypeLabel.Caption := '>';
  end;
end;

constructor TCastleTableView.Create(AOwner: TComponent);
begin
  inherited;
  FDelegate := nil;
  FCells := TCastleTableViewCellList.Create;

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
begin
  if Assigned(FDelegate) then
    ItemCount := FDelegate.TableViewNumberOfRows(Self)
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

    if Assigned(FDelegate) then
      FDelegate.TableViewUpdateCellForRow(Cell, I, Self)
    else
      Cell.FText := Format('Cell %d', [I]);

    Cell.Left := 0;
    Cell.Width := ScrollArea.Width;
    Cell.Height := 50; // TODO
    Cell.Anchor(vpTop, -I*50);
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

  ScrollArea.Height := ItemCount * 50; // TODO
  ScrollArea.Width := CalculatedWidth;
end;

procedure TCastleTableView.SetDelegate(ADelegate: ICastleTableViewDelegate);
begin
  FDelegate := ADelegate;
  ReloadData;
end;

function TCastleTableView.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

end;

function TCastleTableView.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

end;

function TCastleTableView.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;

end;

end.

