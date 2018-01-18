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

unit CastleControls_TableView_Img;

interface

uses
  Classes,
  CastleImages;

type
  TCastleTableViewImages = class
    public
      tviCheckmark, tviDisclosureIndicator: TCastleImage;

      constructor Create;
      destructor Destroy; override;

      procedure LoadImages;
  end;

var
  TableViewImages: TCastleTableViewImages;

implementation

uses
  SysUtils,
  CastleFilesUtils;

constructor TCastleTableViewImages.Create;
begin
  tviCheckmark := nil;
  tviDisclosureIndicator := nil;
end;

destructor TCastleTableViewImages.Destroy;
begin
  FreeAndNil(tviCheckmark);
  FreeAndNil(tviDisclosureIndicator);
  inherited;
end;

procedure TCastleTableViewImages.LoadImages;
begin
  tviCheckmark := CastleImages.LoadImage(ApplicationData('tableview_checkmark.png'));
  tviDisclosureIndicator := CastleImages.LoadImage(ApplicationData('tableview_disclosure.png'));
end;

end.
