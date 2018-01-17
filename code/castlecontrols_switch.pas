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

unit CastleControls_Switch;

interface

uses
  Classes, Generics.Collections,
  CastleControls, CastleColors, CastleKeysMouse, CastleVectors, CastleRectangles;

type
  TCastleSwitch = class(TUIControlFont)
    strict private
      FIsOn: boolean;
      FEnabled: boolean;
      FPressed: boolean;
      FWidth: Cardinal;
      FHeight: Cardinal;
      FOnChange: TNotifyEvent;

      procedure SetIsOn(const Value: boolean);
      procedure SetEnabled(const Value: boolean);

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure Render; override;
      function Rect: TRectangle; override;
      function Press(const Event: TInputPressRelease): boolean; override;
      function Release(const Event: TInputPressRelease): boolean; override;

    published
      property IsOn: boolean read FIsOn write SetIsOn default false;
      property Enabled: boolean read FEnabled write SetEnabled default true;
      property Width: Cardinal read FWidth;
      property Height: Cardinal read FHeight;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  SysUtils, Math,
  CastleUIControls, CastleGLImages;

constructor TCastleSwitch.Create(AOwner: TComponent);
begin
  inherited;
  FIsOn := false;
  FEnabled := true;
  FPressed := false;
  FOnChange := nil;
  FWidth := 50;
  FHeight := 28;
end;

destructor TCastleSwitch.Destroy;
begin
  inherited;
end;

procedure TCastleSwitch.Render;
var
  BaseRect, BackRect, HandleRect: TRectangle;
  HandleImage: TThemeImage;
begin
  inherited;

  BaseRect := ScreenRect;

  // background
  BackRect := BaseRect;
  BackRect.Height := BaseRect.Height div 2;
  BackRect.Bottom := BaseRect.Bottom + (BaseRect.Height div 4);
  //Theme.GLImages[tiProgressFill].IgnoreTooLargeCorners := true;
  //Theme.GLImages[tiProgressBar].IgnoreTooLargeCorners := true;
  if IsOn then
    Theme.Draw(BackRect, tiProgressFill, UIScale)
  else
    Theme.Draw(BackRect, tiProgressBar, UIScale);

  // handle
  HandleRect := BaseRect;
  HandleRect.Width := HandleRect.Height;
  if IsOn then
    HandleRect.Left := BaseRect.Right - HandleRect.Width;

  if FPressed then
    HandleImage := tiButtonPressed
  else if not Enabled then
    HandleImage := tiButtonDisabled
  else if Focused then
    HandleImage := tiButtonFocused
  else
    HandleImage := tiButtonNormal;
  Theme.Draw(HandleRect, HandleImage, UIScale)
end;

function TCastleSwitch.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom, Width, Height);
  Result := Result.ScaleAround0(UIScale);
end;

procedure TCastleSwitch.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    VisibleChange;
  end;
end;

procedure TCastleSwitch.SetIsOn(const Value: boolean);
begin
  if FIsOn <> Value then
  begin
    FIsOn := Value;
    VisibleChange;
  end;
end;

function TCastleSwitch.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  FPressed := true;
  Result := ExclusiveEvents;
end;

function TCastleSwitch.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  FPressed := false;
  FIsOn := not FIsOn;
  VisibleChange;

  if Assigned(FOnChange) then
    FOnChange(Self);
  Result := ExclusiveEvents;
end;

end.

