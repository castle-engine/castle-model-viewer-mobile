{
  Copyright 2017-2017 Michalis Kamburelis and Jan Adamec.

  This file is part of "view3dscene-mobile".

  "view3dscene-mobile" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "view3dscene-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit V3DMOptions;

interface

uses
  Classes, SysUtils;

type
  TAppOptions = class
    ShowBBox, ShowFps, CollisionsOn: boolean;

    procedure Load;
    procedure Save;
  end;

var
  AppOptions: TAppOptions;

implementation

uses
  CastleConfig, CastleWindow;

procedure TAppOptions.Load;
begin
  try
    UserConfig.Load;
  except
    on E: Exception do
    begin
      Application.Log(etInfo, 'UserConfig.Load exception: ' + E.Message);
    end;
  end;

  ShowBBox := UserConfig.GetValue('ShowBBox', true);
  ShowFps := UserConfig.GetValue('ShowFps', false);
  CollisionsOn := UserConfig.GetValue('CollisionsOn', true);
end;

procedure TAppOptions.Save;
begin
  UserConfig.SetValue('ShowBBox', ShowBBox);
  UserConfig.SetValue('ShowFps', ShowFps);
  UserConfig.SetValue('CollisionsOn', CollisionsOn);
  UserConfig.Save;
end;

end.

