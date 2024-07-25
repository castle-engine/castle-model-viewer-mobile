{
  Copyright 2017-2020 Michalis Kamburelis and Jan Adamec.

  This file is part of "castle-model-viewer-mobile".

  "castle-model-viewer-mobile" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "castle-model-viewer-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit GameOptions;

interface

uses
  Classes, SysUtils;

type
  TAppOptions = class
    ShowBBox, ShowFps, CollisionsOn: boolean;
    EnableBlockingDownloads: boolean;

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
  EnableBlockingDownloads := UserConfig.GetValue('EnableBlockingDownloads', false);
end;

procedure TAppOptions.Save;
begin
  UserConfig.SetValue('ShowBBox', ShowBBox);
  UserConfig.SetValue('ShowFps', ShowFps);
  UserConfig.SetValue('CollisionsOn', CollisionsOn);
  UserConfig.SetValue('EnableBlockingDownloads', EnableBlockingDownloads);
  UserConfig.Save;
end;

finalization
  FreeAndNil(AppOptions);
end.

