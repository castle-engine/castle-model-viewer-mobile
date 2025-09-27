{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "castle-model-viewer-mobile".

  "castle-model-viewer-mobile" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle-model-viewer-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle-model-viewer-mobile"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}
{ Auto-tests for some "castle-model-viewer-mobile" code. }

uses SysUtils, Classes,
  GameViewDisplayScene;

{ Regardless of DEBUG or RELEASE, always compile with assertions checking
  code below, as tests rely on Assert. }
{$assertions on}

{ Test TViewDisplayScene.BestSceneInZip. }
procedure TestBestSceneInZip;

  procedure CheckBest(const Files: array of String; const Expected: String);
  var
    FilesList: TStringList;
  begin
    FilesList := TStringList.Create;
    try
      FilesList.AddStrings(Files);
      Assert(TViewDisplayScene.BestSceneInZip(FilesList) = Expected);
      // Writeln('OK: ' + Expected + ' = ' + TViewDisplayScene.BestSceneInZip(FilesList));
    finally FreeAndNil(FilesList) end;
  end;

begin
  // check chooses 1st alphabetically
  CheckBest([
    'a.x3d',
    'b.x3d',
    'c.x3d'
  ], 'a.x3d');
  CheckBest([
    'c.x3d',
    'b.x3d',
    'a.x3d'
  ], 'a.x3d');

  // check library is the last choice
  CheckBest([
    'library.x3d',
    'z.x3d'
  ], 'z.x3d');
  CheckBest([
    'library.x3d'
  ], 'library.x3d');

  // check image is the last choice
  CheckBest([
    'image.png',
    'z.x3d'
  ], 'z.x3d');
  CheckBest([
    'image.png',
    'library.x3d',
    'z.x3d'
  ], 'z.x3d');
  CheckBest([
    'image.png',
    'library.x3d'
  ], 'library.x3d');
  CheckBest([
    'image.png'
  ], 'image.png');
end;

begin
  TestBestSceneInZip;
end.
