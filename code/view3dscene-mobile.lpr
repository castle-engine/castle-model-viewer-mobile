{
  Copyright 2017-2017 Michalis Kamburelis and Jan Adamec.

  This file is part of "view3dscene-mobile".

  "view3dscene-mobile" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "view3dscene-mobile" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "view3dscene-mobile"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ "view3dscene-mobile" standalone game binary. }
//program view3Dscene-mobile; // cannot use - in program name, also it's not needed

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses CastleWindow, CastleConfig, CastleParameters, CastleLog, CastleUtils,
  CastleSoundEngine, CastleClassUtils,
  Game;

const
  Options: array [0..0] of TOption =
  (
    (Short:  #0; Long: 'debug-log'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: InitializeLog;
    else raise EInternalError.Create('OptionProc');
  end;
end;

begin
  Window.ParseParameters;
  Parameters.Parse(Options, @OptionProc, nil);
  Window.OpenAndRun;
end.
