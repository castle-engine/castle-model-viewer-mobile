{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "castle-model-viewer-mobile".

  This file is free software.
  You can use it, redistribute it, and/or modify it under the terms of the

  - GNU GPL >= 2 (just like the rest of Castle Model Viewer Mobile)

  - or, at your choice, you can use it on terms of "modified BSD (3-clause)"
    license. This is the same license as we use for CGE examples,
    https://castle-engine.io/license#_engine_examples_very_permissive_do_what_you_want_bsd .

  ----------------------------------------------------------------------------
}

{ Take a screenshot(s) of arbitrary size of the current window (viewport and UI).
  This is useful to generate screenshots for social media
  or app stores in a particular resolution.

  The size(s) are given to the MakeScreenShot command below.

  - The sizes do not have to match the window size.
    Sizes can be both smaller and *larger* than the current window size.

  - Sizes can be even larger than your monitor resolution.
    So this is more powerful than running with command-line like
    "--geometry 2752x2064" on desktop, which would be still limited by what
    your GUI system supports -- it will most likely clamp the requested size
    to your desktop resolution.
    This approach doesn't have this limitation.

  - You can also take multiple screenshots of different sizes at once,
    just use MakeScreenShot command below multiple times with a few sizes.

  This takes a screenshot using FBO, which is off-screen buffer with arbitrary
  size. }
unit GameScreenShotArbitrarySize;

interface

uses Classes, SysUtils,
  CastleUiControls, CastleKeysMouse, CastleLog;

type
  TScreenShotArbitrarySize = class(TCastleUserInterface)
  public
    constructor Create(AOwner: TComponent); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  { Single instance of TScreenShotArbitrarySize. }
  ScreenShotArbitrarySize: TScreenShotArbitrarySize;

implementation

uses CastleGlImages, CastleGlUtils, CastleFilesUtils, CastleRectangles,
  CastleImages, CastleWindow;

constructor TScreenShotArbitrarySize.Create(AOwner: TComponent);
begin
  inherited;
  KeepInFront := true;
  FullSize := true;
end;

function TScreenShotArbitrarySize.Press(const Event: TInputPressRelease): Boolean;

  { Make screenshot with size W, H. }
  procedure MakeScreenShot(const W, H: Integer);
  var
    RenderToTexture: TGLRenderToTexture;
    Rect: TRectangle;
    Image: TCastleImage;
    SavedWindowWidth, SavedWindowHeight: Integer;
    UrlPattern: String;
  begin
    RenderToTexture := TGLRenderToTexture.Create(W, H);
    try
      { Configure FBO to have the same properties as our regular rendering,
        as much as possible. }
      RenderToTexture.Buffer := tbNone;
      RenderToTexture.Stencil := GLFeatures.StencilBits > 0;
      if GLFeatures.FBOMultiSampling then
        RenderToTexture.MultiSampling := GLFeatures.CurrentMultiSampling;
      RenderToTexture.GLContextOpen;
      RenderToTexture.RenderBegin;

      { This is hack, not guaranteed to be always supported!
        We need to change Container.PixelsWidth / PixelsHeight
        to match the FBO size, to make UI controls adjust to our desired size.

        Right now Container doesn't allow it, and Container.PixelsWidth/Height
        always reflect window size.
        However, we can cheat window size: right now we can set Window.Width/Height
        while the window is open, it will make a warning and not do anything
        else. So this is how we can change Container.PixelsWidth/Height...

        In the future, Container should support something like
        Container.RenderToSize(W, H) and/or the Container.RenderControl
        should just automatically do it. }

      SavedWindowWidth := Application.MainWindow.Width;
      SavedWindowHeight := Application.MainWindow.Height;
      Application.MainWindow.Width := W;
      Application.MainWindow.Height := H;
      try
        { Change container UI scaling, which will be used then by controls. }
        Container.EventResize;

        Rect := Rectangle(0, 0, W, H);
        { EventBeforeRender calls CheckUIScaleChanged, CheckResize, BeforeRender
          on everything.
          Not really needed for now:
          - Container.RenderControl does CheckResize, BeforeRender
            (non-recursive) on View.
          - CheckUIScaleChanged would call UIScaleChanged, but we don't desperately
            need it, it mostly just causes "visible changed" messages.
          But it is nice to be sure we did everything to correctly apply
          new size. }
        Container.EventBeforeRender;
        Container.RenderControl(Container.View, Rect);
        Image := Container.SaveScreen(Rect);
        try
          UrlPattern := Format('screenshot_%dx%d_%%d.png', [W, H]);
          Image.Url := FileNameAutoInc(UrlPattern);
          SaveImage(Image, Image.Url);
          WritelnLog('Saved image %s with size %d %d', [
            Image.Url, Image.Width, Image.Height
          ]);
        finally FreeAndNil(Image) end;

      finally
        Application.MainWindow.Width := SavedWindowWidth;
        Application.MainWindow.Height := SavedWindowHeight;

        { Restore UI scaling }
        Container.EventResize;
      end;

      RenderToTexture.RenderEnd;
    finally FreeAndNil(RenderToTexture) end;
  end;

begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyF5) then
  begin
    { For AppStore "13-inch iPad displays". }
    MakeScreenShot(2752, 2064); // landscape
    // MakeScreenShot(2064, 2752); // portrait
    // MakeScreenShot(2732, 2048); // landscape, alt size
    // MakeScreenShot(2048, 2732); // portrait, alt size
    { For AppStore "iPhone 6.9 Display" }
    // MakeScreenShot(1320, 2868); // portrait
    MakeScreenShot(2868, 1320); // landscape
    // MakeScreenShot(1290, 2796); // portrait, alt size
    // MakeScreenShot(2796, 1290); // landscape, alt size
     Exit(true);
  end;
end;

end.
