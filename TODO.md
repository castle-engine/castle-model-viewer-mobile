## User Visible

- Test iOS:
  - the ability to open custom ZIP, X3D, glTF
  - can we store pictures in Photo library
- On Android, it seems we store in "Pictures" folder, not in "Photos" app.
- Release on Google Play, AppStore
- Animations panel like view3dscene?
- Checkboxes in Options - upgrade look of "switch" to be modern in CGE, use it
- Implement "zip with model" handling at CGE level, also in regular view3dscene
- build release with CGE keys
- put on Google Play open testing
- put on AppStore TestFlight
- implement "Open" command on Android to open media directory
- use standard UI scaling, will make everything always fit
- TransparentBackground in gameviewoptions, gameviewfiles handling is too broad, this also catches clicks in dialog frame e.g. on caption.
- Turntable remove? We never really supported it fully.
- Android:
  `Models.Add('Downloading Network (X3D)', 'castle-data:/demo/needs_download_network_resources.x3dv');`
  fails - both with and without EnableBlockingDownloads.
  It should show model quickly without EnableBlockingDownloads.
  With EnableBlockingDownloads, it should be able to show model after download and play sounds.


## Internals

- Remake UI using editor (for now: gameviewfiles done)
- Upgrade all deprecated CGE usage

## iOS

- handle in CGE `apple_uniform_type_identifier` if needed (test on iOS)

      <!--
        Registered by Apple on system level:
        https://developer.apple.com/library/content/documentation/Miscellaneous/Reference/UTIRef/Articles/System-DeclaredUniformTypeIdentifiers.html#//apple_ref/doc/uid/TP40009259-SW1
        TODO: Ignored by CGE now, see TODO in https://castle-engine.io/project_manifest#_associated_file_types
      -->
      <apple_uniform_type_identifier>com.pkware.zip-archive</apple_uniform_type_identifier>
