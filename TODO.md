## User Visible

High priority:

- use standard UI scaling, will make everything always fit
- Turntable remove? We never really supported it fully.
- needs_download_network_resources.x3dv fails with EnableBlockingDownloads -- download never finishes?

## Internals

High priority:

- Remake UI using editor (almost done now, viewpoints and top panel remain)
- Upgrade all deprecated CGE usage

Lower priority:

- Implement "zip with model" handling at CGE level, also in desktop castle-model-viewer

## Android-specific

Lower priority:

- We store in "Pictures" folder, not in "Photos" app.
    - What's recommended on modern Android and how to do it easily?
- Implement "Open" command on Android to open media directory

## iOS-specific

- Release on iOS (AppStore) too
  - donations welcome, to become iOS developer as CGE!

- Test iOS (we focused on Android testing in later iterations):
  - the ability to open custom ZIP, X3D, glTF
  - can we store pictures in Photo library

- handle in CGE `apple_uniform_type_identifier` if needed (test on iOS)

      <!--
        Registered by Apple on system level:
        https://developer.apple.com/library/content/documentation/Miscellaneous/Reference/UTIRef/Articles/System-DeclaredUniformTypeIdentifiers.html#//apple_ref/doc/uid/TP40009259-SW1
        TODO: Ignored by CGE now, see TODO in https://castle-engine.io/project_manifest#_associated_file_types
      -->
      <apple_uniform_type_identifier>com.pkware.zip-archive</apple_uniform_type_identifier>
