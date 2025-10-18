## User Visible

Lower priority:

- Add demo model with water

## Internals

Lower priority:

- Implement "zip with model" handling at CGE level, also in desktop castle-model-viewer

## Android-specific

Lower priority:

- We store in "Pictures" folder, not in "Photos" app.
    - What's recommended on modern Android and how to do it easily?
- Implement "Open" command on Android to open media directory

## iOS-specific

Lower priority:

- handle in CGE `apple_uniform_type_identifier`, though doesn't really seem needed in practice:

      <!--
        Registered by Apple on system level:
        https://developer.apple.com/library/content/documentation/Miscellaneous/Reference/UTIRef/Articles/System-DeclaredUniformTypeIdentifiers.html#//apple_ref/doc/uid/TP40009259-SW1
        TODO: Ignored by CGE now, see TODO in https://castle-engine.io/project_manifest#_associated_file_types
      -->
      <apple_uniform_type_identifier>com.pkware.zip-archive</apple_uniform_type_identifier>
