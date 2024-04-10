# view3dscene-mobile

Mobile-friendly viewer for 3D models like X3D, VRML, glTF, Collada, Wavefront OBJ, Spine JSON and [other formats supported by the Castle Game Engine](https://castle-engine.io/creating_data_model_formats.php).

In addition to the above formats, it also allows to open a ZIP file that contains a single model and associated media (like textures, sounds etc.). See the discussion below on how to _open your own files_ for details.

# Opening your own files

There are two ways to open your files:

1. Click on any supported file in an application like standard Android _"Downloads"_ app. (Most other Android applications will also behave like that.)

    This will pass the file to _view3dscene-mobile_.

    An important limitation: This approach will only work for _self-contained_ model files. Such files cannot refer to other files (like textures) using relative URLs. The reason behind this is that _view3dscene-mobile_ doesn't get the file path from the Android system, so it cannot resolve relative URLs. We only get file contents.

    Examples that work:

    - glTF GLB variant.
    - ZIP file that contains a model (X3D, glTF, ...) and the associated media (textures etc.)
    - X3D/VRML with media embedded using [data URI scheme](https://en.wikipedia.org/wiki/Data_URI_scheme). You can use [to-data-uri](https://github.com/castle-engine/castle-engine/tree/master/tools/to-data-uri) utility distributed as part of [Castle Game Engine](https://castle-engine.io/) to convert any media to data URI.
    - X3D/VRML with textures embedded using X3D/VRML `PixelTexture` node. Though we recommend "data URI", it is more universal.
    - Models that don't need any additional media (like textures).
    - Models that refer to the additional media using http/https links.

    Use the `example_models` directory for test models that work with this approach.

2. Click on your file in an application like [Total Commander for Android](https://play.google.com/store/apps/details?id=com.ghisler.android.TotalCommander). This passes to _view3dscene-mobile_ the actual filename.

    This works for all cases.

    Note that _view3dscene-mobile_ must have permissions to read from given external storage location. Make sure it is so, in the Android settings, under _Applications_ -> _view3dscene_ -> _Permissions_ -> _Storage_.
