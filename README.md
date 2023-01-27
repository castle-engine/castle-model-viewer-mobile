# view3dscene-mobile

Mobile-friendly viewer for 3D models like X3D, VRML, glTF, Collada, Wavefront OBJ, Spine JSON and [other formats supported by the Castle Game Engine](https://castle-engine.io/creating_data_model_formats.php).

In addition to the above formats, it also allows to open a ZIP file that contains a single model and associated media (like textures, sounds etc.). See the discussion below on how to _open your own files_ for details.

# Opening your own files

There are two ways to open your files:

1. Click on your file in an application like standard Android "Downloads" app. (Most other Android applications will also behave like that.)

    This will pass the file to _view3dscene-mobile_ through a mechanism that *does not* reveal the actual filename of the original file.

     This approach will only work for _self-contained_ model files. Such files don't refer to other files (like textures) through relative URLs. Examples that work:

    - Models that don't need any additional media (like textures).
    - Models that refer to the additional media using http/https links.
    - ZIP file that contains a model (X3D, glTF, ...) and the associated media (textures etc.)
    - X3D/VRML with media embedded using [data URI scheme](https://en.wikipedia.org/wiki/Data_URI_scheme). Specifically for textures, you can also embed them using X3D/VRML `PixelTexture` node.
    - glTF GLB variant.

2. Click on your file in an application like [Total Commander for Android](https://play.google.com/store/apps/details?id=com.ghisler.android.TotalCommander), that can pass to _view3dscene-mobile_ the actual filename.

    This is the perfect approach, that works in all cases.
