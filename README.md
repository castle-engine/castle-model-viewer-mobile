# view3dscene-mobile

Mobile-friendly viewer for 3D models like X3D, VRML, glTF, Collada, Wavefront OBJ, Spine JSON and [other formats supported by the Castle Game Engine](https://castle-engine.io/creating_data_model_formats.php).

In addition to the above formats, it also allows to open a ZIP file that contains a single model and associated media (like textures, sounds etc.). See the discussion below on how to _open your own files_ for details.

# Opening your own files

There are two ways to open your files:

1. Click on your file in an application like standard Android "Downloads" app. (Most other Android applications will also behave like that.)

    This will pass the file to _view3dscene-mobile_ through a mechanism that *does not* reveal the actual filename of the original file. That's nice for security, but...

    Since _view3dscene-mobile_ doesn't know the original filename, this approach will *fail* if you open this way a model (X3D, glTF or other format) that refers to some external resources (like textures or library or prototypes).

    This approach will only work reliably with self-contained model files (that don't refer to files through relative URLs). Examples that work:

    - ZIP file with a model (X3D, glTF, ..) and the associated data (textures etc.)
    - X3D/VRML without textures, or with textures embeded using X3D/VRML PixelTexture or "data" URI mechanism
    - glTF GLB variant
    - Models that refer to textures using http/https links etc

2. Click on your file in an application like [Total Commander for Android](https://play.google.com/store/apps/details?id=com.ghisler.android.TotalCommander), that can pass to _view3dscene-mobile_ the actual filename.

    This is the perfect approach, that works in all cases.
