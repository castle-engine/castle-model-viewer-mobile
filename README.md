# Castle Model Viewer

_(Formerly known as `view3dscene-mobile`. We are in the process of renaming now.)_

Mobile-friendly viewer for 3D and 2D models like glTF, X3D, VRML, Collada, Wavefront OBJ, Spine JSON and [many other formats supported by the Castle Game Engine](https://castle-engine.io/creating_data_model_formats.php).

In addition to the above formats, it also allows to open a ZIP file that contains a single model and associated media (like textures, sounds etc.).

## Opening your own models

All supported files are automatically associated with _"Castle Model Viewer"_. Just open them from any application, for example from a web browser downloads, or a file browser like "My Files", "My Downloads" or ["Total Commander" for Android](https://play.google.com/store/apps/details?id=com.ghisler.android.TotalCommander).

[All model formats supported by the Castle Game Engine](https://castle-engine.io/creating_data_model_formats.php) are supported by this mobile _"Castle Model Viewer"_.

You can try it on models in [example_models](https://github.com/castle-engine/castle-model-viewer-mobile/tree/master/example_models) subdirectory: visit any of the models there, click the _"Download"_ icon and choose to open with _"Castle Model Viewer"_.

## Models must be self-contained

However, an important limitation is that the model file must be _self-contained_.

This means you cannot rely on model referring to other files (like textures) using relative URLs and placing these relative files alongside the main model file (in the same directory) will not work.

The reason behind this is that _"Castle Model Viewer"_ doesn't get the file path from the system, so it cannot resolve relative URLs. We only get file contents.

Examples that work:

- glTF GLB variant. This GLB version was specifically designed to "pack" everything into a single file.
- ZIP file that contains a model (X3D, glTF... -- [anything supported by Castle Game Engine](https://castle-engine.io/creating_data_model_formats.php)) and the associated media (textures etc.). We have implemented support for such ZIP files in _"Castle Model Viewer"_ exactly for this reason.
- X3D/VRML with media embedded using [data URI scheme](https://en.wikipedia.org/wiki/Data_URI_scheme). You can use [to-data-uri](https://github.com/castle-engine/castle-engine/tree/master/tools/to-data-uri) utility distributed as part of [Castle Game Engine](https://castle-engine.io/) to convert any media to data URI.
- X3D/VRML with textures embedded using X3D/VRML `PixelTexture` node. Though we recommend "data URI", it is more universal.
- Models that don't need any additional media (e.g. X3D models that just don't need textures to look reasonably).
- Models that refer to the additional media using http/https links.

    But you need to enable blocking downloads in the settings first. It is disabled by default, as the downloads are synchronous (blocking) for now -- there's no UI to interrupt a large download, you just have to wait for it to finish or kill the application.

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `terrain_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

## License

GNU GPL >= 2.0. See the file `LICENSE`.

## Authors

- Jan Adamec
- Michalis Kamburelis
- Many contributors to the [Castle Game Engine](https://castle-engine.io/) project.

3D demo models:
- cat_murdered_soul_suspect.glb:
    - From https://sketchfab.com/3d-models/cat-murdered-soul-suspect-836312def1b84e588866500a2bf79f0f
    - License:: CC Attribution
    - Author: mark2580 ( https://sketchfab.com/mark2580 )

- steampunk_underwater_explorer.glb
    - From https://sketchfab.com/3d-models/steampunk-underwater-explorer-127471a23e0f4790914b13b9052c4912
    - License:: CC Attribution
    - Author: Andrius Beconis ( https://sketchfab.com/abeconis )
