# Changelog

## v1.1.0 (2011-03-14)

Post-submission enhancements.

### Features

* Menu item to clear board (undoable)
* Viewpoint draggable
* Changed hover appearance of vertices (large and green instead of normal-sized
  but white)

### Enhancements

* Prefer to pick most recently placed vertex

### Bug fixes

* Hover vertex immediately after creation
* Clear hover when undoing vertex under pointer
* Block user from zooming in so far that floating-point error becomes visible
* Maintain delta from dragging pointer to vertex center during drag (otherwise
  center of vertex jumps to pointer)

## v1.0.0 (2011-02-23)

First public release, official submission as homework.

[Semantic versioning](http://semver.org/) starts at this version.

### Features

* Display control polygon, vertices, and Bézier curve
* Menu item to toggle display of control polygon and vertices when curve is
  present and at least cubic (with keybinding)
* Place and drag vertices with mouse (live update)
* Spinners to rotate and zoom canvas (live update)
* "Best fit" button to show linear or larger curve at greatest zoom without
  cropping
* Full undo/redo history (with keybindings) for vertices (and thus curve)
