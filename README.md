# PolygonArt / TriangleArt

![Example](data/demo.png)

Random vector-art generator based on splitting polygons/triangles into
sub-triangles. Written in Haskell λ.

It works by repeatedly connecting a pair of outer nodes of a polygon
with its center, such that a triangle is created. The process is
then recursively applied to the set of constructed polygons.

This started as a toy project and serves now as a training ground to
improve my Haskell skills.

## Prerequisites
You'll need the Haskell development tool [stack](https://haskellstack.org).

### Installation
Follow the official [installation guide](https://docs.haskellstack.org/en/stable/install_and_upgrade/) of stack.
If you use Arch Linux, the following command should do:

```console
pacman -S stack
```

## Building
```console
stack build
```

## Testing
```console
stack test
```

## Usage
You can run it with an arbitrary integer as seed for the random number generator:
```console
stack exec polygon <seed>
```

It will output the image in SVG format.
Here a complete example for generating and displaying an image on a graphical Linux installation:

```console
stack exec polygon 1337 > output.svg && xdg-open output.svg
```

## Documentation (Haddock)
```console
stack haddock
```

## Next milestones
* add random color
* integrate into a web service
* add more tests
