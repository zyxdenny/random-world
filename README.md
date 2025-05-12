# Random terrain map generation using expression trees

> The project is still under development.
> This is just a demo of the feasibility of the idea by genrating images instead of terrain maps.
> Finally it will be integrated with game interface.

## Requirements
The [MaPLe compiler](https://github.com/MPLLang/mpl).

## Build
Run `make` builds the prpgram.

## Usage
After you have the executable, say `main`, the usage is as follows:
```
./main @mpl [mpl_args] -- -r [seed1] -g [seed2] -b [seed3] -d [depth] -o [path]
```
Explanations:
- `@mpl [mpl_args] --`: These are the [mpl runtime options](https://github.com/MPLLang/mpl?tab=readme-ov-file#running-a-program)
- `-r -g -b`: Provide seeds to generate expression trees for r, g, b channels. If not specified, seeds will be generated with `Time.now ()`.
- `-d`: Provide the maximum depth of the trees you want to generate. It should be between $[1, 25]$. Default 15.
- `-o`: Procide the output path. Default `img.ppx`.

The output file is a PPX format image.

For example,
```
./main @mpl procs 16 --
```
will generate a new random `img.ppx` every time the program is run.

