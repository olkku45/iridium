# Potential modules for Iridium's stdlib

## Math functions

Math::sin(x)

Math::cos(x)

Math::tan(x)

Math::asin(x)

Math::acos(x)

Math::atan(x)

Math::atan2(x)

Math::exp(x)

Math::log(x, base)

Math::pi

Math::e

### Math::Matrix

Matrix::identity()
- enter dimensions

Matrix::rref(x)
- reduced row echelon form of x

Matrix::factorize(x)
- lu decomposition of x

Matrix::rotatecw(x)
- clockwise matrix rotation

Matrix::rotateccw(x)
- counterclockwise matrix rotation

Matrix::det(x)
- determinant of matrix

## Random-functions

Random::choice(sequence)

Random::norm()

Random::uniform(a, b)

Random::randint(a, b)

Random::randrange(start, stop, step)

## Collections

HashMap

Deque

Stack

LinkedList

## Warn - compiler warnings

Warn::bound  {}
- block used on a return value for a light lsp warning in the case that
the return value's memory isn't freed in the function

Warn::warn(string)
- generate a generic warning with given input at line

Warn::info(string)
- generate a hint/info at line

Warn::todo(string)
- generate a "TODO: ..." warning at line

Warn::fixme(string)
- generate a "FIXME: ..." warning at line

Warn::conditional(condition, string)
- generate a warning at line if condition is met before call in scope

### Potential additions:

Warn::suppress {}
- block used to disable compiler warnings in scope entirely

## Render

Render::image(values(="IMAGE_NAME.ff"), cmap (optional), cbar (optional), format (optional) ...)
- create image to render

Render::window(image)
- generate window with given input in window, taken from Render::image

Render::open(window)
- open the window generated with Render::window

more to come...

## String functions for UTF-8

String::strip(string, to_strip, mem_buffer_to_use)

String::count()
- return number of times given value appears in string

String::split()

String::replace()

String::join()

String::new(value)

String::from_bytes()

String::to_bytes()

String::is_empty()

String::get()
- return character at index

String::find()
- return first and/or last occurrence index

String::lines()

String::remove()

String::trim()
- trim leading and/or trailing whitespace

more to come...

### String::Packer

Packer::new()
- initialize new string packer using provided memory arena, 
set cursor at 0

Packer::pack()
- copies bytes of string into packer's buffer at current cursor pos, 
returns index of packed string. 

Packer::get()
- retrieves view into buffer for packed string at given index, 
returns vec2: (ptr, len)

Packer::reset()
- discard all packed strings in buffer, reset cursor, 
keep buffer allocated

Packer::find_all()
- scan packed strings, return indices where given string is found

Packer::stats()
- returns info about given packer

Packer::dump()
- prints packed strings and their metadata

## File

File::open()

File::close()

File::read()

File::readline()

File::readlines()

File::readall()

File::read_chunks()
- read chunks with given size into preallocated memory

File::write_chunks()
- writes in chunks from memory

## Noise

Noise::perlin()
- generate array of perlin noise values with specified array 
width and height

Noise::simplex()
- generate array of simplex noise

Noise::white()
- generate array of white noise

Noise::to_2d()
- convert array of noise to 2D

Noise::to_3d()
- convert array of noise to 3D

## Spatial

Spatial::query2d()

Spatial::query3d()

Spatial::query
- generalized query