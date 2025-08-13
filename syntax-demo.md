# Demo for the syntax of iridium

Let's get some general things out of the way first. In iridium, we have a main function at the start of every program, similarly to languages like C.
You have to end every line of code with a semicolon, like in C.
Things like functions and loops go inside of curly brackets, instead of using a colon, like in Python. Iridium will be a low-level language, and it will 
have pointers, manual memory management, etc, with a static type system. The naming convention in Iridium will be snake_case. Also, there is 
type inference.

About indentation: for example, you should indent everything inside curly brackets by one tab, or four spaces.
 

## Comments
```
// this is a comment in iridium  
// you don't need a semicolon at the end of a comment
```
## Declaring variables

### Immutables
```
let a: int = 3;
let b: str = "My name goes here";
let c: bool = True;
let variable_name: f32 = 3.14;
let e: list<int> = [0,1,1,2,3,5];

// you don't necessarily need to declare variable types, they can be inferred
```
### Mutables
```
let mut temperature: f32 = 20.4
```
## Conditionals
```
if (i_am_human == True) {
  // something here
}	

if (i_am_human) {
  // this is the same thing as above
}

if (i_am_human == False) {
  // something
}

if (!i_am_human) {
  // same as above
}

elif (i_am_a_dog) {
  // something
}

else {
  // something
}
```
## Functions
```
fn example_function() {
  let number: int = 42;
	
  if (number == 42) {
    println("This is everything!");
  }
	
  else {
    println("This must not be true!");
  }
}
```
## Expressions 
```
5 + 4
23 - 32
44 * 33
65 / 5
```
## Loops 
```
for (i in 0..10) {
    // range operator, end is exclusive (0 to 9)
    // do something
}

for (i in 0..=10) {
    // range operator, with end inclusive (0 to 10)
}

while (game_running) {
    // do something
}

for (thing in things) {
    // do something
}
```
## Arrays
```
// 1D-arrays:

// fixed size array: 
let fixed_arr: array<int> = [1, 2, 3, 4];

// dynamic size array:
let dynamic_arr: list<int> = [1, 2, 3, 4];

// 2D-arrays:

let mat2_first: mat2 = matrix::mat2_identity();
let mat2_second: mat2 = [[1,2], [3, 4]];
let mat2_third = matrix.mat2(imagine_some_numbers_here);
```
## Game specific stuff (ideas)

### Loops
```
/*
with the 'loop' keyword, you have to provide arguments for the loop,
like in functions, examples below.
*/

loop (step=frame.fixed(60), args=(delta)) {
    // do things 60 times per second
	// delta = dt
	// 'frame' would be built-in
}

loop (step=frame.variable(), args=(delta, alpha)) {
    // do things based on varying game FPS
	// delta = dt, alpha = interpolation factor
}

loop (step=time.sec(2)) {
    // do things every 2 seconds
}

parallel for (thing in things) {
    // parallelized for-iteration (when safe)
}

loop (step=batch.process(entities, size=64), args=(batch)) {
    // do something in batches (size indicated by size=64)
}

for (thing in spatial.query3d(radius=10.0, center=player_pos)) {
    // only for things in spatial radius
	// 'spatial' built-in, query3d being 3D spatial query
}

loop (step=spatial.query3d(radius=10.0, center=player_pos), args=(entity)) {
    // do something with entity in radius
}

loop (step=spatial.query_x(radius=10.0, center=player_pos), args=(entity)) {
    // do something with entity in specified radius of x-axis
}

loop (step=budget.limited(entities, max_time=1), args=(entity)) {
    // budget computing to at most iteration / max_time (ms)
	// if not possible, throw error or something
}
```
### Vector operations
```
let a: vec3 = (1.0, 2.0, 3.0);
let b: vec3 = (4.0, 5.0, 6.0);

let vector_sum = a + b;
let scaled = a * 2.0;
let magnitude = |a|;
let dot_product = dot(a, b);
let cross_product = cross(a, b);
```
### Matrix operations
```
let a: mat2 = ([1, 2], [3, 4]);
let b: mat2 = ([5, 6], [7, 8]);
let c: vec2 = (1, 7);

let matrix_sum = a + b;
let scaled = a * 4;
let multiplied = a * b;
let transpose = transpose(a);
let inverse = inverse(a);
let translated = a * c;

// stdlib juttuja:
let identity_matrix = mat2::identity();
let reduced_row_echelon_form = rref(a);
let lu_decomposition = factorize(b);
let clockwise_rotation = rotatecw(a);
let counterclockwise_rotation = rotateccw(b);
let determinant = det(a);
```
### Interpolation
```
let a: array = [13, 14];
let b: array = [15, 16];

let linear_interpolation = lerp(a, b, 0.5);
let spherical_lerp = slerp(a, b, c, 0.5); // imagine a and b are 3d points ok
let sstep = smoothstep(0.1, 0.2, x);
let sstep2 = smootherstep(0.3, 0.4, x);
```
### Noise
```
let perlin_noise = noise::perlin(seed=42);
let perlin_noise_map = perlin_noise.to3d(800, 800, 800);

let simplex_noise = noise::simplex(seed=43);
let simplex_noise_map = simplex_noise.to2d(800, 800);

let white_noise = noise::white(seed=1);
let white_noise_map = white_noise.to2d(800, 800);
```