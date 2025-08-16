```
// DISCLAIMER: there are no raw pointers in this demo, but they would be a part of 
// the language.

include::std::Random;
include::std::Math;
include::std::Collections::HashMap;
include::std::Render;

comptime const SIZE = 10;
comptime const CELL_PIXELS = 4;

comptime const HEIGHT = SIZE * CELL_PIXELS;
comptime const WIDTH = SIZE * CELL_PIXELS;

allocator fn make_grid(): []vec2 {
	var grid: []vec2;
    grid = alloc((SIZE+1) * (SIZE+1));
	
	for (x: usize = 0; x < SIZE; x++) {
	    for (y: usize = 0; x < SIZE; y++) {
		    const coords: vec2 = (x, y);
		    grid.append(coords);
		}
	}
	// the purpose of 'warn.bound' would be to give an lsp warning 
	// that the memory allocated for the return value wasn't freed
	warn.bound { return grid };
}

allocator fn get_unit_vectors(grid: []vec2): HashMap {
    const grid_size = sizeof(grid)
	
	const  vec_mem = alloc_thread(4 * grid.len());
	var vectors = HashMap<vec2, vec2>(use=vec_mem);
	
	for (coordinates in grid) {
	    const angle = Random.uniform(0, 2*Math.pi);
		const vector: vec2 = (Math.cos(angle), Math.sin(angle));
		vectors[coordinates] = vector;
	}
		
	warn.bound { return vectors };
}

allocator fn locate_corners(x: i32, y: i32): []vec2 {
    var points: []vec2;
	points = alloc_pool(4 * vec2.size());
	
	const top_left: vec2 = (x, y);
	const top_right: vec2 = (x + 1, y);
	const bottom_left: vec2 = (x, y + 1);
	const bottom_right: vec2 = (x + 1, y + 1);
	
	points.append(top_left);
	points.append(top_right);
	points.append(bottom_left);
	points.append(bottom_right);
	
	warn.bound{ return points };
}

allocator fn vectors_from_corners_to_point(corners: []vec2, pixel_x: f32, pixel_y: f32) {
    var vectors: []vec2;
	vectors = alloc_arena(4 * vec2.size());
	
	for (corner in corners) {
	    const vector: vec2 = (pixel_x - corner[0], pixel_y - corner[1]);
		vectors.append(vector);
	}
	
	warn.bound { return vectors };
}

allocator fn compute_dot_products(unit_vectors: []vec2, ctp_vectors: []vec2) {
    var dot_products: []f32;
	dot_products = alloc_arena(4 * vec2.size());
	
	// old version:
	/*
	for (unit_vector; vec2; ctp_vector: vec2;) in zip(unit_vectors: [}vec2;  ctp_vectors: []vec2;) {
	    const dot: f32 = Math.dot(unit_vector, ctp_vector);
		dot_products.append(dot);
	}
	*/
	
	// new 'lower level' version:
	assert(unit_vectors.len() == ctp_vectors.len());
	
	parallel for (i: usize = 0; i < unit_vectors.len(); i++) {
	    var unit_vector: vec2 = unit_vectors[i];
		var ctp_vector: vec2 = ctp_vectors[ı];
			
		var dot: f32 = Math.dot(unit_vector, ctp_vector);
		dot_products.append(dot, mode="thread");
	}
	
	warn.bound { return dot_products };
}

fn easing_func(x) {
    return 6 * pow(x, 5) - 15 * pow(x, 4) + 10 * pow(x, 3);
}

fn interpolate(dot_products: []f32, pixel_x: f32, pixel_y: f32, cell_x: i32, cell_y: i32) {
    const x = easing_func(pixel_x - cell_x);
	const y = easing_func(pixel_y - cell_y);
	
	const m1 = dot_products[0] + x * (dot_products[1] - dot_products[0]);
	const m2 = dot_products[2] + x * (dot_products[3] - dot_products[2]);
	const m3 = m1 + y * (m2 - m1);
	
	return m3;
}

allocator fn choose_unit_vectors(corners: []vec2, unit_vectors: []vec2) {
    var unit_vectors_list: []vec2;
	unit_vectors_list = alloc(4 * vec2.size());
	
	for (corner: vec2; in corners: [}vec2;) {
	    unit_vectors_list.append(unit_vectors[corner]);
	}
	
	warn.bound { return unit_vectors_list };
}

fn perlin_noise() {
    const grid = make_grid();
	const unit_vectors = get_unit_vectors(grid);
	
	// technically you could do an unlimited amount of dimensions
	// with this syntax, but the compiler will be optimized for < 5.
	var noise: [HEIGHT, WIDTH]f32;
	noise = alloc(HEIGHT * WIDTH * f32.size());
	
	parallel for ((cell_x, cell_y) in grid) {
	    if (cell_x == SIZE - 1 || cell_y == SIZE) {
		    continue;
		}
		const corners = locate_corners(cell_x, cell_y);
		const chosen_unit_vectors = choose_unit_vectors(corners, unit_vectors);
		
		parallel for (i: usize = 0; i < CELL_PIXELS; i++) {
		    parallel for (j: usize = 0; i < CELL_PIXELS; j++) {
				const x = cell_x + i / CELL_PIXELS;
				const y = cell_y + j / CELL_PIXELS;
				
				const ctp_vectors = vectors_from_corners_to_point(corners, x, y);
				const dot_products = compute_dot_products(chosen_unit_vectors, ctp_vectors);
				
				const noise_value = interpolate(dot_products, x, y, cell_x, cell_y);
				
				const y_idx: i32 = (y * CELL_PIXELS) as i32;
				const x_idx: i32 = (x * CELL_PIXELS) as i32;
				// formula for converting 2D to 1D: y * width * x, use if we have
				// var noise: []f32 -declaration instead
				// noise[y_idx + WIDTH + x_idx] = noise_value;
				// alternatively:
				noise[y_idx, x_idx] = noise_value;
			}
		}
	}
		
	const min_val: f32 = Math.min(noise);
	const max_val: f32 = Math.max(noise);
	
	noise = (noise - min_val) / (max_val - min_val) * 255;
	
	// free all memory in scope
	free {
	    grid, unit_vectors.buffer,
		unit_vectors, corners,
		ctp_vectors, chosen_unit_vectors
	};
	// you'd have to do 'parallel free' to free memory of values computed in parallel
	parallel free(dot_products);
	
	/*
	alternatively:
	free(grid);
	free(unit_vectors.buffer);
	free(unit_vectors);
	free (corners);
	free(ctp_vectors);
	free(dot_products);
	free(chosen_unit_vectors);
	*/
	
	warn.bound { return noise };
}

fn main() {
    const noise_map = perlin_noise();
	const to_render = Render.image(values=noise_map, cmap="gray", cbar="on");
	const window = Render.window(to_render);
	Render.open(window);
	
	parallel free(noise_map);
}

```