```
// DISCLAIMER: there are no raw pointers in this demo, but they would be a part of 
// the language.

include::std::Random;
include::std::Math;
include::std::Collections::HashMap;
include::std::Render;
include::std::Warn;

comptime const SIZE = 10;
comptime const CELL_PIXELS = 4;

comptime const HEIGHT = SIZE * CELL_PIXELS;
comptime const WIDTH = SIZE * CELL_PIXELS;

allocator fn make_grid(): []vec2 {
    var grid: []vec2 = alloc((SIZE+1) * (SIZE+1));
	
	for (x: usize = 0; x < SIZE; x++) {
	    for (y: usize = 0; x < SIZE; y++) {
		    const coords: vec2 = (x, y);
		    insert(grid: coords);
		}
	}
	// the purpose of 'warn.bound' would be to give an lsp warning 
	// that the memory allocated for the return value wasn't freed
	Warn::bound { return grid };
}

allocator fn get_unit_vectors(grid: []vec2): HashMap {
	// this is just to showcase that we can allocate thread-locally, 
	// probably doesn't make practical sense
	var vectors: HashMap<vec2, vec2> = alloc_thread(4 * lengthof(grid));
	
	for (coordinates in grid) {
	    const angle = Random::uniform(0, 2*Math::pi);
		const vector: vec2 = (Math::cos(angle), Math::sin(angle));
		vectors[coordinates] = vector;
	}
		
	Warn::bound { return vectors };
}

allocator fn locate_corners(x: i32, y: i32): []vec2 {
	var points: []vec2 = alloc_pool(4 * sizeof(vec2));
	
	const top_left: vec2 = (x, y);
	const top_right: vec2 = (x + 1, y);
	const bottom_left: vec2 = (x, y + 1);
	const bottom_right: vec2 = (x + 1, y + 1);
	
	insert(points: top_left);
	insert(points: top_right);
	insert(points: bottom_left);
	insert(points: bottom_right);
	
	Warn::bound{ return points };
}

allocator fn vectors_from_corners_to_point(corners: []vec2, pixel_x: f32, pixel_y: f32) {
	var vectors: []vec2 = alloc_arena(4 * sizeof(vec2));
	
	for (corner in corners) {
	    const vector: vec2 = (pixel_x - corner[0], pixel_y - corner[1]);
		insert(vectors: vector);
	}
	
	Warn::bound { return vectors };
}

allocator fn compute_dot_products(unit_vectors: []vec2, ctp_vectors: []vec2) {
	var dot_products_list: []f32 = alloc_arena(4 * sizeof(vec2));
	
	assert(lengthof(unit_vectors) == lengthof(ctp_vectors));
	
	// with parallel iteration, we need to assign the result to a variable, then local return
	// from each iteration. these local returns get batched up into dot_products here.
	dot_products_list = parallel for (i: usize = 0; i < lengthof(unit_vectors); i++) {
	    var unit_vector: vec2 = unit_vectors[i];
		var ctp_vector: vec2 = ctp_vectors[i];
			
		var dot: f32 = Math::dot(unit_vector, ctp_vector);
		local return dot;
	}
	
	Warn::bound { return dot_products_list };
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
	var unit_vectors_list: []vec2 = alloc_arena(4 * sizeof(vec2));
	
	for (corner in corners) {
	    insert(unit_vectors_list: unit_vectors[corner]);
	}
	
	Warn::bound { return unit_vectors_list };
}

fn perlin_noise() {
    const grid = make_grid();
	const unit_vectors = get_unit_vectors(grid);
	
	// technically you could do an unlimited amount of dimensions
	// with this syntax, but the compiler will be optimized for < 5.
	var noise: [HEIGHT, WIDTH]f32 = alloc_arena(HEIGHT * WIDTH * sizeof(f32));
	
	/*
    '@-marking' not needed in this case, but here just to show it exists
	also just showing that you can do parallel iteration in batches
	for batches, you could also specify 'modes', like 'adaptive', where 
	batch size is selected based on processing time
	also 'min' batch marking: 'min: 100' and 'size: 1000' for instance.
	with this, the batches are of size 1000, but with less than 100 ('min' param) items left, 
	these 'leftovers' get put into the last batch, like so: 
	2070 items, batch(size: 1000, min: 100) -> [1000, 1070], if you need this kind
	of behavior.
	*/
	
	@outer noise = parallel for ((cell_x, cell_y) in grid, batch(size: 100)) {
	    if (cell_x == SIZE - 1 || cell_y == SIZE) {
		    continue;
		}
		const corners = locate_corners(cell_x, cell_y);
		const chosen_unit_vectors = choose_unit_vectors(corners, unit_vectors);
		
		for (i: usize = 0; i < CELL_PIXELS; i++) {
		    for (j: usize = 0; i < CELL_PIXELS; j++) {
				const x = cell_x + i / CELL_PIXELS;
				const y = cell_y + j / CELL_PIXELS;
				
				const ctp_vectors = vectors_from_corners_to_point(corners, x, y);
				const dot_products = compute_dot_products(chosen_unit_vectors, ctp_vectors);
				
				const noise_value = interpolate(dot_products, x, y, cell_x, cell_y);
				
				const y_idx: i32 = (y * CELL_PIXELS) as i32;
				const x_idx: i32 = (x * CELL_PIXELS) as i32;
				
				noise[y_idx, x_idx] = noise_value;
				
				// you can do:
				// @outer local return noise[y_idx, x_idx];
				// good for multiple nested parallel loops (when and if you have them)
			}
		}
	}
	
	const min_val: f32 = Math::min(noise);
	const max_val: f32 = Math::max(noise);
	
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
	
	Warn::bound { return noise };
}

fn main() {
    const noise_map = perlin_noise();
	const to_render = Render::image(values=noise_map, cmap="gray", cbar="on");
	const window = Render::window(to_render);
	Render::open(window);
	
	parallel free(noise_map);
}

```