```
// Iridium example

// changed pointer syntax to be simpler, compiler infers mutability

include::std::Math;

// SoA
// no marking as parallel-safe anymore
struct TransformSystem {
    positions: []vec3,
    rotations: []quat,
    scales: []vec3
}

// AoS
struct Entity {
    id: u32,
    active: bool,
    transform_index: u32
}

// Built-in allocator-aware containers
struct GameWorld {
    entities: array(Entity, arena),
    transforms: TransformSystem,
    velocities: []vec3
}

// mark functions that allocate
allocator fn spawn_entity(world: &GameWorld, pos: vec3): u32 {
    const id = lengthof(world.entities);
    
	// insert-function: append but as a function
	// could be 'put' as well.
    // arena allocation, have to free manually
	insert(world.entities: .{
	    .id = id,
        .active = true,
        .transform_index = lengthof(world.transforms.positions)
	});
    
    // SoA 'append'
	insert(world.transforms.positions: pos);
	insert(world.transforms.positions: quat::identity());
	insert(world.transforms.scales: vec3::one());
	insert(world.velocities: vec3::zero());
	
	// I imagine the memory was automatically freed at the end of scope before, 
	// so I just put this here for now at least.
	free(world.entities);
    
    Warn::bound { return id };
}

// SIMD operations default for array += another_array type operations
// regular CPU instructions for scalar operations
// these are both inferred by the compiler
// there is a possibilty for a simd block: simd{}
// compiler would infer which to use when
fn update_physics(positions: []vec3, velocities: []vec3, dt: f32) {
    positions += velocities * dt;
}

// parallel iteration with safety guarantees
fn update_transforms(world: &GameWorld, dt: f32) {
    // safe to disjoint arrays, changed pointer mutation syntax so
	// references in member ops get auto-deref'd
	// no need for 'local return' here, since we're doing in-place mutations
	parallel for (i: usize = 0; i < lengthof(world.velocities); i++) {
		world.transforms.positions[ı] += world.velocities[i] * dt;
	}
    
    // safe: same indices
	// also possible to do strict_parallel for (...) to guarantee parallel safety
	// also a --strict-parallel compile-time flag would be available
    parallel for (i: usize = 0; i < lengthof(world.transforms.positions); i++) {
        const pos = &world.transforms.positions[i];
        const rot = &world.transforms.rotations[i];
		
        // possible to select .x/y/z if type=vec3 for example
        rot = Math::slerp(quat::identity(), dt * 0.1);
        pos.y = Math::max(pos.y, 0.0); // clamp
    }
}

fn main() {
    // changed to scoped allocation block
	// also possible: var game_arena = arena.alloc(1024 * 1024); 1MB arena
    // defer game_arenaclear();
	// defer ensures cleanup when current scope exits
	
	// allocation scope or something :D
	alloc_scope arena(1024 * 1024) as game_arena {
    
		var world = GameWorld{
			.entities = array(Entity, game_arena)::alloc(),
			.transforms = TransformSystem{
			    // crude change for allocating like this without methods
				.positions = []vec3::alloc(game_arena),
				.rotations = []quat::alloc(game_arena),
				.scales = []vec3::alloc(game_arena)
			},
			.velocities = []vec3::alloc(game_arena)
		};
		
		// spawn some entities
		for (i: usize = 0, i <= 1000; i++) {
			const pos = vec3(
				i % 32 as f32,
				0.0,
				i / 32 as f32
			);
			const entity = spawn_entity(&world, pos);
		}
		
		// Game loop
		const dt = 1.0 / 60.0;
		
		while (true) {
			update_transforms(&world, dt);
			
			// static analysis can verify no allocations in hot loop
			// otherwise throws a compile-time error
			assert: no_alloc {
			update_physics(world.transforms.positions, world.velocities, dt);
			}
		}
	}
}

// convenience method: for instance, if you need to get a specific entity's transform,
// you'd use .get(index), returns a struct

// there would also be pool and thread-local allocators

/*
'comptime' syntax if something will be run at compile time

comptime block: 
comptime {
    const variable_thing1: i32 = 43446;
	const variable_thing2: i32 = 43447;
}

comptime keyword:
comptime const thingy: f32 = 3.14159;
comptime fn example_func(num: i32): i32 {
    // something
}
*/

/*
libraries for day 1 release:
more game dev/sim math operations, string handling, file I/O, asset loading, 
input handling, rendering bindings, audio,  profiling, ecs, networking, 
miscellaneous dx stuff, allocator utils, more collections (maybe built-in)
*/

// package manager "alloy" if we have a package manager

/*
possible to infer what imported module / library a function is from? like so:
instead of: 
include::std::Math;
Math::sin();
we have:
include::std::Math;
sin()
*/ 

// also zero-cost interop with C should be done
```