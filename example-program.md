```
// Iridium example

// changed pointer syntax to be simpler, compiler infers mutability

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
    const id = world.entities.len();
    
    // arena allocation
    world.entities.append(.{
        .id = id,
        .active = true,
        .transform_index = world.transforms.positions.len()
    });
    
    // SoA append
    world.transforms.positions.append(pos);
    world.transforms.rotations.append(quat.identity());
    world.transforms.scales.append(vec3.one());
    world.velocities.append(vec3.zero());
    
    return id;
}

// SIMD operations default for array += another_array type operations
// regular CPU instructions for scalar operations
fn update_physics(positions: []vec3, velocities: []vec3, dt: f32) {
    positions += velocities * dt;
}

// parallel iteration with safety guarantees
fn update_transforms(world: &GameWorld, dt: f32) {
    // safe to disjoint arrays
    parallel for (pos, vel) in zip(world.transforms.positions, world.velocities) {
        *pos += *vel * dt;
    }
    
    // safe: same indices
	// also possible to do strict_parallel for (...) to guarantee parallel safety
	// also a --strict-parallel compile-time flag would be available
    parallel for (i in 0..world.transforms.positions.len()) {
        const pos = &world.transforms.positions[i];
        const rot = &world.transforms.rotations[i];
		
        // possible to select .x/y/z if type=vec3 for example
        *rot = rot.slerp(quat.identity(), dt * 0.1);
        pos.y = math.max(pos.y, 0.0); // Ground clamp
		// alternative: pos.y = clamp() or pos.y.clamp()
    }
}

fn main() {
    var game_arena = arena.alloc(1024 * 1024); // 1MB arena
	// defer ensures cleanup when current scope exits
    defer game_arena.clear();
    
    var world = GameWorld{
        .entities = array(Entity, game_arena).alloc(),
        .transforms = TransformSystem{
            .positions = []vec3.alloc(game_arena),
            .rotations = []quat.alloc(game_arena),
            .scales = []vec3.alloc(game_arena)
        },
        .velocities = []vec3.alloc(game_arena)
    };
    
    // Spawn some entities
    for (i in 0..1000) {
        const pos = vec3.new(
            i % 32 as f32,
            0.0,
            i / 32 as f32
        );
        _ = spawn_entity(&world, pos);
    }
    
    // Game loop
    const dt = 1.0 / 60.0;
	
    while (true) {
        update_transforms(&world, dt);
        
		// static analysis can verify no allocations in hot loop
		assert: no_alloc {
        update_physics(world.transforms.positions, world.velocities, dt);
		}
    }
}

// convenience method: for instance, if you need to get a specific entity's transform,
// you'd use .get(index), returns a struct

// libraries for day 1 release:
// more game dev/sim math operations, string handling, file I/O, asset loading, input handling, rendering/graphics, audio, 
// profiling, ecs, networking, miscellaneous dx stuff, allocator utils, more collections (maybe built-in)
```