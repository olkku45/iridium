```
// my lang example

// these would be built-in, not in libraries
// use std::arena;
// use math::{Vec3, Quat};

// Data-oriented struct - SoA by default
// mark as parallel-safe
psafe struct TransformSystem {
    positions: []vec3,
    rotations: []quat,
    scales: []vec3
}

// AoS
aos struct Entity {
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
alloc fn spawn_entity(world: *mut GameWorld, pos: vec3): u32 {
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

// SIMD operations built-in
simd fn update_physics(positions: []vec3, velocities: []vec3, dt: f32) {
    positions += velocities * dt;
}

// parallel iteration with safety guarantees
fn update_transforms(world: *mut GameWorld, dt: f32) {
    // safe to disjoint arrays
    parallel for (pos, vel) in zip(world.transforms.positions, world.velocities) {
        *pos += *vel * dt;
    }
    
    // safe: psafe annotation + same indices
    parallel for (i in 0..world.transforms.positions.len()) {
        const pos = &mut world.transforms.positions[i];
        const rot = &mut world.transforms.rotations[i];
		
        // possible to select .x/y/z if type=vec3 for example
        *rot = rot.slerp(quat.identity(), dt * 0.1);
        pos.y = math.max(pos.y, 0.0); // Ground clamp
		// alternative: pos.y = clamp() or pos.y.clamp()
    }
}

fn main() {
    var game_arena = arena.alloc(1024 * 1024); // 1MB arena
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
```